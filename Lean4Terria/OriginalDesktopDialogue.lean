import Lean4Terria.Desktop
import Lean4Terria.OriginalForestChatLayout
import Lean4Terria.OriginalForestOldManCurse
import Lean4Terria.OriginalSession
import Lean4Terria.OriginalCombatText

/-! The resident dialogue panel and its runtime Terraria XNB fonts. -/

namespace Lean4Terria.OriginalDesktopDialogue

open OriginalForestChatLayout (Font Input Output)

@[extern "terria_original_chat_font"] private opaque fontRows (itemStack : Bool) : IO String
@[extern "terria_original_combat_font"] private opaque combatFontRows (critical : Bool) : IO String

@[extern "terria_original_chat_sprite"] private opaque sprite (asset : @& String)
    (sourceX sourceY sourceW sourceH destX destY destW destH originX originY sine cosine : Float32)
    (color effects : UInt32) : IO Unit

structure Renderer where
  mouseText : Font
  itemStack : Font
  combatText : OriginalCombatText.Fonts

private def loadFont (name : String) (itemStack : Bool) : IO Font := do
  let font := Font.parse name (← fontRows itemStack)
  if font.pages.isEmpty then throw (IO.userError s!"Could not read the original Terraria {name}.xnb font.")
  return font

def Renderer.load : IO Renderer := do
  let mouseText ← loadFont "Fonts/Mouse_Text" false
  let itemStack ← loadFont "Fonts/Item_Stack" true
  let normal := Font.parse "Fonts/Combat_Text" (← combatFontRows false)
  let critical := Font.parse "Fonts/Combat_Crit" (← combatFontRows true)
  if normal.pages.isEmpty || critical.pages.isEmpty then
    throw (IO.userError "Could not read the original Terraria combat text fonts.")
  return { mouseText, itemStack, combatText := {normal,critical} }

def Renderer.layout (renderer : Renderer) (input : Input) : Except String Output :=
  OriginalForestChatLayout.drawPanel renderer.mouseText renderer.itemStack input

def drawSprites (sprites : Array OriginalForestChatLayout.Sprite) (scale : Float32 := 1) : IO Unit := do
  Desktop.premultipliedAlpha true
  try
    for s in sprites do
      sprite s.tex s.sourceX s.sourceY s.sourceW s.sourceH
        (s.destX * scale) (s.destY * scale) (s.destW * scale) (s.destH * scale)
        s.originX s.originY s.rotSin s.rotCos s.color s.effects.toUInt32
  finally Desktop.premultipliedAlpha false

def draw (output : Output) (scale : Float32) : IO Unit := drawSprites output.sprites scale

namespace Talk

open OriginalForestGuideDialogue (Npc ChatWorld HelpWorld HelpPlayer)
open OriginalForestGuideDialogue renaming State → TalkState, Player → TalkPlayer
open OriginalSession renaming State → Session

def player (session : Session) (mouseInterface tileInteraction : Bool := false) : TalkPlayer :=
  let body := session.player.body
  { body := ⟨body.position.x, body.position.y, body.width, body.height⟩
    mouseInterface, tileInteractionHappened := tileInteraction, dead := session.health.dead
    chest := if session.openStorage.isSome then 0 else -1 }

def npcs (session : Session) : Array Npc :=
  session.combat.actors.zipIdx.filterMap fun (actor, index) => do
    if let some n := actor.resident? then
      let (_, _, frameWidth, frameHeight) := n.textureFrame
      return {
        index, active := n.active, type := n.kind.typeId.toNat, townNPC := true
        body := ⟨n.position.x, n.position.y, n.width, n.height⟩, frameWidth, frameHeight, aiStyle := 7, ai0 := n.aiValue }
    let bound ← actor.boundMechanic?
    let n := bound.body
    return { index, active := n.active, type := OriginalDungeonMechanic.boundType
             body := ⟨n.position.x, n.position.y, n.width, n.height⟩, frameWidth := 42, frameHeight := 34 }

def resident (session : Session) (state : TalkState) : Option OriginalForestGuide.Resident := do
  guard (state.talkNPC ≥ 0)
  let n ← session.combat.actors[state.talkNPC.toNat]?.bind OriginalCombat.Actor.resident?
  guard n.active
  return n

def time (session : Session) : OriginalForestOldManCurse.Time :=
  { dayTime := session.dayTime, remixWorld := session.terrain.header.remixWorld }

def hover (session : Session) (input : OriginalForestGuideDialogue.Input) (pointer : TalkPlayer)
    (state : TalkState) : OriginalForestGuideDialogue.Result :=
  let header := session.terrain.header
  let world : ChatWorld := {
    dayTime := session.dayTime, raining := header.raining, hardMode := header.hardMode
    bloodMoon := header.bloodMoon, eclipse := header.eclipse, slimeRain :=
      ((session.forest.map (·.slimeRainTime)).getD header.slimeRainTime) > 0
    taxCollector := session.combat.actors.any fun n => n.active && n.typeId == 441 }
  let stats : OriginalForestOldManCurse.PlayerStats := { lifeMax := session.health.statLifeMax, defense := 0 }
  let (table, cursor) := session.outerRandom.state
  let otherChat := fun (n : Npc) (rng : OriginalForestGuideDialogue.Rng) => do
    if n.type == OriginalDungeonMechanic.boundType then return (.legacy 156, rng)
    if let some resident := session.combat.actors[n.index]?.bind OriginalCombat.Actor.resident? then
      if resident.kind == .mechanic && resident.homeless then
        let (roll, rng) := rng.next 4
        return (.legacy (157 + roll), rng)
    throw s!"GetChat for type {n.type}"
  OriginalForestOldManCurse.hover input pointer (npcs session) (time session) stats ⟨table, cursor⟩ state world otherChat

def closeIfOutOfReach (session : Session) (state : TalkState) : TalkState × OriginalForestGuideDialogue.Effects :=
  if session.health.dead && state.talkNPC ≥ 0 then OriginalForestGuideDialogue.closeButton state
  else OriginalForestGuideDialogue.updateClose 5 3 (player session) (npcs session) state

private def helpItem (stack : Option OriginalItems.Stack) : OriginalForestGuideDialogue.Item :=
  let type := (stack.map (·.item)).getD 0
  { type := type.toNat
    pick := if type == OriginalItems.copperPickaxe then 35 else 0
    axe := if type == OriginalItems.copperAxe then 7 else 0
    hammer := if (OriginalWallWork.Tables.hammerStats type).isSome then 1 else 0 }

def help (session : Session) (hasText : String → Bool) (state : TalkState) :
    Except String (TalkState × OriginalForestGuideDialogue.Effects) :=
  let header := session.terrain.header
  let player : HelpPlayer := {
    inventory := session.carriedItems.map helpItem, statLifeMax := session.health.statLifeMax
    statManaMax := session.health.statManaMax, difficulty := if header.gameMode == 3 then 3 else 0 }
  let world : HelpWorld := {
    hardMode := header.hardMode, crimson := header.crimson, shadowOrbSmashed := header.shadowOrbSmashed
    downedBoss1 := header.downedEye, downedBoss2 := header.downedEvilBoss, downedBoss3 := session.downedSkeletron
    downedQueenBee := header.downedQueenBee, downedPlantBoss := header.downedPlantBoss
    downedGolemBoss := header.downedGolem, downedGoblins := header.downedGoblins
    npcs := session.combat.actors.filterMap fun n =>
      if n.active then some (n.typeId.toNat, n.resident?.isSome) else none }
  OriginalForestGuideDialogue.helpButton player world (fun n => hasText s!"GuideHelpText.Help_{n}") state

end Talk

end Lean4Terria.OriginalDesktopDialogue
