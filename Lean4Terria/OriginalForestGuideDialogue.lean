import Lean4Terria.OriginalGreenSlime

/-!
# Guide right-click and Help (Terraria 1.4.5.8 desktop, Classic, single player): input and dialogue state

A hand transcription of the evidenced, widget-free part of talking to the Guide, checked against the vanilla
assembly run in a private host (`.lake/original-forest-guide-dialogue/1.4.5.8`; the dialogue methods of
`TerrariaServer.exe` are byte-identical to the desktop `Terraria.exe` apart from generic method-spec token numbers).

IL read: `Main.DrawMouseOver` (the 1 x 1 mouse rectangle), `Main.HoverOverNPCs`, `TileReachCheckSettings.Simple`
(`GetRanges`, `GetTileRegion`, `GetWorldRegion`), `Player.SetTalkNPC`, `Player.dropItemCheck` (its `noThrow` head),
`NPC.GetChat` (the census and the Guide branch), `NPC.HasSpecialEventText`, `Main.HelpText`, `LocalizedText.GetValueIfConditionsMet`, the talk-close block of
`Player.Update` (`IL_6ced` … `IL_6dc6`), `GuideTip.Interact`, `CloseChat.Interact`, `Main.CloseNPCChatOrSign`.

## Where it runs, and which `Main.rand`

The right-click that starts a chat (`HoverOverNPCs`) and the chat panel's buttons (`NPCChatPanel.DrawButtons` →
`NPCInteraction.Interact`: `GuideTip` → `Main.HelpText`, `CloseChat` → `CloseNPCChatOrSign`) run in the draw pass
(`DoDraw` → interface layers 39 / 23). `DoDraw` swaps no generator, and every update phase restores the previous
`Main.rand` when its `SwapRandom` scope ends, so `GetChat` draws from the base `Main.rand` that is live outside
`DoUpdateInWorld`, not from a phase stream (`UpdatePlayers` etc.). `HelpText` and the hover tests draw nothing. The
out-of-range close is in `Player.Update` (the `UpdatePlayers` phase) and draws nothing. `Main.DrawNPCChatButtons` has
no caller in 1.4.5.8.

## Not transcribed (`unsupported`)

`GetChat` for any NPC but the Guide, a pending conditional dialogue (`nextDialogue`), the special event texts
(graveyard, party, rain, windy day, storm: `Language.SelectRandom` draws), hovering the Elder Slime (685) or the
other special hover shapes (87 … 92, 677, 695, 696), smart interact, gamepad input; and every widget effect
(`IngameUIWindows.CloseAll`, `dropItemCheck`, sounds, portrait hop, bestiary chat registration, shopping settings)
which is reported in `Effects` instead of performed.
-/

namespace Lean4Terria.OriginalForestGuideDialogue

/-- The generator `GetChat` draws from (the base `Main.rand` of the draw pass). -/
abbrev Rng := OriginalGreenSlime.Rng

/-- `Microsoft.Xna.Framework.Rectangle`. -/
structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving BEq, Repr

/-- `Rectangle.Intersects`. -/
def Rect.intersects (a b : Rect) : Bool :=
  b.x < a.x + a.w && a.x < b.x + b.w && b.y < a.y + a.h && a.y < b.y + b.h

/-- What `Main.npcChatText` holds, by its source; the client resolves it to text (`Lang.dialog` substitutes the
player name etc.). -/
inductive Line where
  | none
  /-- `Lang.dialog(n)`: `LegacyDialog.n`. -/
  | legacy (n : Nat)
  /-- `Language.GetTextValue(key)`. -/
  | key (k : String)
  deriving BEq, Repr, Inhabited

/-- The localization key of a line (`"-"` for the empty text). -/
def Line.toKey : Line → String
  | .none => "-"
  | .legacy n => s!"LegacyDialog.{n}"
  | .key k => k

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def trunc (f : Float32) : Int := f.toInt32.toInt

/-- A hitbox owner: `position`, `width`, `height`. -/
structure Body where
  x : Float32
  y : Float32
  width : Int
  height : Int
  deriving Repr

/-- `Entity.Hitbox`. -/
def Body.hitbox (b : Body) : Rect := ⟨trunc b.x, trunc b.y, b.width, b.height⟩

/-- `TileReachCheckSettings.Simple.GetWorldRegion(player, 0)`: `Player.tileRangeX / Y` capped at 20 tiles, around the
tiles the body covers, as a world rectangle whose right / bottom edge is the last pixel of the last tile. -/
def reachRegion (tileRangeX tileRangeY : Int) (p : Body) : Rect :=
  let rx := if tileRangeX > 20 then 20 else tileRangeX
  let ry := if tileRangeY > 20 then 20 else tileRangeY
  let x0 := trunc (p.x / 16) - rx
  let x1 := (((p.x + single p.width) / 16).toFloat.ceil).toInt64.toInt - 1 + rx
  let y0 := trunc (p.y / 16) - ry
  let y1 := (((p.y + single p.height) / 16).toFloat.ceil).toInt64.toInt - 1 + ry
  ⟨x0 * 16, y0 * 16, x1 * 16 + 15 - x0 * 16, y1 * 16 + 15 - y0 * 16⟩

/-- The world mouse rectangle `Main.DrawMouseOver` builds: `(int)(MouseScreen + screenPosition)`, 1 x 1 (normal
gravity: `ReverseGravitySupport` changes nothing). -/
def mouseRect (worldX worldY : Float32) : Rect := ⟨trunc worldX, trunc worldY, 1, 1⟩

/-- An NPC slot as `HoverOverNPCs` reads it. `frameWidth / frameHeight` is the draw frame (`NPC.frame`; on the desktop
the texture frame, the Guide 40 x 56 from `NPC_22.xnb` 40 x 1456 / 26). -/
structure Npc where
  index : Nat
  active : Bool := true
  type : Nat
  townNPC : Bool := false
  body : Body
  frameWidth : Int
  frameHeight : Int
  shimmerTransparent : Bool := false
  aiStyle : Int := 0
  ai0 : Float32 := 0
  ai3 : Float32 := 0
  deriving Repr

/-- The draw-frame rectangle the mouse is tested against (`Bottom` minus half the frame width, frame height up). -/
def Npc.hoverRect (n : Npc) : Rect :=
  let bx := trunc (n.body.x + single (n.body.width.tdiv 2))
  let by' := trunc (n.body.y + single n.body.height)
  ⟨bx - n.frameWidth.tdiv 2, by' - n.frameHeight, n.frameWidth, n.frameHeight⟩

/-- The local player's fields the interaction reads. -/
structure Player where
  body : Body
  dead : Bool := false
  stinky : Bool := false
  mouseInterface : Bool := false
  tileInteractionHappened : Bool := false
  /-- `ownedProjectileCounts[651]`. -/
  ownedProjectile651 : Int := 0
  /-- `player.chest` (-1: none open). -/
  chest : Int := -1
  /-- `tileEntityAnchor.InUse`. -/
  anchorInUse : Bool := false
  deriving Repr

/-- The dialogue state this module owns. -/
structure State where
  /-- `player.talkNPC`. -/
  talkNPC : Int := -1
  /-- `Main.npcChatText`. -/
  text : Line := .none
  /-- `Main.npcChatRelease` (set again by input handling once the right button is up). -/
  npcChatRelease : Bool := true
  /-- `Main.helpText`. -/
  helpText : Int := 0
  /-- `Main.npcChatCornerItem`. -/
  cornerItem : Int := 0
  deriving BEq, Repr, Inhabited

/-- Widget / other-system effects a step asks for (performed by the caller). -/
structure Effects where
  /-- MouseOver's tooltip branch persistently clears only this player field.
  This is independent of talk eligibility and preserves the icon ID/text/push. -/
  clearCursorItemIcon : Bool := false
  /-- `Main.HoveringOverAnNPC = true`, `currentNPCShowingChatBubble = i`. -/
  hovering : Option Nat := none
  /-- `player.noThrow` written (4 while hovering an eligible NPC). -/
  noThrow : Option Int := none
  /-- `IngameUIWindows.CloseAll(true)` then `player.dropItemCheck()` before the chat opens (`dropItemCheck` starts with
  `dropItemCheckNoThrow`; the rest of it handles the mouse / guide / reforge items and is the caller's). -/
  closeWindows : Bool := false
  /-- `SoundEngine.PlaySound(type)` (24 open, 12 help, 11 close). -/
  sound : Option Nat := none
  /-- `SetTalkNPC(i)` side effects: bestiary chat registration, shopping settings, portrait. -/
  talkedTo : Option Nat := none
  /-- `Main.DoNPCPortraitHop()`. -/
  portraitHop : Bool := false
  deriving Repr, Inhabited

/-! ## `NPC.GetChat` for the Guide -/

/-- World and player state the Guide's `GetChat` reads. -/
structure ChatWorld where
  dayTime : Bool := true
  bloodMoon : Bool := false
  eclipse : Bool := false
  slimeRain : Bool := false
  hardMode : Bool := false
  lanternsUp : Bool := false
  downedMoonlord : Bool := false
  /-- A Tax Collector (441) is active (the census local 16). -/
  taxCollector : Bool := false
  /-- `NPC.HasSpecialEventText` preconditions: graveyard, party, rain (not storming), happy windy day, storm. -/
  zoneGraveyard : Bool := false
  partyIsUp : Bool := false
  raining : Bool := false
  storming : Bool := false
  happyWindyDay : Bool := false
  /-- `npc.nextDialogue != null` (a pending conditional dialogue). -/
  pendingDialogue : Bool := false
  deriving Repr, Inhabited

/-- `NPC.GetChat()` for `type == 22`: `(line, Main.rand after)`. -/
def guideChat (w : ChatWorld) (rng : Rng) : Except String (Line × Rng) := do
  if w.pendingDialogue then throw "GetChat: ConditionalDialogue.GetChatAndClearCondition"
  if w.zoneGraveyard || w.partyIsUp || (w.raining && !w.storming) || w.happyWindyDay || w.storming then
    throw "GetChat: HasSpecialEventText (Language.SelectRandom)"
  if w.bloodMoon then
    let (r, rng) := rng.next 3
    return (.legacy (170 + r), rng)
  if w.lanternsUp && !w.downedMoonlord then return (.key "GuideSpecialText.Lantern1", rng)
  if w.lanternsUp && w.downedMoonlord then return (.key "GuideSpecialText.Lantern2", rng)
  if w.eclipse then return (.key "GuideSpecialText.Eclipse", rng)
  if w.slimeRain then return (.key "GuideSpecialText.SlimeRain", rng)
  if !w.dayTime then return (.legacy 173, rng)
  let mut g := rng
  if w.hardMode && w.taxCollector then
    let (r, g') := g.next 8
    g := g'
    if r == 0 then return (.key "GuideChatter.Chatter_1", g)
  if w.hardMode then
    let (r, g') := g.next 8
    g := g'
    if r == 0 then return (.key "GuideChatter.Chatter_2", g)
  let (r, g') := g.next 3
  return (.legacy (174 + r), g')

/-! ## Right-click: `Main.HoverOverNPCs` -/

/-- The mouse and frame input of one `HoverOverNPCs` call. -/
structure Input where
  mouse : Rect
  mouseRight : Bool
  tileRangeX : Int := 5
  tileRangeY : Int := 3
  smartInteractNPC : Option Nat := none
  usingGamepad : Bool := false
  /-- An earlier MouseOver/UI branch has already claimed Main.mouseText. -/
  mouseText : Bool := false
  deriving Repr

structure Result where
  state : State
  rng : Rng
  effects : Effects := {}
  unsupported : Option String := none
  deriving Repr

private def specialHover (t : Nat) : Bool := (87 ≤ t && t ≤ 92) || t == 677 || t == 695 || t == 696

/-- `Main.HoverOverNPCs(mouseRect)` (the chat start; the hover tooltip is a widget). The first NPC in slot order whose
frame the mouse touches ends the scan; a town NPC (or one of 105 / 106 / 123 / 354 / 376 / 579 / 453 / 589) whose
hitbox meets the reach region then opens the chat on a fresh right click. -/
def hover (input : Input) (player : Player) (npcs : Array Npc) (chat : ChatWorld) (rng : Rng) (st : State)
    (otherChat : Npc → Rng → Except String (Line × Rng) := fun n _ => .error s!"GetChat for type {n.type}") : Result := Id.run do
  if input.smartInteractNPC.isSome then return { state := st, rng, unsupported := some "smart interact" }
  if input.usingGamepad then return { state := st, rng, unsupported := some "gamepad" }
  let reach := reachRegion input.tileRangeX input.tileRangeY player.body
  let mut st := st
  for n in npcs.qsort (·.index < ·.index) do
    if !n.active || n.shimmerTransparent then continue
    if specialHover n.type then
      if n.hoverRect.intersects input.mouse then return { state := st, rng, unsupported := some s!"hover shape of type {n.type}" }
      continue
    if !n.hoverRect.intersects input.mouse then continue
    if (n.type == 85 || n.type == 341 || n.type == 629 || n.aiStyle == 87) && n.ai0 == 0 then continue
    if n.type == 694 && n.ai3 == 3 then continue
    if n.type == 690 || n.type == 488 then continue
    if n.type == 685 then return { state := st, rng, unsupported := some "Elder Slime" }
    let talkable := n.townNPC || n.type == 105 || n.type == 106 || n.type == 123 || n.type == 354 || n.type == 376 ||
      n.type == 579 || n.type == 453 || n.type == 589
    let mut eligible := talkable && reach.intersects n.body.hitbox
    if player.ownedProjectile651 > 0 || player.stinky || player.mouseInterface then eligible := false
    let effects : Effects := { clearCursorItemIcon := !input.mouseText }
    if !eligible || player.dead then return { state := st, rng, effects }
    let mut effects := { effects with hovering := some n.index, noThrow := some 4 }
    if input.mouseRight && st.npcChatRelease then
      st := { st with npcChatRelease := false }
      if st.talkNPC != (n.index : Int) && !player.tileInteractionHappened then
        let line := if n.type == 22 then guideChat chat rng else otherChat n rng
        match line with
        | .error e => return { state := st, rng, effects, unsupported := some e }
        | .ok (line, rng') =>
          st := { st with talkNPC := n.index, text := line }
          effects := { effects with closeWindows := true, sound := some 24, talkedTo := some n.index }
          return { state := st, rng := rng', effects }
    return { state := st, rng, effects }
  return { state := st, rng }

/-- The first two statements of `Player.dropItemCheck` (run when a chat opens): `noThrow = 0` unless the inventory is
open, then a positive `noThrow` counts down by one. -/
def dropItemCheckNoThrow (playerInventory : Bool) (noThrow : Int) : Int :=
  let t := if playerInventory then noThrow else 0
  if t > 0 then t - 1 else t

/-! ## Out of range: `Player.Update` -/

/-- The talk-close block of `Player.Update`: the chat stays open while the reach region meets the talk NPC's hitbox,
no chest is open, the NPC is active and no tile entity anchor is in use; otherwise `SetTalkNPC(-1)`,
`npcChatCornerItem = 0`, `npcChatText = ""` (sound 11 unless a chest is open). -/
def updateClose (tileRangeX tileRangeY : Int) (player : Player) (npcs : Array Npc) (st : State) : State × Effects :=
  if st.talkNPC < 0 then (st, {}) else
  let keep := match npcs.find? (fun n => (n.index : Int) == st.talkNPC) with
    | some n => (reachRegion tileRangeX tileRangeY player.body).intersects n.body.hitbox && player.chest == -1 && n.active &&
        !player.anchorInUse
    | none => false
  if keep then (st, {})
  else ({ st with talkNPC := -1, cornerItem := 0, text := .none }, { sound := if player.chest == -1 then some 11 else none })

/-- The chat panel's Close button (`CloseChat.Interact` → `Main.CloseNPCChatOrSign(false)`): `SetTalkNPC(-1)`,
`npcChatCornerItem = 0`, `npcChatText = ""`, sound 11 (the sign, mount release and panel close are widgets). -/
def closeButton (st : State) : State × Effects :=
  ({ st with talkNPC := -1, cornerItem := 0, text := .none }, { sound := some 11 })

/-! ## Help: `Main.HelpText` -/

/-- One inventory slot as `HelpText` reads it (`IsAir` when the type is 0). -/
structure Item where
  type : Int := 0
  pick : Int := 0
  axe : Int := 0
  hammer : Int := 0
  shoot : Int := 0
  deriving Repr, Inhabited

structure HelpPlayer where
  /-- `inventory[0..57]`. -/
  inventory : Array Item
  /-- `miscEquips[4]` (the hook slot) is not air. -/
  hookEquipped : Bool := false
  statLifeMax : Int := 100
  statManaMax : Int := 20
  statDefense : Int := 0
  /-- `difficulty` (3: journey). -/
  difficulty : Nat := 0
  anglerQuestsFinished : Int := 0
  wingsLogic : Int := 0
  mountActive : Bool := false
  deriving Repr

structure HelpWorld where
  hardMode : Bool := false
  crimson : Bool := false
  shadowOrbSmashed : Bool := false
  downedBoss1 : Bool := false
  downedBoss2 : Bool := false
  downedBoss3 : Bool := false
  downedQueenBee : Bool := false
  downedMechBoss1 : Bool := false
  downedMechBoss2 : Bool := false
  downedMechBoss3 : Bool := false
  downedMechBossAny : Bool := false
  downedPlantBoss : Bool := false
  downedGolemBoss : Bool := false
  downedGoblins : Bool := false
  downedPirates : Bool := false
  downedMartians : Bool := false
  downedFishron : Bool := false
  downedHalloweenTree : Bool := false
  downedHalloweenKing : Bool := false
  downedChristmasIceQueen : Bool := false
  downedChristmasTree : Bool := false
  downedChristmasSantank : Bool := false
  downedMoonlord : Bool := false
  lunarApocalypseIsUp : Bool := false
  downedDeerclops : Bool := false
  /-- `WorldGen.SavedOreTiers.Adamantite` (111 adamantite, 223 titanium). -/
  adamantiteTier : Int := 111
  /-- The active NPCs: `(type, townNPC)`. -/
  npcs : Array (Nat × Bool) := #[]
  /-- `Main.projHook`. -/
  projHook : Nat → Bool := fun _ => false

/-- The `GuideHelpText.Help_<n>` keys of the loaded language that `HelpText` tries first, with
`GetValueIfConditionsMet` (plain text always; a text with substitutions only when they all resolve). en-US has
1063 (`{Bartender}`: a Bartender, 550, must be active), 1064 (plain) and 1065 (input-trigger names: always). -/
def enUSHelpKey (w : HelpWorld) : Int → Bool
  | 1063 => w.npcs.any (·.1 == 550)
  | 1064 => true
  | 1065 => true
  | _ => false

/-- The flags `HelpText` derives before its ladder (locals 0 … 41). -/
structure HelpFlags where
  lifeAbove100 : Bool
  manaAbove20 : Bool
  onlyStarterTools : Bool
  ore : Bool
  bars : Bool
  fallenStar : Bool
  lens : Bool
  evilMaterial : Bool
  grapple : Bool
  has3347 : Bool
  has174 : Bool
  has1141 : Bool
  hasBiomeKey : Bool
  has : Nat → Bool
  townCount : Nat

def helpFlags (p : HelpPlayer) (w : HelpWorld) : HelpFlags := Id.run do
  let journey := p.difficulty == 3
  let pickType : Int := if journey then 1 else 3509
  let axeType : Int := if journey then 10 else 3506
  let mut starter := true
  let mut ore := false
  let mut bars := false
  let mut star := false
  let mut lens := false
  let mut evil := false
  let mut grapple := p.hookEquipped
  let mut i3347 := false
  let mut i174 := false
  let mut i1141 := false
  let mut key := false
  for it in p.inventory do
    if it.type == 0 then continue
    if it.pick > 0 && it.type != pickType then starter := false
    if it.axe > 0 && it.type != axeType then starter := false
    if it.hammer > 0 && (!journey || it.type != 7) then starter := false
    if [11, 12, 13, 14, 699, 700, 701, 702].contains it.type then ore := true
    if [19, 20, 21, 22, 703, 704, 705, 706].contains it.type then bars := true
    if it.type == 75 then star := true
    if it.type == 38 then lens := true
    if [68, 70, 1330, 1331, 67, 2886].contains it.type then evil := true
    if !grapple && it.shoot > 0 && w.projHook it.shoot.toNat then grapple := true
    if it.type == 3347 then i3347 := true
    if it.type == 174 then i174 := true
    if it.type == 1141 then i1141 := true
    if [1533, 1534, 1535, 1536, 1537, 4714].contains it.type then key := true
  let types := w.npcs.map (·.1)
  let townCount := (w.npcs.filter fun (t, town) => town && t != 37).size
  return { lifeAbove100 := p.statLifeMax > 100, manaAbove20 := p.statManaMax > 20, onlyStarterTools := starter, ore, bars,
           fallenStar := star, lens, evilMaterial := evil, grapple, has3347 := i3347, has174 := i174, has1141 := i1141,
           hasBiomeKey := key, has := fun t => types.contains t, townCount }

private def spec (n : Nat) : Line := .key s!"GuideHelpTextSpecific.Help_{n}"

/-- The ladder of `Main.HelpText` for one value of `helpText` (after the language-key test), in IL order. -/
def helpRule (p : HelpPlayer) (w : HelpWorld) (f : HelpFlags) (h : Int) : Option Line :=
  let s := f.onlyStarterTools
  let life := p.statLifeMax
  let b1 := w.downedBoss1
  let b2 := w.downedBoss2
  let b3 := w.downedBoss3
  let hm := w.hardMode
  let cr := w.crimson
  let rules : List (Bool × Line) := [
    (s && h == 1, .legacy 177), (s && h == 2, .legacy 178), (s && h == 3, .legacy 179), (s && h == 4, .legacy 180),
    (s && h == 5, .legacy 181), (s && h == 6, .legacy 182),
    (s && !f.ore && !f.bars && h == 11, .legacy 183),
    (s && f.ore && !f.bars && h == 21, .legacy 184), (s && f.ore && !f.bars && h == 22, .legacy 185),
    (s && f.bars && h == 31, .legacy 186), (s && f.bars && h == 32, .legacy 187),
    (!f.lifeAbove100 && h == 41, .legacy 188),
    (!f.manaAbove20 && h == 42, .legacy 189),
    (!f.manaAbove20 && !f.fallenStar && h == 43, .legacy 190),
    (!f.has 17 && !f.has 18 && h == 51, .legacy 191), (!f.has 17 && !f.has 18 && h == 52, .legacy 192),
    (!f.has 17 && !f.has 18 && h == 53, .legacy 193), (!f.has 17 && !f.has 18 && h == 54, .legacy 194),
    (!f.has 17 && !f.has 18 && h == 55, .key "GuideHelpText.Help_1065"),
    (!f.has 17 && h == 61, .legacy 195),
    (!f.has 18 && h == 62, .legacy 196),
    (!f.has 19 && h == 63, .legacy 197),
    (!f.has 20 && h == 64, .legacy 198),
    (!f.has 124 && h == 65 && b3, .legacy 199),
    (!f.has 54 && h == 66 && b3, .legacy 200),
    (!f.has 38 && h == 67, .legacy 201),
    (!f.has 107 && b2 && h == 68, .legacy 202),
    (!f.has 108 && hm && h == 69, .legacy 203),
    (!f.has 228 && h == 70 && b2, spec 1100),
    (!f.has 178 && h == 71 && hm, spec 1101),
    (!f.has 209 && h == 72 && w.downedMechBoss1 && w.downedMechBoss2 && w.downedMechBoss3, spec 1102),
    (!f.has 353 && h == 73, spec 1103),
    (!f.has 369 && h == 74, spec 1104),
    (!f.has 441 && h == 75 && hm, spec 1105),
    (!f.has 229 && h == 76 && hm, spec 1106),
    (!f.has 207 && h == 77, spec 1107),
    (!f.has 160 && h == 78 && hm, spec 1108),
    (!f.has 588 && h == 79, spec 1109),
    (!f.has 227 && h == 80 && f.townCount ≥ 5, spec 1110),
    (!f.has 208 && h == 81 && f.townCount ≥ 17, spec 1111),
    (!f.has 550 && b2 && h == 82, spec 1112),
    (!f.has 368 && h == 83 && f.has 17, spec 1113),
    (!f.has 453 && h == 84 && !hm, spec 1114),
    (!f.has 633 && h == 85 && !hm, spec 1115),
    (f.lens && !cr && h == 100, .legacy 204),
    (f.evilMaterial && h == 101, .legacy (if cr then 403 else 205)),
    ((f.lens || f.evilMaterial) && h == 102, .legacy (if cr then 402 else 206)),
    (f.lens && cr && h == 103, spec 1159),
    (!f.grapple && h == 201 && !hm && !b3 && !b2, .legacy 207),
    (h == 202 && !hm && life ≥ 140, spec 1120),
    (h == 203 && hm && w.downedMechBossAny, spec 1121),
    (h == 204 && !w.downedGoblins && life ≥ 200 && w.shadowOrbSmashed, spec 1122),
    (h == 205 && hm && !w.downedPirates && life ≥ 200, spec 1123),
    (h == 206 && hm && w.downedGolemBoss && !w.downedMartians, spec 1124),
    (h == 207 && (b1 || b2 || b3), spec 1125),
    (h == 208 && !hm, spec 1130), (h == 209 && !hm, spec 1131), (h == 210 && !hm, spec 1132),
    (h == 211 && !hm, spec 1133), (h == 212 && !hm, spec 1134), (h == 213 && !hm, spec 1135),
    (h == 214 && !hm && (f.ore || f.bars), spec 1136),
    (h == 215 && p.anglerQuestsFinished < 1, spec 1137),
    (h == 216 && !hm, spec 1138),
    (h == 1000 && !b1 && !b2, .legacy 208),
    (h == 1001 && !b1 && !b2, .legacy 209),
    (h == 1002 && !b2, .legacy (if cr then 331 else 210)),
    (h == 1050 && !b1 && life < 200, .legacy 211),
    (h == 1051 && !b1 && p.statDefense ≤ 10, .legacy 212),
    (h == 1052 && !b1 && life ≥ 200 && p.statDefense > 10, .legacy (if cr then 404 else 213)),
    (h == 1053 && b1 && !b2 && life < 300, .legacy 214),
    (h == 1054 && b1 && !b2 && !cr && life ≥ 300, .legacy 215),
    (h == 1055 && b1 && !b2 && !cr && life ≥ 300, .legacy 216),
    (h == 1056 && b1 && b2 && !b3, .legacy 217),
    (h == 1057 && b1 && b2 && b3 && !hm && life < 400, .legacy 218),
    (h == 1058 && b1 && b2 && b3 && !hm && life ≥ 400, .legacy 219),
    (h == 1059 && b1 && b2 && b3 && !hm && life ≥ 400, .legacy 220),
    (h == 1060 && b1 && b2 && b3 && !hm && life ≥ 400, .legacy 221),
    (h == 1061 && hm && !w.downedPlantBoss, .legacy (if cr then 401 else 222)),
    (h == 1062 && hm && !w.downedPlantBoss, .legacy 223),
    (h == 1140 && b1 && !b2 && cr && life ≥ 300, spec 1140),
    (h == 1141 && b1 && !b2 && cr && life ≥ 300, spec 1141),
    (h == 1142 && b2 && !hm, spec 1142),
    (h == 1143 && b2 && !w.downedQueenBee && life ≥ 300, spec 1143),
    (h == 1144 && f.has3347, spec 1144),
    (h == 1145 && f.has174 && !hm, spec 1145),
    (h == 1146 && hm && p.wingsLogic == 0 && !p.mountActive && !w.downedPlantBoss, spec 1146),
    (h == 1147 && hm && w.adamantiteTier == 111 && !w.downedMechBossAny, spec 1147),
    (h == 1148 && hm && w.adamantiteTier == 223 && !w.downedMechBossAny, spec 1148),
    (h == 1149 && hm && w.downedMechBossAny && life < 500, spec 1149),
    (h == 1150 && hm && w.downedMechBoss1 && w.downedMechBoss2 && w.downedMechBoss3 && !w.downedPlantBoss, spec 1150),
    (h == 1151 && hm && w.downedPlantBoss && !w.downedGolemBoss && f.has1141, spec 1151),
    (h == 1152 && hm && w.downedPlantBoss && !w.downedGolemBoss && !f.has1141, spec 1152),
    (h == 1153 && hm && f.hasBiomeKey, spec 1153),
    (h == 1154 && hm && !w.downedFishron, spec 1154),
    (h == 1155 && hm && w.downedGolemBoss && !w.downedHalloweenTree && !w.downedHalloweenKing, spec 1155),
    (h == 1156 && hm && w.downedGolemBoss && !w.downedChristmasIceQueen && !w.downedChristmasTree &&
      !w.downedChristmasSantank, spec 1156),
    (h == 1157 && hm && w.downedGolemBoss && f.has 437 && !w.downedMoonlord, spec 1157),
    (h == 1158 && hm && w.lunarApocalypseIsUp && !w.downedMoonlord, spec 1158),
    (h == 1159 && b1 && b2 && !w.downedDeerclops, spec 1160)]
  (rules.find? (·.1)).map (·.2)

/-- `Main.HelpText()`: `(helpText, line)` after one press. Each round increments `helpText`, tries the language key
`GuideHelpText.Help_<helpText>`, then the ladder; above 1200 it wraps to 0 and goes on. -/
def helpText (p : HelpPlayer) (w : HelpWorld) (langKey : Int → Bool) (h0 : Int) : Except String (Int × Line) := do
  let f := helpFlags p w
  let mut h := h0
  for _ in [0:2500] do
    h := h + 1
    if langKey h then return (h, .key s!"GuideHelpText.Help_{h}")
    if let some line := helpRule p w f h then return (h, line)
    if h > 1200 then h := 0
  throw "HelpText: no line in two full cycles"

/-- The Help button (`GuideTip.Interact`): sound 12, `Main.HelpText()`, the portrait hop. -/
def helpButton (p : HelpPlayer) (w : HelpWorld) (langKey : Int → Bool) (st : State) : Except String (State × Effects) := do
  let (h, line) ← helpText p w langKey st.helpText
  return ({ st with helpText := h, text := line }, { sound := some 12, portraitHop := true })

end Lean4Terria.OriginalForestGuideDialogue
