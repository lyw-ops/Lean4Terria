import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalForestCritter
import Lean4Terria.OriginalMerchantShop
import Lean4Terria.OriginalPlayerCollision
import Lean4Terria.OriginalStarterMelee.LineOfSight
import Lean4Terria.UnifiedRandom
import Std.Data.HashMap

/-!
# Town NPC behaviour (Terraria desktop 1.4.5.8, Classic, single player)

A hand transcription from the desktop CIL (`.lake/original-town-npc-ai/il/`, folded statement listings `fold-*.txt`)
of what one `NPC.UpdateNPC(i)` does for an ordinary town NPC (`aiStyle = 7`, `isLikeATownNPC`, not a town pet, town
slime, town critter or one of the special types listed in `unsupportedType`): the common frame (`UpdateNPC`,
`UpdateNPC_UpdateGravity`, `CheckDrowning`, `CheckLifeRegen`, `UpdateCollision` with `Collision_WalkDownSlopes`,
`Collision_DecideFallThroughPlatforms`, `Collision_MoveWhileDry`, `Collision_MoveSlopesAndStairFall`), the whole of
`AI_007_TownEntities` except the attack states, the town branch of `FindFrame`, `WorldGen.OpenDoor` /
`WorldGen.CloseDoor`, and the chat buttons a player can press (`OpenShop`, `NurseHeal` → `Main.GetNurseHealCost`,
`Main.NPCChatText_DoNurseHeal`, `Player.HealEffect`).

The NPC phase runs `update` for every slot in ascending order on one shared `Main.rand`; `Sim` holds the whole
`Main.npc` array because the AI reads and writes other slots (chat partners, resting spots, danger scan).

Shared ports reused: `OriginalCollision` (`tileCollision`, `slopeCollision`, `walkDownSlope`), `OriginalForestCritter`
(`stepUp`, `solidTiles`, `solidTilesVersatile`, `wetCollision`, `drownCollision`, `canHit`, `vecDistance`),
`OriginalMerchantShop.buyItem` (`Player.BuyItem`).

## Boundaries (`Stop.boundary`; the caller must then run the original for this frame)

* entering or running an attack state (`ai[0]` 10, 12, 13, 14, 15) and state 24, the shimmer state 25;
* `FindFrame` frames that pick an emote (`EmoteBubble.NewBubbleNPC` → `PickNPCEmote`), choose a game emote
  (`Utils.SelectRandom`), or spawn dust / gore from `Main.rand` (party dance, state 11);
* a hostile NPC actually hitting the town NPC (`GetHurtByOtherNPCs` → `StrikeNPC`), drowning dust, water / lava /
  honey / shimmer contact, pressure plates and conveyor belts next to the NPC, `TeleportToHome` failing (→ `QuickFindHome`),
  `OpenDoor` cutting a plant, the bottom of the world.
-/

namespace Lean4Terria.OriginalTownNpcAI

open OriginalCollision (Grid Vector single tileIndex slopeId TileRules)
open OriginalWorld (Tile TileShape)

instance : Inhabited Vector := ⟨⟨0, 0⟩⟩

/-! ## Inputs -/

/-- Static per-type and per-tile tables (read natively; see `survey`). -/
structure Tables where
  /-- `Main.tileCut`. -/
  tileCut : UInt16 → Bool
  /-- `TileID.Sets.AvoidedByNPCs`. -/
  avoidedByNPCs : UInt16 → Bool
  /-- `TileID.Sets.CanBeSatOnForNPCs`. -/
  canBeSatOnForNPCs : UInt16 → Bool
  /-- `TileID.Sets.InteractableByNPCs`. -/
  interactableByNPCs : UInt16 → Bool
  /-- `TileID.Sets.IsADripTile`. -/
  isADripTile : UInt16 → Bool
  /-- `Main.tileDungeon`. -/
  tileDungeon : UInt16 → Bool
  /-- `NPCID.Sets.DangerDetectRange` (-1: none). -/
  dangerDetectRange : Int → Int
  /-- `NPCID.Sets.PrettySafe` (-1: none). -/
  prettySafe : Int → Int
  /-- `NPCID.Sets.AttackType`, `AttackAverageChance`, `AttackTime`. -/
  attackType : Int → Int
  attackAverageChance : Int → Int
  attackTime : Int → Int
  /-- `Main.npcFrameCount`, `NPCID.Sets.AttackFrameCount`, `NPCID.Sets.ExtraFramesCount`. -/
  npcFrameCount : Int → Int
  attackFrameCount : Int → Int
  extraFramesCount : Int → Int
  /-- `NPCID.Sets.IsTownPet` / `IsTownSlime` / `TownCritter` / `CritterThatCanTurnOnPlayers` / `Skeletons`. -/
  isTownPet : Int → Bool
  /-- `NPCID.Sets.CritterThatCanTurnOnPlayers`. -/
  critterThatCanTurnOnPlayers : Int → Bool
  /-- `TileID.Sets.IgnoredByNpcStepUp` (`Collision.StepUp`, special checks mode 1). -/
  ignoredByNpcStepUp : UInt16 → Bool

/-- World statics the town AI reads. -/
structure World where
  /-- Tiles before this phase (`Sim.tiles` holds the door changes made since). -/
  grid : Grid
  tables : Tables
  worldSurface : Float
  maxTilesX : Int
  maxTilesY : Int
  dayTime : Bool := true
  raining : Bool := false
  eclipse : Bool := false
  slimeRain : Bool := false
  storming : Bool := false
  invasionType : Int := 0
  expertMode : Bool := false
  masterMode : Bool := false
  lowTiles : Bool := false
  infectedSeed : Bool := false
  partyIsUp : Bool := false
  combatBookWasUsed : Bool := false
  combatBookVolumeTwoWasUsed : Bool := false
  /-- `NPC.downed…` flags read by the defense / attack-strength block. -/
  downedSlimeKing : Bool := false
  downedBoss1 : Bool := false
  downedDeerclops : Bool := false
  downedBoss2 : Bool := false
  downedBoss3 : Bool := false
  downedQueenBee : Bool := false
  hardMode : Bool := false
  downedQueenSlime : Bool := false
  downedMechBoss1 : Bool := false
  downedMechBoss2 : Bool := false
  downedMechBoss3 : Bool := false
  downedPlantBoss : Bool := false
  downedEmpressOfLight : Bool := false
  downedFishron : Bool := false
  downedGolemBoss : Bool := false
  downedAncientCultist : Bool := false
  /-- `NPC.sWidth`, `NPC.sHeight`, `NPC.safeRangeX`, `NPC.safeRangeY`. -/
  sWidth : Int := 1920
  sHeight : Int := 1200
  safeRangeX : Int := 62
  safeRangeY : Int := 39
  /-- `Main.bottomWorld`. -/
  bottomWorld : Float32
  /-- `FindFrame`'s frame height (`Utils.Height(texture) / npcFrameCount` on the client, 1 on a server). -/
  frameHeight : Int → Int

/-- A player slot as the town AI reads it. -/
structure Player where
  index : Nat
  active : Bool := true
  dead : Bool := false
  ghost : Bool := false
  position : Vector
  width : Int := 20
  height : Int := 42
  /-- `Player.talkNPC`. -/
  talkNPC : Int := -1
  stinky : Bool := false
  /-- `Player.CanBeTalkedTo`. -/
  canBeTalkedTo : Bool := true
  /-- `sitting.isSitting` and the tile under `Center`. -/
  sittingAt : Option (Int × Int) := none
  deriving Repr, Inhabited

def Player.center (p : Player) : Vector :=
  ⟨p.position.x + single p.width / 2, p.position.y + single p.height / 2⟩

/-- One `Main.npc` slot: the town NPC being updated reads and writes every field; the other slots are read (danger
scan, chat partners, resting spots) and some written (a chat partner). -/
structure Npc where
  whoAmI : Nat
  type : Int := 0
  active : Bool := false
  aiStyle : Int := 7
  townNPC : Bool := false
  friendly : Bool := false
  damage : Int := 0
  stinky : Bool := false
  noTileCollide : Bool := false
  /-- `CanBeChasedBy(townNPC, false)` (for hostile slots). -/
  chaseable : Bool := false
  position : Vector := ⟨0, 0⟩
  velocity : Vector := ⟨0, 0⟩
  oldPosition : Vector := ⟨0, 0⟩
  oldVelocity : Vector := ⟨0, 0⟩
  width : Int := 18
  height : Int := 40
  direction : Int := 0
  directionY : Int := 0
  oldDirection : Int := 0
  oldDirectionY : Int := 0
  spriteDirection : Int := -1
  target : Int := 255
  oldTarget : Int := 255
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  ai2 : Float32 := 0
  ai3 : Float32 := 0
  localAI0 : Float32 := 0
  localAI1 : Float32 := 0
  localAI2 : Float32 := 0
  localAI3 : Float32 := 0
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  wetCount : Nat := 0
  lavaWet : Bool := false
  honeyWet : Bool := false
  shimmerWet : Bool := false
  life : Int := 250
  lifeMax : Int := 250
  defLifeMax : Int := 250
  defense : Int := 15
  defDefense : Int := 15
  dryadWard : Bool := false
  tipsy : Bool := false
  immortal : Bool := false
  dontTakeDamage : Bool := false
  netUpdate : Bool := false
  justHit : Bool := false
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  stairFall : Bool := false
  soundDelay : Int := 0
  immune255 : Int := 0
  frameY : Int := 0
  frameCounter : Float := 0
  homeTileX : Int := -1
  homeTileY : Int := -1
  oldHomeTileX : Int := -1
  oldHomeTileY : Int := -1
  homeless : Bool := false
  oldHomeless : Bool := false
  closeDoor : Bool := false
  doorX : Int := 0
  doorY : Int := 0
  breath : Int := 200
  breathCounter : Int := 0
  friendlyRegen : Int := 0
  lookForHomeTimeout : Int := 0
  shimmering : Bool := false
  extraValue : Int := 0
  waterMovementSpeed : Float32 := 0.5
  deriving Repr, Inhabited

def Npc.center (n : Npc) : Vector := ⟨n.position.x + single n.width / 2, n.position.y + single n.height / 2⟩
def Npc.top (n : Npc) : Vector := ⟨n.position.x + single n.width / 2, n.position.y⟩
def Npc.bottom (n : Npc) : Vector := ⟨n.position.x + single n.width / 2, n.position.y + single n.height⟩

/-- Types whose paths this module does not transcribe (pets, slimes, critters, shimmer-bound or special AIs). -/
def unsupportedType (t : Int) : Bool :=
  #[37, 54, 124, 142, 148, 149, 160, 300, 361, 362, 364, 368, 369, 441, 445, 447, 453, 602, 608, 610, 614, 616, 617, 625,
    633, 637, 638, 656, 663, 670, 678, 679, 680, 681, 682, 683, 684, 687].contains t

/-! ## Simulation state -/

inductive Boundary where
  | attack (state : Int)
  | emote
  | shimmer
  | hurt
  | liquid
  | wiredTile
  | teleportFailed
  | doorCut
  | bottomOfWorld
  | unsupported
  deriving BEq, Repr

inductive Stop where
  | boundary (kind : Boundary) (detail : String)
  | error (msg : String)
  deriving Repr

/-- Side effects outside the modelled state, in call order. -/
inductive Effect where
  | openDoor (x y direction : Int)
  | closeDoor (x y : Int)
  | tileFrame (x0 x1 y0 y1 : Int)
  | sitting (npc : Nat) (x y : Int)
  | teleportHome (npc : Nat) (x y : Int)
  | sound (kind : Nat) (x y : Int)
  deriving BEq, Repr

structure Sim where
  /-- `Main.npc[0..199]`. -/
  npcs : Array Npc
  players : Array Player
  /-- `Main.rand`. -/
  rng : UnifiedRandom
  /-- Tiles written by doors since the world was built. -/
  tiles : Std.HashMap (Int × Int) Tile := {}
  /-- `NPC.gravity` (static). -/
  gravity : Float32 := 0.3
  effects : Array Effect := #[]

abbrev S := StateT Sim (Except Stop)

private def stop (k : Boundary) (d : String) : S α := throw (.boundary k d)
private def fail (m : String) : S α := throw (.error m)

/-- `(int)f` for a `float` (CIL `conv.i4`, truncation). -/
def trunc (f : Float32) : Int := f.toInt32.toInt
/-- C# `int` division. -/
def cdiv (a b : Int) : Int := a.tdiv b

/-- `Main.tile[x, y]` as it is now (door writes applied). -/
def tileNow (w : World) (s : Sim) (x y : Int) : Tile :=
  match s.tiles.get? (x, y) with
  | some t => t
  | none => if x < 0 || y < 0 || x ≥ w.grid.width || y ≥ w.grid.height then default else w.grid.tileAt x.toNat y.toNat

/-- The collision grid of the current tiles. -/
def gridNow (w : World) (s : Sim) : Grid :=
  { w.grid with tileAt := fun x y => tileNow w s x y }

private def tile (w : World) (x y : Int) : S Tile := return tileNow w (← get) x y
private def grid (w : World) : S Grid := return gridNow w (← get)

private def ttype (t : Tile) : UInt16 := t.id.getD 0
private def isActive (t : Tile) : Bool := t.id.isSome
private def nactive (t : Tile) : Bool := t.id.isSome && !t.actuated
private def rules (w : World) (t : Tile) : TileRules := w.grid.rules (ttype t)
private def frameX (t : Tile) : Int := (t.frame.map (·.1)).getD 0
private def frameY (t : Tile) : Int := (t.frame.map (·.2)).getD 0

private def next (n : Int) : S Int := do
  let s ← get
  let (v, r) := s.rng.next 0 n
  set { s with rng := r }
  return v

private def next2 (lo hi : Int) : S Int := do
  let s ← get
  let (v, r) := s.rng.next lo hi
  set { s with rng := r }
  return v

private def emit (e : Effect) : S Unit := modify fun s => { s with effects := s.effects.push e }

section
variable (i : Nat)

private def me : S Npc := do
  let some n := (← get).npcs[i]? | fail s!"Main.npc[{i}] not supplied"
  return n

private def upd (f : Npc → Npc) : S Unit := modify fun s => { s with npcs := s.npcs.modify i f }

private def npcAt (k : Nat) : S Npc := return ((← get).npcs[k]?).getD { whoAmI := k }
private def updAt (k : Nat) (f : Npc → Npc) : S Unit := modify fun s => { s with npcs := s.npcs.modify k f }

end

/-- `Vector2.Distance` (float squares, double square root, back to float). -/
def dist (a b : Vector) : Float32 := OriginalForestCritter.vecDistance a b

/-- `Utils.ToDirectionInt(b)`. -/
def dirInt (b : Bool) : Int := if b then 1 else -1

/-- `Rectangle.Intersects`. -/
def intersects (ax ay aw ah bx by_ bw bh : Int) : Bool :=
  bx < ax + aw && ax < bx + bw && by_ < ay + ah && ay < by_ + bh

/-- `Utils.ToTileCoordinates(Vector2)`: `(int)x >> 4`. -/
def toTile (v : Vector) : Int × Int := (trunc v.x >>> 4, trunc v.y >>> 4)

/-- `WorldGen.InWorld(x, y, fluff)`. -/
def inWorld (w : World) (x y fluff : Int) : Bool :=
  x ≥ fluff && x < w.maxTilesX - fluff && y ≥ fluff && y < w.maxTilesY - fluff


/-! ## Helpers of `AI_007_TownEntities` -/

/-- `NPC.UpdateHomeTileState(homeless, x, y)` (its `SendData` is a no-op in single player). -/
def updateHomeTileState (n : Npc) (homeless : Bool) (x y : Int) : Npc :=
  { n with homeless, oldHomeless := homeless, homeTileX := x, oldHomeTileX := x, homeTileY := y, oldHomeTileY := y }

/-- `NPC.SolidOrSlopedTileOrPlatform(tile)`. -/
def solidOrSlopedTileOrPlatform (w : World) (t : Tile) : Bool :=
  isActive t && (((rules w t).solid && !(rules w t).solidTop) || (rules w t).platform) && !t.actuated

/-- `(Bottom + UnitY * -2).ToTileCoordinates()`: the tile an NPC sits on. -/
def sitTile (n : Npc) : Int × Int := toTile ⟨n.bottom.x, n.bottom.y + (-2 : Float32)⟩

private def someoneSitsAt (s : Sim) (p : Int × Int) : Bool :=
  s.npcs.any fun o => o.active && o.aiStyle == 7 && o.townNPC && o.ai0 == 5 && sitTile o == p

/-- `NPC.AI_007_FindGoodRestingSpot(myTileX, myTileY, out floorX, out floorY)`. -/
def findGoodRestingSpot (w : World) (s : Sim) (n : Npc) (tx ty : Int) : Int × Int := Id.run do
  -- IL_0000..001c
  let fx := n.homeTileX
  let mut fy := n.homeTileY
  if fx == -1 || fy == -1 then return (fx, fy)
  -- IL_001d..0049: drop to the floor
  while !solidOrSlopedTileOrPlatform w (tileNow w s fx fy) && fy < w.maxTilesY - 20 do
    fy := fy + 1
  -- IL_0049..007a
  if w.dayTime then return (fx, fy)
  if n.ai0 == 5 && (tx - fx).natAbs < 7 && (ty - fy).natAbs < 7 then return (fx, fy)
  -- IL_007a..00d1 (types 638, 656 and town slimes are not supported here)
  if n.ai0 == 5 then return (fx, fy)
  -- IL_00d1..01a5: the nearest seat
  let mut best : Int := -1
  let mut bx : Int := -1
  let mut by_ : Int := -1
  let mut x := fx - 7
  while x ≤ fx + 7 do
    let mut y := fy + 2
    while y ≥ fy - 6 do
      let t := tileNow w s x y
      if isActive t && w.tables.canBeSatOnForNPCs (ttype t) && (frameY t % 40 != 0 || y + 1 ≤ fy + 2) then
        let d := ((x - fx).natAbs + (y - fy).natAbs : Nat)
        if best == -1 || (d : Int) < best then
          best := d; bx := x; by_ := y
      y := y - 2
    x := x + 1
  if best == -1 then return (fx, fy)
  -- IL_01b4..0209
  let t := tileNow w s bx by_
  if ttype t == 497 || ttype t == 15 then
    if frameY t % 40 != 0 then by_ := by_ - 1
    by_ := by_ + 2
  -- IL_0209..0287: taken by another sitting NPC
  if someoneSitsAt s (bx, by_) then return (fx, fy)
  return (bx, by_)

/-- `NPC.AI_007_TownEntities_IsInAGoodRestingSpot(myTileX, myTileY, floorX, floorY)` (types 361 / 445 / 687 excluded). -/
def isInAGoodRestingSpot (w : World) (n : Npc) (tx ty rx ry : Int) : Bool :=
  if !w.dayTime && n.ai0 == 5 then (tx - rx).natAbs ≤ 7 && (ty - ry).natAbs ≤ 7
  else tx == rx && ty == ry

/-- `Utils.PlotLine(x0, y0, x1, y1, plot, jump)`: the end point itself is not plotted. -/
def plotLine (x0 y0 x1 y1 : Int) (plot : Int → Int → Bool) (jump : Bool) : Bool := Id.run do
  if x0 == x1 && y0 == y1 then return plot x0 y0
  let steep := (y1 - y0).natAbs > (x1 - x0).natAbs
  let (x0, y0, x1, y1) := if steep then (y0, x0, y1, x1) else (x0, y0, x1, y1)
  let dx : Int := (x1 - x0).natAbs
  let dy : Int := (y1 - y0).natAbs
  let mut err := cdiv dx 2
  let mut y := y0
  let xs : Int := if x0 < x1 then 1 else -1
  let ys : Int := if y0 < y1 then 1 else -1
  let mut x := x0
  let mut fuel := dx + 2
  while x != x1 && fuel > 0 do
    if !(if steep then plot y x else plot x y) then return false
    err := err - dy
    if err < 0 then
      y := y + ys
      if !jump then
        if !(if steep then plot y x else plot x y) then return false
      err := err + dx
    x := x + xs
    fuel := fuel - 1
  return true

/-- `Utils.PlotTileLine(Vector2 start, Vector2 end, float width, plot)` through its `Vector2D` overload. -/
def plotTileLine (start end_ : Vector) (width : Float32) (plot : Int → Int → Bool) : Bool :=
  let sx := start.x.toFloat; let sy := start.y.toFloat
  let ex := end_.x.toFloat; let ey := end_.y.toFloat
  let half := width.toFloat / 2
  let dx := ex - sx; let dy := ey - sy
  let len := (dx * dx + dy * dy).sqrt
  let ux := dx / len; let uy := dy / len
  let px := -uy * half; let py := ux * half
  let tt (x y : Float) : Int × Int := (x.toInt32.toInt >>> 4, y.toInt32.toInt >>> 4)
  let p1 := tt (sx - px) (sy - py)
  let p2 := tt (sx + px) (sy + py)
  let a := tt sx sy
  let b := tt ex ey
  let minOff := (p1.1 - a.1, p1.2 - a.2)
  let maxOff := (p2.1 - a.1, p2.2 - a.2)
  plotLine a.1 a.2 b.1 b.2 (fun x y => plotLine (x + minOff.1) (y + minOff.2) (x + maxOff.1) (y + maxOff.2) plot false) true

/-- `DelegateMethods.SearchAvoidedByNPCs(x, y)`. -/
def searchAvoidedByNPCs (w : World) (s : Sim) (x y : Int) : Bool :=
  if !inWorld w x y 1 then false
  else
    let t := tileNow w s x y
    !isActive t || !w.tables.avoidedByNPCs (ttype t)

/-- `NPC.AI_007_TownEntities_GetWalkPrediction(myTileX, homeFloorX, canBreathUnderWater, currentlyDrowning,
tileX, tileY, out keepwalking, out avoidFalling)`. -/
def walkPrediction (w : World) (s : Sim) (n : Npc) (tx homeX : Int) (drowning : Bool) (x y : Int) :
    Except Stop (Bool × Bool) := Id.run do
  let mut keep := false
  let mut avoid := true
  let nearHome := !(tx < homeX - 35) && !(tx > homeX + 35)
  -- IL_001c..00f5 (isLikeATownNPC)
  if n.ai1 < 30 then
    keep := !plotTileLine n.top n.bottom (single n.width) (searchAvoidedByNPCs w s)
    if !keep then
      let rx := trunc n.position.x - 20
      let ry := trunc n.position.y
      for o in s.npcs do
        if o.active && o.friendly && o.whoAmI != n.whoAmI && o.velocity.x == 0 &&
            intersects rx ry (n.width + 40) n.height (trunc o.position.x) (trunc o.position.y) o.width o.height then
          keep := true
          break
  -- IL_00f5..0104
  if !keep && drowning then keep := true
  -- IL_0104..012e
  if avoid then
    let sign : Int := if homeX - tx > 0 then 1 else if homeX - tx < 0 then -1 else 0
    if !nearHome && n.direction == sign then avoid := false
  -- IL_012e..0240
  if avoid then
    let mut k : Int := -1
    while k ≤ 4 do
      let t := tileNow w s x (y + k)
      if t.liquid.isSome then
        return .error (.boundary .liquid s!"GetWalkPrediction: liquid at ({x}, {y + k})")
      if nactive t && (rules w t).solid then
        avoid := false
        break
      k := k + 1
  return .ok (keep, avoid)



/-! ## Doors -/

private def setTile (x y : Int) (t : Tile) : S Unit := modify fun s => { s with tiles := s.tiles.insert (x, y) t }

/-- `Collision.EmptyTile(x, y, ignoreTiles)`. -/
def emptyTile (w : World) (s : Sim) (x y : Int) (ignoreTiles : Bool) : Bool := Id.run do
  if isActive (tileNow w s x y) && !ignoreTiles then return false
  for p in s.players do
    if p.active && !p.dead && !p.ghost &&
        intersects (x * 16) (y * 16) 16 16 (trunc p.position.x) (trunc p.position.y) p.width p.height then return false
  for o in s.npcs do
    if o.active && intersects (x * 16) (y * 16) 16 16 (trunc o.position.x) (trunc o.position.y) o.width o.height then
      return false
  return true

/-- `WorldGen.ValidateTileSquareIsActiveAndOfType(x, y, width, height, type)` on a valid square (an invalid one
kills tiles and is a boundary). -/
private def validateSquare (w : World) (x y wd ht : Int) (type : UInt16) : S Bool := do
  let s ← get
  for dx in [0:wd.toNat] do
    for dy in [0:ht.toNat] do
      let (tx, ty) := (x + dx, y + dy)
      if !inWorld w tx ty 0 then return false
      let t := tileNow w s tx ty
      if !(isActive t && ttype t == type) then
        stop .unsupported s!"ValidateTileSquareIsActiveAndOfType({x}, {y}): broken door kills tiles"
  return true

/-- `WorldGen.IsLockedDoor(tile)`. -/
def isLockedDoor (t : Tile) : Bool := ttype t == 10 && frameY t ≥ 594 && frameY t ≤ 646 && frameX t < 54

/-- `WorldGen.OpenDoor(x, y, direction)`. -/
def openDoor (w : World) (x y direction : Int) : S Bool := do
  let t ← tile w x y
  if !(isActive t && ttype t == 10) then return false
  if isLockedDoor t then return false
  let mut openX : Int := 0     -- V_2
  let mut fy := frameY t       -- V_3
  let mut style : Int := 0     -- V_4
  while fy ≥ 54 do
    fy := fy - 54; style := style + 1
  if frameX t ≥ 54 then
    let k := cdiv (frameX t) 54
    style := style + 36 * k
    openX := openX + 72 * k
  let top := y - cdiv fy 18    -- V_0
  if !(← validateSquare w x top 1 3 10) then return false
  let paints : Array UInt8 := #[(← tile w x top).tilePaint, (← tile w x (top + 1)).tilePaint, (← tile w x (top + 2)).tilePaint]
  let (left, side) := if direction == -1 then (x - 1, x - 1) else (x, x + 1)
  if direction == -1 then openX := openX + 36
  for r in [0:3] do
    let o ← tile w side (top + r)
    if isActive o && !w.tables.tileCut (ttype o) && ttype o != 165 && !w.tables.isADripTile (ttype o) then return false
  for r in [0:3] do
    let o ← tile w side (top + r)
    if isActive o && (w.tables.tileCut (ttype o) || ttype o == 165 || w.tables.isADripTile (ttype o)) then
      stop .doorCut s!"OpenDoor({x}, {y}): KillTile at ({side}, {top + r})"
  let frameTop := (style % 36) * 54     -- V_10
  emit (.sound 8 (x * 16) (y * 16))
  for r in [0:3] do
    for c in [0:2] do
      let tx := left + c
      let old ← tile w tx (top + r)
      setTile tx (top + r) { old with id := some 11, frame := some (openX + 18 * c, frameTop + 18 * r), tilePaint := paints[r]! }
  emit (.openDoor x y direction)
  emit (.tileFrame (left - 1) (left + 2) (top - 1) (top + 2))
  return true

/-- `WorldGen.CloseDoor(x, y, forced)`. -/
def closeDoor (w : World) (x y : Int) (forced : Bool) : S Bool := do
  let t ← tile w x y
  let fx := frameX t
  if !(isActive t && ttype t == 11) then return false
  let mut fy := frameY t
  let mut style : Int := 0
  let mut closedX : Int := 0     -- V_7
  while fy ≥ 54 do
    fy := fy - 54; style := style + 1
  if fx ≥ 72 then
    style := style + 36 * cdiv fx 72
    closedX := closedX + 54 * cdiv fx 72
  let top := y - cdiv fy 18      -- V_2
  let m := fx % 72
  let (col, dir) : Int × Int := if m == 0 then (x, 1) else if m == 18 then (x - 1, 1) else if m == 36 then (x + 1, -1)
    else if m == 54 then (x, -1) else (x, 0)
  let left := if dir == -1 then col - 1 else col     -- V_9
  if !forced then
    for r in [0:3] do
      if !emptyTile w (← get) col (top + r) true then return false
  if !(← validateSquare w left top 2 3 11) then return false
  for c in [0:2] do
    for r in [0:3] do
      let tx := left + c
      let o ← tile w tx (top + r)
      if isActive o && ttype o == 11 then
        if tx == col then
          -- `WorldGen.genRand` is `Main.rand` in 1.4.5.8 (`get_genRand`: `ldsfld Main::rand; ret`)
          let k ← next 3
          setTile tx (top + r) { o with id := some 10, frame := some (k * 18 + closedX, frameY o) }
        else
          setTile tx (top + r) { o with id := none }
  emit (.closeDoor x y)
  emit (.tileFrame (col - 1) (col + 1) (top - 1) (top + 2))
  emit (.sound 9 (x * 16) (y * 16))
  return true

/-! ## `NPC.AI_007_TownEntities` -/

/-- `AI_007_TownEntities` IL_0079: the chance to sit down while walking (`Main.rand.Next(300)`). -/
def sitChance : Int := 300


/-- IL `a >=un b` (branch taken when `a ≥ b` or unordered) as a condition. -/
private def geU (a b : Float32) : Bool := !(a < b)
private def leU (a b : Float32) : Bool := !(a > b)
private def gtU (a b : Float32) : Bool := !(a ≤ b)
private def ltU (a b : Float32) : Bool := !(a ≥ b)

private def f (k : Int) : Float32 := single k

/-- `NPC.AI_007_TryForcingSitting(floorX, floorY)` (types 638, 656 and town slimes are unsupported). -/
def tryForcingSitting (w : World) (i : Nat) (fx fy : Int) : S Unit := do
  if !inWorld w fx fy 0 then return
  let n ← me i
  let t ← tile w fx (fy - 1)
  let mut ok := n.ai0 != 5 && isActive t && (ttype t == 15 || ttype t == 497)
  if ok && ttype t == 15 && frameY t ≥ 1080 && frameY t ≤ 1098 then ok := false
  if ok then
    if someoneSitsAt (← get) (sitTile n) then ok := false
  if !ok then return
  let r ← next 10800
  let dir : Int := if frameX t != 0 then 1 else -1
  let bottom : Vector := ⟨f (fx * 16 + 8 + 2 * dir), f (fy * 16)⟩
  upd i fun n => { n with ai0 := 5, ai1 := f (900 + r), direction := dir, position := ⟨bottom.x - f (cdiv n.width 2), bottom.y - f n.height⟩, velocity := ⟨0, 0⟩, localAI3 := 0, netUpdate := true }

/-- `NPC.AI_007_TownEntities_TeleportToHome(homeFloorX, homeFloorY)`. -/
def teleportToHome (w : World) (i : Nat) (fx fy : Int) : S Unit := do
  for k in [0:3] do
    let x := fx + (if k == 0 then 0 else if k == 1 then -1 else 1)
    let g ← grid w
    if !OriginalForestCritter.solidTiles g (x - 1) (x + 1) (fy - 3) (fy - 1) then
      upd i fun n => { n with velocity := ⟨0, 0⟩, position := ⟨f (x * 16 + 8 - cdiv n.width 2), f (fy * 16 - n.height) - 0.1⟩, netUpdate := true }
      emit (.teleportHome i x fy)
      tryForcingSitting w i fx fy
      return
  stop .teleportFailed "TeleportToHome: no free column (homeless = true, QuickFindHome)"

/-- The danger scan result (`V_21` … `V_29`). -/
structure Danger where
  inRange : Bool := false    -- V_21
  found : Bool := false      -- V_22
  stinky : Bool := false     -- V_23
  left : Float32 := -1       -- V_25
  right : Float32 := -1      -- V_26
  fleeDir : Int := 0         -- V_27
  leftTarget : Int := -1     -- V_28
  rightTarget : Int := -1    -- V_29

/-- `AI_007_TownEntities` IL_13f8..1623: hostile (or stinky) NPCs, then stinky players. -/
def scanDanger (w : World) (n : Npc) (talked : Bool) (range range2 : Float32) : S Danger := do
  let s ← get
  let g := gridNow w s
  let mut d : Danger := {}
  for o in s.npcs do
    if !o.active || w.tables.critterThatCanTurnOnPlayers o.type || o.type == 690 then continue
    if !((!o.friendly && o.damage > 0) || o.stinky) then continue
    if o.whoAmI == n.whoAmI then continue
    if geU (dist o.center n.center) range2 then continue
    if !o.noTileCollide && !OriginalForestCritter.canHit g n.center o.center then continue
    d := { d with inRange := true }
    if dist o.center n.center ≥ range then continue
    let chase := o.chaseable
    d := { d with found := true }
    if o.stinky then d := { d with stinky := true }
    let dx := o.center.x - n.center.x
    if !geU dx 0 then
      if d.left == -1 || !leU dx d.left then
        d := { d with left := dx }
        if chase then d := { d with leftTarget := o.whoAmI }
    if !leU dx 0 then
      if d.right == -1 || !geU dx d.right then
        d := { d with right := dx }
        if chase then d := { d with rightTarget := o.whoAmI }
  -- IL_1623..176d: stinky players
  if !d.found && !talked then
    for p in s.players do
      if !p.active || p.dead || !p.stinky then continue
      if geU (dist p.center n.center) range then continue
      d := { d with found := true, stinky := true }
      let dx := p.center.x - n.center.x
      if !geU dx 0 then
        if d.left == -1 || !leU dx d.left then d := { d with left := dx, leftTarget := p.index }
      if !leU dx 0 then
        if d.right == -1 || !geU dx d.right then d := { d with right := dx, rightTarget := p.index }
  return d

private def fleeDirection (d : Danger) : Int :=
  if d.left == -1 then 1 else if d.right == -1 then -1 else dirInt (d.right < -d.left)

/-- One `NPC.AI_007_TownEntities()` call for an ordinary town NPC in slot `i`. -/
def ai007 (w : World) (i : Nat) : S Unit := do
  let n0 ← me i
  if unsupportedType n0.type then fail s!"AI_007: type {n0.type} is not transcribed"
  -- IL_0079..00ec: sitting chance and the "go home" flag
  let mut goHome := w.raining
  if !w.dayTime then goHome := true
  if w.eclipse then goHome := true
  if w.slimeRain then goHome := true
  if w.storming && (n0.position.y / 16).toFloat < w.worldSurface then goHome := true
  -- IL_00ec..0161: defense from the difficulty
  let mut strength : Float32 := 1      -- V_2
  let ward := n0.dryadWard
  let baseDef := n0.defDefense + (if ward then (if w.masterMode then 20 else if w.expertMode then 15 else 10) else 0)
  upd i fun n => { n with defense := baseDef }
  let mut chance : Float32 := 2        -- V_3
  let mut infectedDryad := false       -- V_4
  -- IL_0161..0514 (isLikeATownNPC)
  do
    let n ← me i
    let wasFull := n.life == n.lifeMax
    let mut lifeMax := n.defLifeMax
    let mut defense := n.defense
    if w.combatBookWasUsed then
      lifeMax := lifeMax + 250; chance := chance * 0.8; strength := strength + 0.25; defense := defense + 8
    if w.combatBookVolumeTwoWasUsed then
      lifeMax := lifeMax + 250; chance := chance * 0.8; strength := strength + 0.25; defense := defense + 8
    let mut life := n.life
    if lifeMax > n.defLifeMax && wasFull then life := lifeMax
    for (flag, add, defAdd) in [(w.downedSlimeKing, (0.05 : Float32), (2 : Int)), (w.downedBoss1, 0.05, 2),
        (w.downedDeerclops, 0.1, 3), (w.downedBoss2, 0.1, 3), (w.downedBoss3, 0.1, 3), (w.downedQueenBee, 0.1, 3),
        (w.hardMode, 0.4, 12), (w.downedQueenSlime, 0.15, 6), (w.downedMechBoss1, 0.15, 6), (w.downedMechBoss2, 0.15, 6),
        (w.downedMechBoss3, 0.15, 6), (w.downedPlantBoss, 0.15, 8), (w.downedEmpressOfLight, 0.15, 8),
        (w.downedFishron, 0.15, 8), (w.downedGolemBoss, 0.15, 8), (w.downedAncientCultist, 0.15, 20)] do
      if flag then
        chance := chance * 0.985; strength := strength + add; defense := defense + defAdd
    let mut defDefense := n.defDefense
    let mut immortal := n.immortal
    if w.infectedSeed && n.type == 20 then
      infectedDryad := true; life := lifeMax; defDefense := 99999; immortal := true
    if w.lowTiles then
      if chance > 1 then chance := (chance + 1) / 2
      strength := strength + 0.5
    if n.tipsy then
      defense := (defense.toFloat * 1.1).toInt64.toInt
      strength := strength * 1.1; chance := chance * 0.9
    upd i fun n => { n with lifeMax, life, defense, defDefense, immortal }
  -- IL_067b..0b39: UpdateSavedStates (no-op for the supported types), the shimmer state
  upd i fun n => { n with dontTakeDamage := false }
  if (← me i).ai0 == 25 then stop .shimmer "AI_007: ai[0] = 25"
  -- IL_0b39..0c89: an unset home tile
  do
    let n ← me i
    if n.homeTileX == -1 && n.homeTileY == -1 && n.velocity.y == 0 && !n.shimmering then
      upd i fun n => updateHomeTileState n n.homeless (cdiv (trunc n.center.x) 16) (cdiv (trunc (n.position.y + f n.height + 4)) 16)
  -- IL_0c89..0ce7
  let n ← me i
  let tx := cdiv (trunc (n.position.x + f (cdiv n.width 2))) 16          -- V_6
  let ty := cdiv (trunc (n.position.y + f n.height + 1)) 16               -- V_7
  let (rx, ry) := findGoodRestingSpot w (← get) n tx ty                   -- V_8, V_9
  upd i fun n => { n with directionY := -1, direction := if n.direction == 0 then 1 else n.direction }
  -- IL_0cfd..0e1f: players talking to this NPC
  let mut talked := false   -- V_5
  let mut talkDir : Int := 0 -- V_10
  if (← me i).ai0 != 24 then
    for p in (← get).players do
      if !p.active || p.talkNPC != (i : Int) then continue
      talked := true
      let n ← me i
      if n.ai0 == 10 || n.ai0 == 12 || n.ai0 == 14 || n.ai0 == 15 then continue
      if n.ai0 != 0 then upd i fun n => { n with netUpdate := true }
      upd i fun n => { n with ai0 := 0, ai1 := 300, localAI3 := 100 }
      talkDir := if geU (p.position.x + f (cdiv p.width 2)) (n.position.x + f (cdiv n.width 2)) then 1 else -1
  -- IL_0e1f..0e93: ai[3] = 1 (an NPC that leaves)
  if (← me i).ai3 == 1 then fail "AI_007: ai[3] = 1"
  -- IL_10ff: Main.tile[V_6, V_7].type == 666
  if ttype (← tile w tx ty) == 666 && isActive (← tile w tx ty) then fail "AI_007: standing on tile 666"
  -- IL_1130..12f7: teleport home when nobody sees it
  do
    let n ← me i
    if !n.homeless && n.townNPC && goHome && !isInAGoodRestingSpot w n tx ty rx ry then
      let mut free := true
      for k in [0:2] do
        if !free then break
        let (x0, y0) :=
          if k == 1 then (rx * 16 + 8 - cdiv w.sWidth 2 - w.safeRangeX, ry * 16 + 8 - cdiv w.sHeight 2 - w.safeRangeY)
          else (trunc (n.position.x + f (cdiv n.width 2) - f (cdiv w.sWidth 2) - f w.safeRangeX),
                trunc (n.position.y + f (cdiv n.height 2) - f (cdiv w.sHeight 2) - f w.safeRangeY))
        for p in (← get).players do
          if p.active && intersects (trunc p.position.x) (trunc p.position.y) p.width p.height x0 y0
              (w.sWidth + w.safeRangeX * 2) (w.sHeight + w.safeRangeY * 2) then
            free := false
            break
      if free then teleportToHome w i rx ry
  -- IL_12f7..13f8: type flags (all false for the supported types), danger ranges
  let range : Float32 := let r := w.tables.dangerDetectRange n.type; if r == -1 then 200 else f r   -- V_19
  let mut range2 := range                                                                           -- V_20
  if talked && w.tables.attackType n.type == 3 && range2 < 250 then range2 := 250
  -- IL_13f8..176d
  let d ← scanDanger w (← me i) talked range range2
  let mut danger := d.found   -- V_22
  -- IL_176d..1a8f
  if danger && !talked then
    let flee := fleeDirection d
    let mut nearest : Float32 := 0   -- V_55
    if d.left != -1 then nearest := -d.left
    if nearest == 0 || (d.right < nearest && d.right > 0) then nearest := d.right
    let n ← me i
    if n.ai0 == 8 then
      if n.direction == -flee then
        let r ← next 300
        upd i fun n => { n with ai0 := 1, ai1 := f (300 + r), ai2 := 0, localAI3 := 0, netUpdate := true }
    else if n.ai0 == 10 || n.ai0 == 12 || n.ai0 == 13 || n.ai0 == 14 || n.ai0 == 15 then pure ()
    else if w.tables.prettySafe n.type != -1 && !geU (f (w.tables.prettySafe n.type)) nearest then
      danger := false
    else if n.ai0 != 1 then
      let ax := trunc ((n.position.x + f (cdiv n.width 2) + f (15 * n.direction)) / 16)
      let ay := trunc ((n.position.y + f n.height - 16) / 16)
      let (_, avoid) ← match walkPrediction w (← get) n tx rx false ax ay with
        | .ok r => pure r | .error e => throw e
      if !avoid then
        let n ← me i
        if n.ai0 == 3 || n.ai0 == 4 || n.ai0 == 16 || n.ai0 == 17 then
          let k := (trunc n.ai2).toNat
          let partner ← npcAt k
          if partner.active then
            let r ← next 120
            updAt k fun o => { o with ai0 := 1, ai1 := f (120 + r), ai2 := 0, localAI3 := 0, direction := -flee, netUpdate := true }
        let r ← next 120
        upd i fun n => { n with ai0 := 1, ai1 := f (120 + r), ai2 := 0, localAI3 := 0, direction := if infectedDryad then flee else -flee, netUpdate := true }
    else if !infectedDryad && n.ai0 == 1 && n.direction != -flee then
      upd i fun n => { n with direction := -flee, netUpdate := true }
  -- the state machine
  let n ← me i
  if n.ai0 == 0 then
    -- IL_1a8f..2074: idle
    if talked && talkDir != 0 && !danger && !d.inRange then upd i fun n => { n with direction := talkDir }
    if (← me i).stinky then upd i fun n => { n with ai0 := 1 }
    if (← me i).localAI3 > 0 then upd i fun n => { n with localAI3 := n.localAI3 - 1 }
    if goHome && !talked then
      -- IL_1b6c..1cb1
      if tx == rx && ty == ry then
        let n ← me i
        if n.velocity.x != 0 then upd i fun n => { n with netUpdate := true }
        let n ← me i
        if n.velocity.x > 0.1 then upd i fun n => { n with velocity.x := n.velocity.x - 0.1 }
        else if n.velocity.x < -0.1 then upd i fun n => { n with velocity.x := n.velocity.x + 0.1 }
        else
          upd i fun n => { n with velocity.x := 0 }
          tryForcingSitting w i rx ry
      else
        upd i fun n => { n with direction := if tx > rx then -1 else 1 }
        let r ← next 200
        upd i fun n => { n with ai0 := 1, ai1 := f (200 + r), ai2 := 0, localAI3 := 0, netUpdate := true }
    else
      -- IL_1cb1..1f95
      let n ← me i
      if n.velocity.x > 0.1 then upd i fun n => { n with velocity.x := n.velocity.x - 0.1 }
      else if n.velocity.x < -0.1 then upd i fun n => { n with velocity.x := n.velocity.x + 0.1 }
      else upd i fun n => { n with velocity.x := 0 }
      if (← me i).ai1 > 0 then upd i fun n => { n with ai1 := n.ai1 - 1 }
      let n ← me i
      let ax := trunc ((n.position.x + f (cdiv n.width 2) + f (15 * n.direction)) / 16)
      let ay := trunc ((n.position.y + f n.height - 16) / 16)
      let (_, avoid) ← match walkPrediction w (← get) n tx rx false ax ay with
        | .ok r => pure r | .error e => throw e
      if n.wet then stop .liquid "AI_007 idle: wet"
      let mut walk := true    -- V_63
      if avoid then walk := false
      if !gtU (← me i).ai1 0 then
        if walk && !avoid then
          let r ← next 300
          upd i fun n => { n with ai0 := 1, ai1 := f (200 + r), ai2 := 0, localAI3 := 0, netUpdate := true }
        else
          let r ← next 120
          upd i fun n => { n with direction := n.direction * -1, ai1 := f (60 + r), netUpdate := true }
    -- IL_1f95..2074: drift back towards home
    let n ← me i
    if !goHome || isInAGoodRestingSpot w n tx ty rx ry then
      if tx < rx - 25 || tx > rx + 25 then
        if n.localAI3 == 0 then
          if tx < rx - 50 && n.direction == -1 then upd i fun n => { n with direction := 1, netUpdate := true }
          else if tx > rx + 50 && n.direction == 1 then upd i fun n => { n with direction := -1, netUpdate := true }
      else
        if (← next 80) == 0 && (← me i).localAI3 == 0 then
          upd i fun n => { n with localAI3 := 200, direction := n.direction * -1, netUpdate := true }
  else if n.ai0 == 1 then
    walkState w i goHome danger tx ty rx ry
  else if n.ai0 == 2 || n.ai0 == 11 then
    -- IL_3312..33e7
    upd i fun n => { n with localAI3 := n.localAI3 - 1 }
    if (← next 60) == 0 && (← me i).localAI3 == 0 then
      upd i fun n => { n with localAI3 := 60, direction := n.direction * -1, netUpdate := true }
    upd i fun n => { n with ai1 := n.ai1 - 1, velocity.x := n.velocity.x * 0.8 }
    if !gtU (← me i).ai1 0 then
      let r ← next 60
      upd i fun n => { n with localAI3 := 40, ai0 := 0, ai1 := f (60 + r), netUpdate := true }
  else if #[3, 4, 5, 8, 9, 16, 17, 20, 21, 22, 23].contains n.ai0 then
    -- IL_3495..35e5
    upd i fun n => { n with velocity.x := n.velocity.x * 0.8, ai1 := n.ai1 - 1 }
    let n ← me i
    if n.ai0 == 8 && n.ai1 < 60 && danger then upd i fun n => { n with ai1 := 180, netUpdate := true }
    let n ← me i
    if n.ai0 == 5 then
      let (sx, sy) := sitTile n
      let t ← tile w sx sy
      if isActive t && (ttype t == 15 || ttype t == 497) then emit (.sitting i sx sy)
      else upd i fun n => { n with ai1 := 0 }
    if !gtU (← me i).ai1 0 then
      let r ← next 60
      let r2 ← next 60
      upd i fun n => { n with ai0 := 0, ai1 := f (60 + r), ai2 := 0, localAI3 := f (30 + r2), netUpdate := true }
  else if n.ai0 == 6 || n.ai0 == 7 || n.ai0 == 18 || n.ai0 == 19 then
    -- IL_3624..379c
    let n ← me i
    if n.ai0 == 18 && (n.localAI3 < 1 || n.localAI3 > 2) then upd i fun n => { n with localAI3 := 2 }
    upd i fun n => { n with velocity.x := n.velocity.x * 0.8, ai1 := n.ai1 - 1 }
    let n ← me i
    let k := trunc n.ai2
    let s ← get
    let pl := s.players.find? (fun p => (p.index : Int) == k)
    let g := gridNow w s
    let ok := match pl with
      | some p => 0 ≤ k && k ≤ 255 && p.canBeTalkedTo && !(dist p.center n.center > 200) &&
          OriginalStarterMelee.LineOfSight.canHitLine g n.top 0 0 ⟨p.position.x + single p.width / 2, p.position.y⟩ 0 0
      | none => false
    if !ok then upd i fun n => { n with ai1 := 0 }
    let n ← me i
    if !leU n.ai1 0 then
      let some p := pl | fail "talking partner missing"
      let dir : Int := if n.center.x < p.center.x then 1 else -1
      if dir != n.direction then upd i fun n => { n with netUpdate := true }
      upd i fun n => { n with direction := dir }
    else
      let r ← next 60
      let r2 ← next 60
      upd i fun n => { n with ai0 := 0, ai1 := f (60 + r), ai2 := 0, localAI3 := f (30 + r2), netUpdate := true }
  else if n.ai0 == 10 || n.ai0 == 12 || n.ai0 == 13 || n.ai0 == 14 || n.ai0 == 15 || n.ai0 == 24 then
    stop (.attack (trunc n.ai0)) s!"AI_007: attack state {n.ai0}"
  -- IL_55f8..5818: swimming types only
  -- IL_5818..6d4a: what to do next
  tail w i talked danger d chance
where
  -- `ai[0] = 1`: walking (IL_2074..32f1).
  walkState (w : World) (i : Nat) (goHome danger : Bool) (tx ty rx ry : Int) : S Unit := do
    let n ← me i
    if goHome && isInAGoodRestingSpot w n tx ty rx ry then
      let r ← next 200
      upd i fun n => { n with ai0 := 0, ai1 := f (200 + r), localAI3 := 60, netUpdate := true }
      return
    -- IL_20f7..21e0 (V_70: drowning, false on dry land)
    let g ← grid w
    let drowning := OriginalForestCritter.drownCollision g n.position n.width.toNat n.height.toNat
    if drowning then stop .liquid "AI_007 walk: drowning"
    if !n.homeless && !w.tables.tileDungeon (ttype (← tile w tx ty)) && (tx < rx - 35 || tx > rx + 35) then
      if n.position.x < f (rx * 16) && n.direction == -1 then upd i fun n => { n with ai1 := n.ai1 - 5 }
      else if n.position.x > f (rx * 16) && n.direction == 1 then upd i fun n => { n with ai1 := n.ai1 - 5 }
    upd i fun n => { n with ai1 := n.ai1 - 1 }
    if !gtU (← me i).ai1 0 then
      let r ← next 300
      let r2 ← next 900
      upd i fun n => { n with ai0 := 0, ai1 := f (300 + r) + f r2, ai2 := 0, localAI3 := 60, netUpdate := true }
    -- IL_228b..24e1: closing the door behind
    let n ← me i
    if n.closeDoor then
      let cx := (n.position.x + f (cdiv n.width 2)) / 16
      if cx > f (n.doorX + 2) || !geU cx (f (n.doorX - 2)) then
        let t ← tile w n.doorX n.doorY
        if ttype t == 11 && isActive t then
          let closed ← closeDoor w n.doorX n.doorY false
          if closed then
            upd i fun n => { n with closeDoor := false }
          let n ← me i
          let cx := (n.position.x + f (cdiv n.width 2)) / 16
          let cy := (n.position.y + f (cdiv n.height 2)) / 16
          if cx > f (n.doorX + 4) || cx < f (n.doorX - 4) || cy > f (n.doorY + 4) || !geU cy (f (n.doorY - 4)) then
            upd i fun n => { n with closeDoor := false }
        else if ttype t == 389 && isActive t then fail "tall gate"
        else upd i fun n => { n with closeDoor := false }
    -- IL_24e1..2778: walking speed
    let n ← me i
    let mut maxSpeed : Float32 := 1      -- V_71
    let mut accel : Float32 := 0.07      -- V_72
    if n.friendly && danger then
      maxSpeed := 1.5
      let hurt := 1 - (f n.life / f n.lifeMax)
      maxSpeed := maxSpeed + hurt * 0.9
      accel := 0.1
    let vx := n.velocity.x
    if vx < -maxSpeed || !leU vx maxSpeed then
      if n.velocity.y == 0 then upd i fun n => { n with velocity := ⟨n.velocity.x * 0.8, n.velocity.y * 0.8⟩ }
    else if !geU vx maxSpeed && n.direction == 1 then
      let v := vx + accel
      upd i fun n => { n with velocity.x := if !leU v maxSpeed then maxSpeed else v }
    else if !leU vx (-maxSpeed) && n.direction == -1 then
      let v := vx - accel
      upd i fun n => { n with velocity.x := if !leU v maxSpeed then maxSpeed else v }
    -- IL_2778..282d: StepDown / StepUp
    let n ← me i
    let allowPlatform := leU (f (n.homeTileY * 16 - 32)) n.position.y     -- V_73
    if !allowPlatform && n.velocity.y == 0 then
      let g ← grid w
      let body : OriginalPlayerCollision.Body := { position := n.position, velocity := n.velocity, width := n.width.toNat, height := n.height.toNat, gfxOffY := n.gfxOffY, stepSpeed := n.stepSpeed }
      let b := body.stepDown g
      upd i fun n => { n with position := b.position, velocity := b.velocity, gfxOffY := b.gfxOffY, stepSpeed := b.stepSpeed }
    let n ← me i
    if geU n.velocity.y 0 then
      let s ← get
      let cw : OriginalForestCritter.World := { grid := gridNow w s, worldSurface := w.worldSurface, bottomWorld := w.bottomWorld, ignoredByNpcStepUp := w.tables.ignoredByNpcStepUp }
      let r := OriginalForestCritter.stepUp cw n.position n.velocity n.width.toNat n.height.toNat n.stepSpeed n.gfxOffY allowPlatform 1
      upd i fun n => { n with position := r.position, velocity := r.velocity, stepSpeed := r.stepSpeed, gfxOffY := r.gfxOffY }
    -- IL_282d..31d4
    let n ← me i
    if n.velocity.y == 0 then
      let ax := trunc ((n.position.x + f (cdiv n.width 2) + f (15 * n.direction)) / 16)   -- V_76
      let ay := trunc ((n.position.y + f n.height - 16) / 16)                            -- V_77
      let (keep0, avoid0) ← match walkPrediction w (← get) n tx rx false ax ay with
        | .ok r => pure r | .error e => throw e
      let mut keep := keep0    -- V_79
      let mut avoid := avoid0  -- V_80
      -- IL_2987..2a57: about to fall
      if avoid then
        let fx := trunc ((n.position.x + f (cdiv n.width 2)) / 16)
        let mut solids := 0
        for dx in [0:3] do
          let t ← tile w (fx + dx - 1) (ay + 1)
          if nactive t && (rules w t).solid then solids := solids + 1
        if !(solids > 2) then
          if n.velocity.x != 0 then upd i fun n => { n with netUpdate := true }
          avoid := false
          keep := false
          let r ← next 50
          upd i fun n => { n with ai0 := 0, ai1 := f (50 + r), ai2 := 0, localAI3 := 40 }
      -- IL_2a57..2ae3
      let n ← me i
      if n.position.x == n.localAI3 then
        upd i fun n => { n with direction := n.direction * -1, netUpdate := true, localAI3 := 180 }
      upd i fun n => { n with localAI3 := -1 }
      -- IL_2ae3..2cbe: doors
      let n ← me i
      let t0 ← tile w ax ay
      let t1 ← tile w ax (ay - 1)
      let t2 ← tile w ax (ay - 2)
      let short := cdiv n.height 16 < 3
      let mut handled := false
      if n.townNPC && nactive t2 && (ttype t2 == 10 || ttype t2 == 388) then
        let r ← next 10
        if r == 0 || goHome then
          handled := true
          if ttype t2 == 388 then fail "tall gate"
          let opened ← openDoor w ax (ay - 2) n.direction
          if opened then
            upd i fun n => { n with closeDoor := true, doorX := ax, doorY := ay - 2, netUpdate := true, ai1 := n.ai1 + 80 }
          else
            let opened2 ← openDoor w ax (ay - 2) (-n.direction)
            if opened2 then
              upd i fun n => { n with closeDoor := true, doorX := ax, doorY := ay - 2, netUpdate := true, ai1 := n.ai1 + 80 }
            else
              -- ShiftTallGate(x, y - 2, false) is false for a tile that is not 388
              upd i fun n => { n with direction := n.direction * -1, netUpdate := true }
      if !handled then
        -- IL_2cbe..304b: jumps and turns
        let n ← me i
        if (n.velocity.x < 0 && n.direction == -1) || (n.velocity.x > 0 && n.direction == 1) then
          let mut turn := false      -- V_95
          let mut cornered := false  -- V_96
          let g ← grid w
          let solidNotTop (t : Tile) := nactive t && (rules w t).solid && !(rules w t).solidTop
          let d := n.direction
          if solidNotTop t2 && (!short || solidNotTop t1) then
            if !OriginalForestCritter.solidTilesVersatile g (ax - d * 2) (ax - d) (ay - 5) (ay - 1) &&
                !OriginalForestCritter.solidTiles g ax ax (ay - 5) (ay - 3) then
              upd i fun n => { n with velocity.y := -6, netUpdate := true }
            else if danger then cornered := true; turn := true
            else turn := true
          else if solidNotTop t1 then
            if !OriginalForestCritter.solidTilesVersatile g (ax - d * 2) (ax - d) (ay - 4) (ay - 1) &&
                !OriginalForestCritter.solidTiles g ax ax (ay - 4) (ay - 2) then
              upd i fun n => { n with velocity.y := -5, netUpdate := true }
            else if danger then cornered := true; turn := true
            else turn := true
          else if !leU (n.position.y + f n.height - f (ay * 16)) 20 && nactive t0 && (rules w t0).solid &&
              !(let sl := slopeId t0.shape; sl == 1 || sl == 2) then
            if !OriginalForestCritter.solidTilesVersatile g (ax - d * 2) ax (ay - 3) (ay - 1) then
              upd i fun n => { n with velocity.y := -4.4, netUpdate := true }
            else if danger then cornered := true; turn := true
            else turn := true
          else if avoid then
            turn := true
            if danger then cornered := true
          if cornered then
            keep := false
            upd i fun n => { n with velocity.x := 0, ai0 := 8, ai1 := 240, netUpdate := true }
          if turn then upd i fun n => { n with direction := n.direction * -1, velocity.x := n.velocity.x * -1, netUpdate := true }
          if keep then upd i fun n => { n with ai1 := 90, netUpdate := true }
          if (← me i).velocity.y < 0 then upd i fun n => { n with localAI3 := n.position.x }
        -- IL_304b..31d4: wet jump boost (unreachable on dry land)
    -- IL_31d4..32f1: stuck
    let n ← me i
    let g := (← get).gravity
    if n.oldPosition == n.position && n.oldVelocity == ⟨n.velocity.x + 0, n.velocity.y + g⟩ && !danger then
      let c := n.localAI2
      upd i fun n => { n with localAI2 := c + 1 }
      if !ltU c 30 then
        upd i fun n => { n with localAI2 := 0 }
        let r ← next 300
        upd i fun n => { n with ai0 := 1, ai1 := f (200 + r), ai2 := 0, direction := n.direction * -1, velocity.x := n.velocity.x * -1, netUpdate := true }
  -- IL_5818..6d4a: chats, games, sitting, furniture, nurse healing and attacks.
  tail (w : World) (i : Nat) (talked danger : Bool) (d : Danger) (chance0 : Float32) : S Unit := do
    if talked && !danger then return
    let mut chance := chance0
    let flee := fleeDirection d
    if talked then chance := chance * 0.8
    let n ← me i
    let free := !geU n.ai0 2 && !danger && !n.wet                                    -- V_185
    let mut attackReady := (n.ai0 < 2 || n.ai0 == 8) && danger && !d.stinky           -- V_186
    if n.localAI1 > 0 then upd i fun n => { n with localAI1 := n.localAI1 - 1 }
    if (← me i).localAI1 > 0 then attackReady := false
    if attackReady && n.type == 20 then
      -- no town NPC carries buff 165 (Dryad's Blessing) here: any town NPC within 1200 keeps the Dryad ready
      attackReady := (← get).npcs.any fun o => o.active && o.townNPC && !(dist n.center o.center > 1200)
    let canTalk := n.velocity.y == 0 && !w.tables.isTownPet n.type
    let mut done := false
    -- IL_5980..5b86: NPC chat (ai 3 / 4)
    if canTalk && free && n.ai0 == 0 && n.velocity.y == 0 then
      if (← next 300) == 0 then
        done := true
        let dur ← chatDuration
        startChat w i dur 3 4 false
    -- IL_5b86..5dec: games (ai 16 / 17)
    if !done then
      let n ← me i
      if canTalk && free && n.ai0 == 0 && n.velocity.y == 0 then
        if (← next 1800) == 0 then
          done := true
          let dur ← chatDuration
          startChat w i dur 16 17 true
    -- IL_5dec..5f41: party (ai 6)
    if !done then
      let n ← me i
      if !w.tables.isTownPet n.type && free && n.ai0 == 0 && n.velocity.y == 0 then
        if (← next 1200) == 0 then
          if n.type == 208 || (w.partyIsUp && w.tables.attackType n.type == w.tables.attackType 208) then
            done := true
            talkToPlayer w i 300 150 6 true
    -- IL_5f41..6063: type 550 only
    if !done then
      let n ← me i
      if free && n.ai0 == 0 && n.velocity.y == 0 then
        if (← next 600) == 0 then
          if n.type == 550 then fail "type 550"
    -- IL_6063..60da: pause (ai 2)
    if !done then
      let n ← me i
      if !w.tables.isTownPet n.type && free && n.ai0 == 0 && n.velocity.y == 0 then
        if (← next 1800) == 0 then
          done := true
          let r ← next2 1 2
          upd i fun n => { n with ai0 := 2, ai1 := f (45 * r), netUpdate := true }
    -- IL_60da..6152: type 229 only
    if !done then
      let n ← me i
      if free && n.ai0 == 0 && n.velocity.y == 0 then
        if (← next 600) == 0 then
          if n.type == 229 then fail "type 229"
    -- IL_6152..6258: talk to a player (ai 7)
    if !done then
      let n ← me i
      if free && n.ai0 == 0 && n.velocity.y == 0 then
        if (← next 1200) == 0 then
          done := true
          talkToPlayer w i 220 150 7 false
    -- IL_6258..64c6: sit down while walking (ai 5)
    if !done then
      let n ← me i
      if free && n.ai0 == 1 && n.velocity.y == 0 && sitChance > 0 then
        if (← next sitChance) == 0 then
          done := true
          let (sx, sy) := sitTile n
          let mut ok := inWorld w sx sy 1
          if ok && someoneSitsAt (← get) (sx, sy) then ok := false
          if ok then
            for p in (← get).players do
              if p.active && p.sittingAt == some (sx, sy) then
                ok := false
                break
          if ok then
            let t ← tile w sx sy
            ok := isActive t && (ttype t == 15 || ttype t == 497)
            if ok && ttype t == 15 && frameY t ≥ 1080 && frameY t ≤ 1098 then ok := false
            if ok then
              let r ← next 10800
              let dir : Int := if frameX t != 0 then 1 else -1
              let bottom : Vector := ⟨f (sx * 16 + 8 + 2 * dir), f (sy * 16 + 16)⟩
              upd i fun n => { n with ai0 := 5, ai1 := f (900 + r), direction := dir, position := ⟨bottom.x - f (cdiv n.width 2), bottom.y - f n.height⟩, velocity := ⟨0, 0⟩, localAI3 := 0, netUpdate := true }
    -- IL_64c6..65e9: furniture (ai 9)
    if !done then
      let n ← me i
      if free && n.ai0 == 1 && n.velocity.y == 0 then
        if (← next 600) == 0 then
          let s ← get
          if plotTileLine n.top n.bottom (single n.width) (searchAvoidedByNPCs w s) then
            let (px, py) := toTile ⟨n.center.x + f (n.direction * 10), n.center.y⟩
            let mut ok := inWorld w px py 1
            if ok then
              let t ← tile w px py
              if !(nactive t && w.tables.interactableByNPCs (ttype t)) then ok := false
            if ok then
              let r ← next 90
              upd i fun n => { n with ai0 := 9, ai1 := f (40 + r), velocity := ⟨0, 0⟩, localAI3 := 0, netUpdate := true }
    -- IL_65e9..6761: the Nurse heals a hurt town NPC (ai 13)
    let n ← me i
    if !geU n.ai0 2 && n.velocity.y == 0 && n.type == 18 && n.breath > 0 then
      let s ← get
      let g := gridNow w s
      let mut best : Int := -1
      for o in s.npcs do
        if !o.active || !o.townNPC || o.life == o.lifeMax then continue
        if best != -1 then
          let b := s.npcs[best.toNat]!
          if o.lifeMax - o.life ≤ b.lifeMax - b.life then continue
        if !OriginalStarterMelee.LineOfSight.canHitLine g n.position n.width n.height o.position o.width o.height then continue
        if geU (dist n.center o.center) 500 then continue
        best := o.whoAmI
      if best != -1 then stop (.attack 13) s!"the Nurse starts healing NPC {best}"
    -- IL_6761..6d4a: attacks (a hit target starts ai 10 / 12 / 14 / 15)
    let n ← me i
    let avg := w.tables.attackAverageChance n.type
    let mut k := (f avg * chance).toInt32.toInt
    if k < 1 then k := 1
    let kind := w.tables.attackType n.type
    if attackReady && n.velocity.y == 0 && avg > 0 && 0 ≤ kind && kind ≤ 3 then
      if (← next k) == 0 then
        let s ← get
        let g := gridNow w s
        let first := if flee == 1 then d.rightTarget else d.leftTarget
        let second := if flee == 1 then d.leftTarget else d.rightTarget
        let reach (t : Int) : Bool :=
          let o := s.npcs[t.toNat]!
          if kind == 1 || kind == 2 then OriginalStarterMelee.LineOfSight.canHitLine g n.center 0 0 o.center 0 0
          else OriginalForestCritter.canHit g n.center o.center
        let mut target := first
        if target != -1 && !reach target then
          target := if second != -1 && reach second then second else -1
        if target != -1 then
          if kind == 1 then
            let o := s.npcs[target.toNat]!
            let dx := o.center.x - n.center.x
            let dy := o.center.y - n.center.y
            let len := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
            let y := dy / len
            if !(y > 0.5 || y < -0.5) then stop (.attack 12) s!"attack type 1 at NPC {target}"
          else stop (.attack (if kind == 0 then 10 else if kind == 2 then 14 else 15)) s!"attack type {kind} at {target}"
        else if kind == 2 && n.type == 20 then stop (.attack 14) "the Dryad's ward"
  chatDuration : S Int := do
    let base : Int := 420
    if (← next 2) == 0 then return base * (← next2 1 4)
    return base * (← next2 1 3)
  startChat (w : World) (i : Nat) (dur : Int) (me' partner' : Float32) (game : Bool) : S Unit := do
    let s ← get
    let n ← me i
    let g := gridNow w s
    for o in s.npcs do
      let busy := (o.ai0 == 1 && o.closeDoor) || (o.ai0 == 1 && o.ai1 > 200) || o.ai0 > 1 || o.wet
      if o.whoAmI == i || !o.active || !(o.townNPC && o.aiStyle == 7 && o.velocity.y == 0) || busy then continue
      if game && w.tables.isTownPet o.type then continue
      if geU (dist o.center n.center) 100 || leU (dist o.center n.center) 20 then continue
      if !OriginalForestCritter.canHit g n.center o.center then continue
      let dir := dirInt (n.position.x < o.position.x)
      if game then
        let a ← next 4
        let b ← next (3 - a)
        upd i fun n => { n with ai0 := me', ai1 := f dur, ai2 := f o.whoAmI, localAI2 := f a, localAI3 := f b, direction := dir, netUpdate := true }
        updAt o.whoAmI fun o => { o with ai0 := partner', ai1 := f dur, ai2 := f i, localAI2 := 0, localAI3 := 0, direction := -dir, netUpdate := true }
      else
        upd i fun n => { n with ai0 := me', ai1 := f dur, ai2 := f o.whoAmI, direction := dir, netUpdate := true }
        updAt o.whoAmI fun o => { o with ai0 := partner', ai1 := f dur, ai2 := f i, direction := -dir, netUpdate := true }
      return
  talkToPlayer (w : World) (i : Nat) (dur : Int) (range : Float32) (state : Float32) (alive : Bool) : S Unit := do
    let s ← get
    let n ← me i
    let g := gridNow w s
    for p in s.players do
      let ok := if alive then p.active && !p.dead else p.canBeTalkedTo
      if !ok || geU (dist p.center n.center) range then continue
      if !OriginalStarterMelee.LineOfSight.canHitLine g n.top 0 0 ⟨p.position.x + single p.width / 2, p.position.y⟩ 0 0 then continue
      upd i fun n => { n with ai0 := state, ai1 := f dur, ai2 := f p.index, direction := dirInt (n.position.x < p.position.x), netUpdate := true }
      return


/-! ## `NPC.FindFrame` (town branch, IL_3ed1..61ad and IL_61ad..6447) -/

/-- The talking-state frame reset (`(V_67 - row) - 1 ≤ 1 (unsigned)` or `- 4 ≤ 1`, or row 0). -/
private def keepTalkRow (top row : Int) : Bool :=
  let d := top - row
  d == 1 || d == 2 || d == 4 || d == 5 || row == 0

/-- C# `double % double` for a non-negative dividend. -/
private def fmod (a b : Float) : Float := a - (a / b).floor * b

def findFrame (w : World) (i : Nat) : S Unit := do
  let n ← me i
  let num := w.frameHeight n.type
  let top := w.tables.npcFrameCount n.type - w.tables.attackFrameCount n.type     -- V_67
  if n.velocity.y != 0 then
    -- IL_61ad: airborne (no town slime, not 462)
    upd i fun n => { n with frameCounter := 0, frameY := num }
    return
  if n.direction == 1 then upd i fun n => { n with spriteDirection := 1 }
  if n.direction == -1 then upd i fun n => { n with spriteDirection := -1 }
  let n ← me i
  let a := n.ai0
  let row := cdiv n.frameY num
  let talkReset (n : Npc) : Npc := if keepTalkRow top row then n else { n with frameY := 0, frameCounter := 0 }
  if a == 23 || (20 ≤ a && a ≤ 22) || a == 11 || a == 6 || a == 18 || a == 25 || a == 12 || a == 14 || a == 24 || a == 10 || a == 13 || a == 15 then
    stop .unsupported s!"FindFrame: ai[0] = {a}"
  if a == 2 then
    let fc := n.frameCounter + 1
    if row == top - 1 && fc ≥ 5 then upd i fun n => { n with frameY := 0, frameCounter := 0 }
    else if row == 0 && fc ≥ 40 then upd i fun n => { n with frameY := num * (top - 1), frameCounter := 0 }
    else if n.frameY != 0 && n.frameY != num * (top - 1) then upd i fun n => { n with frameY := 0, frameCounter := 0 }
    else upd i fun n => { n with frameCounter := fc }
    return
  if a == 5 then
    upd i fun n => { n with frameY := num * (top - 3), frameCounter := 0 }
    return
  if a == 7 || a == 19 then
    upd i fun n => talkReset { n with frameCounter := n.frameCounter + 1 }
    let fc := (← me i).frameCounter
    if fc == 16 || fc == 160 then stop .emote s!"FindFrame: NewBubbleNPC at frameCounter {fc} (PickNPCEmote)"
    let r : Int := if fc < 16 then 0 else if fc < 128 then (if fmod fc 16 < 8 then top - 2 else 0)
      else if fc < 160 then 0 else if fc < 220 then (if fmod fc 12 < 6 then top - 2 else 0) else 0
    upd i fun n => { n with frameY := num * r, frameCounter := if fc ≥ 220 then 0 else n.frameCounter }
    return
  if a == 9 then
    upd i fun n => talkReset { n with frameCounter := n.frameCounter + 1 }
    let n ← me i
    let fc := n.frameCounter
    let mut r : Int := if fc < 10 then 0 else if fc < 16 then top - 5 else top - 4
    if n.ai1 < 16 then r := top - 5
    if n.ai1 < 10 then r := 0
    upd i fun n => { n with frameY := num * r }
    return
  let canTalk := n.townNPC && n.aiStyle == 7 && !w.tables.isTownPet n.type
  if canTalk && (a == 3 || a == 4) then
    upd i fun n => talkReset { n with frameCounter := n.frameCounter + 1 }
    let n ← me i
    let fc := n.frameCounter
    let starter := a == 3
    let mut mine : Int := 0
    let mut emoteA := false
    if fc < 10 then mine := 0 else if fc < 16 then mine := top - 5 else if fc < 46 then mine := top - 4
    else if fc < 60 then mine := top - 5 else if fc < 216 then mine := 0 else if fc == 216 then emoteA := true
    else if fc < 286 then mine := if fmod fc 12 < 6 then top - 2 else 0 else if fc < 320 then mine := 0
    else if fc == 320 then emoteA := true else if fc < 420 then mine := if fmod fc 16 < 8 then top - 2 else 0 else mine := 0
    let mut theirs : Int := 0
    let mut emoteB := false
    if fc < 70 then theirs := 0 else if fc == 70 then emoteB := true
    else if fc < 160 then theirs := if fmod fc 16 < 8 then top - 2 else 0 else if fc < 166 then theirs := top - 5
    else if fc < 186 then theirs := top - 4 else if fc < 200 then theirs := top - 5 else if fc < 320 then theirs := 0
    else if fc < 326 then theirs := top - 1 else theirs := 0
    if starter then
      let partner ← npcAt (trunc n.ai2).toNat
      if emoteA || (emoteB && partner.townNPC && partner.aiStyle == 7 && partner.velocity.y == 0 && !w.tables.isTownPet partner.type) then
        stop .emote s!"FindFrame: chat emote at frameCounter {fc}"
    upd i fun n => { n with frameY := num * (if starter then mine else theirs), frameCounter := if fc ≥ 420 then 0 else n.frameCounter }
    return
  if canTalk && (a == 16 || a == 17) then
    upd i fun n => talkReset { n with frameCounter := n.frameCounter + 1 }
    let n ← me i
    let fc := n.frameCounter
    let mut r : Int := 0
    let mut event := false
    -- the V_110 ladder of IL_5a33..5cd1, step by step
    let ladder : List (Float × Int) := [(10, 0), (16, top - 5), (22, top - 4), (28, top - 5), (34, top - 4), (40, top - 5)]
    let ladder2 : List (Float × Int) := [(70, top - 4), (76, top - 5), (82, top - 4), (88, top - 5), (94, top - 4), (100, top - 5)]
    let ladder3 : List (Float × Int) := [(130, top - 4), (136, top - 5), (142, top - 4), (148, top - 5), (154, top - 4), (160, top - 5)]
    let pick (l : List (Float × Int)) : Option Int := (l.find? (fun (lim, _) => fc < lim)).map (·.2)
    if let some v := pick ladder then r := v
    else if fc == 40 then event := true
    else if let some v := pick ladder2 then r := v
    else if fc == 100 then event := true
    else if let some v := pick ladder3 then r := v
    else if fc == 160 then event := true
    else if fc < 220 then r := top - 4
    else if fc < 226 then r := top - 5
    else r := 0
    if a == 16 && event then stop .emote s!"FindFrame: game emote at frameCounter {fc}"
    upd i fun n => { n with frameY := num * r, frameCounter := if fc ≥ 420 then 0 else n.frameCounter }
    return
  -- IL_5f32..61ad: standing or walking
  if n.velocity.x == 0 then
    upd i fun n => { n with frameY := 0, frameCounter := 0 }
    return
  let speed : Float := 6
  let fc := n.frameCounter + (n.velocity.x.abs * 2).toFloat + 1
  let base := num * 2
  let mut fy := if n.frameY < base then base else n.frameY
  let mut fc := fc
  if fc > speed then
    fy := fy + num; fc := 0
  if cdiv fy num ≥ w.tables.npcFrameCount n.type - w.tables.extraFramesCount n.type then fy := base
  upd i fun n => { n with frameY := fy, frameCounter := fc }

/-! ## `NPC.UpdateCollision` for a town NPC -/

private def anyTileIn (w : World) (s : Sim) (x0 x1 y0 y1 : Int) (p : Tile → Bool) : Bool := Id.run do
  let mut x := x0
  while x ≤ x1 do
    let mut y := y0
    while y ≤ y1 do
      if p (tileNow w s x y) then return true
      y := y + 1
    x := x + 1
  return false

def updateCollision (w : World) (i : Nat) : S Unit := do
  let s ← get
  let g := gridNow w s
  let n ← me i
  -- Collision_WalkDownSlopes
  upd i fun n => { n with velocity := OriginalCollision.walkDownSlope g n.position n.velocity n.width.toNat n.height.toNat s.gravity }
  -- Collision_LavaCollision / Collision_WaterCollision: only a dry body away from liquid is transcribed
  let n ← me i
  let bx0 := trunc (n.position.x / 16) - 1
  let bx1 := trunc ((n.position.x + f n.width) / 16) + 2
  let by0 := trunc (n.position.y / 16) - 1
  let by1 := trunc ((n.position.y + f n.height) / 16) + 2
  if n.wet || anyTileIn w s bx0 bx1 by0 by1 (fun t => t.liquid.isSome) then stop .liquid "UpdateCollision: liquid next to the NPC"
  upd i fun n => { n with lavaWet := false, honeyWet := false, shimmerWet := false, wetCount := if n.wetCount > 0 then n.wetCount - 1 else 0 }
  -- Collision_DecideFallThroughPlatforms (aiStyle 7)
  let n ← me i
  let mut fall := false
  let night := n.townNPC && (!w.dayTime || w.invasionType > 0 || w.eclipse)
  if !night then
    let feet := cdiv (trunc (n.position.y + f n.height)) 16
    if n.homeTileY - feet > 16 then fall := true
  else if (n.position.y + f n.height - 8) / 16 < f (n.homeTileY - 1) then fall := true
  upd i fun n => { n with oldVelocity := n.velocity, collideX := false, collideY := false }
  -- ApplyTileCollision → Collision_MoveNormal, then Collision_MoveWhileDry
  let n ← me i
  let hit := OriginalCollision.tileCollision g n.position n.velocity n.width.toNat n.height.toNat { fallThrough := fall, fall2 := fall, hoik := true }
  let mut v := hit.displacement
  if hit.hitAbove then v := { v with y := 0.01 }
  upd i fun n => { n with velocity := v, collideX := n.oldVelocity.x != v.x, collideY := n.oldVelocity.y != v.y, oldPosition := n.position, oldDirection := n.direction, position := n.position + v }
  -- Collision_MoveSlopesAndStairFall(fall)
  if fall then upd i fun n => { n with stairFall := true }
  let n ← me i
  let cx := cdiv (trunc n.center.x) 16
  let cy := cdiv (trunc n.position.y) 16
  if inWorld w cx cy 0 then
    let t ← tile w cx cy
    let mut v8 := isActive t && (rules w t).solid
    if !n.townNPC then v8 := false
    if !w.dayTime || w.eclipse then v8 := true
    else if n.homeTileY - cdiv (trunc (n.position.y + f n.height)) 16 > 16 then v8 := true
    if v8 then
      let below := (n.position.y + f n.height - 8) / 16 < f n.homeTileY
      upd i fun n => { n with stairFall := below }
  let n ← me i
  let r := OriginalCollision.slopeCollision g n.position n.velocity n.width.toNat n.height.toNat n.stairFall
  if r.stairFall then upd i fun n => { n with stairFall := true }
  else if !fall then upd i fun n => { n with stairFall := false }
  if r.stair && (r.position.y - n.position.y).abs > 8 then
    upd i fun n => { n with gfxOffY := n.gfxOffY - (r.position.y - n.position.y), stepSpeed := 2 }
  upd i fun n => { n with position := r.position, velocity := r.velocity }
  -- ConveyorBeltCollision: only an empty answer
  let n ← me i
  let bx := trunc (n.position.x / 16)
  let by' := trunc ((n.position.y + f n.height) / 16)
  if anyTileIn w (← get) (bx - 2) (bx + 3) (by' - 2) (by' + 2) (fun t => isActive t && (ttype t == 421 || ttype t == 422)) then
    stop .wiredTile "conveyor belt next to the NPC"

/-! ## `NPC.UpdateNPC(i)` -/

/-- One `NPC.UpdateNPC(i)` call for the ordinary town NPC in slot `i` (single player). -/
def update (w : World) (i : Nat) : S Unit := do
  let n ← me i
  if !n.active then return
  if unsupportedType n.type || !n.townNPC || n.aiStyle != 7 then fail s!"UpdateNPC: slot {i} type {n.type} is not an ordinary town NPC"
  if n.extraValue > 0 then fail "extraValue"
  -- IL_0205: lookForHomeTimeout
  if n.lookForHomeTimeout > 0 then upd i fun n => { n with lookForHomeTimeout := n.lookForHomeTimeout - 1 }
  -- IL_0211..024f: the bottom of the world
  if !leU n.position.y (w.bottomWorld - 640 + f n.height) then stop .bottomOfWorld "UpdateNPC: below bottomWorld - 640"
  -- IL_0343..038d: UpdateNPC_UpdateGravity (no buffs)
  let n ← me i
  let ww := f w.maxTilesX / 4200
  let ww := ww * ww
  let mut fac := ((n.position.y / 16 - (60 + 10 * ww)).toFloat / (w.worldSurface / 6)).toFloat32
  if fac.toFloat < 0.25 then fac := 0.25
  if fac > 1 then fac := 1
  let (gravity, maxFall) : Float32 × Float32 := if n.wet then (0.2, 7) else (0.3 * fac, 10)
  modify fun s => { s with gravity }
  if n.soundDelay > 0 then upd i fun n => { n with soundDelay := n.soundDelay - 1 }
  if (← me i).life ≤ 0 then
    upd i fun n => { n with active := false, netUpdate := false, justHit := false }
    return
  -- IL_03b3..049a
  upd i fun n => { n with oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY }
  let n ← me i
  let ease := 1 + n.velocity.x.abs / 3
  let mut gy := n.gfxOffY
  if gy > 0 then
    gy := gy - ease * n.stepSpeed
    if gy < 0 then gy := 0
  else if gy < 0 then
    gy := gy + ease * n.stepSpeed
    if gy > 0 then gy := 0
  if gy > 16 then gy := 16
  if gy < -16 then gy := -16
  upd i fun n => { n with gfxOffY := gy }
  -- IL_049a..04a6: AI
  ai007 w i
  -- IL_05c8..05f1: immune[] countdown
  upd i fun n => { n with immune255 := if n.immune255 > 0 then n.immune255 - 1 else n.immune255 }
  -- IL_0671..06e5: gravity
  upd i fun n => { n with velocity.y := n.velocity.y + gravity }
  upd i fun n => { n with velocity.y := if !leU n.velocity.y maxFall then maxFall else n.velocity.y }
  upd i fun n => { n with velocity.x := if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then 0 else n.velocity.x }
  -- IL_06e5..072c: CheckDrowning, CheckLifeRegen, GetHurtByOtherNPCs (friendly town NPC, not type 37)
  let n ← me i
  let g := gridNow w (← get)
  if OriginalForestCritter.drownCollision g n.position n.width.toNat n.height.toNat then stop .liquid "CheckDrowning: under water"
  upd i fun n => { n with breath := if n.breath + 3 > 200 then 200 else n.breath + 3, breathCounter := 0 }
  let n ← me i
  if n.life < n.lifeMax then
    let bonus : Int := (if n.dryadWard then 10 else 0) + (if n.type == 22 then 5 else if n.type == 209 then 9 else 0)
    let regen := n.friendlyRegen + 1 + bonus
    if regen > 180 then upd i fun n => { n with friendlyRegen := 0, life := n.life + 1 }
    else upd i fun n => { n with friendlyRegen := regen }
  let n ← me i
  if n.immune255 == 0 then
    for o in (← get).npcs do
      if o.whoAmI != i && o.active && !o.friendly && o.damage > 0 && o.type != 624 && o.type != 690 &&
          intersects (trunc n.position.x) (trunc n.position.y) n.width n.height (trunc o.position.x) (trunc o.position.y) o.width o.height then
        stop .hurt s!"GetHurtByOtherNPCs: NPC {o.whoAmI} hits"
  -- IL_078e..07ed
  if !n.noTileCollide then updateCollision w i
  -- IL_07ed..0929: Collision.SwitchTiles (pressure plates, mines): only an empty answer
  let n ← me i
  let sx := trunc (n.position.x / 16)
  let sy := trunc (n.position.y / 16)
  if anyTileIn w (← get) (sx - 1) (trunc ((n.position.x + f n.width) / 16) + 2) (sy - 1) (trunc ((n.position.y + f n.height) / 16) + 2)
      (fun t => isActive t && (ttype t == 135 || ttype t == 210 || ttype t == 442 || ttype t == 443)) then
    stop .wiredTile "SwitchTiles: a pressure plate or mine next to the NPC"
  -- IL_0929..0956: CheckDialogue (no state), FindFrame, CheckActive (a town NPC returns at once)
  findFrame w i
  upd i fun n => { n with netUpdate := false, justHit := false }

/-- The NPC phase over the town NPCs in ascending slot order (`Main.UpdateWorld_NPCs` for these slots). -/
def updateTownSlots (w : World) (slots : List Nat) : S Unit := do
  for i in slots do update w i


/-! ## Chat buttons: `OpenShop` and `NurseHeal` -/

/-- `NPCInteractions.Initialize`: the `OpenShop(npcType, shopIndex)` registrations, in order (type 227 has a second
one, shop 25, behind `GameUI.PainterDecor`). -/
def shops : List (Int × Nat) :=
  [(17, 1), (19, 2), (20, 3), (38, 4), (54, 5), (107, 6), (108, 7), (124, 8), (142, 9), (160, 10), (178, 11), (207, 12),
   (208, 13), (209, 14), (227, 15), (228, 16), (229, 17), (353, 18), (368, 19), (453, 20), (550, 21), (588, 22), (633, 23),
   (663, 24), (227, 25)]

/-- `OpenShop.Condition()` for the NPC the player talks to: the shop indices offered, in registration order. -/
def shopButtons (talkNPCType : Int) : List Nat := (shops.filter (·.1 == talkNPCType)).map (·.2)

/-- What `OpenShop.Interact()` → `Main.OpenShop(shopIndex)` does: `playerInventory = true`, `stackSplit = 9999`,
`npcChatText = ""`, `SetNPCShopIndex(shopIndex)`, `shop[shopIndex].SetupShop(shopIndex)` (`OriginalMerchantShop` for
the Merchant), `SoundEngine.PlaySound(12)`. -/
structure OpenShopRequest where
  shopIndex : Nat
  deriving BEq, Repr

def pressShop (talkNPCType : Int) (shopIndex : Nat) : Option OpenShopRequest :=
  if (shopButtons talkNPCType).contains shopIndex then some { shopIndex } else none

/-- The local player as the Nurse reads it. -/
structure Patient where
  statLife : Int
  statLifeMax2 : Int
  /-- `buffType[k]`, `buffTime[k]` for `k < Player.maxBuffs` (empty slots are `(0, 0)`). -/
  buffs : Array (Int × Int)
  wallet : OriginalMerchantShop.Wallet
  discountAvailable : Bool := false
  /-- `currentShoppingSettings.PriceAdjustment`. -/
  priceAdjustment : Float := 1

structure NurseWorld where
  /-- `Main.debuff`, `BuffID.Sets.NurseCannotRemoveDebuff`, `BuffID.Count`. -/
  debuff : Int → Bool
  nurseCannotRemoveDebuff : Int → Bool
  buffCount : Int
  downedGolemBoss : Bool := false
  downedPlantBoss : Bool := false
  downedMechBossAny : Bool := false
  hardMode : Bool := false
  downedBoss3 : Bool := false
  downedQueenBee : Bool := false
  downedBoss2 : Bool := false
  downedBoss1 : Bool := false
  expertMode : Bool := false
  /-- `ChildSafety.Disabled`. -/
  childSafetyDisabled : Bool := true

private def removable (w : NurseWorld) (t : Int) : Bool :=
  !(0 ≤ t && t < w.buffCount && w.nurseCannotRemoveDebuff t)

/-- `Main.GetNurseHealCost()`. -/
def nurseHealCost (w : NurseWorld) (p : Patient) : Int := Id.run do
  let i32 (v : Int) : Int := (Int32.ofInt v).toInt
  let mut cost := i32 (p.statLifeMax2 - p.statLife)
  for (t, time) in p.buffs do
    if w.debuff t && time > 60 && removable w t then cost := i32 (cost + 100)
  if w.downedGolemBoss then cost := i32 (cost * 200)
  else if w.downedPlantBoss then cost := i32 (cost * 150)
  else if w.downedMechBossAny then cost := i32 (cost * 100)
  else if w.hardMode then cost := i32 (cost * 60)
  else if w.downedBoss3 || w.downedQueenBee then cost := i32 (cost * 25)
  else if w.downedBoss2 then cost := i32 (cost * 10)
  else if w.downedBoss1 then cost := i32 (cost * 3)
  if w.expertMode then cost := i32 (cost * 2)
  if p.discountAvailable then cost := (Float32.ofInt cost * 0.8).toInt32.toInt
  cost := (Float32.ofInt cost * p.priceAdjustment.toFloat32).toInt32.toInt
  return cost

/-- What `NurseHeal.Interact()` → `Main.NPCChatText_DoNurseHeal(GetNurseHealCost())` did. -/
structure NurseResult where
  cost : Int
  paid : Bool
  patient : Patient
  /-- `Player.HealEffect(amount, broadcast = true)`: the healed amount shown as combat text. -/
  healEffect : Option Int := none
  /-- `Lang.dialog(k)` put in `Main.npcChatText`. -/
  dialog : Nat
  rng : UnifiedRandom

/-- `NPCChatText_DoNurseHeal(GetNurseHealCost())` with `Main.rand`. Sounds (12, Item4) and the portrait hop are
not modelled. -/
def pressNurseHeal (w : NurseWorld) (p : Patient) (rng : UnifiedRandom) : NurseResult := Id.run do
  let cost := nurseHealCost w p
  if cost > 0 then
    let (paid, wallet) := OriginalMerchantShop.buyItem p.wallet cost
    if paid then
      let amount := p.statLifeMax2 - p.statLife
      let life := p.statLife.toFloat
      let max := p.statLifeMax2.toFloat
      let dialog := if life < max * 0.25 then 227 else if life < max * 0.5 then 228 else if life < max * 0.75 then 229 else 230
      -- DelBuff(k) and restart at slot 0 until no removable debuff is left
      let mut buffs := p.buffs
      let mut fuel := buffs.size + 1
      let mut k := 0
      while k < buffs.size && fuel > 0 do
        let (t, time) := buffs[k]!
        if w.debuff t && time > 0 && removable w t then
          buffs := delBuff buffs k
          k := 0
          fuel := fuel - 1
        else k := k + 1
      return { cost, paid := true, patient := { p with wallet, statLife := p.statLifeMax2, buffs }, healEffect := some amount, dialog, rng }
    let (r, rng) := rng.next 0 3
    return { cost, paid := false, patient := { p with wallet }, dialog := if r == 0 then 52 else if r == 1 then 53 else 54, rng }
  let (r, rng) := rng.next 0 3
  let (r, rng) := if w.childSafetyDisabled then (r, rng) else rng.next 1 3
  return { cost, paid := false, patient := p, dialog := if r == 0 then 55 else if r == 1 then 56 else 57, rng }
where
  -- `Player.DelBuff(k)`: clear the slot, then move every non-empty slot below `maxBuffs - 1` to the front, in order
  -- (`buffs.size` is `Player.maxBuffs`).
  delBuff (b : Array (Int × Int)) (k : Nat) : Array (Int × Int) := Id.run do
    let mut b := b.set! k (0, 0)
    let mut j := 0
    for v in [0:b.size - 1] do
      let (t, time) := b[v]!
      if time != 0 && t != 0 then
        if j < v then
          b := (b.set! j (t, time)).set! v (0, 0)
        j := j + 1
    return b

end Lean4Terria.OriginalTownNpcAI
