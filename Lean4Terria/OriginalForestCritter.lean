import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalGreenSlime

/-!
# Forest critter (Terraria 1.4.5.8, Classic): the bunny, `type = 46`, `aiStyle = 7`

A hand transcription of what `NPC.UpdateNPC(i)` does for one bunny in a single-player world
(`Main.netMode = 0`), checked frame by frame against the vanilla dedicated server on the Guide-path
bunny of the continuous seed-42 run (`.lake/original-forest-critter/1.4.5.8`, frames 148 … 533).

IL read for this file: `NPC.UpdateNPC`, `UpdateNPC_UpdateGravity`, `AI_007_TownEntities` (the
`TownCritter` branches: setup, idle `ai[0] = 0`, walk `ai[0] = 1`, the stuck turn),
`AI_007_TownEntities_GetWalkPrediction`, `TargetClosest` (`TryTrackingTarget`,
`SetTargetTrackingValues`), `UpdateHomeTileState`, `SubAI_HandleTemporaryCatchableNPCPlayerInvulnerability`,
`GetHurtByOtherNPCs`, `UpdateCollision` with `Collision_WalkDownSlopes`, `Collision_WaterCollision`,
`Collision_DecideFallThroughPlatforms`, `ApplyTileCollision`, `Collision_MoveWhileDry`,
`Collision_MoveWhileWet`, `Collision_MoveSlopesAndStairFall`, the bunny case of `FindFrame`, `CheckActive`,
and `Collision.StepUp`, `WetCollision`, `DrownCollision`, `SolidTiles`, `SolidTilesVersatile`.
`TileCollision`, `SlopeCollision` and `WalkDownSlope` are the shared `OriginalCollision` port.

## Stage contract

`update` is one `NPC.UpdateNPC(i)` call. In the vanilla frame it runs inside the NPC stage
(`Main.UpdateWorld_NPCs`): after `UpdatePlayers` and the spawn stage, with `player.nearbyActiveNPCs`
reset to 0 at the start of the stage, slots 0 … 199 in order, all sharing one `Main.rand`
(the `"UpdateNPCs"` stream). A bunny created by `NPC.NewNPC` in the spawn stage (`spawn`) gets its first
`update` in the same frame. The caller passes the stream as it is at this slot's turn (after every lower
slot's update) and continues with the returned stream. `Step.nearby` lists the
`nearbyActiveNPCs` increments, `Step.noSpawnCycle` the `NPC.noSpawnCycle` write of a despawn, and
`Step.gravity` the static `NPC.gravity` left behind.

Tiles are read by coordinate through `OriginalCollision.Grid.tileAt`; nothing is snapshotted.

## Desktop-only stream consumers

`Dust.NewDust` and the splash `SoundEngine.PlaySound(19, …)` at water entry run on the shared stream on
the desktop client (dust only when the dust rectangle meets the screen and the pool has room). The
dedicated server, and the client with the splash off screen, draw nothing there. `World.effects` is the
hook: the default draws nothing (the captured host); the requests are also returned in `Step.dust` /
`Step.sounds`.

## Not transcribed (the step reports `unsupported` instead of guessing)

Other critter types and town NPCs, eclipse / rain / storm / slime-rain behaviour, expert and
master mode, buffs, `life < lifeMax`, hostile NPC contact, the `ai[0] = 8` state a cornered fleeing critter
switches to (the danger scan itself and fleeing are transcribed), players talking to the critter, `ai[0]`
other than 0 and 1,
`ai[3] = 1`, the idle drown check (`AI_007_TownEntities_CheckIfWillDrown`), lava, honey and shimmer, doors, pressure plates and other wired tiles,
conveyor belts, stair falls from `homeTileY`, `releaseOwner`, statue spawns, multiplayer smoothing and
the bottom of the world.
-/

namespace Lean4Terria.OriginalForestCritter

open OriginalCollision (Grid Vector single tileIndex slopeId)
open OriginalWorld (Tile TileShape LiquidKind)

/-- The shared `Main.rand` (`UnifiedRandom`) of the NPC stage: the same type as the green slime's, so one
value can be threaded through every slot of the stage. -/
abbrev Rng := OriginalGreenSlime.Rng

/-! ## Inputs -/

/-- `Microsoft.Xna.Framework.Rectangle`. -/
structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving BEq, Repr

def Rect.intersects (a b : Rect) : Bool :=
  b.x < a.x + a.w && a.x < b.x + b.w && b.y < a.y + a.h && a.y < b.y + b.h

/-- One `Dust.NewDust(position, width, height, type, 0, 0, 0, default, 1)` call. -/
structure DustRequest where
  position : Vector
  width : Int
  height : Int
  dustType : Nat
  deriving Repr

/-- One `SoundEngine.PlaySound(type, x, y, style, 1, 0)` call. -/
structure SoundRequest where
  soundType : Nat
  x : Int
  y : Int
  deriving Repr

/-- What the visual / audio systems do with the shared stream. The defaults draw nothing: the dedicated
server (`Main.gameMenu`), and a client whose dust and sound calls are off screen or muted. -/
structure Effects where
  newDust : Rng → DustRequest → Rng := fun rng _ => rng
  playSound : Rng → SoundRequest → Rng := fun rng _ => rng

instance : Inhabited Effects := ⟨{}⟩

/-- World statics read by the update. `frameHeight` is `FindFrame`'s frame height: `1` on the dedicated
server, `Texture(NPC_46).Height / npcFrameCount[46] = 280 / 7 = 40` on the desktop client. -/
structure World where
  grid : Grid
  worldSurface : Float
  /-- `Main.bottomWorld` (`maxTilesY * 16`). -/
  bottomWorld : Float32
  dayTime : Bool := true
  raining : Bool := false
  eclipse : Bool := false
  slimeRain : Bool := false
  storming : Bool := false
  expertMode : Bool := false
  masterMode : Bool := false
  onlyShimmerOceanWorlds : Bool := false
  activeRangeX : Int := 4032
  activeRangeY : Int := 2520
  activeTime : Int := 750
  /-- `NPC.sWidth` / `NPC.sHeight`: 1920 × 1200 on the dedicated server, the zoomed screen on desktop. -/
  sWidth : Int := 1920
  sHeight : Int := 1200
  frameHeight : Int := 1
  frameCount : Int := 7
  /-- `Dust.dustWater()` (33 for the forest water style). -/
  waterDust : Nat := 33
  /-- `Main.tileDungeon`. -/
  tileDungeon : UInt16 → Bool := fun t => t == 41 || t == 43 || t == 44
  /-- `TileID.Sets.IgnoredByNpcStepUp`. -/
  ignoredByNpcStepUp : UInt16 → Bool := fun _ => false
  /-- `NPCID.Sets.CritterThatCanTurnOnPlayers`. -/
  critterThatCanTurnOnPlayers : Nat → Bool := fun _ => false
  /-- `NPC.npcsFoundForCheckActive[210] || [211]` (bees). -/
  beesActive : Bool := false
  /-- `NPCID.Sets.PrettySafe[46]` (`-1`: no such threshold). -/
  prettySafe : Int := -1
  effects : Effects := {}

structure Player where
  index : Nat
  active : Bool := true
  dead : Bool := false
  ghost : Bool := false
  position : Vector
  width : Nat := 20
  height : Nat := 42
  aggro : Int := 0
  itemAnimation : Int := 0
  /-- `npcTypeNoAggro[46]`. -/
  noAggro : Bool := false
  /-- `Player.talkNPC`. -/
  talkNPC : Int := -1
  stinky : Bool := false
  tankPet : Int := -1
  deriving Repr

instance : Inhabited Player := ⟨{ index := 0, position := ⟨0, 0⟩ }⟩

def Player.center (p : Player) : Vector :=
  ⟨p.position.x + single (p.width / 2 : Nat), p.position.y + single (p.height / 2 : Nat)⟩

/-- Another active NPC slot, as far as the bunny's update reads it. -/
structure Other where
  index : Nat
  type : Nat
  friendly : Bool
  damage : Int := 0
  stinky : Bool := false
  position : Vector
  width : Nat := 0
  height : Nat := 0
  noTileCollide : Bool := false
  deriving Repr

def Other.center (o : Other) : Vector :=
  ⟨o.position.x + single o.width / 2, o.position.y + single o.height / 2⟩

/-- The bunny's fields that `UpdateNPC` reads or writes on the transcribed paths. Defaults are
`NPC.NewNPC(…, 46)` (`SetDefaults(46)` plus `NewNPC`'s own writes). -/
structure Critter where
  whoAmI : Nat
  type : Nat := 46
  aiStyle : Nat := 7
  active : Bool := true
  position : Vector
  velocity : Vector := ⟨0, 0⟩
  oldPosition : Vector := ⟨0, 0⟩
  oldVelocity : Vector := ⟨0, 0⟩
  width : Nat := 18
  height : Nat := 20
  direction : Int := 0
  directionY : Int := 1
  oldDirection : Int := 0
  oldDirectionY : Int := 0
  spriteDirection : Int := -1
  target : Int := 255
  oldTarget : Int := 255
  /-- `targetRect` as `(x, y, width, height)`. -/
  targetRect : Int × Int × Int × Int := (0, 0, 0, 0)
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
  /-- `Entity.wetCount` (a byte). -/
  wetCount : Nat := 0
  lavaWet : Bool := false
  honeyWet : Bool := false
  shimmerWet : Bool := false
  life : Int := 5
  lifeMax : Int := 5
  defense : Int := 0
  defDefense : Int := 0
  dryadWard : Bool := false
  friendly : Bool := true
  dontTakeDamage : Bool := false
  catchableNPCTempImmunityCounter : Int := 90
  netUpdate : Bool := true
  justHit : Bool := false
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  stairFall : Bool := false
  soundDelay : Int := 0
  /-- `immune[0..255]`. -/
  immune : Array Int := Array.replicate 256 0
  frameY : Int := 0
  frameCounter : Float := 0
  timeLeft : Int := 0
  homeTileX : Int := -1
  homeTileY : Int := -1
  oldHomeTileX : Int := -1
  oldHomeTileY : Int := -1
  homeless : Bool := false
  oldHomeless : Bool := false
  npcSlots : Float32 := 1
  knockBackResist : Float32 := 1
  /-- `catchItem` (the bait / critter item a bug net yields). -/
  catchItem : Int := 2019
  stinky : Bool := false
  closeDoor : Bool := false
  waterMovementSpeed : Float32 := 0.5
  releaseOwner : Int := 255
  spawnedFromStatue : Bool := false
  extraValue : Int := 0
  despawnEncouraged : Bool := false
  /-- Any `buffType[i] != 0`. -/
  hasBuffs : Bool := false
  deriving Repr

instance : Inhabited Critter := ⟨{ whoAmI := 0, position := ⟨0, 0⟩ }⟩

def Critter.center (n : Critter) : Vector :=
  ⟨n.position.x + single (n.width / 2 : Nat), n.position.y + single (n.height / 2 : Nat)⟩

/-- `SetDefaults` constants of the forest critters met so far that differ between them:
`(aiStyle, width, height, npcSlots, knockBackResist, catchItem)`. Everything else `NPC.NewNPC` leaves
(`life = lifeMax = 5`, `friendly`, `catchableNPCTempImmunityCounter = 90`, `target = 255`, `direction = 0`,
`spriteDirection = -1`, `homeTile = (-1, -1)`, `waterMovementSpeed = 0.5`, `netUpdate`) is shared and checked
against all five birds and the bunny of the seed-42 Guide run. -/
def critterDefaults : Nat → Option (Nat × Nat × Nat × Float32 × Float32 × Int)
  | 46 => some (7, 18, 20, 1, 1, 2019)        -- Bunny
  | 74 => some (24, 14, 14, 0.4, 0.8, 2015)   -- Bird
  | 297 => some (24, 14, 14, 0.4, 0.8, 2016)  -- Blue Jay
  | 298 => some (24, 14, 14, 0.4, 0.8, 2017)  -- Cardinal (same template; not met in the run)
  | _ => none

/-- `Content/Images/NPC_<type>.xnb` texture size `(width, height)` and `Main.npcFrameCount[type]` of 1.4.5.8. -/
def critterTexture : Nat → Option (Nat × Nat × Nat)
  | 46 => some (48, 280, 7)
  | 74 => some (30, 120, 5)
  | 297 => some (30, 120, 5)
  | 298 => some (30, 120, 5)
  | _ => none

/-- `FindFrame`'s frame height on the desktop client: texture height / `npcFrameCount` (the dedicated server
uses 1). Bunny 280 / 7 = 40, birds 120 / 5 = 24. -/
def desktopFrameHeight (type : Nat) : Option Int :=
  (critterTexture type).map fun (_, h, n) => (h / n : Nat)

/-- `NPC.NewNPC(source, x, y, type, 0, ai0, ai1, ai2, ai3, target)` into slot `whoAmI` for a forest critter of
`critterDefaults`: `Bottom = (x, y)`, `timeLeft = (int)(activeTime * 1.25)`. `NewNPC` draws nothing from
`Main.rand` for these types (natural spawns pass `ai = 0` and `target = 255`). -/
def spawnCritter (world : World) (type whoAmI : Nat) (x y : Int) (ai0 ai1 ai2 ai3 : Float32 := 0)
    (target : Int := 255) : Option Critter := do
  let (aiStyle, width, height, npcSlots, knockBackResist, catchItem) ← critterDefaults type
  let base : Critter := { whoAmI, type, aiStyle, width, height, npcSlots, knockBackResist, catchItem, position := ⟨0, 0⟩ }
  return { base with
    position := ⟨single (x - (width / 2 : Nat)), single (y - height)⟩
    timeLeft := (Float.ofInt world.activeTime * 1.25).toInt64.toInt
    ai0, ai1, ai2, ai3, target }

/-- `NPC.NewNPC(…, x, y, 46, …)`: the bunny. -/
def spawn (world : World) (whoAmI : Nat) (x y : Int) (ai0 ai1 ai2 ai3 : Float32 := 0) (target : Int := 255) : Critter :=
  (spawnCritter world 46 whoAmI x y ai0 ai1 ai2 ai3 target).getD default

/-! ## Tile queries -/

/-- C# `int` division (truncates toward zero). -/
private def cdiv (a b : Int) : Int := a.tdiv b

/-- `(int)f` for a `float`. -/
private def trunc (f : Float32) : Int := f.toInt32.toInt

private def tileAt (g : Grid) (x y : Int) : Tile :=
  if x < 0 || y < 0 || x ≥ g.width || y ≥ g.height then default else g.tileAt x.toNat y.toNat

private def ttype (t : Tile) : UInt16 := t.id.getD 0
private def isActive (t : Tile) : Bool := t.id.isSome
private def nactive (t : Tile) : Bool := t.id.isSome && !t.actuated
private def solid (g : Grid) (t : Tile) : Bool := (g.rules (ttype t)).solid
private def solidTop (g : Grid) (t : Tile) : Bool := (g.rules (ttype t)).solidTop
private def platform (g : Grid) (t : Tile) : Bool := (g.rules (ttype t)).platform
private def slope (t : Tile) : UInt8 := slopeId t.shape
private def halfBrick (t : Tile) : Bool := t.shape == .halfBlock
private def topSlope (t : Tile) : Bool := slope t == 1 || slope t == 2
private def blockType (t : Tile) : Nat := if halfBrick t then 1 else if slope t > 0 then (slope t).toNat + 1 else 0
private def frameY (t : Tile) : Int := (t.frame.map (·.2)).getD (-1)
private def liquidAmount (t : Tile) : Nat := (t.liquid.map (·.amount.toNat)).getD 0
private def liquidIs (t : Tile) (k : LiquidKind) : Bool := (t.liquid.map (·.kind == k)).getD false

/-- `WorldGen.InWorld(x, y, fluff)`. -/
private def inWorld (g : Grid) (x y : Int) (fluff : Int := 0) : Bool :=
  x ≥ fluff && x < (g.width : Int) - fluff && y ≥ fluff && y < (g.height : Int) - fluff

private def clampI (v lo hi : Int) : Int := if v < lo then lo else if v > hi then hi else v

/-- The tile box `(x0, x1, y0, y1)` (end exclusive) that `WetCollision`, `DrownCollision` and
`LavaCollision` scan: `Utils.Clamp` of one tile around the body, two below / right. -/
private def liquidBox (g : Grid) (pos : Vector) (w h : Nat) : Int × Int × Int × Int :=
  (clampI (trunc (pos.x / 16) - 1) 0 ((g.width : Int) - 1),
   clampI (trunc ((pos.x + single w) / 16) + 2) 0 ((g.width : Int) - 1),
   clampI (trunc (pos.y / 16) - 1) 0 ((g.height : Int) - 40),
   clampI (trunc ((pos.y + single h) / 16) + 2) 0 ((g.height : Int) - 40))

/-- Any tile in `[x0, x1] × [y0, y1]` (inclusive) satisfying `p`. -/
private def anyTile (g : Grid) (x0 x1 y0 y1 : Int) (p : Tile → Bool) : Bool := Id.run do
  let mut x := x0
  while x ≤ x1 do
    let mut y := y0
    while y ≤ y1 do
      if p (tileAt g x y) then return true
      y := y + 1
    x := x + 1
  return false

/-! ## `Collision` methods not in `OriginalCollision` -/

/-- `Collision.SolidTiles(int startX, int endX, int startY, int endY)`: bounds inclusive; outside the
world counts as solid. -/
def solidTiles (g : Grid) (x0 x1 y0 y1 : Int) : Bool :=
  if x0 < 0 || x1 ≥ g.width || y0 < 0 || y1 ≥ (g.height : Int) - 40 then true
  else anyTile g x0 x1 y0 y1 fun t => isActive t && !t.actuated && solid g t && !solidTop g t

/-- `Collision.SolidTilesVersatile`: `SolidTiles` with each range put in order. -/
def solidTilesVersatile (g : Grid) (a b c d : Int) : Bool :=
  let (a, b) := if a > b then (b, a) else (a, b)
  let (c, d) := if c > d then (d, c) else (c, d)
  solidTiles g a b c d

/-- `Collision.WetCollision(position, width, height)`: `(wet, Collision.honey, Collision.shimmer)`. -/
def wetCollision (g : Grid) (pos : Vector) (w h : Nat) : Bool × Bool × Bool := Id.run do
  let cx := pos.x + single (w / 2 : Nat)
  let cy := pos.y + single (h / 2 : Nat)
  let nw : Int := if 10 > (w : Int) then w else 10
  let nh : Int := if ((h / 2 : Nat) : Int) > h then h else (h / 2 : Nat)
  let vx := cx - single (cdiv nw 2)
  let vy := cy - single (cdiv nh 2)
  let (x0, x1, y0, y1) := liquidBox g pos w h
  let mut x := x0
  while x < x1 do
    let mut y := y0
    while y < y1 do
      let t := tileAt g x y
      let amount := liquidAmount t
      if amount > 0 then
        let tx := single (x * 16)
        let mut ty := single (y * 16)
        let fill := single (256 - amount) / 32
        ty := ty + fill * 2
        let depth : Int := 16 - trunc (fill * 2)
        if vx + single nw > tx && vx < tx + 16 && vy + single nh > ty && vy < ty + single depth then
          return (true, liquidIs t .honey, liquidIs t .shimmer)
      else if isActive t && slope t != 0 && y > 0 && liquidAmount (tileAt g x (y - 1)) > 0 then
        let tx := single (x * 16)
        let ty := single (y * 16)
        if vx + single nw > tx && vx < tx + 16 && vy + single nh > ty && vy < ty + 16 then
          let above := tileAt g x (y - 1)
          let honey := liquidIs above .honey
          return (true, honey, !honey && liquidIs above .shimmer)
      y := y + 1
    x := x + 1
  return (false, false, false)

/-- `Collision.DrownCollision(position, width, height, gravDir = 1, includeSlopes = true)`. -/
def drownCollision (g : Grid) (pos : Vector) (w h : Nat) : Bool := Id.run do
  let cx := pos.x + single (w / 2 : Nat)
  let nw : Int := if 10 > (w : Int) then w else 10
  let nh : Int := if 12 > (h : Int) then h else 12
  let vx := cx - single (cdiv nw 2)
  let vy := pos.y + (-2 : Float32)
  let (x0, x1, y0, y1) := liquidBox g pos w h
  let topRow := y0
  let mut x := x0
  while x < x1 do
    let mut y := y0
    while y < y1 do
      let t := tileAt g x y
      let amount := liquidAmount t
      if amount > 0 && !liquidIs t .lava && !liquidIs t .shimmer then
        let skip := y == topRow && isActive t && solid g t && !solidTop g t && blockType t == 0
        if !skip then
          let tx := single (x * 16)
          let mut ty := single (y * 16)
          let fill := single (256 - amount) / 32
          ty := ty + fill * 2
          let depth : Int := 16 - trunc (fill * 2)
          if vx + single nw > tx && vx < tx + 16 && vy + single nh > ty && vy < ty + single depth then
            return true
      y := y + 1
    x := x + 1
  return false

/-- Result of `Collision.StepUp`. -/
structure StepUpResult where
  position : Vector
  velocity : Vector
  stepSpeed : Float32
  gfxOffY : Float32
  deriving Repr

/-- `Collision.StepUp(ref position, ref velocity, width, height, ref stepSpeed, ref gfxOffY, gravDir = 1,
holdsMatchingUp, specialChecksMode)`, the `gravDir = 1` half. -/
def stepUp (world : World) (pos vel : Vector) (w h : Nat) (stepSpeed gfxOffY : Float32)
    (holdsMatchingUp : Bool) (specialChecksMode : Nat) : StepUpResult := Id.run do
  let g := world.grid
  let same : StepUpResult := ⟨pos, vel, stepSpeed, gfxOffY⟩
  let mut dir : Int := 0
  if vel.x < 0 then dir := -1
  if vel.x > 0 then dir := 1
  let p1x := pos.x + vel.x
  let x := trunc ((p1x + single (w / 2 : Nat) + single (((w / 2 : Nat) + 1 : Int) * dir)) / 16)
  let y := trunc ((pos.y + single h - 1) / 16)
  let ht : Int := (h / 16 : Nat) + (if h % 16 != 0 then 1 else 0)
  if !inWorld g x y 1 then return same
  if y ≥ (g.height : Int) - 40 then return same
  let mut flag7 := true
  let mut flag8 := true
  let mut i : Int := 1
  while i < ht + 2 do
    if !inWorld g x (y - i) then return same
    i := i + 1
  if !inWorld g (x - dir) (y - ht) then return same
  i := 2
  while i < ht + 1 do
    let t := tileAt g x (y - i)
    flag7 := flag7 && (!nactive t || !solid g t || solidTop g t)
    i := i + 1
  let back := tileAt g (x - dir) (y - ht)
  flag8 := flag8 && (!nactive back || !solid g back || solidTop g back)
  let halfW := single (w / 2 : Nat)
  let t5 := tileAt g x (y - 1)
  let t6 := tileAt g x (y - (ht + 1))
  let flag9 :=
    if !nactive t5 || !solid g t5 || solidTop g t5 then true
    else if slope t5 == 1 && pos.x + halfW > single (x * 16) then true
    else if slope t5 == 2 && pos.x + halfW < single (x * 16 + 16) then true
    else if halfBrick t5 then !nactive t6 || !solid g t6 || solidTop g t6
    else false
  let ahead := tileAt g x y
  let above := tileAt g x (y - 1)
  let flag11 := if specialChecksMode == 1 then !world.ignoredByNpcStepUp (ttype ahead) else true
  let fromAhead : Option Bool :=
    if !nactive ahead then none
    else
      let slopeOk :=
        if topSlope ahead then
          (slope ahead == 1 && pos.x + halfW < single (x * 16)) ||
          (slope ahead == 2 && pos.x + halfW > single (x * 16 + 16))
        else true
      if !slopeOk then none
      else if topSlope ahead && !(pos.y + single h > single (y * 16)) then none
      else if solid g ahead && !solidTop g ahead then some true
      else
        let support := holdsMatchingUp &&
          ((solidTop g ahead && frameY ahead == 0) || platform g ahead || ttype ahead == 380) &&
          (!solid g above || !nactive above)
        if support && flag11 then some true else none
  let mut flag10 := match fromAhead with
    | some b => b
    | none => halfBrick above && nactive above
  flag10 := flag10 && (!solidTop g ahead || !solidTop g above)
  if !(single (x * 16) < p1x + single w) then return same
  if !(single (x * 16 + 16) > p1x) then return same
  if !(flag10 && flag9 && flag7 && flag8) then return same
  let mut top := single (y * 16)
  if halfBrick above then top := top - 8
  else if halfBrick ahead then top := top + 8
  if !(top < pos.y + single h) then return same
  let rise := pos.y + single h - top
  if rise.toFloat > 16.1 then return same
  return { position := { pos with y := top - single h }, velocity := vel
           gfxOffY := gfxOffY + (pos.y + single h - top)
           stepSpeed := if rise < 9 then 1 else 2 }

/-! ## One frame -/

/-- Everything a frame returns besides the critter. -/
structure Step where
  critter : Critter
  rng : Rng
  /-- The static `NPC.gravity` after the frame (`none`: an inactive slot leaves it unchanged). -/
  gravity : Option Float32
  /-- `Player.nearbyActiveNPCs` increments from `CheckActive`, by player index. -/
  nearby : Array (Nat × Float32) := #[]
  /-- `CheckActive` despawned the critter and set `NPC.noSpawnCycle = true`. -/
  noSpawnCycle : Bool := false
  dust : Array DustRequest := #[]
  sounds : Array SoundRequest := #[]
  /-- A path this port does not transcribe was reached; the other fields are then not meaningful. -/
  unsupported : Option String := none
  deriving Repr

/-- `UpdateNPC_UpdateGravity` for the bunny: `(NPC.gravity, maxFallSpeed)`. -/
def gravityFor (world : World) (n : Critter) : Float32 × Float32 :=
  let width := single world.grid.width / 4200
  let width := width * width
  let depth := n.position.y / 16 - (60 + 10 * width)
  let factor := (depth.toFloat / (world.worldSurface / 6.0)).toFloat32
  let factor := if factor.toFloat < 0.25 then 0.25 else factor
  let factor := if factor > 1 then 1 else factor
  if n.wet then (0.2, 7) else (0.3 * factor, 10)

/-- `NPC.UpdateHomeTileState(homeless, x, y)` (the `SendData` is a no-op in single player). -/
def updateHomeTileState (n : Critter) (homeless : Bool) (x y : Int) : Critter :=
  { n with homeless, oldHomeless := homeless, homeTileX := x, oldHomeTileX := x, homeTileY := y, oldHomeTileY := y }

/-- `NPC.TargetClosest(faceTarget = true)` with `TryTrackingTarget` and `SetTargetTrackingValues`. -/
def targetClosest (players : Array Player) (n : Critter) : Except String Critter := do
  let mut n := n
  let mut found := false
  let mut best : Float32 := 0
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.dead || p.ghost || p.index ≥ 255 then continue
    let c := p.center
    let own := n.center
    let distance := (c.x - own.x).abs + (c.y - own.y).abs
    let mut score := distance - single p.aggro
    if p.noAggro && n.direction != 0 then score := score + 1000
    if !found || score < best then
      found := true
      best := score
      n := { n with target := p.index }
      if p.tankPet ≥ 0 then throw "TryTrackingTarget: player tank pet"
  if n.target < 0 || n.target ≥ 255 then n := { n with target := 0 }
  let some p := players.find? (fun p => (p.index : Int) == n.target)
    | throw "TargetClosest: target player not described"
  let rect := (trunc p.position.x, trunc p.position.y, (p.width : Int), (p.height : Int))
  n := { n with targetRect := rect }
  let mut face := true
  if p.dead then face := false
  if p.noAggro && n.direction != 0 then face := false
  if face then
    let oldValid := 0 ≤ n.oldTarget && n.oldTarget ≤ 254
    let sneaking := p.itemAnimation == 0 && p.aggro < 0
    if !(oldValid && sneaking) then
      let (rx, ry, rw, rh) := rect
      n := { n with direction := if single (rx + cdiv rw 2) < n.position.x + single (n.width / 2 : Nat) then -1 else 1 }
      n := { n with directionY := if single (ry + cdiv rh 2) < n.position.y + single (n.height / 2 : Nat) then -1 else 1 }
  if n.direction != n.oldDirection && !n.collideX && !n.collideY then n := { n with netUpdate := true }
  return n

/-- `Vector2.Distance(a, b)`: the squared length in `float`, `Math.Sqrt` in `double`, back to `float`. -/
def vecDistance (a b : Vector) : Float32 :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  (dx * dx + dy * dy).toFloat.sqrt.toFloat32

/-- `Collision.CanHit(Vector2 a, 0, 0, Vector2 b, 0, 0)`: both points through `Utils.ToPoint`, then the `Point`
overload's tile walk. It steps along the longer axis and is blocked by a full solid block on both sides of the
step, or by a solid (not solid-top) block on the stepped tile. An index past the world edge is the overload's
`catch` (false). -/
def canHit (g : Grid) (a b : Vector) : Bool := Id.run do
  let w : Int := g.width
  let h : Int := g.height
  let clampX (x : Int) : Int := if x ≤ 1 then 1 else if x ≥ w then w - 1 else x
  let clampY (y : Int) : Int := if y ≤ 1 then 1 else if y ≥ h - 40 then h - 40 else y
  let mut x1 := clampX (cdiv (trunc a.x) 16)
  let mut y1 := clampY (cdiv (trunc a.y) 16)
  let x2 := clampX (cdiv (trunc b.x) 16)
  let y2 := clampY (cdiv (trunc b.y) 16)
  let fullBlock (t : Tile) : Bool :=
    !t.actuated && isActive t && solid g t && !solidTop g t && slope t == 0 && !halfBrick t
  let blocks (t : Tile) : Bool := !t.actuated && isActive t && solid g t && !solidTop g t
  let steps := (x1 - x2).natAbs + (y1 - y2).natAbs + 1
  for _ in [0:steps] do
    if x1 == x2 && y1 == y2 then return true
    if (x1 - x2).natAbs > (y1 - y2).natAbs then
      x1 := if x1 < x2 then x1 + 1 else x1 - 1
      if y1 + 1 ≥ h then return false
      if fullBlock (tileAt g x1 (y1 - 1)) && fullBlock (tileAt g x1 (y1 + 1)) then return false
    else
      y1 := if y1 < y2 then y1 + 1 else y1 - 1
      if x1 + 1 ≥ w then return false
      if fullBlock (tileAt g (x1 - 1) y1) && fullBlock (tileAt g (x1 + 1) y1) then return false
    if blocks (tileAt g x1 y1) then return false
  return true

/-- `AI_007_TownEntities_GetWalkPrediction` for a `TownCritter`: `keepWalking = drowning`,
`avoidFalling = false`. -/
private def walkPrediction (drowning : Bool) : Bool × Bool := (drowning, false)

/-- `NPC.AI_007_TownEntities` for the bunny (`TownCritter`, not a town NPC, pet or town slime). -/
def ai007 (world : World) (players : Array Player) (others : Array Other) (gravity : Float32)
    (rng : Rng) (n : Critter) : Except String (Critter × Rng) := do
  let g := world.grid
  if world.onlyShimmerOceanWorlds then throw "AI_007: onlyShimmerOceanWorlds"
  if world.eclipse then throw "AI_007: eclipse"
  if world.slimeRain then throw "AI_007: slime rain"
  if world.storming then throw "AI_007: storm"
  if world.masterMode || world.expertMode then throw "AI_007: expert / master mode"
  if n.dryadWard then throw "AI_007: dryadWard"
  let mut n := { n with defense := n.defDefense, dontTakeDamage := false }
  let mut rng := rng
  if n.ai0 == 25 then throw "AI_007: ai[0] = 25"
  -- TownCritter: pick the closest player once, and remember where it started
  if n.target == 255 then
    n ← targetClosest players n
    let some p := players.find? (fun p => (p.index : Int) == n.target)
      | throw "AI_007: target player not described"
    if n.position.x < p.position.x then n := { n with direction := 1, spriteDirection := 1 }
    if n.position.x > p.position.x then n := { n with direction := -1, spriteDirection := -1 }
    if n.homeTileX == -1 then
      n := updateHomeTileState n n.homeless (trunc ((n.position.x + single (n.width / 2 : Nat)) / 16)) n.homeTileY
  else if n.homeTileX == -1 && n.homeTileY == -1 && n.velocity.y == 0 then
    n := updateHomeTileState n n.homeless (cdiv (trunc n.center.x) 16)
      (cdiv (trunc (n.position.y + single n.height + 4)) 16)
  let tileX := cdiv (trunc (n.position.x + single (n.width / 2 : Nat))) 16
  let tileY := cdiv (trunc (n.position.y + single n.height + 1)) 16
  -- AI_007_FindGoodRestingSpot
  let floorX := n.homeTileX
  let floorY := n.homeTileY
  if floorX != -1 && floorY != -1 then throw "AI_007_FindGoodRestingSpot: a home tile"
  n := { n with directionY := -1 }
  if n.direction == 0 then n := { n with direction := 1 }
  if n.ai0 != 24 then
    for p in players do
      if p.active && p.index < 255 && p.talkNPC == (n.whoAmI : Int) then throw "AI_007: a player talks to the critter"
  if n.ai3 == 1 then throw "AI_007: ai[3] = 1"
  if ttype (tileAt g tileX tileY) == 666 && isActive (tileAt g tileX tileY) then throw "AI_007: tile 666"
  if n.ai0 != 0 && n.ai0 != 1 then throw s!"AI_007: ai[0] = {n.ai0}"
  -- danger scan (IL_13f8 … IL_1a8f; dangerDetectRange = 200 for the bunny, AttackType gate off: not a town NPC).
  -- An NPC counts if it is hostile with damage or stinky, is another slot, is closer than 200 px centre to centre,
  -- and can be seen (Collision.CanHit centre to centre, skipped for noTileCollide NPCs); then the nearest such
  -- x offset on each side is kept. The bunny itself never counts (whoAmI gate).
  let me := n.center
  let mut danger := false
  let mut left : Float32 := -1
  let mut right : Float32 := -1
  for o in others.qsort (·.index < ·.index) do
    if o.index ≥ 200 then continue
    if world.critterThatCanTurnOnPlayers o.type || o.type == 690 then continue
    if !((!o.friendly && o.damage > 0) || o.stinky) then continue
    if o.index == n.whoAmI then continue
    let oc := o.center
    let dist := vecDistance oc me
    if !(dist < 200) then continue
    if !o.noTileCollide && !canHit g me oc then continue
    if dist ≥ 200 then continue
    danger := true
    let dx := oc.x - me.x
    if dx < 0 && (left == -1 || dx > left) then left := dx
    if dx > 0 && (right == -1 || dx < right) then right := dx
  if !danger then
    for p in players.qsort (·.index < ·.index) do
      if !p.active || p.index ≥ 255 || p.dead || !p.stinky then continue
      let pc := p.center
      if !(vecDistance pc me < 200) then continue
      danger := true
      let dx := pc.x - me.x
      if dx < 0 && (left == -1 || dx > left) then left := dx
      if dx > 0 && (right == -1 || dx < right) then right := dx
  if danger then
    let away : Int := if left == -1 then 1 else if right == -1 then -1 else if right < -left then 1 else -1
    let mut nearest : Float32 := 0
    if left != -1 then nearest := -left
    if nearest == 0 || (right < nearest && right > 0) then nearest := right
    if world.prettySafe != -1 && !(single world.prettySafe ≥ nearest) then
      danger := false
    else if n.ai0 == 1 then
      if n.direction != -away then n := { n with direction := -away, netUpdate := true }
    else
      -- idle: GetWalkPrediction never says "avoid falling" for a TownCritter, so it starts to flee at once
      let (a, r) := rng.next 120
      rng := r
      n := { n with ai0 := 1, ai1 := single (120 + a), ai2 := 0, localAI3 := 0, direction := -away, netUpdate := true }
  if n.ai0 == 0 then
    -- idle
    if n.stinky then throw "AI_007: stinky critter"
    if n.localAI3 > 0 then n := { n with localAI3 := n.localAI3 - 1 }
    if world.raining then throw "AI_007: rain"
    if n.velocity.x > 0.1 then n := { n with velocity.x := n.velocity.x - 0.1 }
    else if n.velocity.x < -0.1 then n := { n with velocity.x := n.velocity.x + 0.1 }
    else n := { n with velocity.x := 0 }
    if n.ai1 > 0 then n := { n with ai1 := n.ai1 - 1 }
    let mut canWalk := true
    let wetArg := n.wet
    let (_, avoidFalling) := walkPrediction wetArg
    if n.wet then throw "AI_007: idle in water (AI_007_TownEntities_CheckIfWillDrown)"
    if avoidFalling then canWalk := false
    if !(n.ai1 > 0) then
      if canWalk && !avoidFalling then
        let (a, r) := rng.next 300
        rng := r
        n := { n with ai0 := 1, ai1 := single (200 + a), ai2 := 0 }
        let (b, r) := rng.nextRange 200 400
        rng := r
        n := { n with ai1 := n.ai1 + single b, localAI3 := 0, netUpdate := true }
      else
        let (a, r) := rng.next 120
        rng := r
        n := { n with direction := -n.direction, ai1 := single (60 + a), netUpdate := true }
    -- In shelter conditions the idle turn roll requires the precise resting tile.
    if !world.dayTime && (tileX != floorX || tileY != floorY) then return (n, rng)
    if tileX < floorX - 25 || tileX > floorX + 25 then
      if n.localAI3 == 0 then
        if tileX < floorX - 50 && n.direction == -1 then n := { n with direction := 1, netUpdate := true }
        else if tileX > floorX + 50 && n.direction == 1 then n := { n with direction := -1, netUpdate := true }
    else
      let (roll, r) := rng.next 80
      rng := r
      if roll == 0 && n.localAI3 == 0 then
        n := { n with localAI3 := 200, direction := -n.direction, netUpdate := true }
    return (n, rng)
  if n.ai0 != 1 then throw s!"AI_007: ai[0] = {n.ai0}"
  -- walk
  if world.raining then throw "AI_007: rain"
  let drowning := drownCollision g n.position n.width n.height
  if !drowning then
    if !n.homeless && !world.tileDungeon (ttype (tileAt g tileX tileY)) then
      if tileX < floorX - 35 || tileX > floorX + 35 then
        if n.position.x < single (floorX * 16) && n.direction == -1 then n := { n with ai1 := n.ai1 - 5 }
        else if n.position.x > single (floorX * 16) && n.direction == 1 then n := { n with ai1 := n.ai1 - 5 }
    n := { n with ai1 := n.ai1 - 1 }
  if !(n.ai1 > 0) then
    let (a, r) := rng.next 300
    rng := r
    n := { n with ai0 := 0, ai1 := single (300 + a), ai2 := 0 }
    let (b, r) := rng.next 100
    rng := r
    n := { n with ai1 := n.ai1 - single b, localAI3 := 60, netUpdate := true }
  if n.closeDoor then throw "AI_007: closeDoor"
  let mut maxSpeed : Float32 := 1
  let mut accel : Float32 := 0.07
  if n.friendly && (danger || drowning) then
    maxSpeed := 1.5
    let hurt : Float32 := 1 - single n.life / single n.lifeMax
    maxSpeed := maxSpeed + hurt * 0.9
    accel := 0.1
  let vx := n.velocity.x
  if vx < -maxSpeed || vx > maxSpeed then
    if n.velocity.y == 0 then n := { n with velocity := ⟨n.velocity.x * 0.8, n.velocity.y * 0.8⟩ }
  else if vx < maxSpeed && n.direction == 1 then
    n := { n with velocity.x := n.velocity.x + accel }
    if n.velocity.x > maxSpeed then n := { n with velocity.x := maxSpeed }
  else if vx > -maxSpeed && n.direction == -1 then
    n := { n with velocity.x := n.velocity.x - accel }
    if n.velocity.x > maxSpeed then n := { n with velocity.x := maxSpeed }
  if single (n.homeTileY * 16 - 32) > n.position.y then throw "AI_007: below the home tile"
  if !(n.velocity.y < 0) then
    let s := stepUp world n.position n.velocity n.width n.height n.stepSpeed n.gfxOffY true 1
    n := { n with position := s.position, velocity := s.velocity, stepSpeed := s.stepSpeed, gfxOffY := s.gfxOffY }
  if n.velocity.y == 0 then
    let aheadX := trunc ((n.position.x + single (n.width / 2 : Nat) + single (15 * n.direction)) / 16)
    let footY := trunc ((n.position.y + single n.height - 16) / 16)
    let stuckTime : Int := 180
    let (keepWalking0, avoidFalling) := walkPrediction drowning
    let mut keepWalking := keepWalking0
    if avoidFalling then throw "AI_007: avoid falling"
    if n.position.x == n.localAI3 then
      n := { n with direction := -n.direction, netUpdate := true, localAI3 := single stuckTime }
    if drowning then
      if n.localAI3 > single stuckTime then n := { n with localAI3 := single stuckTime }
      if n.localAI3 > 0 then n := { n with localAI3 := n.localAI3 - 1 }
    else
      n := { n with localAI3 := -1 }
    let t83 := tileAt g aheadX footY
    let t84 := tileAt g aheadX (footY - 1)
    let t85 := tileAt g aheadX (footY - 2)
    let shortBody := (n.height / 16 : Nat) < 3
    let solidBlock (t : Tile) : Bool := nactive t && solid g t && !solidTop g t
    if (n.velocity.x < 0 && n.direction == -1) || (n.velocity.x > 0 && n.direction == 1) then
      let mut turn := false
      let mut halt := false
      let dir := n.direction
      if solidBlock t85 && (!shortBody || solidBlock t84) then
        if !solidTilesVersatile g (aheadX - dir * 2) (aheadX - dir) (footY - 5) (footY - 1) &&
            !solidTiles g aheadX aheadX (footY - 5) (footY - 3) then
          n := { n with velocity.y := -6, netUpdate := true }
        else
          halt := danger
          turn := true
      else if solidBlock t84 then
        if !solidTilesVersatile g (aheadX - dir * 2) (aheadX - dir) (footY - 4) (footY - 1) &&
            !solidTiles g aheadX aheadX (footY - 4) (footY - 2) then
          n := { n with velocity.y := -5, netUpdate := true }
        else
          halt := danger
          turn := true
      else if n.position.y + single n.height - single (footY * 16) > 20 &&
          nactive t83 && solid g t83 && !topSlope t83 then
        if !solidTilesVersatile g (aheadX - dir * 2) (aheadX - dir) (footY - 3) (footY - 1) then
          n := { n with velocity.y := -4.4, netUpdate := true }
        else
          halt := danger
          turn := true
      if halt then
        -- cornered while fleeing: stop and switch to ai[0] = 8 (not transcribed beyond this frame)
        keepWalking := false
        n := { n with velocity.x := 0, ai0 := 8, ai1 := 240, netUpdate := true }
      if turn then
        n := { n with direction := -n.direction, velocity.x := n.velocity.x * -1, netUpdate := true }
      if keepWalking then n := { n with ai1 := 90, netUpdate := true }
      if n.velocity.y < 0 then n := { n with localAI3 := n.position.x }
    if n.velocity.y < 0 && n.wet then n := { n with velocity.y := n.velocity.y * 1.2 }
    if n.velocity.y < 0 then n := { n with velocity.y := n.velocity.y * 1.2 }
  -- stuck: the last move changed nothing but gravity
  if n.oldPosition == n.position && n.oldVelocity == ⟨n.velocity.x + 0, n.velocity.y + gravity⟩ && !danger then
    let before := n.localAI2
    n := { n with localAI2 := before + 1 }
    if !(before < 30) then
      n := { n with localAI2 := 0, ai0 := 1 }
      let (a, r) := rng.next 300
      rng := r
      n := { n with ai1 := single (200 + a), ai2 := 0 }
      let (b, r) := rng.nextRange 200 400
      rng := r
      n := { n with ai1 := n.ai1 + single b, direction := -n.direction, velocity.x := n.velocity.x * -1, netUpdate := true }
  return (n, rng)

/-- What `Collision_WaterCollision(lava = false)` and the wet flags leave: the critter, the stream, and
the splash requests. -/
private def waterCollision (world : World) (rng : Rng) (n : Critter) :
    Except String (Critter × Rng × Array DustRequest × Array SoundRequest) := do
  if n.ai0 == 25 then throw "Collision_WaterCollision: ai[0] = 25"
  let (hit, honey, shimmer) := wetCollision world.grid n.position n.width n.height
  if honey || shimmer then throw "Collision_WaterCollision: honey / shimmer"
  let mut n := n
  let mut rng := rng
  let mut dust : Array DustRequest := #[]
  let mut sounds : Array SoundRequest := #[]
  -- the water splash of IL_0563 (entry) and IL_0d7b (exit): 30 water dusts around the body, then sound 19 style 0
  let splash (n : Critter) (rng : Rng) (dust : Array DustRequest) (sounds : Array SoundRequest) :
      Rng × Array DustRequest × Array SoundRequest := Id.run do
    let mut rng := rng
    let mut dust := dust
    for _ in [:30] do
      let d : DustRequest :=
        { position := ⟨n.position.x - 6, n.position.y + single (n.height / 2 : Nat) - 8⟩
          width := n.width + 12, height := 24, dustType := world.waterDust }
      rng := world.effects.newDust rng d
      dust := dust.push d
    let s : SoundRequest := { soundType := 19, x := trunc n.position.x, y := trunc n.position.y }
    return (world.effects.playSound rng s, dust, sounds.push s)
  if hit then
    if !n.wet && n.wetCount == 0 then
      n := { n with wetCount := 10 }
      (rng, dust, sounds) := splash n rng dust sounds
    n := { n with wet := true }
  else if n.wet then
    n := { n with velocity.x := n.velocity.x * 0.5, wet := false }
    if n.wetCount == 0 then
      n := { n with wetCount := 10 }
      if n.lavaWet || n.shimmerWet || n.honeyWet then throw "Collision_WaterCollision: lava / shimmer / honey exit"
      (rng, dust, sounds) := splash n rng dust sounds
  return (n, rng, dust, sounds)

/-- `NPC.UpdateCollision` for the bunny. -/
def updateCollision (world : World) (gravity : Float32) (rng : Rng) (n : Critter) :
    Except String (Critter × Rng × Array DustRequest × Array SoundRequest) := do
  let g := world.grid
  let mut n := n
  -- Collision_WalkDownSlopes
  n := { n with velocity := OriginalCollision.walkDownSlope g n.position n.velocity n.width n.height gravity }
  -- Collision_LavaCollision: only an empty answer is transcribed
  let (x0, x1, y0, y1) := liquidBox g n.position n.width n.height
  if anyTile g x0 (x1 - 1) y0 (y1 - 1) (fun t => liquidAmount t > 0 && liquidIs t .lava) then
    throw "Collision_LavaCollision: lava next to the critter"
  let (n', rng, dust, sounds) ← waterCollision world rng n
  n := n'
  if !n.wet then n := { n with lavaWet := false, honeyWet := false, shimmerWet := false }
  if n.wetCount > 0 then n := { n with wetCount := n.wetCount - 1 }
  -- Collision_DecideFallThroughPlatforms (aiStyle 7, not a town NPC)
  if n.homeTileY - cdiv (trunc (n.position.y + single n.height)) 16 > 16 then
    throw "Collision_DecideFallThroughPlatforms: far above the home tile"
  let fall := false
  n := { n with oldVelocity := n.velocity, collideX := false, collideY := false }
  let before := n.velocity
  let hit := OriginalCollision.tileCollision g n.position n.velocity n.width n.height
    { fallThrough := fall, fall2 := fall, hoik := true }
  n := { n with velocity := hit.displacement }
  if n.wet then
    -- Collision_MoveWhileWet(before, waterMovementSpeed)
    if hit.hitAbove then n := { n with velocity.y := 0.01 }
    let mut move : Vector := ⟨n.velocity.x * n.waterMovementSpeed, n.velocity.y * n.waterMovementSpeed⟩
    if n.velocity.x != before.x then
      move := { move with x := n.velocity.x }
      n := { n with collideX := true }
    if n.velocity.y != before.y then
      move := { move with y := n.velocity.y }
      n := { n with collideY := true }
    n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + move }
  else
    -- Collision_MoveWhileDry
    if hit.hitAbove then n := { n with velocity.y := 0.01 }
    if n.oldVelocity.x != n.velocity.x then n := { n with collideX := true }
    if n.oldVelocity.y != n.velocity.y then n := { n with collideY := true }
    n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  -- Collision_MoveSlopesAndStairFall(fall)
  if n.homeTileY - cdiv (trunc (n.position.y + single n.height)) 16 > 16 then
    throw "Collision_MoveSlopesAndStairFall: far above the home tile"
  let slope := OriginalCollision.slopeCollision g n.position n.velocity n.width n.height n.stairFall
  if slope.stairFall then n := { n with stairFall := true }
  else if !fall then n := { n with stairFall := false }
  if slope.stair && (slope.position.y - n.position.y).abs > 8 then
    n := { n with gfxOffY := n.gfxOffY - (slope.position.y - n.position.y), stepSpeed := 2 }
  n := { n with position := slope.position + ⟨0, 0⟩, velocity := slope.velocity }
  -- ConveyorBeltCollision → Collision.StepConveyorBelt(this, 1, false): only an empty answer is transcribed
  let bx := trunc (n.position.x / 16)
  let by' := trunc ((n.position.y + single n.height) / 16)
  if anyTile g (bx - 2) (bx + 3) (by' - 2) (by' + 2) (fun t => isActive t && (ttype t == 421 || ttype t == 422)) then
    throw "StepConveyorBelt: a conveyor belt next to the critter"
  return (n, rng, dust, sounds)

/-- The bunny case of `NPC.FindFrame`. -/
def findFrame (world : World) (n : Critter) : Critter := Id.run do
  let num := world.frameHeight
  let mut n := n
  if n.velocity.y == 0 then
    if n.direction == 1 then n := { n with spriteDirection := 1 }
    if n.direction == -1 then n := { n with spriteDirection := -1 }
    if n.velocity.x == 0 then
      n := { n with frameY := 0, frameCounter := 0 }
    else
      n := { n with frameCounter := n.frameCounter + (n.velocity.x.abs * 1).toFloat }
      n := { n with frameCounter := n.frameCounter + 1 }
      if n.frameCounter > 6 then n := { n with frameY := n.frameY + num, frameCounter := 0 }
      if cdiv n.frameY num ≥ world.frameCount then n := { n with frameY := 0 }
  else if n.velocity.y < 0 then
    n := { n with frameCounter := 0, frameY := num * 4 }
  else if n.velocity.y > 0 then
    n := { n with frameCounter := 0, frameY := num * 6 }
  return n

/-- `NPC.CheckActive`: players in the active range keep the critter and get `npcSlots` added to
`nearbyActiveNPCs`; players inside the `sWidth × sHeight` rectangle refresh `timeLeft`. -/
def checkActive (world : World) (players : Array Player) (n : Critter) :
    Except String (Critter × Array (Nat × Float32) × Bool) := do
  let mut n := n
  let cx := n.position.x + single (n.width / 2 : Nat)
  let cy := n.position.y + single (n.height / 2 : Nat)
  let range : Rect := ⟨trunc (cx - single world.activeRangeX), trunc (cy - single world.activeRangeY),
    world.activeRangeX * 2, world.activeRangeY * 2⟩
  let screen : Rect := ⟨(cx.toFloat - Float.ofInt world.sWidth * 0.5 - Float.ofNat n.width).toInt64.toInt,
    (cy.toFloat - Float.ofInt world.sHeight * 0.5 - Float.ofNat n.height).toInt64.toInt,
    world.sWidth + n.width * 2, world.sHeight + n.height * 2⟩
  let mut keep := false
  let mut nearby := #[]
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.index ≥ 255 then continue
    let box : Rect := ⟨trunc p.position.x, trunc p.position.y, p.width, p.height⟩
    if range.intersects box then
      keep := true
      if n.releaseOwner == 255 && n.lifeMax > 0 then
        if world.slimeRain then throw "CheckActive: slime rain"
        nearby := nearby.push (p.index, n.npcSlots)
    if screen.intersects box then
      n := { n with timeLeft := world.activeTime, despawnEncouraged := false }
  n := { n with timeLeft := n.timeLeft - 1 }
  if n.timeLeft ≤ 0 then keep := false
  if !keep then
    if n.extraValue > 0 then throw "CheckActive: extraValue"
    n := { n with active := false }
  return (n, nearby, !keep)

/-- One `NPC.UpdateNPC(i)` call for the bunny, failing on paths this port does not transcribe. -/
def updateCore (world : World) (players : Array Player) (others : Array Other) (rng : Rng) (n : Critter) :
    Except String Step := do
  if !n.active then return { critter := n, rng, gravity := none }
  if n.type != 46 then throw s!"type {n.type}"
  if n.hasBuffs then throw "buffs"
  if n.releaseOwner != 255 || n.spawnedFromStatue then throw "released or statue critter"
  if n.position.y > world.bottomWorld - 640 + single n.height then throw "bottom of the world"
  if n.wet && (n.honeyWet || n.shimmerWet) then throw "honey / shimmer gravity"
  let (gravity, maxFall) := gravityFor world n
  let mut n := n
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  if n.life ≤ 0 then throw "life ≤ 0"
  n := { n with oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY }
  let ease := 1 + n.velocity.x.abs / 3
  if n.gfxOffY > 0 then
    n := { n with gfxOffY := n.gfxOffY - ease * n.stepSpeed }
    if n.gfxOffY < 0 then n := { n with gfxOffY := 0 }
  else if n.gfxOffY < 0 then
    n := { n with gfxOffY := n.gfxOffY + ease * n.stepSpeed }
    if n.gfxOffY > 0 then n := { n with gfxOffY := 0 }
  if n.gfxOffY > 16 then n := { n with gfxOffY := 16 }
  if n.gfxOffY < -16 then n := { n with gfxOffY := -16 }
  let (n', rng) ← ai007 world players others gravity rng n
  n := n'
  -- SubAI_HandleTemporaryCatchableNPCPlayerInvulnerability
  let wasFriendly := n.friendly
  if n.catchableNPCTempImmunityCounter > 0 then
    n := { n with catchableNPCTempImmunityCounter := n.catchableNPCTempImmunityCounter - 1, friendly := true }
  else
    n := { n with friendly := false }
  if n.friendly != wasFriendly then n := { n with netUpdate := true }
  if n.extraValue > 0 then throw "extraValue"
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  n := { n with velocity.y := n.velocity.y + gravity }
  if n.velocity.y > maxFall then n := { n with velocity.y := maxFall }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  -- friendly, or NPCID.Sets.TakesDamageFromHostilesWithoutBeingFriendly[46]: CheckLifeRegen, GetHurtByOtherNPCs
  if n.life < n.lifeMax then throw "CheckLifeRegen: life < lifeMax"
  if n.immune[255]! == 0 && !n.dontTakeDamage then
    let own : Rect := ⟨trunc n.position.x, trunc n.position.y, n.width, n.height⟩
    for o in others do
      if o.index == n.whoAmI || o.friendly || o.damage ≤ 0 || world.critterThatCanTurnOnPlayers o.type then continue
      let box : Rect := ⟨trunc o.position.x, trunc o.position.y, o.width, o.height⟩
      if own.intersects box then throw "GetHurtByOtherNPCs: hostile contact"
  if world.beesActive then throw "npcsFoundForCheckActive[210/211]"
  let (n', rng, dust, sounds) ← updateCollision world gravity rng n
  n := n'
  -- Collision.SwitchTiles (lifeMax > 1): only an empty answer is transcribed
  let sx := trunc (n.position.x / 16)
  let sy := trunc (n.position.y / 16)
  if anyTile world.grid (sx - 1) (trunc ((n.position.x + single n.width) / 16) + 2) (sy - 1)
      (trunc ((n.position.y + single n.height) / 16) + 2)
      (fun t => isActive t && (ttype t == 135 || ttype t == 210 || ttype t == 442 || ttype t == 443)) then
    throw "SwitchTiles: a pressure plate or mine next to the critter"
  n := findFrame world n
  let (n', nearby, despawned) ← checkActive world players n
  n := { n' with netUpdate := false, justHit := false }
  return { critter := n, rng, gravity := some gravity, nearby, noSpawnCycle := despawned, dust, sounds }

/-- One `NPC.UpdateNPC(i)` call for the bunny. `others` are the other active NPC slots. On a path this
port does not transcribe, `unsupported` names it and the critter and stream are returned unchanged. -/
def update (world : World) (players : Array Player) (others : Array Other) (rng : Rng) (n : Critter) : Step :=
  match updateCore world players others rng n with
  | .ok step => step
  | .error why => { critter := n, rng, gravity := none, unsupported := some why }

end Lean4Terria.OriginalForestCritter
