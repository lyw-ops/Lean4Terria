import Lean4Terria.OriginalForestCritter
import Lean4Terria.OriginalForestGuideBow
import Lean4Terria.OriginalForestResidentNight
import Lean4Terria.OriginalPlayerCollision
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalTownCombat
import Lean4Terria.UnifiedRandom

/-!
# Guide and Old Man in a new Classic world

The dry, single-player part of Terraria 1.4.5.8's `NPC.UpdateNPC`
for the saved Guide and Old Man. The caller walks the shared NPC slots in
ascending order after the spawn phase and passes the persistent NPC random
stream from slot to slot. Call `townCount` from the scene scan after NPC and
time updates. The spawn phase reads the previously scanned count.

This path covers ordinary activity, shelter and sitting in an unopened
Dungeon world, including flight from nearby hostile NPCs and ordinary hostile
contact. Doors, other furniture, liquids and special events have separate paths.
-/

namespace Lean4Terria.OriginalForestGuide

open OriginalCollision (Grid Vector single tileIndex)
open OriginalWorld (SavedTownNpc Tile)
open OriginalPlayerCollision (Body)

def Resident.ofSaved (record : SavedTownNpc) : Option Resident := do
  let kind ← if record.typeId == Kind.guide.typeId then some Kind.guide
    else if record.typeId == Kind.oldMan.typeId then some Kind.oldMan
    else if record.typeId == Kind.mechanic.typeId then some Kind.mechanic else none
  return { kind, name := record.name, position := ⟨record.position.1, record.position.2⟩
           homeTile := record.homeTile, homeless := record.homeless, variation := record.variation
           homelessDespawn := record.homelessDespawn }

/-- Preserve the saved-town array index even for kinds handled by other code. -/
def load (archive : OriginalWorld.Archive) : Except String (Array (Option Resident)) :=
  return (← archive.readNpcs).town.map Resident.ofSaved

private def pixel (p : Float32) : Int := p.toInt32.toInt

/-- `SceneMetrics.ScanNPCPositions` uses the integer NPC center and the
half-open 3840 × 2400 area centered on the local player. -/
def inTownScan (playerCenter npcCenter : Vector) : Bool :=
  let x := pixel (playerCenter.x - 1920)
  let y := pixel (playerCenter.y - 1200)
  let npcX := pixel npcCenter.x
  let npcY := pixel npcCenter.y
  x ≤ npcX && npcX < x + 3840 && y ≤ npcY && npcY < y + 2400

def townCount (playerCenter : Vector) (residents : Array Resident) : Nat :=
  residents.foldl (fun count n => if n.active && inTownScan playerCenter n.center then count + 1 else count) 0

structure Player where
  position : Vector
  width : Nat := 20
  height : Nat := 42
  active : Bool := true
  canBeTalkedTo : Bool := true
  life : Int := 100
  lifeMax : Int := 100
  selectedItem : Nat := 0
  zones : List (String × Bool) := []
  deriving Repr

structure World where
  grid : Grid
  dungeon : Int × Int
  players : Array Player
  otherResidents : Array Resident := #[]
  threats : Array OriginalForestCritter.Other := #[]
  dayTime : Bool := true
  eclipse : Bool := false
  downedSkeletron : Bool := false
  time : Float := 13500
  surface : Float := 0
  rockLayer : Float := 0
  cloudBackgroundActive : Bool := false
  crimson : Bool := false
  downedEye : Bool := false
  downedEvilBoss : Bool := false
  downedQueenBee : Bool := false
  downedGoblins : Bool := false
  raining : Bool := false
  hardMode : Bool := false

inductive Boundary where
  | worldState
  | actorState
  | wet
  | door
  | furniture
  | social
  | emote
  | stuck
  | unsafeGround
  deriving BEq, Repr

structure Roll where
  lower : Int
  upper : Int
  value : Int
  deriving BEq, Repr

/-- A resident update returns the same RNG type as the rest of the NPC phase.
`rolls` records integer draws made directly by the town AI. -/
structure Step where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  resident : Resident
  random : UnifiedRandom
  rolls : Array Roll
  bow : OriginalForestGuideBow.State
  shot : Option OriginalForestGuideBow.Shot
  /-- A hostile NPC hit this update (`NPCHit1`; with `killed`, `NPCDeath1`, announcement, optional
  tombstone, drops, gores). A killed resident is inactive; `checkDead` set `noSpawnCycle`. -/
  hit : Option OriginalTownCombat.Hit := none
  pool : OriginalForestGuideArrow.DustPool := { enabled := false }
  noSpawnCycle : Bool := false

private structure RandomState where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  random : UnifiedRandom
  rolls : Array Roll := #[]
  bow : OriginalForestGuideBow.State := {}
  shot : Option OriginalForestGuideBow.Shot := none
  effects : OriginalTownCombat.Effects := {}
  pool : OriginalForestGuideArrow.DustPool := { enabled := false }
  hit : Option OriginalTownCombat.Hit := none

private abbrev Update := StateT RandomState (Except Boundary)

private def next (upper : Int) (lower : Int := 0) : Update Int := do
  let s ← get
  let (value, random) := s.random.next lower upper
  set { s with random, rolls := s.rolls.push ⟨lower, upper, value⟩ }
  return value

private def nextFloat : Update Float32 := do
  let s ← get
  let (value, random) := s.random.sample
  set { s with random }
  return value.toFloat32

private def tile (grid : Grid) (x y : Int) : Tile :=
  grid.tileAt x.toNat y.toNat

private def solid (grid : Grid) (t : Tile) : Bool :=
  t.id.any (fun id => (grid.rules id).solid) && !t.actuated

private def solidWall (grid : Grid) (t : Tile) : Bool :=
  solid grid t && !(grid.rules (t.id.getD 0)).solidTop

private def clearRect (grid : Grid) (x1 x2 y1 y2 : Int) (flatOnly : Bool) : Bool := Id.run do
  for x in [(min x1 x2).toNat : (max x1 x2).toNat + 1] do
    for y in [(min y1 y2).toNat : (max y1 y2).toNat + 1] do
      let t := grid.tileAt x y
      if solidWall grid t && (!flatOnly || t.shape == .full) then return false
  return true

private def feet (n : Resident) : Float32 := n.position.y + single n.height

private def settleLedges (world : World) (n : Resident) : Resident :=
  let body : Body := {
    position := n.position, velocity := n.velocity, width := n.width, height := n.height
    gfxOffY := n.gfxOffY, stepSpeed := n.stepSpeed }
  let allowPlatform := n.position.y ≥ single (n.homeTile.2 * 16 - 32)
  let body := if !allowPlatform && body.velocity.y == 0 then body.stepDown world.grid else body
  let body := if body.velocity.y ≥ 0 then
    ({ body with controlUp := allowPlatform }).stepUp world.grid else body
  { n with position := body.position, velocity := body.velocity, gfxOffY := body.gfxOffY
           stepSpeed := body.stepSpeed }

private def aheadTile (n : Resident) : Int × Int :=
  (tileIndex (n.center.x + single (15 * n.direction)), tileIndex (feet n - 16))

private structure NearbyDanger where
  fleeDirection : Int
  targets : OriginalForestGuideBow.Targets

private def nearbyDanger (world : World) (n : Resident) (bowActive : Bool) : Option NearbyDanger := Id.run do
  let range : Float32 := if n.kind == .guide then 700 else 200
  let mut detected := false
  let mut stinky := false
  let mut left : Float32 := -1
  let mut right : Float32 := -1
  let mut leftTarget : Option OriginalForestCritter.Other := none
  let mut rightTarget : Option OriginalForestCritter.Other := none
  for other in world.threats do
    if other.type == 690 || !((!other.friendly && other.damage > 0) || other.stinky) then continue
    let center := other.center
    if OriginalForestCritter.vecDistance center n.center ≥ range then continue
    if !other.noTileCollide && !OriginalForestCritter.canHit world.grid n.center center then continue
    detected := true
    stinky := stinky || other.stinky
    let dx := center.x - n.center.x
    let canBeChased := !other.friendly && other.damage > 0
    if dx < 0 && (left == -1 || dx > left) then
      left := dx
      if canBeChased then leftTarget := some other
    if dx > 0 && (right == -1 || dx < right) then
      right := dx
      if canBeChased then rightTarget := some other
  if !detected then return none
  let nearest := if left == -1 || (right > 0 && right < -left) then right else -left
  -- The cornered-state check precedes PrettySafe, retaining the longer attack range.
  if n.kind == .guide && n.activity != .cornered && !bowActive && nearest > 200 then return none
  let targetRight := left == -1 || (right != -1 && right < -left)
  let targets : OriginalForestGuideBow.Targets := {
    danger := true, stinky
    nearest := if targetRight then rightTarget else leftTarget
    opposite := if targetRight then leftTarget else rightTarget }
  return some ⟨if targetRight then -1 else 1, targets⟩

private def applyDanger (world : World) (n : Resident) (homeX direction : Int) : Update Resident := do
  if n.walking then return { n with direction }
  if n.activity == .cornered then
    if n.direction != direction then return n
    let delay ← next 300
    return { n with activity := .walking, timer := single (300 + delay), turnTimer := 0 }
  let (x, y) := aheadTile n
  let centerX := tileIndex n.center.x
  let towardHome := (centerX - homeX).natAbs > 35 &&
    n.direction == (if centerX < homeX then 1 else -1)
  if !towardHome && !(List.range 6).any (fun offset => solid world.grid (tile world.grid x (y + offset - 1))) then
    return n
  let delay ← next 120
  return { n with activity := .walking, talkTarget := 0, timer := single (120 + delay), turnTimer := 0, direction }

private def turnAtObstacle (n : Resident) (fleeing : Bool) : Resident :=
  if fleeing then { n with direction := -n.direction, velocity.x := 0, activity := .cornered, timer := 240 }
  else { n with direction := -n.direction, velocity.x := -n.velocity.x }

private def walkTerrain (world : World) (n : Resident) (homeX : Int) (fleeing : Bool) : Update Resident := do
  let n := settleLedges world n
  if n.velocity.y != 0 then return n
  let (x, y) := aheadTile n
  let grid := world.grid
  let mut n := n
  if n.position.x == n.turnTimer && !fleeing then n := { n with direction := -n.direction }
  n := { n with turnTimer := -1 }
  let head := tile grid x (y - 2)
  if head.id.any (fun id => id == 10 || id == 388) && !head.actuated then throw .door
  if n.velocity.x * single n.direction ≤ 0 then return n
  let lower := tile grid x y
  let upper := tile grid x (y - 1)
  let d := n.direction
  let noFloor := (tileIndex n.center.x - homeX).natAbs ≤ 35 &&
    clearRect grid x x (y - 1) (y + 4) false
  if noFloor then
    let cx := tileIndex n.center.x
    if clearRect grid (cx - 1) (cx + 1) (y + 1) (y + 1) false then throw .unsafeGround
  if solidWall grid head && solidWall grid upper then
    if clearRect grid (x - 2*d) (x - d) (y - 5) (y - 1) false &&
        clearRect grid x x (y - 5) (y - 3) true then
      n := { n with velocity.y := -6 }
    else n := turnAtObstacle n fleeing
  else if solidWall grid upper then
    if clearRect grid (x - 2*d) (x - d) (y - 4) (y - 1) false &&
        clearRect grid x x (y - 4) (y - 2) true then
      n := { n with velocity.y := -5 }
    else n := turnAtObstacle n fleeing
  else if feet n - single (y * 16) > 20 && solid grid lower &&
      !(let slope := OriginalCollision.slopeId lower.shape; slope == 1 || slope == 2) then
    if clearRect grid (x - 2*d) x (y - 3) (y - 1) false then
      n := { n with velocity.y := -4.4 }
    else n := turnAtObstacle n fleeing
  else if noFloor then
    n := turnAtObstacle n fleeing
  if n.velocity.y < 0 then n := { n with turnTimer := n.position.x }
  return n

private def roam (world : World) (n : Resident) (homeX : Int) (fleeing : Bool) : Update Resident := do
  let mut n := { n with timer := n.timer - 1 }
  if n.timer ≤ 0 then
    let duration ← next 300
    let extra ← next 900
    n := { n with activity := .idle, timer := single (300 + duration + extra), turnTimer := 60 }
  let x := n.velocity.x
  let cap : Float32 := if fleeing then 1.5 else 1
  let accel : Float32 := if fleeing then 0.1 else 0.07
  -- On the negative branch the IL compares against positive max speed
  -- after subtracting the acceleration, so a walking step can overshoot -1.
  let vx := if x < -cap || x > cap then (if n.velocity.y == 0 then x * 0.8 else x)
    else if n.direction == 1 && x < cap then min cap (x + accel)
    else if n.direction == -1 && x > -cap then x - accel else x
  n ← walkTerrain world { n with velocity.x := vx } homeX fleeing
  if n.oldPosition == n.position && n.oldVelocity == n.velocity + ⟨0, 0.3⟩ then
    if n.stuckTimer ≥ 30 then throw .stuck
    n := { n with stuckTimer := n.stuckTimer + 1 }
  return n

private def idle (n : Resident) (centerX homeX : Int) : Update Resident := do
  let x := n.velocity.x
  let x := if x > 0.1 then x - 0.1 else if x < -0.1 then x + 0.1 else 0
  let timer := if n.timer > 0 then n.timer - 1 else n.timer
  let mut n := { n with velocity.x := x, timer
                        turnTimer := if n.turnTimer > 0 then n.turnTimer - 1 else n.turnTimer }
  if n.timer ≤ 0 then
    let duration ← next 300
    n := { n with activity := .walking, timer := single (200 + duration), turnTimer := 0 }
  let distance := centerX - homeX
  if distance.natAbs ≤ 25 then
    if (← next 80) == 0 && n.turnTimer == 0 then
      n := { n with direction := -n.direction, turnTimer := 200 }
  else if n.turnTimer == 0 && distance.natAbs > 50 then
    n := { n with direction := if distance < 0 then 1 else -1 }
  return n

private def pause (n : Resident) : Update Resident := do
  let turnTimer := n.turnTimer - 1
  let turn := (← next 60) == 0 && turnTimer == 0
  let n := { n with turnTimer := if turn then 60 else turnTimer,
                    direction := if turn then -n.direction else n.direction,
                    timer := n.timer - 1, velocity.x := n.velocity.x * 0.8 }
  if n.timer > 0 then return n
  let delay ← next 60
  return { n with activity := .idle, timer := single (60 + delay), turnTimer := 40 }

private def cornered (n : Resident) (fleeing : Bool) : Update Resident := do
  let timer := n.timer - 1
  let timer := if fleeing && timer < 60 then 180 else timer
  let n := { n with timer, velocity.x := n.velocity.x * 0.8 }
  if timer > 0 then return n
  let delay ← next 60
  let turnDelay ← next 60
  return { n with activity := .idle, timer := single (60 + delay), turnTimer := single (30 + turnDelay) }

private def Player.center (p : Player) : Vector :=
  p.position + ⟨single p.width * 0.5, single p.height * 0.5⟩

private def talkLine (world : World) (n : Resident) (p : Player) : Bool :=
  OriginalStarterMelee.LineOfSight.canHitLine world.grid
    ⟨n.center.x, n.position.y⟩ 0 0 ⟨p.center.x, p.position.y⟩ 0 0

private def talking (world : World) (n : Resident) : Update Resident := do
  let n := { n with timer := n.timer - 1, velocity.x := n.velocity.x * 0.8 }
  if let some p := world.players[n.talkTarget]? then
    if n.timer > 0 && p.active && p.canBeTalkedTo &&
        OriginalForestCritter.vecDistance p.center n.center ≤ 200 && talkLine world n p then
      return { n with direction := if n.center.x < p.center.x then 1 else -1 }
  let delay ← next 60
  let turnDelay ← next 60
  return { n with activity := .idle, talkTarget := 0, timer := single (60 + delay)
                  turnTimer := single (30 + turnDelay) }

private def ambient (world : World) (n : Resident) (danger : Option NearbyDanger) : Update Resident := do
  let bow := (← get).bow.afterTownCooldown
  modify fun state => { state with bow }
  if bow.active || n.velocity.y != 0 || n.activity == .pausing || n.activity == .talking then return n
  if let some danger := danger then
    if n.kind == .guide && bow.cooldown == 0 && !danger.targets.stinky then
      if (← next OriginalForestGuideBow.attemptChance) == 0 then
        if let some (resident, bow) := OriginalForestGuideBow.enter world.grid n bow danger.targets then
          modify fun state => { state with bow }
          return { resident with stuckTimer := n.activity.aiValue.toUInt64.toNat }
    return n
  if n.activity == .cornered then return n
  if n.walking then
    if (← next 300) == 0 then
      if (tile world.grid (tileIndex n.center.x) (tileIndex (feet n - 2))).id.any
          (fun id => id == 15 || id == 497) then throw .furniture
    else if (← next 600) == 0 then
      let x := tileIndex (n.center.x + single (10 * n.direction))
      let y := tileIndex n.center.y
      if (tile world.grid x y).id.isSome then throw .furniture
    return n
  -- Ordinary conversation and the alternate emote conversation both roll the
  -- duration before searching the resident slots, and exit AI_007 after the search.
  for chance in #[300, 1800] do
    if (← next chance) == 0 then
      let variant ← next 2
      let _ ← next (if variant == 0 then 4 else 3) 1
      let canStartConversation (other : Resident) :=
        let dx := n.center.x - other.center.x
        let dy := n.center.y - other.center.y
        let distance := (dx * dx + dy * dy).toFloat.sqrt.toFloat32
        other.active && !other.wet && !(other.walking && other.timer > 200) &&
          20 < distance && distance < 100 &&
          OriginalStarterMelee.LineOfSight.canHit world.grid ⟨n.center, 0, 0⟩ ⟨other.center, 0, 0⟩
      if world.otherResidents.any canStartConversation then throw .social
      return n
  -- Guide and Old Man take these calls before the type or housing test.
  let _ ← next 1200
  let _ ← next 600
  if (← next 1800) == 0 then
    let duration ← next 2 1
    return { n with activity := .pausing, timer := single (45 * duration) }
  let _ ← next 600
  if (← next 1200) == 0 then
    for (p, index) in world.players.zipIdx do
      if p.active && p.canBeTalkedTo &&
          OriginalForestCritter.vecDistance p.center n.center < 150 && talkLine world n p then
        return { n with activity := .talking, timer := 220, talkTarget := index
                        direction := if n.position.x < p.position.x then 1 else -1 }
  return n

private def biomeEmote (world : World) (p : Player) : Nat :=
  let y := (p.position.y / 16).toFloat
  let zone := fun name => (p.zones.lookup name).getD false
  if y < world.surface * 0.45 then 22
  else if y > world.rockLayer + (world.grid.height / 2).toFloat - 100 then 31
  else if y > world.rockLayer then 30
  else if zone "ZoneHallow" then 27
  else if zone "ZoneCorrupt" then 26
  else if zone "ZoneCrimson" then 25
  else if zone "ZoneJungle" then 24
  else if zone "ZoneSnow" then 32
  else if y < world.surface &&
      (p.position.x < 4000 || p.position.x > single (16 * (world.grid.width - 250))) then 29
  else if zone "ZoneDesert" then 28
  else 23

/-- The solo town bubble pool in the daytime, dry, early Classic world.
Probes append entries in native order, including duplicate weights. -/
private def pickEmote (world : World) (n : Resident) (duration : Nat) : Update Emote := do
  if world.raining || world.hardMode then throw .emote
  let some p := world.players[n.talkTarget]? | throw .emote
  let mut pool : Array Nat := #[]
  if (← next 3) == 0 then
    for kind in #[Kind.guide, Kind.oldMan] do
      if kind != n.kind && world.otherResidents.any (fun npc => npc.active && npc.kind == kind) then
        pool := pool.push (if kind == .guide then 105 else 106)
  if (← next 3) == 0 then
    pool := pool ++ #[0, 1, 2, 3, 15, 16, 17, 87, 91, 136, 134, 135, 137, 138, 139]
  if (← next 3) == 0 then pool := pool.push (biomeEmote world p)
  if (← next 2) == 0 then
    let underground := n.center.y.toFloat ≥ world.rockLayer * 16
    if (← nextFloat) ≤ (if underground then 0.2 else 1) then
      pool := pool ++ #[13, 12, 68, 62, 63, 69, 70]
      if world.time < 5400 || world.time > 48600 then pool := pool.push 61
      if world.downedGoblins then pool := pool.push 64
      if world.crimson then pool := pool.push 67
    if (← nextFloat) ≤ (if underground then 1 else 0.2) then pool := pool ++ #[72, 69]
  if (← next 2) == 0 then
    pool := pool ++ #[7, 73, 74, 75, 78, 90]
    if p.life < p.lifeMax / 2 then pool := pool.push 84
  if (← next 5) == 0 then
    let earlyBoss := world.downedEye || world.downedEvilBoss
    let includeEarly ← if world.downedQueenBee then do
      pure ((← next 10) == 0)
      else pure earlyBoss
    if includeEarly then
      pool := pool ++ #[39, if world.crimson then 41 else 40, 51]
    if world.downedEvilBoss || world.downedQueenBee then pool := pool ++ #[43, 42]
  if (← next 2) == 0 then
    if p.center.y > single (world.grid.height * 16 - 3200) then pool := pool.push 9
    if (← next 2) == 0 then pool := pool.push 11
    if (p.zones.lookup "ZoneJungle").getD false then pool := pool.push 8
    let mushed ← if p.selectedItem == 215 then pure true else do
      pure ((← next 3) == 0)
    if mushed then pool := pool.push 10
  if (← next 2) == 0 && !world.dayTime then
    if (← next 4) == 0 then pool := pool.push 18
    if world.downedEvilBoss then pool := pool.push 99
  if (← next 2) == 0 then
    if world.cloudBackgroundActive then pool := pool.push 96
    pool := pool.push 95
    if (p.zones.lookup "ZoneHallow").getD false then pool := pool.push 6
  pool := pool ++ (if n.kind == .guide then #[79] else #[43, 43, 43, 72, 72])
  let choice ← next pool.size
  return { id := pool[choice.toNat]!, duration, timeLeft := duration }

private def talkingFrame (world : World) (n : Resident) : Update Resident := do
  let row := n.kind.frameCount - 5
  let valid := n.frameRow == 0 || (row - n.frameRow == 1 || row - n.frameRow == 2) ||
    (row - n.frameRow == 4 || row - n.frameRow == 5)
  let counter := if valid then n.frameCounter + 1 else 0
  let n := { n with frameCounter := if counter ≥ 220 then 0 else counter, spriteDirection := n.direction }
  if counter == 16 || counter == 160 then
    return { n with frameRow := 0, emote := some (← pickEmote world n (if counter == 16 then 112 else 60)) }
  let phase := counter.toUInt64.toNat
  let speak := (16 < phase && phase < 128 && phase % 16 < 8) ||
    (160 < phase && phase < 220 && phase % 12 < 6)
  return { n with frameRow := if speak then row - 2 else 0 }

private def findFrame (n : Resident) : Resident :=
  if n.velocity.y != 0 then { n with frameRow := 1, frameCounter := 0 }
  else if n.activity == .pausing then
    let n := { n with spriteDirection := n.direction, frameCounter := n.frameCounter + 1 }
    -- Guide has five attack rows, and Old Man has none. Both have 21 non-attack rows.
    if n.frameRow == 0 then
      if n.frameCounter ≥ 40 then { n with frameRow := 20, frameCounter := 0 } else n
    else if n.frameRow != 20 || n.frameCounter ≥ 5 then { n with frameRow := 0, frameCounter := 0 }
    else n
  else if n.velocity.x == 0 then
    { n with frameRow := 0, frameCounter := 0, spriteDirection := n.direction }
  else
    let counter := (n.frameCounter + (2 * n.velocity.x.abs).toFloat) + 1
    let frame := max n.frameRow 2
    let (frame, counter) := if counter > 6 then (frame + 1, 0) else (frame, counter)
    let frame := if frame ≥ n.kind.frameCount - n.kind.extraFrameCount then 2 else frame
    { n with frameRow := frame, frameCounter := counter, spriteDirection := n.direction }

private def updateCollision (world : World) (n : Resident) : Resident :=
  let grid := world.grid
  let fallThrough := if world.dayTime then n.homeTile.2 - tileIndex (feet n) > 16
    else (feet n - 8) / 16 < single (n.homeTile.2 - 1)
  let velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height 0.3
  let hit := OriginalCollision.tileCollision grid n.position velocity n.width n.height
    { fallThrough, fall2 := fallThrough, hoik := true }
  let moved := if hit.hitAbove then { hit.displacement with y := 0.01 } else hit.displacement
  let position := n.position + moved
  let x := tileIndex (position.x + single (n.width / 2 : Nat))
  let blocked := solid grid (tile grid x (tileIndex position.y))
  let checkStairs := !world.dayTime || blocked || n.homeTile.2 - tileIndex (position.y + single n.height) > 16
  let stairFall := if checkStairs then (position.y + single n.height - 8) / 16 < single n.homeTile.2
    else fallThrough || n.stairFall
  let slope := OriginalCollision.slopeCollision grid position moved n.width n.height stairFall
  let dy := slope.position.y - position.y
  let stairStep := slope.stair && dy.abs > 8
  { n with oldVelocity := velocity, oldPosition := n.position,
           position := slope.position, velocity := slope.velocity,
           collideX := velocity.x != moved.x, collideY := velocity.y != moved.y
           stairFall := slope.stairFall || (fallThrough && stairFall),
           gfxOffY := if stairStep then n.gfxOffY - dy else n.gfxOffY,
           stepSpeed := if stairStep then 2 else n.stepSpeed }

/-- `CheckLifeRegen` and `GetHurtByOtherNPCs` of a town resident through `OriginalTownCombat`:
regeneration by type (Guide +6, Mechanic +1), the hostile hit with the panic roll, hit dust and,
for the rescued Mechanic, death (the resident becomes inactive; collision and frame still run).
The Old Man (type 37) is never hurt by NPCs. A Guide death is a boundary. -/
private def hostileContact (world : World) (n : Resident) : Update Resident := do
  if n.kind == .oldMan then return n
  let state ← get
  let others : Array OriginalTownCombat.Other := world.threats.map fun o =>
    { slot := o.index, typeId := o.type, friendly := o.friendly, damage := o.damage
      position := o.position, width := o.width, height := o.height }
  let target : OriginalTownCombat.Target := {
    typeId := n.kind.typeId, townNPC := true, position := n.position, velocity := n.velocity
    width := n.width, height := n.height, life := n.life, lifeMax := n.lifeMax, damage := n.damage
    defense := n.defense, direction := n.direction
    immune255 := ((n.hostileImmunity - 1 : Nat) : Int), friendlyRegen := n.friendlyRegen }
  let target := OriginalTownCombat.lifeRegen target
  let n := { n with life := target.life, friendlyRegen := target.friendlyRegen.toNat
                    hostileImmunity := n.hostileImmunity - 1 }
  let (struck, hit, pool, random) ← match OriginalTownCombat.hurt 1000 others
      {state.effects with text := state.text} state.pool state.random target with
    | .ok result => pure result
    | .error _ => throw .actorState
  -- The partner panic of StrikeNPC_Inner applies to ai[0] = 3, 4, 16 and 17 (NPC-to-NPC chats and
  -- games), which this resident model does not enter; talking to the player is ai[0] = 7.
  let some hit := hit | return n
  set { state with
    random, pool, text := hit.text, hit := some hit
    bow := if hit.text.boundary.isSome then state.bow else { state.bow with previousActivity := none } }
  if hit.text.boundary.isSome then return n
  let some panic := hit.panic | throw .actorState
  return { n with life := struck.life, velocity := struck.velocity, direction := struck.direction
                  active := n.active && !hit.killed, hostileImmunity := struck.immune255.toNat
                  activity := .walking, sitting := false, talkTarget := 0
                  timer := single panic, turnTimer := 0 }

private def applyShelter (progress : OriginalForestResidentNight.Progress) : Update Resident := do
  modify fun state => { state with
    random := progress.random
    rolls := state.rolls ++ progress.rolls.map (fun roll => ⟨0, roll.upper, roll.value⟩) }
  return { progress.state.resident with sitting := progress.state.sitting }

private def update (world : World) (n : Resident) : Update Resident := do
  if !n.active then return n
  if world.eclipse || (world.downedSkeletron && n.kind == .oldMan) then throw .worldState
  if n.wet then throw .wet
  let lower := n.position + ⟨single n.width, single n.height⟩
  if tileIndex n.position.x < 1 || tileIndex n.position.y < 1 ||
      tileIndex lower.x ≥ (world.grid.width : Int) - 1 ||
      tileIndex lower.y ≥ (world.grid.height : Int) - 20 then throw .worldState
  if n.life ≤ 0 || (n.target != 255 && !(n.kind == .mechanic && n.target == 0)) || n.homelessDespawn then
    throw .actorState
  let ease := 1 + n.velocity.x.abs / 3
  let gfxOffY := if n.gfxOffY > 0 then max 0 (n.gfxOffY - n.stepSpeed * ease)
    else min 0 (n.gfxOffY + n.stepSpeed * ease)
  let emote := n.emote.bind fun e =>
    if e.timeLeft > 1 then some { e with timeLeft := e.timeLeft - 1 } else none
  let context : OriginalForestResidentNight.Context := {
    grid := world.grid, dungeon := world.dungeon, dayTime := world.dayTime
    players := world.players.map fun p => ⟨p.position, p.width, p.height, p.active⟩
    occupiedSeats := world.otherResidents.filterMap fun other =>
      if other.active then OriginalForestResidentNight.State.seatAnchor ⟨other, other.sitting⟩ else none }
  let n := { n with gfxOffY, emote }
  let prepared := OriginalForestResidentNight.prepare context ⟨n, n.sitting⟩ (← get).random
  if prepared.needsHousingSearch then throw .furniture
  let mut n ← applyShelter prepared.progress
  let x := prepared.location.current.1
  let homeX := prepared.location.rest.1
  let state ← get
  let danger := nearbyDanger world n state.bow.active
  if let some attack := OriginalForestGuideBow.advance n state.bow
      ((danger.map (·.targets)).getD {}) 0 state.random then
    set { state with bow := attack.state, random := attack.random, shot := attack.shot }
    n := if attack.state.active then attack.resident else
      { attack.resident with turnTimer := single attack.state.localTimer }
  else
    if let some danger := danger then n ← applyDanger world n homeX danger.fleeDirection
    let progress : OriginalForestResidentNight.Progress := {
      state := ⟨n, n.sitting⟩, random := (← get).random }
    if let some shelter := OriginalForestResidentNight.activity context prepared.location false progress then
      n ← applyShelter shelter
    else
      n ← match n.activity with
        | .idle => idle n x homeX
        | .walking => roam world n homeX danger.isSome
        | .pausing => pause n
        | .talking => talking world n
        | .cornered => cornered n danger.isSome
  if n.sitting then modify fun s => { s with bow := s.bow.afterTownCooldown }
  else n ← ambient world n danger
  let velocity := ⟨if n.velocity.x.abs.toFloat < 0.005 then 0 else n.velocity.x,
                    min 10 (n.velocity.y + 0.3)⟩
  n := { n with velocity }
  n ← hostileContact world n
  if (← get).text.boundary.isSome then return n
  n := updateCollision world n
  if (← get).bow.active then return OriginalForestGuideBow.findFrame n
  else if n.velocity.y == 0 && n.activity == .talking then talkingFrame world n
  else if let some seated := OriginalForestResidentNight.State.sittingFrame ⟨n, n.sitting⟩ then return seated
  else return findFrame n

/-- The town collision and ordinary frame selection that close every resident update, including
the update in which the resident was killed (the original still moves it and runs `FindFrame`). -/
def finishUpdate (world : World) (n : Resident) : Resident := findFrame (updateCollision world n)

/-- Update one existing NPC at its current shared slot. On a boundary the
caller receives no replacement state or RNG, so it can stop before the step. -/
def step (world : World) (random : UnifiedRandom) (resident : Resident)
    (bow : OriginalForestGuideBow.State := {}) (effects : OriginalTownCombat.Effects := {}) : Except Boundary Step := do
  let (resident, state) ← (update world resident).run {
    random, bow, effects, pool := effects.dustPool,text := effects.text }
  return { resident, random := state.random, rolls := state.rolls, bow := state.bow, shot := state.shot,text := state.text
           hit := state.hit, pool := state.pool, noSpawnCycle := state.hit.any (·.killed) }

end Lean4Terria.OriginalForestGuide
