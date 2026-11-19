import Lean4Terria.EyeOfCthulhu
import Lean4Terria.Water

namespace Lean4Terria

def eaterSegmentCount : Nat := 67
def eaterSegmentMaxHealth : Nat := 150
def eaterMaxHealth : Nat := eaterSegmentCount * eaterSegmentMaxHealth
def eaterMaxSpeed : Int := 10 * (tileUnits / 16)
def eaterHalfSize : Int := 19 * (tileUnits / 16)
def eaterSegmentSpacing : Int := 2 * eaterHalfSize
def eaterMotionCycle : Nat := 125

inductive WormSegmentKind where
  | head | body | tail
  deriving BEq, Repr, Inhabited

def WormSegmentKind.defense : WormSegmentKind → Nat
  | .head => 2
  | .body => 4
  | .tail => 8

def WormSegmentKind.contactDamage : WormSegmentKind → Nat
  | .head => 22
  | .body => 13
  | .tail => 11

structure WormSegment where
  id : Nat
  motion : EyeMotion
  health : Nat
  deriving BEq, Repr, Inhabited

structure EaterOfWorlds where
  /-- Each chain is head-first. IDs keep their original order through splits. -/
  chains : Array (Array WormSegment)
  retreating : Bool := false
  /-- Distributes fractional acceleration across ticks in the integer simulation. -/
  motionTick : Nat := 0
  deriving BEq, Repr, Inhabited

def WormSegment.hitBox (segment : WormSegment) : HitBox :=
  let center := segment.motion
  ⟨center.x - eaterHalfSize, center.y - eaterHalfSize,
    center.x + eaterHalfSize, center.y + eaterHalfSize⟩

private def segmentRole (index length : Nat) : WormSegmentKind :=
  if index == 0 then .head else if index + 1 == length then .tail else .body

def EaterOfWorlds.health (boss : EaterOfWorlds) : Nat :=
  boss.chains.foldl (fun total chain => chain.foldl (fun sum segment => sum + segment.health) total) 0

def EaterOfWorlds.segment? (boss : EaterOfWorlds) (id : Nat) : Option WormSegment := Id.run do
  for chain in boss.chains do
    if let some segment := chain.find? (·.id == id) then return some segment
  return none

def EaterOfWorlds.segmentKind (boss : EaterOfWorlds) (id : Nat) : Option WormSegmentKind := Id.run do
  for chain in boss.chains do
    if let some index := chain.findIdx? (·.id == id) then return some (segmentRole index chain.size)
  return none

def EaterOfWorlds.targets (boss : EaterOfWorlds) : Array (Nat × HitBox) :=
  boss.chains.flatMap fun chain => chain.map fun segment => (segment.id, segment.hitBox)

def EaterOfWorlds.valid (boss : EaterOfWorlds) (world : World) : Bool := Id.run do
  if boss.chains.isEmpty || boss.motionTick ≥ eaterMotionCycle then return false
  let mut previous : Option Nat := none
  for chain in boss.chains do
    let some first := chain[0]? | return false
    if chain.size < 2 || first.id + chain.size > eaterSegmentCount ||
        previous.any (fun last => last + 1 ≥ first.id) then return false
    for index in [:chain.size] do
      let segment := chain[index]!
      if segment.id != first.id + index || segment.health == 0 || segment.health > eaterSegmentMaxHealth ||
          !segment.motion.valid world eaterMaxSpeed then return false
      if index > 0 && (segment.motion.vx != 0 || segment.motion.vy != 0) then return false
    previous := some (first.id + chain.size - 1)
  return true

def EaterOfWorlds.contact (boss : EaterOfWorlds) (player : Player) : Nat := Id.run do
  let mut damage := 0
  for chain in boss.chains do
    for index in [:chain.size] do
      if chain[index]!.hitBox.overlapsPlayer player then
        damage := max damage (segmentRole index chain.size).contactDamage
  return damage

def EaterOfWorlds.segmentDefense (boss : EaterOfWorlds) (id : Nat) : Nat :=
  (boss.chains.findSome? fun chain => do
    let index ← chain.findIdx? (·.id == id)
    return (segmentRole index chain.size).defense).getD 0

def EaterOfWorlds.damageSegment (boss : EaterOfWorlds) (id raw : Nat) (ignoreDefense := false) : EaterOfWorlds := Id.run do
  if raw == 0 then return boss
  for chainIndex in [:boss.chains.size] do
    let chain := boss.chains[chainIndex]!
    let some index := chain.findIdx? (·.id == id) | continue
    let segment := chain[index]!
    let health := segment.health - (if ignoreDefense then raw else max 1 (raw - (segmentRole index chain.size).defense / 2))
    if health > 0 then
      return { boss with chains := boss.chains.set! chainIndex (chain.set! index { segment with health }) }
    let leading := chain.extract 0 index
    let trailing := chain.extract (index + 1) chain.size
    let mut remaining := boss.chains.extract 0 chainIndex
    -- The nearest body segments become the new tail and head by their positions
    -- in each chain. A segment left with neither neighbor dies with the split.
    if leading.size ≥ 2 then remaining := remaining.push leading
    if trailing.size ≥ 2 then
      let head := trailing[0]!
      let head := { head with motion := { head.motion with vx := 0, vy := 0 } }
      remaining := remaining.push (trailing.set! 0 head)
    return { boss with chains := remaining ++ boss.chains.extract (chainIndex + 1) boss.chains.size }
  return boss

/-- Start the chain in the ground below the summoner. As in the desktop NPC
spawn, the linked segments begin at the head and unfurl as it approaches. -/
def spawnEater (world : World) (player : Player) : EaterOfWorlds :=
  let x := bodyCenterX player
  let side := if x + 16 * tileUnits < world.width * tileUnits then 1 else -1
  let y := min ((world.height : Int) - 5) (player.centerTile.y + 12)
  let start : EyeMotion := ⟨x + side * 16 * tileUnits, y * tileUnits + tileUnits / 2, 0, 0⟩
  { chains := #[(List.range eaterSegmentCount).toArray.map fun id => ⟨id, start, eaterSegmentMaxHealth⟩] }

/-- Native worm steering engages when its box touches a solid or top-solid tile,
or any tile carrying more than 64 units of liquid. -/
def WormSegment.burrowing (segment : WormSegment) (world : World) (water : Water) : Bool := Id.run do
  let box := segment.hitBox
  let left := box.left / tileUnits
  let top := box.top / tileUnits
  let right := (box.right - 1) / tileUnits
  let bottom := (box.bottom - 1) / tileUnits
  for dx in [:(right - left + 1).toNat] do
    for dy in [:(bottom - top + 1).toNat] do
      let position : TilePos := ⟨left + dx, top + dy⟩
      if world.contains position then
        let tile := world.tile position
        if tile.solid || tile == .platform || water.level world position > 64 then return true
  return false

private def fraction (tick numerator : Nat) : Int :=
  ((tick + 1) * numerator / eaterMotionCycle - tick * numerator / eaterMotionCycle : Nat)

private def approach (value target amount : Int) : Int :=
  if value < target then min target (value + amount) else max target (value - amount)

private def headingWith (velocity target : Int) : Bool :=
  (velocity < 0 && target < 0) || (0 < velocity && 0 < target)

private def pushAlong (velocity amount : Int) : Int :=
  if velocity > 0 then velocity + amount else velocity - amount

private def freeFall (motion : EyeMotion) (targetX : Int) (tick : Nat) : EyeMotion :=
  let vy := min eaterMaxSpeed (motion.vy + fraction tick 220)
  let vx :=
    if motion.vx.natAbs + vy.natAbs < (eaterMaxSpeed * 2 / 5).toNat then
      if motion.vx < 0 then motion.vx - fraction tick 154 else motion.vx + fraction tick 154
    else if vy == eaterMaxSpeed then approach motion.vx targetX (fraction tick 140)
    else if 4 * (tileUnits / 16) < vy then approach motion.vx 0 (fraction tick 126)
    else motion.vx
  { motion with vx, vy }

private def burrowSteer (motion : EyeMotion) (dx dy : Int) (tick : Nat) : EyeMotion := Id.run do
  let acceleration := fraction tick 140
  let slow := fun vx vy : Int => vx.natAbs + vy.natAbs < (eaterMaxSpeed / 2).toNat
  let some (wantedX, wantedY) := aimVelocity eaterMaxSpeed dx dy |
    return if slow motion.vx motion.vy then { motion with vx := pushAlong motion.vx acceleration }
      else motion
  let turn := fraction tick 280
  if headingWith motion.vx wantedX || headingWith motion.vy wantedY then
    let vx := approach motion.vx wantedX acceleration
    let vy := approach motion.vy wantedY acceleration
    let threshold := (eaterMaxSpeed / 5).toNat
    if wantedY.natAbs < threshold && headingWith vx (-wantedX) then
      return { motion with vx, vy := pushAlong vy turn }
    if wantedX.natAbs < threshold && headingWith vy (-wantedY) then
      return { motion with vx := pushAlong vx turn, vy }
    return { motion with vx, vy }
  let corner := fraction tick 154
  if dx.natAbs > dy.natAbs then
    let vx := approach motion.vx wantedX corner
    let vy := if slow vx motion.vy then
        pushAlong motion.vy acceleration else motion.vy
    return { motion with vx, vy }
  let vy := approach motion.vy wantedY corner
  let vx := if slow motion.vx vy then
      pushAlong motion.vx acceleration else motion.vx
  return { motion with vx, vy }

private def advanceWormHead (world : World) (water : Water) (player : Player)
    (segment : WormSegment) (tick : Nat) (retreat : Bool) : WormSegment :=
  let motion := segment.motion
  let playerX := bodyCenterX player
  let playerY := bodyCenterY player
  let farRange := (1000 * (tileUnits / 16)).toNat
  let far := (playerX - motion.x).natAbs > farRange || (playerY - motion.y).natAbs > farRange
  let dx := (playerX.tdiv tileUnits - motion.x.tdiv tileUnits) * tileUnits
  let dy := (playerY.tdiv tileUnits - motion.y.tdiv tileUnits) * tileUnits
  let moved := if segment.burrowing world water || far then
      if retreat then burrowSteer motion 0 eaterMaxSpeed tick else burrowSteer motion dx dy tick
    else freeFall motion (if retreat then 0 else dx) tick
  let bound := fun velocity => max (-eaterMaxSpeed) (min eaterMaxSpeed velocity)
  let vx := bound moved.vx
  let vy := bound moved.vy
  { segment with motion := ⟨motion.x + vx, motion.y + vy, vx, vy⟩ }

private def followWormSegment (world : World) (segment predecessor : WormSegment) : WormSegment :=
  let prior := predecessor.motion
  let current := segment.motion
  let follow := aimVelocity eaterSegmentSpacing (prior.x - current.x) (prior.y - current.y)
  let motion := match follow with
    | some (dx, dy) => ⟨prior.x - dx, prior.y - dy, 0, 0⟩
    | none => current
  if motion.valid world eaterMaxSpeed then { segment with motion } else segment

/-- The compact world puts the midpoint of its cavern layer three quarters down. -/
def eaterRetreatDepth (world : World) : Int := (world.height * 3 / 4 : Nat) * tileUnits

private def advanceWormChain (world : World) (water : Water) (player : Player)
    (tick : Nat) (retreat : Bool) (chain : Array WormSegment) : Option (Array WormSegment) := do
  let head ← chain[0]?
  let head := advanceWormHead world water player head tick retreat
  if !head.motion.valid world eaterMaxSpeed || (retreat && head.motion.y > eaterRetreatDepth world) then
    none
  else Id.run do
    let mut next := #[head]
    for index in [1:chain.size] do
      next := next.push (followWormSegment world chain[index]! next[index - 1]!)
    return some next

/-- Advance all heads independently. A retreat can be interrupted by returning
to the biome before the remaining chains dive past the cavern midpoint. -/
def advanceEater (world : World) (water : Water) (player : Player) (boss : EaterOfWorlds)
    (retreat : Bool) : Option EaterOfWorlds :=
  let chains := boss.chains.filterMap (advanceWormChain world water player boss.motionTick retreat)
  if chains.isEmpty then none
  else some { chains, retreating := retreat, motionTick := (boss.motionTick + 1) % eaterMotionCycle }

end Lean4Terria
