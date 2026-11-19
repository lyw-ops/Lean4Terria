import Lean4Terria.Ranged
import Lean4Terria.DarkCaster
import Lean4Terria.UnderworldCreatures
import Lean4Terria.UnderworldShots

namespace Lean4Terria.CultistRitual

def initialDelay : Nat := 86400
def respawnDelay : Nat := 43200

structure Clock where
  delay : Int := initialDelay
  recheck : Nat := 0
  deriving BEq, Repr, Inhabited

def Clock.cultistSlain (clock : Clock) : Clock := { clock with delay := clock.delay - 3600 }

def Clock.tabletDestroyed (clock : Clock) : Clock := { clock with delay := respawnDelay }

/-- The returned flag requests a spawn attempt this frame. The native clock
checks for bosses and events before looking at the dungeon or its entrance. -/
def Clock.advance (clock : Clock) (dayRate : Nat) (danger : Bool) : Clock × Bool :=
  let delay := max 0 (clock.delay - dayRate)
  let recheck := clock.recheck - dayRate
  if delay != 0 || recheck != 0 then (⟨delay, recheck⟩, false)
  else (⟨0, if danger then 3600 else 600⟩, !danger)

structure Progress where
  hardmode : Bool := false
  golemDefeated : Bool := false
  skeletronDefeated : Bool := false
  tabletExists : Bool := false
  deriving BEq, Repr, Inhabited

structure PlayerBounds where
  x : Float32
  y : Float32
  width : Nat
  height : Nat
  deriving Repr

/-- WorldGen.PlayerLOS uses the spawn viewport rather than tile visibility. -/
def PlayerBounds.seesSpawn (player : PlayerBounds) (position : TilePos) : Bool :=
  let left := (player.x.toFloat + player.width.toFloat / 2 - 1152).toInt32.toInt
  let top := (player.y.toFloat + player.height.toFloat / 2 - 720).toInt32.toInt
  let view : HitBox := ⟨left, top, left + 2304, top + 1440⟩
  let tile : HitBox := ⟨position.x * 16, position.y * 16, (position.x + 1) * 16, (position.y + 1) * 16⟩
  view.intersects tile

private def solid (world : World) (x y : Int) : Bool := (world.tile? ⟨x, y⟩).any Tile.solid

private def hasStandingRoom (world : World) (x y : Int) : Bool :=
  (!solid world x (y - 3) && !solid world x (y - 2) && !solid world x (y - 1)) &&
    [-1, 1].all (fun dx => !solid world (x + dx) (y - 3) && !solid world (x + dx) (y - 2))

private def floorPoint (world : World) (center : TilePos) (offset : Int) : Option TilePos :=
  (List.range 17).findSome? fun (row : Nat) => do
    let p : TilePos := ⟨center.x + offset, center.y - 5 + (row : Int)⟩
    let tile ← world.tile? p
    if (tile.solid || tile == .platform) && hasStandingRoom world p.x p.y then some p else none

/-- Ritual followers occupy the first valid floor in each column, searched
from five tiles above the tablet's center through eleven tiles below it. -/
def checkFloor (world : World) (center : TilePos) : Option (Array TilePos) :=
  #[-10, -6, 6, 10].mapM (floorPoint world center)

structure Spawn where
  tabletX : Int
  tabletBottomY : Int
  followerFloors : Array TilePos
  deriving BEq, Repr

def trySpawning (world : World) (entrance : TilePos) (clock : Clock) (progress : Progress)
    (players : Array PlayerBounds) (force := false) : Option Spawn := do
  if !world.contains entrance || entrance.y < 7 then return ← none
  if !force && (clock.delay != 0 || !progress.hardmode || !progress.golemDefeated ||
      !progress.skeletronDefeated || progress.tabletExists ||
      players.any (fun p => p.seesSpawn ⟨entrance.x - 6, entrance.y⟩ ||
        p.seesSpawn ⟨entrance.x + 6, entrance.y⟩)) then return ← none
  let center : TilePos := ⟨entrance.x, entrance.y - 7⟩
  if (world.tile center).solid then return ← none
  let followerFloors ← checkFloor world center
  return ⟨entrance.x * 16 + 8, (entrance.y - 4) * 16 - 8, followerFloors⟩

private def px (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def tile (n : Float32) : Int := n.toInt32.toInt.tdiv 16
private def sign (n : Float32) : Int := if n < 0 then -1 else if n > 0 then 1 else 0

inductive FollowerKind where
  | archer | devotee
  deriving BEq, Repr, Inhabited

def FollowerKind.isArcher (kind : FollowerKind) : Bool := kind == .archer
def FollowerKind.npcId (kind : FollowerKind) : Nat := if kind.isArcher then 379 else 438
def FollowerKind.width (kind : FollowerKind) : Nat := if kind.isArcher then 18 else 22
def FollowerKind.health (kind : FollowerKind) : Nat := if kind.isArcher then 210 else 400
def FollowerKind.defense (kind : FollowerKind) : Nat := if kind.isArcher then 14 else 0
def FollowerKind.name (kind : FollowerKind) : String :=
  if kind.isArcher then "Cultist Archer" else "Lunatic Devotee"

structure Follower where
  id : Nat
  kind : FollowerKind
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  previousX : Float32 := 0
  health : Nat := kind.health
  direction : Int := 1
  directionY : Int := -1
  spriteDirection : Int := -1
  idleClock : Nat := 0
  shotClock : Nat := 0
  aim : Nat := 0
  stuck : Nat := 0
  turnClock : Nat := 0
  frame : Nat := 0
  frameClock : Float := 0
  wet : Bool := false
  justHit : Bool := false
  deriving Repr, Inhabited

def Follower.centerX (follower : Follower) : Float32 := follower.x + px (follower.kind.width / 2)
def Follower.centerY (follower : Follower) : Float32 := follower.y + 20
def Follower.hitBox (follower : Follower) : HitBox :=
  CasterAI.npcBox follower.x follower.y follower.kind.width 40

def Follower.atFloor (id : Nat) (kind : FollowerKind) (floor : TilePos) : Follower :=
  { id, kind, x := px (floor.x * 16 + 8 - (kind.width / 2 : Nat)), y := px (floor.y * 16 - 48 - 40) }

def Follower.struck (follower : Follower) (damage : Nat) (direction : Int)
    (force : Float32) : Option Follower := do
  let health := follower.health - damage
  if health == 0 then return ← none
  let (vx, vy) := if follower.kind.isArcher then
      npcKnockbackVelocity follower.vx follower.vy force 0.55 0.75 damage follower.kind.health direction false
    else (follower.vx, follower.vy)
  return { follower with health, vx, vy, justHit := true }

structure Tablet where
  x : Int
  bottomY : Int
  frameClock : Nat := 0
  vanishing : Nat := 0
  deriving Repr, Inhabited

def Tablet.centerY (tablet : Tablet) : Int := tablet.bottomY - 27
def Tablet.frame (tablet : Tablet) : Nat := tablet.frameClock / 5

inductive RitualMood where
  | passive | provoked | hostile
  deriving BEq, Repr, Inhabited

structure Gathering where
  tablet : Tablet
  followers : Array Follower
  mood : RitualMood := .passive
  deriving Repr, Inhabited

def Gathering.spawn (placement : Spawn) : Gathering :=
  { tablet := ⟨placement.tabletX, placement.tabletBottomY, 0, 0⟩
    followers := placement.followerFloors.mapIdx fun i floor =>
      Follower.atFloor (i + 1) (if i == 1 || i == 2 then .devotee else .archer) floor }

def Gathering.struck (gathering : Gathering) (index damage : Nat) (direction : Int)
    (force : Float32) : Gathering :=
  let follower := gathering.followers[index]!
  let followers := match follower.struck damage direction force with
    | some hurt => gathering.followers.set! index hurt
    | none => gathering.followers.eraseIdxIfInBounds index
  { gathering with followers, mood := if gathering.mood == .passive then .provoked else gathering.mood }

structure Arrow where
  x : Float32
  y : Float32
  vx : Float32
  vy : Float32
  age : Nat := 0
  slot : Option Nat := none
  deriving Repr, Inhabited

def Arrow.hitBox (arrow : Arrow) : HitBox := CasterAI.npcBox (arrow.x - 5) (arrow.y - 5) 10 10

def Arrow.rotation (arrow : Arrow) : Float :=
  ((Float.atan2 arrow.vy.toFloat arrow.vx.toFloat).toFloat32 + (1.5707964 : Float32)).toFloat

def Arrow.advance (arrow : Arrow) (world : World) (water magma honey : Water) : Option Arrow := do
  let age := arrow.age + 1
  let vy := min 16 (arrow.vy + if age ≥ 20 then 0.07 else 0)
  let (dx, dy) := clipShotMotion world arrow.x arrow.y arrow.vx vy 5 true
  if dx != arrow.vx || dy != vy then return ← none
  let wet := shotLiquidContact world water arrow.x arrow.y 5 2.5 ||
    shotLiquidContact world magma arrow.x arrow.y 5 2.5 ||
    shotLiquidContact world honey arrow.x arrow.y 5 2.5
  let scale : Float32 := if wet then 0.5 else 1
  let x := arrow.x + scale * dx
  let y := arrow.y + scale * dy
  if x < -16 || y < -16 || x ≥ px (16 * world.width + 16) || y ≥ px (16 * world.height + 16) then
    return ← none
  return { arrow with x, y, vy, age }

private def facePlayer (follower : Follower) (player : Player) (dead : Bool) : Follower :=
  if dead then follower else
    let x := px (player.x.tdiv 16 + playerWidth / 32)
    let y := px (player.y.tdiv 16 + playerHeight / 32)
    { follower with
      direction := if x < follower.centerX then -1 else 1
      directionY := if y ≤ follower.y + 40 then -1 else 1 }

private def faceTablet (follower : Follower) (tablet : Tablet) : Follower :=
  let direction := sign (px tablet.x - follower.centerX)
  let vx := follower.vx * 0.93
  let vx := if direction != follower.direction || vx.abs.toFloat < 0.1 then 0 else vx
  { follower with
    vx, direction, directionY := -1
    spriteDirection := if follower.kind.isArcher then follower.spriteDirection else direction
    idleClock := (follower.idleClock + 1) % 300 }

private def bowVector (follower : Follower) (player : Player) (jitter : Nat → Int) : Float32 × Float32 :=
  let dx := px (player.x.tdiv 16 + playerWidth / 32) - follower.centerX
  let dy := px (player.y.tdiv 16 + playerHeight / 32) - follower.centerY - dx.abs * 0.1
  (dx + px (jitter 0), dy + px (jitter 1))

private def aimPose (vx vy : Float32) : Nat :=
  if vy.abs > vx.abs * 2 then (if vy > 0 then 1 else 5)
  else if vx.abs > vy.abs * 2 then 3
  else if vy > 0 then 2 else 4

/-- The bow branch runs after target bookkeeping and before the obstacle jumps. -/
private def Follower.bow (follower : Follower) (world : World) (player : Player) (dead : Bool)
    (jitter : Nat → Int) : Follower × Option Arrow := Id.run do
  let mut next := if follower.justHit then { follower with shotClock := 30, aim := 0 }
    else { follower with shotClock := follower.shotClock - 1 }
  let mut arrow := none
  if next.aim != 0 then
    next := facePlayer next player dead
    if next.shotClock == 40 then
      let (dx, dy) := bowVector next player jitter
      let size := ((dx * dx + dy * dy).toFloat.sqrt).toFloat32
      if size > 0 then
        let scale := 11 / size
        let (vx, vy) := (dx * scale, dy * scale)
        arrow := some { x := next.centerX + vx, y := next.centerY + vy, vx, vy }
        next := { next with aim := aimPose vx vy }
    if next.vy != 0 || next.shotClock == 0 then next := { next with aim := 0, shotClock := 0 }
    else next := { next with vx := next.vx * 0.9, spriteDirection := next.direction }
  if next.aim == 0 && next.vy == 0 && next.shotClock == 0 && !dead &&
      npcCanSee world next.x next.y next.kind.width 40 player then
    let (dx, dy) := bowVector next player jitter
    let size := ((dx * dx + dy * dy).toFloat.sqrt).toFloat32
    if size < 700 then
      next := { next with vx := next.vx * 0.5, shotClock := 80, aim := aimPose dx dy }
  if next.aim == 0 then
    let vx := if next.vx.abs > 1 then (if next.vy == 0 then next.vx * 0.8 else next.vx)
      else max (-1) (min 1 (next.vx + 0.07 * px next.direction))
    next := { next with vx }
  return (next, arrow)

private def supported (world : World) (x y : Int) : Bool :=
  solid world x y || world.tile ⟨x, y⟩ == .platform

private def Follower.jump (follower : Follower) (world : World) : Follower := Id.run do
  let left := tile (follower.x + 8)
  let right := tile (follower.x + 10)
  let bottom := follower.y + 40
  let foot := tile (bottom + 7)
  let roof := tile (follower.y - 9)
  let columns := List.range' left.toNat (right - left + 1).toNat
  let onFloor := follower.vy == 0 && columns.any (fun x => supported world x foot) &&
    !columns.any (fun x => solid world x roof)
  let mut next := follower
  let ahead := follower.x + follower.vx
  let side := sign follower.vx
  let col := tile (ahead + px (9 + 10 * side))
  let row := tile (bottom - 1)
  let step := px (row * 16)
  if follower.vy ≥ 0 && world.contains ⟨col, row⟩ && step < bottom && bottom - step ≤ 16.1 &&
      px (col * 16) < ahead + 18 && ahead < px ((col + 1) * 16) && solid world col row &&
      ![1, 2, 3].any (fun depth => solid world col (row - depth)) && !supported world (col - side) (row - 3) then
    next := { next with y := step - 40 }
  if !onFloor || next.vx * px next.spriteDirection ≤ 0 then return next
  let col := tile (next.centerX + px (15 * next.direction))
  let row := tile (next.y + 25)
  if solid world col (row - 2) then return { next with vy := if solid world col (row - 3) then -8 else -7 }
  if solid world col (row - 1) then return { next with vy := -6 }
  if next.y + 40 - px (row * 16) > 20 && solid world col row then return { next with vy := -5 }
  if next.directionY < 0 && !supported world col (row + 1) && !supported world (col + next.direction) (row + 1) then
    return { next with vy := -8, vx := next.vx * 1.5 }
  return next

private def Follower.chase (follower : Follower) (player : Player) (dead : Bool) : Follower :=
  if follower.aim != 0 then follower else
    let opposite := follower.vy == 0 && follower.vx * px follower.direction < 0
    let stuck := if follower.x == follower.previousX || follower.stuck ≥ 60 || opposite then follower.stuck + 1
      else if follower.vx.abs.toFloat > 0.9 then follower.stuck - 1 else follower.stuck
    let stuck := if stuck > 600 || follower.justHit || follower.hitBox.intersects (HitBox.ofNpcTarget player)
      then 0 else stuck
    let next := { follower with stuck }
    if stuck < 60 then facePlayer next player dead
    else if follower.vx != 0 then { next with turnClock := 0 }
    else if follower.vy != 0 then next
    else if follower.turnClock + 1 < 2 then { next with turnClock := follower.turnClock + 1 }
    else { next with turnClock := 0, direction := -next.direction, spriteDirection := -next.spriteDirection }

private def Follower.animate (follower : Follower) (hostile : Bool) : Follower :=
  if !follower.kind.isArcher then
    let clock := follower.frameClock + if hostile then 2 else 1
    let frameClock := if clock ≥ 49 then 0 else clock
    let step := frameClock.toUInt64.toNat / 7
    { follower with frameClock, frame := if step ≥ 4 then 6 - step else step }
  else if follower.vy != 0 then { follower with frame := 1, frameClock := 0 }
  else
    let next := { follower with spriteDirection := if follower.direction == 0 then follower.spriteDirection else follower.direction }
    if next.aim > 0 then { next with frame := next.aim + 1, frameClock := 0 }
    else if next.vx == 0 then { next with frame := 0 }
    else
      let clock : Float := next.frameClock + (next.vx.abs * (2 : Float32)).toFloat
      let frame := max (7 : Nat) next.frame
      if clock > 6 then { next with frame := if frame ≥ 19 then 7 else frame + 1, frameClock := 0 }
      else { next with frame := if frame ≥ 20 then 7 else frame, frameClock := clock }

def Follower.advance (follower : Follower) (world : World) (water magma : Water) (tablet : Tablet)
    (hostile : Bool) (player : Player) (dead : Bool) (surface : Nat) (jitter : Nat → Int) : Follower × Option Arrow :=
  let (next, arrow) := if follower.kind.isArcher && hostile then
      let (archer, arrow) := (follower.chase player dead).bow world player dead jitter
      (archer.jump world, arrow)
    else (faceTablet follower tablet, none)
  let size := px world.width / 4200
  let altitude := ((next.y / 16 - (60 + 10 * (size * size))).toFloat / (surface.toFloat / 6)).toFloat32
  let gravity := if next.wet then 0.2 else 0.3 * max 0.25 (min 1 altitude)
  let vx := if next.vx.abs.toFloat < 0.005 then 0 else next.vx
  let vy := min (if next.wet then 7 else 10) (next.vy + gravity)
  let moved := moveNpc world water magma next.x next.y vx vy next.kind.width 40 next.wet (next.directionY != 1)
  let next := { next with
    x := moved.x, y := moved.y, vx := moved.vx, vy := moved.vy
    previousX := next.x, wet := moved.wet, justHit := false }
  (next.animate hostile, arrow)

structure Summon where
  x : Int
  bottomY : Int
  direction : Int
  deriving BEq, Repr

/-- Once the tablet has no followers, vanilla picks one side, then searches
for a solid floor with a clear 3 × 3 area. It uses the last row as a fallback. -/
def Tablet.summon (tablet : Tablet) (world : World) (roll : Nat) : Summon :=
  let centerY := tablet.centerY.tdiv 16
  let side : Int := if roll % 2 == 0 then 1 else -1
  let col := tablet.x.tdiv 16 + 11 * side
  let floor := (List.range 17).findSome? fun (i : Nat) =>
    let row := centerY - 5 + (i : Int)
    if solid world col row && [-1, 0, 1].all (fun dx =>
        [1, 2, 3].all (fun dy => !solid world (col + dx) (row - dy))) then some row else none
  let x := col * 16 + 10
  ⟨x, floor.getD (centerY + 11) * 16 - 2, sign (px (tablet.x - x))⟩

structure Frame where
  gathering : Option Gathering
  arrows : Array Arrow := #[]
  summon : Option Summon := none
  deriving Repr

def Gathering.advance (gathering : Gathering) (world : World) (water magma : Water) (player : Player)
    (dead : Bool) (surface summonRoll : Nat) (jitter : Nat → Nat → Int) : Frame := Id.run do
  let tablet := { gathering.tablet with frameClock := (gathering.tablet.frameClock + 1) % 20 }
  if gathering.followers.isEmpty then
    let vanish := tablet.vanishing + 1
    return {
      gathering := if vanish > 300 then none else some { gathering with tablet := { tablet with vanishing := vanish } }
      summon := if vanish == 1 then some (tablet.summon world summonRoll) else none }
  let mood := if gathering.mood == .provoked then .hostile else gathering.mood
  let mut followers := #[]
  let mut arrows := #[]
  for follower in gathering.followers do
    let (next, arrow) := follower.advance world water magma tablet (mood == .hostile) player dead surface (jitter follower.id)
    followers := followers.push next
    if let some arrow := arrow then arrows := arrows.push arrow
  return { gathering := some { tablet, followers, mood }, arrows }

end Lean4Terria.CultistRitual
