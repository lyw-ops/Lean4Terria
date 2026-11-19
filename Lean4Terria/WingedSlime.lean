import Lean4Terria.Magma

namespace Lean4Terria

def wingedSlimeHeightBoost : Int := 20 * (tileUnits / 16)
def wingedSlimeFlightTime : Nat := 80
def wingedSlimeMaxX : Int := 124
def wingedSlimeMaxY : Int := 161

structure WingedSlime where
  vx : Float32 := 0
  vy : Float32 := 0
  remainderX : Float32 := 0
  remainderY : Float32 := 0
  flight : Nat := 0
  jump : Nat := 0
  wetSlime : Nat := 0
  jumpHeld : Bool := false
  stompTicks : Nat := 21
  stompCount : Nat := 0
  frame : Nat := 0
  frameCounter : Float32 := 0
  wingFrame : Nat := 0
  wingCounter : Nat := 0
  deriving Repr, Inhabited

instance : BEq WingedSlime where
  beq a b :=
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.remainderX.toBits == b.remainderX.toBits && a.remainderY.toBits == b.remainderY.toBits &&
    a.flight == b.flight && a.jump == b.jump && a.wetSlime == b.wetSlime && a.jumpHeld == b.jumpHeld &&
    a.stompTicks == b.stompTicks && a.stompCount == b.stompCount && a.frame == b.frame &&
    a.frameCounter.toBits == b.frameCounter.toBits && a.wingFrame == b.wingFrame && a.wingCounter == b.wingCounter

private def pixelVelocity (value : Int) : Float32 := (Int32.ofInt value).toFloat32 / 16

def WingedSlime.start (player : Player) : WingedSlime :=
  { vx := max (-7.5) (min 7.5 (pixelVelocity player.vx))
    vy := max (-10) (min 10 (pixelVelocity player.vy)) }

def WingedSlime.valid (mount : WingedSlime) : Bool :=
  mount.vx.isFinite && mount.vx.abs ≤ 7.7 && mount.vy.isFinite && mount.vy.abs ≤ 10.01 &&
    mount.remainderX.isFinite && mount.remainderX.abs < 1 && mount.remainderY.isFinite && mount.remainderY.abs < 1 &&
    mount.flight ≤ wingedSlimeFlightTime && mount.jump ≤ 10 && mount.wetSlime ≤ 30 &&
    mount.stompTicks ≤ 21 && mount.stompCount ≤ 3 && mount.frame ≤ 5 &&
    mount.frameCounter.isFinite && mount.frameCounter ≥ 0 && mount.frameCounter ≤ 16 &&
    mount.wingFrame ≤ 3 && mount.wingCounter ≤ 7

/-- Terraria grants six collision-attack immunity ticks on the first two stomps
in a sequence. A gap of more than twenty frames starts a new sequence. -/
def WingedSlime.stomp (mount : WingedSlime) : WingedSlime × Bool :=
  let count := if mount.stompTicks > 20 then 1 else min 3 (mount.stompCount + 1)
  ({ mount with vy := -10, jump := 0, stompTicks := 0, stompCount := count }, count < 3)

private def WingedSlime.advanceFrame (mount : WingedSlime) (vx vy : Float32) : WingedSlime :=
  let wingCounter := if vy == 0 then 0 else mount.wingCounter + if mount.flight > 0 then 2 else 1
  let wingFrame := if vy == 0 then 3
    else if wingCounter > 7 then (mount.wingFrame + 1) % 4 else mount.wingFrame
  let wingCounter := if wingCounter > 7 then 0 else wingCounter
  let count := (if mount.frame < 5 then mount.frameCounter else 0) + vx.abs * 0.5
  let frame := if vy != 0 then 5 else if vx == 0 then 0
    else ((if mount.frame < 5 then mount.frame else 0) + if count > 16 then 1 else 0) % 5
  let frameCounter := if frame == 5 || vx == 0 then 0 else if count > 16 then count - 16 else count
  { mount with frame, frameCounter, wingFrame, wingCounter }

private def splitDelta (velocity remainder : Float32) : Int × Float32 :=
  let distance := velocity * 16 + remainder
  let delta := distance.toInt32
  (delta.toInt, distance - delta.toFloat32)

/-- Desktop 1.4.5.8 mount 50 runs a second vertical DryCollision when airborne.
Velocity is kept in original pixels per frame and each collision pass moves
the Lean body in its existing subpixel units. -/
def advanceWingedSlime (world : World) (water honey magma : Water) (player : Player)
    (mount : WingedSlime) (horizontal : Int) (holdJump down : Bool) (knockback : Option Int) :
    Player × WingedSlime := Id.run do
  let honeyWet := Magma.touches honey world player
  let wet := honeyWet || Magma.touches water world player || Magma.touches magma world player
  let dryGround := (moveBodyDelta world player 0 0 down wingedSlimeHeightBoost).grounded
  let horizontal := max (-1) (min 1 horizontal)
  let grounded := mount.vy == 0 && dryGround
  let slowdown : Float32 := if mount.vy == 0 then 0.2 else 0.1
  let speed : Float32 := if mount.frame == 5 then 7.5 else 5.5
  let direction : Float32 := (Int32.ofInt horizontal).toFloat32
  let forward := mount.vx * direction
  let vx := if let some side := knockback then pixelVelocity (side * 72)
    else if horizontal != 0 && forward < speed then
      let braking : Float32 := if forward < -0.2 then 0.2 else 0
      direction * (forward + braking + 0.2)
    else if mount.vx > 0 then max 0 (mount.vx - slowdown)
    else min 0 (mount.vx + slowdown)
  let gravity : Float32 := if honeyWet then 0.1 else if wet then 0.2 else 0.4
  let maxFall : Float32 := if honeyWet then 3.01 else if wet then 5.01 else 10.01
  let launched := holdJump && (grounded || (mount.wetSlime > 0 && !mount.jumpHeld))
  let flight := if launched && grounded || (grounded && !holdJump) then wingedSlimeFlightTime else mount.flight
  let jump := if launched then 10 else if holdJump then mount.jump - 1 else 0
  let jumping := holdJump && (launched || (mount.jump > 0 && mount.vy != 0))
  let flightUse := holdJump && !grounded && jump == 0
  let vy := if knockback.isSome then -4
    else if flightUse && flight > 0 then
      if down then
        let damped := mount.vy * 0.9
        if -1 < damped && damped < 0.5 then 0.00001 else damped
      else max (-5.4375) (mount.vy - if mount.vy > 0 then 0.5 else 0.1)
    else if flightUse then
      min (if down then maxFall else maxFall / 3) ((if jumping then -7.25 else mount.vy) + gravity / 3)
    else min maxFall ((if jumping then -7.25 else mount.vy) + gravity)
  let flight := if flightUse && knockback.isNone then flight - 1 else flight
  let vy := if wet then max (-4) ((if vy > 2 then vy * 0.9 else vy) - 0.5) else vy
  let drag : Float32 := if wet then 0.5 else 1
  let (dx, remainderX) := splitDelta (vx * drag) mount.remainderX
  let (dy, remainderY) := splitDelta (vy * drag) mount.remainderY
  let first := moveBodyDelta world player dx dy down wingedSlimeHeightBoost
  let secondDryPass := !wet && (mount.wetSlime == 0 || holdJump) && first.vy != 0
  let (dy2, remainderY) := if secondDryPass then splitDelta vy remainderY else (0, remainderY)
  let second := if secondDryPass then moveBodyDelta world first 0 dy2 down wingedSlimeHeightBoost else first
  let hitX := first.x - player.x != dx
  let landed := second.grounded || (dy > 0 && first.y - player.y != dy) ||
    (dy2 > 0 && second.y - first.y != dy2)
  let hitCeiling := (dy < 0 && first.y - player.y != dy) || (dy2 < 0 && second.y - first.y != dy2)
  let vx := if hitX then 0 else vx
  let vy := if landed || hitCeiling then 0 else vy
  let mount := { mount with
    vx, vy, flight, jumpHeld := holdJump
    remainderX := if hitX then 0 else remainderX
    remainderY := if landed || hitCeiling then 0 else remainderY
    jump := if landed || hitCeiling then 0 else jump
    wetSlime := if wet then 30 else mount.wetSlime - 1
    stompTicks := min 21 (mount.stompTicks + 1) }
  let player := { second with vx := (vx * 16).toInt32.toInt, vy := (vy * 16).toInt32.toInt }
  return (player, mount.advanceFrame vx vy)

end Lean4Terria
