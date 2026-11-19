import Lean4Terria.World

namespace Lean4Terria

def tileUnits : Int := 256
def playerWidth : Int := 180
def playerHeight : Int := 352
def maxHorizontalSpeed : Int := 30
def maxVerticalSpeed : Int := 120

structure Player where
  x : Int
  y : Int
  vx : Int
  vy : Int
  grounded : Bool
  deriving BEq, Repr, Inhabited

def Player.centerTile (player : Player) : TilePos :=
  { x := (player.x + playerWidth / 2) / tileUnits
    y := (player.y + playerHeight / 2) / tileUnits }

def Player.overlapsTile (player : Player) (pos : TilePos) (heightBoost : Int := 0) : Bool :=
  let left := pos.x * tileUnits
  let top := pos.y * tileUnits
  player.x < left + tileUnits && left < player.x + playerWidth &&
    player.y - heightBoost < top + tileUnits && top < player.y + playerHeight

private def blockedColumn (world : World) (player : Player) (col heightBoost : Int) : Bool := Id.run do
  let top := (player.y - heightBoost) / tileUnits
  let bottom := (player.y + playerHeight - 1) / tileUnits
  for offset in [:(bottom - top + 1).toNat] do
    if (world.tile { x := col, y := top + offset }).solid then
      return true
  return false

private def blockedRow (world : World) (player : Player) (row : Int)
    (includePlatforms := false) : Bool := Id.run do
  let left := player.x / tileUnits
  let right := (player.x + playerWidth - 1) / tileUnits
  for offset in [:(right - left + 1).toNat] do
    let tile := world.tile { x := left + offset, y := row }
    if tile.solid || (includePlatforms && tile == .platform) then
      return true
  return false

def Player.collides (player : Player) (world : World) (heightBoost : Int := 0) : Bool := Id.run do
  let top := (player.y - heightBoost) / tileUnits
  let bottom := (player.y + playerHeight - 1) / tileUnits
  for offset in [:(bottom - top + 1).toNat] do
    if blockedRow world player (top + offset) then
      return true
  return false

private def supported (world : World) (player : Player) (includePlatforms : Bool) : Bool :=
  let bottom := player.y + playerHeight
  bottom % tileUnits == 0 && blockedRow world player (bottom / tileUnits) includePlatforms

private def moveX (world : World) (player : Player) (dx : Int) (heightBoost : Int := 0) : Player :=
  let nextX := player.x + dx
  if dx > 0 then
    let col := (nextX + playerWidth - 1) / tileUnits
    if blockedColumn world player col heightBoost then
      { player with x := col * tileUnits - playerWidth, vx := 0 }
    else
      { player with x := nextX, vx := dx }
  else if dx < 0 then
    let col := nextX / tileUnits
    if blockedColumn world player col heightBoost then
      { player with x := (col + 1) * tileUnits, vx := 0 }
    else
      { player with x := nextX, vx := dx }
  else
    { player with vx := 0 }

private def moveY (world : World) (player : Player) (dy : Int) (dropThrough : Bool)
    (heightBoost : Int := 0) : Player :=
  let nextY := player.y + dy
  if dy > 0 then
    let row := (nextY + playerHeight - 1) / tileUnits
    -- Only a foot crossing from above can land on a platform. This also lets
    -- a body continue through a platform after releasing the drop control.
    let landOnPlatform := !dropThrough && player.y + playerHeight ≤ row * tileUnits
    if blockedRow world player row landOnPlatform then
      { player with y := row * tileUnits - playerHeight, vy := 0, grounded := true }
    else
      let player := { player with y := nextY }
      let grounded := supported world player (!dropThrough)
      { player with vy := if grounded then 0 else dy, grounded }
  else if dy < 0 then
    let row := (nextY - heightBoost) / tileUnits
    if blockedRow world player row then
      { player with y := (row + 1) * tileUnits + heightBoost, vy := 0, grounded := false }
    else
      { player with y := nextY, vy := dy, grounded := false }
  else
    { player with vy := 0, grounded := supported world player (!dropThrough) }

/-- Move by a requested subpixel displacement without applying gravity. Each
axis is limited to less than one tile so the leading edge cannot skip a wall. -/
def moveBodyDelta (world : World) (player : Player) (dx dy : Int) (dropThrough := false)
    (heightBoost : Int := 0) : Player :=
  let limit := tileUnits - 1
  let clamp (distance : Int) := max (-limit) (min distance limit)
  moveY world (moveX world player (clamp dx) heightBoost) (clamp dy) dropThrough heightBoost

private def moveBodyCore (world : World) (player : Player) (dx : Int) (jump dropThrough : Bool) :
    Player :=
  let dy := if jump && supported world player (!dropThrough) then -82 else player.vy + 5
  moveY world (moveX world player dx) (max (-maxVerticalSpeed) (min dy maxVerticalSpeed)) dropThrough

def moveBody (world : World) (player : Player) (dx : Int) (jump : Bool) : Player :=
  moveBodyCore world player dx jump false

def moveEquippedPlayer (world : World) (player : Player) (horizontal : Int) (jump : Bool)
    (speed : Int) (dropThrough := false) : Player :=
  moveBodyCore world player (max 0 speed * max (-1) (min horizontal 1)) jump dropThrough

def movePlayer (world : World) (player : Player) (horizontal : Int) (jump : Bool) : Player :=
  moveEquippedPlayer world player horizontal jump maxHorizontalSpeed

/-- Sweep the leading edge in at most two movements when a dash exceeds one tile. -/
def moveDashingPlayer (world : World) (player : Player) (dx : Int) (jump dropThrough : Bool) : Player :=
  let limit := tileUnits - 1
  let first := max (-limit) (min dx limit)
  let started := moveX world player first
  let moved := if dx == first || started.vx == 0 then started
    else moveX world started (dx - first)
  let dy := if jump && supported world player (!dropThrough) then -82 else player.vy + 5
  moveY world { moved with vx := if moved.vx == 0 then 0 else dx }
    (max (-maxVerticalSpeed) (min dy maxVerticalSpeed)) dropThrough

/-- Swim with drag and an upward stroke while the swim control is held. -/
def moveSwimmingPlayer (world : World) (player : Player) (horizontal : Int) (up : Bool)
    (honey := false) : Player :=
  let dx := (if honey then 8 else 17) * max (-1) (min horizontal 1)
  let dy := if up then max (if honey then -29 else -58) (min 20 player.vy - 14)
    else min (if honey then 19 else 38) (max (-52) player.vy + 3)
  moveY world (moveX world player dx) dy true

def generateWorld (seed : UInt64) : World × Player :=
  let player : Player :=
    { x := 19 * tileUnits + (tileUnits - playerWidth) / 2
      y := 40 * tileUnits - playerHeight
      vx := 0, vy := 0, grounded := true }
  (generateTerrain seed, player)

end Lean4Terria
