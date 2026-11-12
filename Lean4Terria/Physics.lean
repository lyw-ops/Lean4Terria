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

def Player.overlapsTile (player : Player) (pos : TilePos) : Bool :=
  let left := pos.x * tileUnits
  let top := pos.y * tileUnits
  player.x < left + tileUnits && left < player.x + playerWidth &&
    player.y < top + tileUnits && top < player.y + playerHeight

private def blockedColumn (world : World) (player : Player) (col : Int) : Bool := Id.run do
  let top := player.y / tileUnits
  let bottom := (player.y + playerHeight - 1) / tileUnits
  for offset in [:(bottom - top + 1).toNat] do
    if (world.tile { x := col, y := top + offset }).solid then
      return true
  return false

private def blockedRow (world : World) (player : Player) (row : Int) : Bool := Id.run do
  let left := player.x / tileUnits
  let right := (player.x + playerWidth - 1) / tileUnits
  for offset in [:(right - left + 1).toNat] do
    if (world.tile { x := left + offset, y := row }).solid then
      return true
  return false

def Player.collides (player : Player) (world : World) : Bool := Id.run do
  let top := player.y / tileUnits
  let bottom := (player.y + playerHeight - 1) / tileUnits
  for offset in [:(bottom - top + 1).toNat] do
    if blockedRow world player (top + offset) then
      return true
  return false

private def supported (world : World) (player : Player) : Bool :=
  let bottom := player.y + playerHeight
  bottom % tileUnits == 0 && blockedRow world player (bottom / tileUnits)

private def moveX (world : World) (player : Player) (dx : Int) : Player :=
  let nextX := player.x + dx
  if dx > 0 then
    let col := (nextX + playerWidth - 1) / tileUnits
    if blockedColumn world player col then
      { player with x := col * tileUnits - playerWidth, vx := 0 }
    else
      { player with x := nextX, vx := dx }
  else if dx < 0 then
    let col := nextX / tileUnits
    if blockedColumn world player col then
      { player with x := (col + 1) * tileUnits, vx := 0 }
    else
      { player with x := nextX, vx := dx }
  else
    { player with vx := 0 }

private def moveY (world : World) (player : Player) (dy : Int) : Player :=
  let nextY := player.y + dy
  if dy > 0 then
    let row := (nextY + playerHeight - 1) / tileUnits
    if blockedRow world player row then
      { player with y := row * tileUnits - playerHeight, vy := 0, grounded := true }
    else
      let player := { player with y := nextY }
      let grounded := supported world player
      { player with vy := if grounded then 0 else dy, grounded }
  else if dy < 0 then
    let row := nextY / tileUnits
    if blockedRow world player row then
      { player with y := (row + 1) * tileUnits, vy := 0, grounded := false }
    else
      { player with y := nextY, vy := dy, grounded := false }
  else
    { player with vy := 0, grounded := supported world player }

/-- Advance one fixed tick. Each axis moves by less than one tile, so testing its
leading edge catches every tile boundary crossed on that axis. -/
def movePlayer (world : World) (player : Player) (horizontal : Int) (jump : Bool) : Player :=
  let dx := maxHorizontalSpeed * max (-1) (min horizontal 1)
  let dy := if jump && supported world player then -82 else player.vy + 5
  let player := moveX world player dx
  moveY world player (max (-maxVerticalSpeed) (min dy maxVerticalSpeed))

def generateWorld (seed : UInt64) : World × Player :=
  let player : Player :=
    { x := 19 * tileUnits + (tileUnits - playerWidth) / 2
      y := 40 * tileUnits - playerHeight
      vx := 0, vy := 0, grounded := true }
  (generateTerrain seed, player)

end Lean4Terria
