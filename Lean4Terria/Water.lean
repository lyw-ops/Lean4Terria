import Lean4Terria.Physics

namespace Lean4Terria

def maxBreath : Nat := 480

structure Water where
  levels : ByteArray
  deriving BEq, Inhabited

namespace Water

def capacity : Nat := 255

def fromEighths (amount : Nat) : Nat :=
  (min amount 8 * capacity + 7) / 8

def empty (world : World) : Water :=
  { levels := ByteArray.mk (Array.replicate (world.width * world.height) 0) }

private def indexAt (world : World) (pos : TilePos) : Nat := pos.y.toNat * world.width + pos.x.toNat

def level (water : Water) (world : World) (position : TilePos) : Nat :=
  if world.contains position then (water.levels[indexAt world position]?.getD 0).toNat else 0

def setLevel (water : Water) (world : World) (position : TilePos) (fill : Nat) : Water :=
  if !world.contains position then water
  else
    let levels := if water.levels.isEmpty then (empty world).levels else water.levels
    { levels := levels.set! (indexAt world position) (min capacity fill).toUInt8 }

def liquidBarrier (first second : Water) : ByteArray := Id.run do
  if first.levels.isEmpty then return second.levels
  if second.levels.isEmpty then return first.levels
  let mut levels := ByteArray.emptyWithCapacity first.levels.size
  for index in [:first.levels.size] do
    levels := levels.push (first.levels[index]! ||| (second.levels[index]?.getD 0))
  return levels

private def initialFrom (world : World) (fillAt : TilePos → Nat) : Water := Id.run do
  let mut levels := ByteArray.emptyWithCapacity (world.width * world.height)
  for y in [:world.height] do
    for x in [:world.width] do
      let pos : TilePos := ⟨x, y⟩
      let fill := fillAt pos
      levels := levels.push (if fill > 0 && !(world.tile pos).solid then (fromEighths fill).toUInt8 else 0)
  return { levels }

def initial (world : World) (seed := defaultSeed) : Water :=
  initialFrom world (initialWaterLevel seed)

def initialLegacy (world : World) : Water :=
  initialFrom world initialPreFishingWaterLevel

private def permeable (world : World) (barrier : ByteArray) (index : Nat) : Bool :=
  barrier[index]?.getD 0 == 0 && (world.tiles[index]?.bind Tile.ofCode?).any (fun tile => !tile.solid)

private def fallsFrom (levels barrier : ByteArray) (world : World) (index : Nat) : Bool :=
  let below := index + world.width
  below < levels.size && levels[below]!.toNat < capacity && permeable world barrier below

private def lowerBeyond (levels barrier : ByteArray) (world : World) (high low level : Nat) : Bool :=
  let column := low % world.width
  let beyond := if high < low then
    if column + 1 < world.width then some (low + 1) else none
  else if column > 0 then some (low - 1) else none
  beyond.any fun index => levels[index]!.toNat < level && permeable world barrier index

private def balance (levels pressure barrier : ByteArray) (world : World) (left right : Nat) : ByteArray :=
  let leftLevel := pressure[left]!.toNat
  let rightLevel := pressure[right]!.toNat
  if leftLevel == rightLevel || !permeable world barrier left || !permeable world barrier right then levels
  else
    let (high, low, highLevel, lowLevel) :=
      if leftLevel > rightLevel then (left, right, leftLevel, rightLevel)
      else (right, left, rightLevel, leftLevel)
    let difference := highLevel - lowLevel
    let flow := if difference > 1 then difference / 2
      -- Keep a one-unit gradient moving toward a lower level or an open drop.
      else if lowerBeyond pressure barrier world high low lowLevel ||
        (fallsFrom pressure barrier world low && !fallsFrom pressure barrier world high) then 1 else 0
    if flow == 0 then levels
    else levels.set! high (highLevel - flow).toUInt8 |>.set! low (lowLevel + flow).toUInt8

/-- Transfer at most one cell downward and one sideways per step. Alternating the
paired columns gives each horizontal edge an equal turn without a scan direction. -/
def advance (water : Water) (world : World) (reverse : Bool) (barrier : ByteArray := .empty) :
    Water := Id.run do
  if water.levels.size != world.width * world.height then return water
  let mut levels := water.levels
  for row in [:world.height - 1] do
    let start := (world.height - 2 - row) * world.width
    for col in [:world.width] do
      let index := start + col
      let fill := levels[index]!.toNat
      if fill > 0 && permeable world barrier index && fallsFrom levels barrier world index then
        let below := index + world.width
        let lower := levels[below]!.toNat
        let flow := min fill (capacity - lower)
        levels := levels.set! index (fill - flow).toUInt8 |>.set! below (lower + flow).toUInt8
  let parity : Nat := if reverse then 1 else 0
  let pairs := (world.width - parity) / 2
  -- Every pair samples the same post-gravity levels, independent of scan order.
  let pressure := levels
  for row in [:world.height] do
    let start := row * world.width + parity
    for pair in [:pairs] do
      let left := start + 2 * pair
      levels := balance levels pressure barrier world left (left + 1)
  return { levels }

/-- Move all water out of a cell before accepting an edit. The supplied world
already contains the proposed construction, so newly solid neighbours are blocked. -/
def displace (water : Water) (world : World) (position : TilePos)
    (barrier : ByteArray := .empty) : Option Water := Id.run do
  if !world.contains position then return none
  let mut remaining := water.level world position
  if remaining == 0 then return some water
  let mut levels := water.levels
  let adjacent : List TilePos :=
    [⟨position.x, position.y + 1⟩, ⟨position.x - 1, position.y⟩,
      ⟨position.x + 1, position.y⟩, ⟨position.x, position.y - 1⟩]
  for neighbor in adjacent do
    if remaining == 0 then break
    let index := indexAt world neighbor
    if world.contains neighbor && index < levels.size && permeable world barrier index then
      let fill := levels[index]!.toNat
      let flow := min remaining (capacity - fill)
      if flow > 0 then
        levels := levels.set! index (fill + flow).toUInt8
        remaining := remaining - flow
  if remaining != 0 then return none
  return some { levels := levels.set! (indexAt world position) 0 }

/-- Test a world-space subpixel point against the surface within its tile. -/
def submerged (water : Water) (world : World) (x y : Int) : Bool :=
  let fill := water.level world ⟨x / tileUnits, y / tileUnits⟩
  fill > 0 && y % tileUnits ≥ tileUnits - (fill : Int) * tileUnits / capacity

def wetsPlayer (water : Water) (world : World) (player : Player) : Bool := Id.run do
  let centerX := player.x + playerWidth / 2
  let centerY := player.y + playerHeight / 2
  let left := centerX - 5 * 16
  let right := centerX + 5 * 16
  let top := centerY - (playerHeight / 32 / 2) * 16
  let bottom := top + (playerHeight / 32) * 16
  for x in [(left / tileUnits).toNat:((right - 1) / tileUnits).toNat + 1] do
    for y in [(top / tileUnits).toNat:((bottom - 1) / tileUnits).toNat + 1] do
      let position : TilePos := ⟨x, y⟩
      let fill := if (world.tile position).solid then 0 else water.level world position
      -- Collision.WetCollision uses (256 - liquid) / 16 native pixels.
      if fill > 0 && (y : Int) * tileUnits + (256 - fill : Nat) < bottom then return true
  return false

end Water

end Lean4Terria
