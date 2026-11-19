import Lean4Terria.Water
import Lean4Terria.Inventory
import Lean4Terria.UnderworldShots
import Lean4Terria.Night
import Lean4Terria.Weather
import Lean4Terria.FishingCrates

namespace Lean4Terria

inductive BobberPhase where
  | drifting (clock : Nat)
  | hooked (remaining : Nat) (catchItem baitItem : Item)
  | returning (catchItem : Option Item)
  deriving BEq, Repr, Inhabited

/-- Projectile 360 uses a 14 by 14 hitbox, with a Float32 top-left in native pixels. -/
structure FishingBobber where
  x : Float32
  y : Float32
  vx : Float32 := 0
  vy : Float32 := 0
  rotation : Float32 := 0
  wet : Bool := false
  reelTicks : Nat := 0
  slot : Nat
  deriving Repr

instance : BEq FishingBobber where
  beq a b := a.x.toBits == b.x.toBits && a.y.toBits == b.y.toBits &&
    a.vx.toBits == b.vx.toBits && a.vy.toBits == b.vy.toBits &&
    a.rotation.toBits == b.rotation.toBits && a.wet == b.wet &&
    a.reelTicks == b.reelTicks && a.slot == b.slot

inductive FishingState where
  | idle
  | waiting (target : TilePos) (ticks : Nat)
  | biting (target : TilePos) (ticks : Nat)
  | soaking (target : TilePos) (clock : Nat)
  | hooked (target : TilePos) (remaining : Nat) (catchItem baitItem : Item)
  | bobber (motion : FishingBobber) (phase : BobberPhase)
  deriving BEq, Repr, Inhabited

def FishingState.slot : FishingState → Option Nat
  | .bobber motion _ => some motion.slot
  | _ => none

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def pixels (subpixels : Int) : Float32 := single subpixels / 16
private def bobberLength (x y : Float32) : Float32 := (x * x + y * y).toFloat.sqrt.toFloat32
private def bobberAngle (x y : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32 + 1.57

def FishingBobber.centerX (bobber : FishingBobber) : Float32 := bobber.x + 7
def FishingBobber.centerY (bobber : FishingBobber) : Float32 := bobber.y + 7

def FishingBobber.tile (bobber : FishingBobber) : TilePos :=
  ⟨(bobber.centerX / 16).toInt32.toInt, (bobber.centerY / 16).toInt32.toInt⟩

def FishingBobber.inWorld (bobber : FishingBobber) (world : World) : Bool :=
  bobber.centerX ≥ 0 && bobber.centerY ≥ 0 &&
    bobber.centerX < single (world.width * 16 : Nat) && bobber.centerY < single (world.height * 16 : Nat)

def FishingBobber.distanceTo (bobber : FishingBobber) (player : Player) : Float32 :=
  bobberLength (pixels (bodyCenterX player) - bobber.centerX) (pixels (bodyCenterY player) - bobber.centerY)

def FishingBobber.touchingPlayer (bobber : FishingBobber) (player : Player) : Bool :=
  let left := bobber.x.toInt32.toInt * 16
  let top := bobber.y.toInt32.toInt * 16
  (HitBox.mk left top (left + 224) (top + 224)).intersects (HitBox.ofNpcTarget player)

def fishingWaitTicks : Nat := 105
def fishingBiteTicks : Nat := 48

inductive FishingWater where
  | surface | underground
  deriving BEq, Repr

private def wetLevel (world : World) (water : Water) (position : TilePos) : Nat :=
  if (world.tile position).solid then 0 else min Water.capacity (water.level world position)

private def FishingBobber.touchesLiquid (bobber : FishingBobber) (world : World) (water magma honey : Water) : Bool :=
  -- WetCollision centers a 10×7 rectangle using integer division.
  let y := bobber.centerY + 0.5
  shotLiquidContact world water bobber.centerX y 5 3.5 ||
    shotLiquidContact world magma bobber.centerX y 5 3.5 ||
    shotLiquidContact world honey bobber.centerX y 5 3.5

def FishingBobber.launch (world : World) (water magma : Water) (player : Player)
    (aim : WorldAim) (facing : Int) (slot : Nat) (speed : Float32 := 9) (honey : Water := default) : FishingBobber :=
  let x := pixels (bodyCenterX player)
  let y := pixels (bodyCenterY player)
  let dx := pixels aim.x - x
  let dy := pixels aim.y - y
  let distance := bobberLength dx dy
  let (vx, vy) := if distance == 0 then (single facing * speed, 0) else
    let scale : Float32 := speed / distance
    (dx * scale, dy * scale)
  let bobber : FishingBobber := { x := x - 7, y := y - 7, vx, vy, slot }
  { bobber with wet := bobber.touchesLiquid world water magma honey }

private def FishingBobber.waterLine (bobber : FishingBobber) (world : World) (water magma honey : Water) : Float32 :=
  let tile := bobber.tile
  let rows := [tile.y - 1, tile.y, tile.y + 1]
  let line := rows.findSome? fun row =>
    let pos : TilePos := ⟨tile.x, row⟩
    let fill := max (wetLevel world water pos) (max (wetLevel world magma pos) (wetLevel world honey pos))
    if fill > 0 then some (single ((row + 1) * 16 - (fill / 16 : Nat))) else none
  line.getD (bobber.y + 14)

/-- Style 61 AI sees the previous tick's wet flag. WetCollision updates it
before the projectile moves. -/
def FishingBobber.drift (bobber : FishingBobber) (world : World) (water magma : Water) (player : Player)
    (honey : Water := default) : FishingBobber :=
  if bobber.wet then
    let y := bobber.centerY
    let line := bobber.waterLine world water magma honey
    let vy := if bobber.vy > 0 then bobber.vy * 0.5 else bobber.vy
    let vy := max (-8) (vy - 0.1)
    let vy := if y ≤ line || y + vy < line then line - y else vy
    { bobber with vx := bobber.vx * 0.9, vy, rotation := 0 }
  else
    let vx := if bobber.vy == 0 then bobber.vx * 0.95 else bobber.vx
    let rotation := bobberAngle (pixels (bodyCenterX player) - bobber.centerX)
      (pixels (bodyCenterY player) - bobber.centerY)
    { bobber with vx := vx * 0.98, vy := min (bobber.vy + 0.2) 15.9, rotation }

def FishingBobber.move (bobber : FishingBobber) (world : World) (water magma : Water)
    (honey : Water := default) : FishingBobber :=
  let wet := bobber.touchesLiquid world water magma honey
  let (clipX, clipY) := clipShotMotion world bobber.centerX bobber.centerY bobber.vx bobber.vy 7 false
  let hitX := clipX != bobber.vx
  let hitY := clipY != bobber.vy
  let vx := if hitX then bobber.vx * (-0.3) else clipX
  let vy := if hitY && bobber.vy > 1 then bobber.vy * (-0.3) else clipY
  let dx := if !wet then vx else if hitX then clipX else clipX * 0.5
  let dy := if !wet then vy else if hitY then clipY else clipY * 0.5
  { bobber with x := bobber.x + dx, y := bobber.y + dy, vx, vy, wet }

def FishingBobber.pull (bobber : FishingBobber) : FishingBobber :=
  if bobber.wet then { bobber with vy := min (-10) bobber.vy } else bobber

def FishingBobber.reel (bobber : FishingBobber) (world : World) (water magma : Water) (player : Player)
    (honey : Water := default) : FishingBobber :=
  let dx := pixels (bodyCenterX player) - bobber.centerX
  let dy := pixels (bodyCenterY player) - bobber.centerY
  let distance := bobberLength dx dy
  let scale : Float32 := if distance == 0 then 0 else 15.9 / distance
  let vx := (bobber.vx * 9 + dx * scale) / 10
  let vy := (bobber.vy * 9 + dy * scale) / 10
  let wet := bobber.touchesLiquid world water magma honey
  -- A returning bobber disables tile collision. In liquid UpdatePosition
  -- receives the velocity saved before AI.
  let x := bobber.x + if wet then bobber.vx else vx
  let y := bobber.y + if wet then bobber.vy else vy
  { bobber with
    x, y, vx, vy, wet, rotation := bobberAngle vx vy
    reelTicks := min 100 (bobber.reelTicks + 1) }

def FishingBobber.still (bobber : FishingBobber) : Bool :=
  bobber.wet && -0.01 ≤ bobber.vy.toFloat && bobber.vy.toFloat ≤ 0.01

def FishingBobber.dip (bobber : FishingBobber) (sampleX sampleY : Nat) : FishingBobber :=
  if bobber.vy == 0 then
    { bobber with
      vx := single ((sampleX % 201 : Nat) - (100 : Int)) * 0.015
      vy := single (100 + sampleY % 400 : Nat) * 0.015
      wet := false }
  else bobber

def FishingBobber.atSurface (tile : TilePos) (fill slot : Nat) : FishingBobber :=
  { x := single (tile.x * 16 + 1)
    y := single ((tile.y + 1) * 16 - (fill / 16 : Nat)) - 7
    wet := true, slot }

def fishingSurfaceY (row : Int) (fill : Nat) : Int :=
  (row + 1) * tileUnits - (fill : Int) * tileUnits / Water.capacity

private def dryStanding (world : World) (water : Water) (player : Player) : Bool := Id.run do
  let feet := player.y + playerHeight
  if !player.grounded || player.collides world || feet % tileUnits != 0 then return false
  let left := player.x / tileUnits
  let right := (player.x + playerWidth - 1) / tileUnits
  let top := player.y / tileUnits
  let floor := feet / tileUnits
  let mut supported := false
  for column in [:(right - left + 1).toNat] do
    let x := left + column
    let footing : TilePos := ⟨x, floor⟩
    let tile := world.tile footing
    supported := supported || (world.contains footing && (tile.solid || tile == .platform))
    for row in [:(floor - top).toNat] do
      let y := top + row
      let fill := wetLevel world water ⟨x, y⟩
      if fill > 0 && fishingSurfaceY y fill < feet then return false
  return supported

private def exposedSurface (world : World) (water : Water) (target : TilePos) : Option TilePos := Id.run do
  let mut position := target
  for _ in [:world.height] do
    let fill := wetLevel world water position
    if fill == 0 then return none
    if fill < Water.capacity then return some position
    let above := { position with y := position.y - 1 }
    if wetLevel world water above > 0 then
      position := above
    else
      return if (world.tile above).solid then none else some position
  return none

/-- Traverse every tile crossed by the line, including both neighbours of a corner. -/
private def clearCast (world : World) (player : Player) (surface : TilePos) (fill : Nat) : Bool := Id.run do
  let startX := player.x + playerWidth / 2
  let startY := player.y + playerHeight / 3
  let endX := surface.x * tileUnits + tileUnits / 2
  let endY := fishingSurfaceY surface.y fill + 1
  let dx : Int := (endX - startX).natAbs
  let dy : Int := (endY - startY).natAbs
  let stepX : Int := if endX < startX then -1 else 1
  let stepY : Int := if endY < startY then -1 else 1
  let mut cell : TilePos := ⟨startX / tileUnits, startY / tileUnits⟩
  let cells := (surface.x - cell.x).natAbs + (surface.y - cell.y).natAbs + 1
  for _ in [:cells] do
    if (world.tile cell).solid then return false
    if cell == surface then return true
    let nextX := { cell with x := cell.x + stepX }
    let nextY := { cell with y := cell.y + stepY }
    if cell.x == surface.x then
      cell := nextY
    else if cell.y == surface.y then
      cell := nextX
    else
      let distX := if stepX > 0 then (cell.x + 1) * tileUnits - startX else startX - cell.x * tileUnits
      let distY := if stepY > 0 then (cell.y + 1) * tileUnits - startY else startY - cell.y * tileUnits
      let crossX := distX * dy
      let crossY := distY * dx
      if crossX < crossY then cell := nextX
      else if crossY < crossX then cell := nextY
      else
        if (world.tile nextX).solid || (world.tile nextY).solid then return false
        cell := ⟨nextX.x, nextY.y⟩
  return false

private def fishingVolume : Nat := 6 * Water.capacity

/-- Count a connected component only until enough water has been seen. A lower
vertical cell must be full for its water to reach the one above. -/
private def hasFishingVolume (world : World) (water : Water) (start : TilePos) : Bool := Id.run do
  let mut seen := #[start]
  let mut volume := wetLevel world water start
  let mut cursor := 0
  while cursor < seen.size do
    let position := seen[cursor]!
    cursor := cursor + 1
    let fill := wetLevel world water position
    let neighbors : List TilePos := [⟨position.x - 1, position.y⟩, ⟨position.x + 1, position.y⟩,
      ⟨position.x, position.y - 1⟩, ⟨position.x, position.y + 1⟩]
    for neighbor in neighbors do
      let amount := wetLevel world water neighbor
      let touches := neighbor.y == position.y ||
        (if neighbor.y < position.y then fill == Water.capacity else amount == Water.capacity)
      if amount > 0 && touches && !seen.contains neighbor then
        volume := volume + amount
        if volume ≥ fishingVolume then return true
        seen := seen.push neighbor
  return false

/-- The older adventure's cast rule, also used to validate its historical saves. -/
def legacyFishingSpot (world : World) (water : Water) (player : Player) (target : TilePos) : Option TilePos := do
  if !dryStanding world water player then none else do
    let surface ← exposedSurface world water target
    let center := player.centerTile
    if (surface.x - center.x).natAbs + (surface.y - center.y).natAbs > 8 then none
    else if !clearCast world player surface (wetLevel world water surface) then none
    else if !hasFishingVolume world water surface then none
    else some surface

private def fishingDistance (player : Player) (surface : TilePos) (fill : Nat) : Bool :=
  let dx := surface.x * tileUnits + tileUnits / 2 - (player.x + playerWidth / 2)
  let dy := fishingSurfaceY surface.y fill - (player.y + playerHeight / 2)
  dx * dx + dy * dy ≤ 14400 * 14400

/-- Cast preview for the click-to-water interface. Once the float is in the
water, only the projectile's range and the new water surface matter. -/
def fishingSpot (world : World) (water : Water) (player : Player) (target : TilePos) : Option TilePos := do
  let surface ← exposedSurface world water target
  let fill := wetLevel world water surface
  if fishingDistance player surface fill && clearCast world player surface fill then some surface else none

def fishingFloatAt (world : World) (water : Water) (player : Player) (target : TilePos) : Option TilePos := do
  let surface ← (exposedSurface world water target).orElse (fun _ =>
    exposedSurface world water { target with y := target.y + 1 })
  if fishingDistance player surface (wetLevel world water surface) then some surface else none

def fishingLiquidAt (world : World) (water magma : Water) (player : Player) (target : TilePos)
    (honey : Water := default) : Option (TilePos × Nat) :=
  [water, magma, honey].findSome? fun liquid =>
    (fishingFloatAt world liquid player target).map fun surface => (surface, wetLevel world liquid surface)

structure FishingPond where
  tiles : Nat := 0
  lava : Bool := false
  honey : Bool := false
  deriving BEq, Repr

/-- GetFishingPondState scans all contiguous wet columns, even if the liquid
type changes within a column. Any honey adds half the raw tile count. -/
def fishingPond (world : World) (water magma honey : Water) (surface : TilePos) : FishingPond := Id.run do
  let wet (position : TilePos) := wetLevel world water position > 0 ||
    wetLevel world magma position > 0 || wetLevel world honey position > 0
  if !wet surface then return {}
  let mut left := surface.x
  let mut right := surface.x
  while wet ⟨left - 1, surface.y⟩ do left := left - 1
  while wet ⟨right + 1, surface.y⟩ do right := right + 1
  let mut pond : FishingPond := {}
  for column in [left.toNat:right.toNat + 1] do
    for row in [surface.y.toNat:world.height] do
      let position : TilePos := ⟨column, row⟩
      if (world.tile position).solid then break
      let lava := magma.level world position > 0
      let honeyHere := honey.level world position > 0
      if !lava && !honeyHere && water.level world position == 0 then break
      pond := { tiles := pond.tiles + 1, lava := pond.lava || lava, honey := pond.honey || honeyHere }
  return pond

def FishingPond.effectiveTiles (pond : FishingPond) : Nat :=
  if pond.honey then pond.tiles * 3 / 2 else pond.tiles

def fishingPondTiles (world : World) (water : Water) (surface : TilePos) : Nat :=
  (fishingPond world water default default surface).tiles

/-- Classic TryBuildFishingContext compares the floating projectile's Y with
the unmounted player's center only when WetCollision is true. Equality passes. -/
def fishingCanBiteAt (world : World) (water magma : Water) (player : Player) (floatY : Float32)
    (honey : Water := default) : Bool :=
  let wet := water.wetsPlayer world player || magma.wetsPlayer world player ||
    honey.wetsPlayer world player
  !wet || pixels (bodyCenterY player) ≤ floatY

def fishingCanBite (world : World) (water magma : Water) (player : Player) (surface : TilePos)
    (fill : Nat) (honey : Water := default) : Bool :=
  fishingCanBiteAt world water magma player (single ((surface.y + 1) * 16 - (fill / 16 : Nat))) honey

def fishingCheckClock : Nat := 661
def fishingMaxBite : Nat := 500

def fishingWaitGain (power : Nat) (roll : Nat → Nat) : Nat :=
  1 + roll 2 % 2 + power / 30 + (if roll 0 % 300 < power then 1 + roll 1 % 2 else 0) +
    (if roll 3 % 60 == 0 then 60 else 0)

def fishingRarity (power numerator minimum sample : Nat) : Bool :=
  sample % max minimum (numerator / power) == 0

/-- Player.Fishing_GetPowerMultiplier rounds each product as Single. -/
def fishingPowerMultiplier (daytime : Bool) (time moonPhase : Nat)
    (raining overcast bloodMoon : Bool) : Float32 :=
  let rain : Float32 := if raining then 1.2 else 1
  let weather := if overcast then rain * 1.1 else rain
  let timeOfDay :=
    if daytime && (time < 5400 || time > 48600) then weather * 1.3
    else if (daytime && 16200 < time && time < 37800) || (!daytime && 6480 < time && time < 25920) then
      weather * 0.8
    else weather
  let moon := match moonPhase with
    | 0 => timeOfDay * 1.1
    | 1 | 7 => timeOfDay * 1.05
    | 3 | 5 => timeOfDay * 0.95
    | 4 => timeOfDay * 0.9
    | _ => timeOfDay
  if bloodMoon then moon * 1.1 else moon

def fishingPower (clock baitPower : Nat) (weather : Weather) (polePower := 5) : Nat :=
  if baitPower == 0 then 0 else
    let daytime := clock < classicDayTicks
    let time := if daytime then clock else clock - classicDayTicks
    let multiplier := fishingPowerMultiplier daytime time weather.moonPhase weather.raining weather.overcast false
    ((Int32.ofNat (polePower + baitPower)).toFloat32 * multiplier).toUInt32.toNat

/-- TryBuildFishingContext uses the top-left Y in pixels for atmosphere. -/
def fishingWaterNeeded (worldWidth : Nat) (worldSurface : Float) (bobberY : Float32) : Nat :=
  let width := (Int32.ofNat worldWidth).toFloat32 / 4200
  let above := bobberY / 16 - (60 + 10 * (width * width))
  let atmosphere := max 0.25 (min 1 (above.toFloat / (worldSurface / 6)).toFloat32)
  (300 * atmosphere).toUInt32.toNat

def fishingPondPower (power tiles needed : Nat) : Nat :=
  let quality := (Int32.ofNat tiles).toFloat32 / (Int32.ofNat needed).toFloat32
  if quality < 1 then ((Int32.ofNat power).toFloat32 * quality).toUInt32.toNat else power

/-- Ocean's native extent and pond cutoff are calibrated for a 4200-column Small world. -/
def nativeOceanPondTiles (width tiles : Nat) : Nat := tiles * 4200 / width

structure FishingBiomes where
  dungeon : Bool := false
  beach : Bool := false
  ocean : Bool := false
  corruption : Bool := false
  crimson : Bool := false
  hallow : Bool := false
  jungle : Bool := false
  snow : Bool := false
  desert : Bool := false
  sky : Bool := false

def FishingBiomes.resolve (biomes : FishingBiomes) (sample : Nat → Nat) : FishingBiomes :=
  { biomes with
    corruption := biomes.corruption && (!biomes.crimson || sample 0 % 2 == 0)
    crimson := biomes.crimson && (!biomes.corruption || sample 0 % 2 == 1)
    snow := biomes.snow && (!biomes.jungle || sample 1 % 2 != 0)
    desert := biomes.desert && !biomes.dungeon }

def FishingBiomes.isOcean (biomes : FishingBiomes) (layer oceanTiles : Nat) : Bool :=
  biomes.ocean && oceanTiles > 1000 && layer ≤ 1

def FishingBiomes.crate (biomes : FishingBiomes) (hardmode : Bool) (evilChoice : Nat) : Option Item :=
  if biomes.dungeon then some (if hardmode then .stockadeCrate else .dungeonCrate)
  else if biomes.beach then some (if hardmode then .seasideCrate else .oceanCrate)
  else if biomes.corruption && (!biomes.crimson || evilChoice % 2 == 0) then
    some (if hardmode then .defiledCrate else .corruptCrate)
  else if biomes.crimson then some (if hardmode then .hematicCrate else .crimsonCrate)
  else if biomes.hallow then some (if hardmode then .divineCrate else .hallowedCrate)
  else if biomes.jungle then some (if hardmode then .brambleCrate else .jungleCrate)
  else if biomes.snow then some (if hardmode then .borealCrate else .frozenCrate)
  else if biomes.desert then some (if hardmode then .mirageCrate else .oasisCrate)
  else if biomes.sky then some (if hardmode then .azureCrate else .skyCrate)
  else none

def cratePotionTicks : Nat := 4 * 60 * 60
def fishingPotionTicks : Nat := 8 * 60 * 60
def sonarPotionTicks : Nat := 8 * 60 * 60

structure FishingRarity where
  common : Bool := false
  uncommon : Bool := false
  rare : Bool := false
  veryRare : Bool := false
  legendary : Bool := false

private structure WaterCatchRule where
  condition : Bool
  rarity : Bool
  numerator : Nat
  denominator : Nat
  items : Array Item

/-- Normal-world water rules in GameContentFishDropPopulator order.
An eligible rule consumes the chance roll before checking rarity. -/
def waterFishingCatch (rarity : FishingRarity) (biomes : FishingBiomes) (trueSnow hardmode : Bool)
    (layer tiles : Nat) (quest : Option Item) (hallowDesert : Bool) (rng : UnifiedRandom)
    (oceanTiles := tiles) :
    Option Item := Id.run do
  let common := rarity.common
  let uncommon := rarity.uncommon
  let rare := rarity.rare
  let veryRare := rarity.veryRare
  let legendary := rarity.legendary
  let underground := layer > 1
  let truffle := trueSnow && hardmode && layer == 3
  let ocean := biomes.isOcean layer oceanTiles
  let rule (quality : Bool) (item : Item) (condition := true) (denominator := 1) (numerator := 1) :
      WaterCatchRule := ⟨condition, quality, numerator, denominator, #[item]⟩
  let questRule (fish : Item) (condition := true) := rule uncommon fish (condition && quest == some fish)
  let desertRules (desert : Bool) (denominator : Nat) := #[
    rule legendary .brasilianSkies desert denominator,
    questRule .scarabFish desert, questRule .scorpioFish desert,
    rule uncommon .oyster desert, rule true .rockLobster desert 3, rule true .flounder desert]
  let rules := #[
    rule legendary .frogLeg true 5,
    rule legendary .balloonPufferfish true 5,
    rule legendary .zephyrFish true 10,
    rule (!legendary && !veryRare && uncommon) .bombFish true 5,
    rule veryRare .alchemyTable biomes.dungeon 12,
    rule veryRare .bewitchingTable biomes.dungeon 12,
    rule legendary .scalyTruffle (biomes.corruption && truffle) 3 2,
    rule legendary .toxikarp (biomes.corruption && hardmode) 2,
    rule rare .purpleClubberfish biomes.corruption,
    questRule .cursedfish biomes.corruption,
    questRule .infectedScabbardfish biomes.corruption,
    questRule .eaterofPlankton biomes.corruption,
    rule uncommon .ebonkoi biomes.corruption,
    rule legendary .scalyTruffle (biomes.crimson && truffle) 3 2,
    rule legendary .bladetongue (biomes.crimson && hardmode) 2,
    questRule .bloodyManowar biomes.crimson,
    questRule .ichorfish biomes.crimson,
    rule uncommon .hemopiranha biomes.crimson,
    rule common .crimsonTigerfish biomes.crimson] ++
  desertRules (biomes.desert && biomes.hallow && hallowDesert) 1 ++ #[
    rule legendary .scalyTruffle (biomes.hallow && truffle) 3 2,
    rule legendary .crystalSerpent (biomes.hallow && hardmode) 2,
    rule legendary .ladyOfTheLake (biomes.hallow && hardmode) 3 2,
    rule veryRare .chaosFish (biomes.hallow && underground),
    questRule .mirageFish (biomes.hallow && underground),
    questRule .pixiefish (biomes.hallow && layer ≤ 1),
    rule rare .prismite biomes.hallow,
    questRule .unicornFish biomes.hallow,
    rule uncommon .princessFish biomes.hallow,
    questRule .pengfish (biomes.snow && layer ≤ 1),
    questRule .tundraTrout (biomes.snow && layer == 1),
    questRule .fishron (biomes.snow && underground),
    questRule .mutantFlinxfin (biomes.snow && underground),
    rule common .frostDaggerfish biomes.snow 12,
    rule uncommon .frostDaggerfish biomes.snow 6,
    rule uncommon .frostMinnow biomes.snow,
    rule common .atlanticCod biomes.snow,
    rule true .specularFish (biomes.snow && underground) 3,
    rule legendary .barnacleStaff (biomes.jungle && hardmode) 3 2,
    rule legendary .itsScragglinTime biomes.jungle 3 2,
    questRule .catfish (biomes.jungle && layer == 1),
    questRule .derpfish (biomes.jungle && layer == 1),
    questRule .tropicalBarracuda (biomes.jungle && layer == 1),
    questRule .mudfish (biomes.jungle && layer ≥ 1),
    rule uncommon .variegatedLardfish (biomes.jungle && underground),
    rule uncommon .doubleCod biomes.jungle,
    rule common .neonTetra biomes.jungle,
    rule veryRare .reaverShark ocean 2,
    rule veryRare .sawtoothShark ocean,
    rule rare .pinkJellyfish ocean 5,
    rule rare .swordfish ocean 3,
    questRule .capnTunabeard ocean,
    questRule .clownfish ocean,
    rule uncommon .shrimp ocean,
    rule common .redSnapper ocean 2,
    rule common .tuna ocean,
    rule true .trout ocean] ++
  desertRules biomes.desert 3 ++ #[
    questRule .harpyfish (layer ≤ 1),
    questRule .cloudfish (layer == 0),
    questRule .wyverntail (layer == 0),
    questRule .angelfish (layer == 0),
    questRule .fallenStarfish (layer ≤ 1),
    questRule .theFishofCthulu (layer ≤ 1),
    rule uncommon .damselfish (layer == 0),
    questRule .dirtfish (layer == 1 || layer == 2),
    questRule .bunnyfish (layer == 1),
    questRule .dynamiteFish (layer == 1),
    questRule .zombieFish (layer == 1),
    rule rare .greenJellyfish (hardmode && underground) 10,
    rule rare .blueJellyfish (hardmode && underground) 9,
    rule rare .blueJellyfish (!hardmode && underground) 5,
    rule legendary .goldenCarp underground 3 2,
    rule veryRare .rockfish underground 2,
    rule rare .stinkfish underground,
    questRule .bonefish underground,
    questRule .batfish underground,
    questRule .jewelfish underground,
    questRule .spiderfish underground,
    questRule .hungerfish (layer > 2),
    questRule .demonicHellfish (layer > 2),
    questRule .guideVoodooFish (layer > 2),
    questRule .fishotron (layer > 2),
    rule uncommon .armoredCavefish underground 4 3,
    ⟨underground, common || uncommon, 1, 4, #[.armoredCavefish, .specularFish, .specularFish, .specularFish]⟩,
    questRule .slimefish,
    rule common .salmon (tiles > 1000),
    rule true .bass]
  let mut rng := rng
  for entry in rules do
    if !entry.condition then continue
    let (chance, next) := rng.next 0 entry.denominator
    rng := next
    if chance.toNat < entry.numerator && entry.rarity then
      let (index, _) := rng.next 0 entry.items.size
      return entry.items[index.toNat]?
  return none

/-- The two stackable catches use fishing power before pond penalties. -/
def fishingCatchQuantity (item : Item) (power : Nat) (roll : Nat → Nat) : Nat :=
  let extra := ([50, 100, 150, 200] : List Nat).countP fun limit => roll limit % limit < power
  let quantity (low high : Nat) := low + roll 0 % (high + 1 - low)
  match item with
  | .bombFish => quantity ((power / 20 + 3) / 2) ((power / 10 + 6) / 2 + extra)
  | .frostDaggerfish => quantity ((power / 4 + 15) / 2) ((power / 2 + 40) / 2 + 6 * extra)
  | _ => 1

/-- HoneyDrops tries a guaranteed rare catch, then a 1-in-2 uncommon
catch, then the active uncommon quest. Its stopper skips crates and junk. -/
def honeyFishingCatch (rare uncommon : Bool) (quest : Option Item) (rng : UnifiedRandom) : Option Item :=
  let (_, rng) := rng.next 0 1
  if rare then some .honeyfin
  else
    let (chance, _) := rng.next 0 2
    if uncommon && chance == 0 then some .honeyfin
    else if uncommon && quest == some .bumblebeeTuna then some .bumblebeeTuna
    else none

/-- Native pond quality and the represented water and lava catch rules. -/
def fishingCatch (lava lavaBait underground : Bool) (tiles power : Nat) (roll : Nat → Nat) (needed := 300)
    (hardmode := false) (biomes : FishingBiomes := {}) (cratePotion := false)
    (questFish : Option Item := none) (layer := if underground then 2 else 1) (oceanTiles := tiles)
    (honey := false) :
    Option (Item × Nat) := do
  if tiles < 75 || power == 0 || (lava && !lavaBait) then failure
  let pondTiles := if !lava && !honey && biomes.isOcean layer oceanTiles then oceanTiles else tiles
  let level := fishingPondPower power pondTiles needed
  if level == 0 then failure
  if roll 20 % 100 > (level + 75) / 2 then failure
  let common := fishingRarity level 150 2 (roll 2)
  let uncommon := fishingRarity level 300 3 (roll 3)
  let rare := fishingRarity level 1050 4 (roll 4)
  let veryRare := fishingRarity level 2250 5 (roll 5)
  let legendary := fishingRarity level 4500 6 (roll 6)
  let crate := roll 11 % 100 < (if cratePotion then 25 else 10)
  let caught ← if lava then
      if crate && roll 12 % 6 == 0 then some (if hardmode then Item.hellstoneCrate else .obsidianCrate)
      else if legendary && roll 13 % 3 == 0 then
        let items := if hardmode then #[Item.demonConch, .bottomlessLavaBucket, .lavaAbsorbantSponge, .obsidianSwordfish]
          else #[Item.demonConch, .bottomlessLavaBucket, .lavaAbsorbantSponge]
        some items[roll 14 % items.size]!
      else if veryRare then some Item.flarefinKoi else if rare then some .obsidifish else none
    else if honey then
      honeyFishingCatch rare uncommon questFish (UnifiedRandom.fromEntropy (roll 24).toUInt64)
    else if pondTiles < needed && roll 0 % 50 > level && roll 1 % 50 > level then
      some (if roll 7 % 8 == 0 then Item.jojaCola else #[Item.oldShoe, .seaweed, .tinCan][roll 8 % 3]!)
    else if crate then
      let biome := if rare then biomes.crate hardmode (roll 15) else none
      some (biome.getD (standardFishingCrate hardmode uncommon rare veryRare legendary))
    else
      waterFishingCatch { common, uncommon, rare, veryRare, legendary }
        (biomes.resolve (fun draw => roll (21 + draw))) biomes.snow hardmode layer tiles
        questFish (roll 23 % 2 == 0) (UnifiedRandom.fromEntropy (roll 24).toUInt64) oceanTiles
  return (caught, 91 + roll 10 % 150 + level)

def fishingWater (seed : UInt64) (target : TilePos) : FishingWater :=
  let column := target.x.toNat
  let surface := if amberBluffsStart ≤ column && column ≤ amberBluffsEnd then amberBluffSurface seed column
    else groundHeight seed column
  if target.y ≤ (surface : Int) + 8 then .surface else .underground

def advanceLegacyFishing (world : World) (water : Water) (player : Player) : FishingState → FishingState
  | .idle => .idle
  | .waiting target ticks =>
    if legacyFishingSpot world water player target != some target then .idle
    else if ticks ≤ 1 then .biting target fishingBiteTicks
    else .waiting target (ticks - 1)
  | .biting target ticks =>
    if ticks ≤ 1 || legacyFishingSpot world water player target != some target then .idle
    else .biting target (ticks - 1)
  | .soaking .. | .hooked .. | .bobber .. => .idle


end Lean4Terria
