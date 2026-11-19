import Lean4Terria.Water

namespace Lean4Terria

namespace Magma

def contactDamage : Nat := 80

def fillFromEighths (liquid : Water) (world : World) (cells : Array (TilePos × Nat)) : Water := Id.run do
  let mut levels := liquid.levels
  for (position, amount) in cells do
    if world.contains position && !(world.tile position).solid then
      levels := levels.set! (position.y.toNat * world.width + position.x.toNat)
        (Water.fromEighths amount).toUInt8
  return { levels }

def seedUnderworld (magma : Water) (world : World) (seed : UInt64) : Water := Id.run do
  if world.height != underworldHeight then return magma
  let mut levels := magma.levels
  for y in [underworldStart:world.height] do
    for x in [:world.width] do
      let pos : TilePos := ⟨x, y⟩
      let amount := (if (world.tile pos).solid then 0 else underworldLavaLevel seed pos).toUInt8
      let index := y * world.width + x
      levels := if index < levels.size then levels.set! index amount else levels.push amount
  return { levels }

def initial (world : World) (seed : UInt64) : Water :=
  let magma := Water.empty world
  let magma := if world.tile magmaMarker == .magmaMarker then fillFromEighths magma world (magmaGallery seed).magma
    else magma
  seedUnderworld magma world seed

/-- A body touches magma as soon as any part reaches its liquid surface. -/
def touches (magma : Water) (world : World) (body : Player) : Bool := Id.run do
  let bottom := body.y + playerHeight
  let right := body.x + playerWidth
  for row in [(body.y / tileUnits).toNat:((bottom - 1) / tileUnits).toNat + 1] do
    for column in [(body.x / tileUnits).toNat:((right - 1) / tileUnits).toNat + 1] do
      let level := magma.level world ⟨column, row⟩
      let surface := (row : Int) * tileUnits + ((256 - level) / 16 : Nat) * (tileUnits / 16)
      if level > 0 && surface < bottom then return true
  return false

def burnPlatforms (world : World) (before after : Water) : World := Id.run do
  let mut terrain := world
  for index in [:after.levels.size] do
    if (before.levels[index]! != 0 || after.levels[index]! != 0) &&
        terrain.tiles[index]? == some Tile.platform.code then
      terrain := terrain.setTile ⟨index % world.width, index / world.width⟩ .air
  return terrain

private def waterAboveOrBeside (levels : ByteArray) (world : World) (index : Nat) : List Nat :=
  let column := index % world.width
  [if world.width ≤ index then some (index - world.width) else none,
    if column > 0 then some (index - 1) else none,
    if column + 1 < world.width then some (index + 1) else none].filterMap fun cell =>
      cell.filter fun neighbor => levels[neighbor]?.getD 0 != 0

private def mergeThreshold : Nat := 24

private def replaceableByObsidian : Tile → Bool
  | .air | .platform | .torch => true
  | _ => false

/-- Desktop 1.4.5.8 checks water beside or above the lava first. Water below
the lava is a separate case and the resulting block occupies the lower cell. -/
def cool (world : World) (water magma : Water) (canSolidify : World → TilePos → Bool) :
    World × Water × Water × Array TilePos := Id.run do
  let mut terrain := world
  let mut waterLevels := water.levels
  let mut magmaLevels := magma.levels
  let mut formed := #[]
  for index in [:magmaLevels.size] do
    let amount := magmaLevels[index]!.toNat
    if amount == 0 then continue
    let position : TilePos := ⟨index % world.width, index / world.width⟩
    let tile := terrain.tile position
    if tile.solid then continue
    let sources := waterAboveOrBeside waterLevels world index
    if !sources.isEmpty then
      let total := sources.foldl (fun amount cell => amount + waterLevels[cell]!.toNat) 0
      if total < mergeThreshold || !replaceableByObsidian tile then
        waterLevels := sources.foldl (fun levels cell => levels.set! cell 0) waterLevels
      else
        let solid := terrain.setTile position .obsidian
        if canSolidify solid position then
          waterLevels := sources.foldl (fun levels cell => levels.set! cell 0) waterLevels
          magmaLevels := magmaLevels.set! index 0
          terrain := solid
          formed := formed.push position
    else
      let below := index + world.width
      if (waterLevels[below]?.getD 0) == 0 then continue
      let target : TilePos := ⟨position.x, position.y + 1⟩
      if !replaceableByObsidian (terrain.tile target) then continue
      if amount < mergeThreshold then
        magmaLevels := magmaLevels.set! index 0
      else
        let solid := terrain.setTile target .obsidian
        if canSolidify solid target then
          waterLevels := waterLevels.set! below 0
          magmaLevels := magmaLevels.set! index 0
          terrain := solid
          formed := formed.push target
  return (terrain, { levels := waterLevels }, { levels := magmaLevels }, formed)

/-- HoneyCheck uses the same 24-unit threshold as LavaCheck. Lava takes
precedence when both liquids are beside the honey. -/
def coolHoney (world : World) (water magma honey : Water) (canSolidify : World → TilePos → Bool) :
    World × Water × Water × Water := Id.run do
  let mut terrain := world
  let mut waterLevels := water.levels
  let mut magmaLevels := magma.levels
  let mut honeyLevels := honey.levels
  for index in [:honeyLevels.size] do
    let amount := honeyLevels[index]!.toNat
    if amount == 0 then continue
    let position : TilePos := ⟨index % world.width, index / world.width⟩
    let waterSources := waterAboveOrBeside waterLevels world index
    let lavaSources := waterAboveOrBeside magmaLevels world index
    if !waterSources.isEmpty || !lavaSources.isEmpty then
      let total := waterSources.foldl (fun n cell => n + waterLevels[cell]!.toNat) 0 +
        lavaSources.foldl (fun n cell => n + magmaLevels[cell]!.toNat) 0
      if total ≥ mergeThreshold && replaceableByObsidian (terrain.tile position) then
        let solid := terrain.setTile position (if lavaSources.isEmpty then .honeyBlock else .crispyHoneyBlock)
        if !canSolidify solid position then continue
        honeyLevels := honeyLevels.set! index 0
        terrain := solid
      waterLevels := waterSources.foldl (fun levels cell => levels.set! cell 0) waterLevels
      magmaLevels := lavaSources.foldl (fun levels cell => levels.set! cell 0) magmaLevels
    else
      let below := index + world.width
      let lava := magmaLevels[below]?.getD 0 != 0
      if !lava && waterLevels[below]?.getD 0 == 0 then continue
      let target : TilePos := ⟨position.x, position.y + 1⟩
      if !replaceableByObsidian (terrain.tile target) then continue
      if amount < mergeThreshold then
        honeyLevels := honeyLevels.set! index 0
      else
        let solid := terrain.setTile target (if lava then .crispyHoneyBlock else .honeyBlock)
        if canSolidify solid target then
          if lava then magmaLevels := magmaLevels.set! below 0
          else waterLevels := waterLevels.set! below 0
          honeyLevels := honeyLevels.set! index 0
          terrain := solid
  return (terrain, ⟨waterLevels⟩, ⟨magmaLevels⟩, ⟨honeyLevels⟩)

end Magma

end Lean4Terria
