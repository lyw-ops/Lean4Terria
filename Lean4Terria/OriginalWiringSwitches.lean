import Lean4Terria.OriginalHammerForeground
import Lean4Terria.OriginalWiringSwitches.Tables

/-!
# Switches, levers and actuators (desktop Terraria 1.4.5.8 Classic, single player)

Hand translation of

* `Wiring.HitSwitch` for the Switch (136: `frameY` 0 ↔ 18, `TripWire(x, y, 1, 1)`) and the Lever
  (132, 2 × 2: `frameX` ± 36 on its cells, `TileFrame` of the top-left, `TripWire(left, top, 2, 2)`), each
  after `SoundEngine.PlaySound(28, x·16, y·16)`;
* `Wiring.TripWire`: per colour red, blue, green, yellow, the wired cells of the rectangle (column by
  column) start `HitWire`; teleporters, pumps, pixel boxes and logic gates have nothing to do in scope;
* `Wiring.HitWire`: breadth-first over the colour. Start cells are put in the skip set (never hit) with a
  `_toProcess` count of 4; a newly reached cell is queued with count 3 and every further arrival from
  another side only decrements its count (removed at 0), so each cell is hit at most once per colour.
  Neighbour order is down (y+1), up, right (x+1), left; cells at most 2 from the world edge are not
  entered. Skip set and counts are cleared after each colour — a block on two colours is hit twice;
* `Wiring.HitWireSingle`'s common prefix (`if actuator: ActuateForced`) for tiles whose type has no
  branch of their own (ordinary blocks, platforms, switches, levers, empty wire cells);
* `Wiring.ActuateForced` / `Actuate` / `ReActive` / `DeActive` with the vanilla refusals: not solid
  (`tileSolid && !NotReallySolid`, minus 314, 379, 386–389, 476), Lihzahrd brick below the surface before
  Plantera, and a tile above that `PreventsActuationUnder` or makes `CanKillTile` false;
* the `SquareTileFrame(x, y, false)` that follows a toggle, as requests (`TileFrame` of the 3 × 3) with the
  empty-cell cleanup and `Liquid.AddWater` of liquid cells. Ordinary blocks' atlas frames
  (`TileFrameCosmetic`) belong to the framing owner and are not computed here.

Random stream: only the Mech sound draws (`Main.rand.Next(-10, 11)` for its pitch) when it is audible
and the previous Mech instance is not still playing. Tiles with a wiring behaviour of their own
(lamps, doors, traps, statues, gates, junction boxes, teleporters, pumps, timers, …) are reported in
`Outcome.unsupported` before any state change of that hit.
-/
namespace Lean4Terria.OriginalWiringSwitches

open OriginalCollision (Vector single)
open OriginalWorld (Terrain Tile)

structure Rules where
  maxTilesX : Nat
  maxTilesY : Nat
  worldSurface : Float
  downedPlantBoss : Bool := false
  netMode : Int := 0
  generatingWorld : Bool := false

/-- Inputs of the Mech (type 28) legacy sound: the shared sound context plus the backend fact that
the previous Mech instance is still playing (`SoundEffectInstance.State == Playing`). -/
structure Sound where
  context : OriginalTileHitEffects.Context
  mechPlaying : Bool := false

structure State where
  terrain : Terrain
  random : UnifiedRandom

inductive Event where
  | hitSwitch (x y : Int)
  | sound (request : OriginalTileHitEffects.SoundRequest) (played : Option OriginalTileHitEffects.PlayedSound)
  | tripWire (x y w h : Int)
  | hitWire (color : Nat) (starts : Nat)
  | visit (x y : Int)
  | actuateForced (x y : Int)
  | reActive (x y : Int)
  | deActive (x y : Int)
  | square (x y : Int)
  | tileFrame (x y : Int)
  | addWater (x y : Int)
  deriving Repr

structure Outcome where
  state : State
  events : Array Event := #[]
  unsupported : Option String := none

/-! ## Tile helpers -/

private def tileAt (terrain : Terrain) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else (terrain.tile? x.toNat y.toNat).getD default
private def typeOf (t : Tile) : Nat := (t.id.getD 0).toNat
private def active (t : Tile) : Bool := t.id.isSome
private def frameX (t : Tile) : Int := (t.frame.map (·.1)).getD 0
private def frameY (t : Tile) : Int := (t.frame.map (·.2)).getD 0
/-- `wire()`, `wire2()`, `wire3()`, `wire4()` for colour 1 … 4 (red, blue, green, yellow). -/
def hasWire (t : Tile) (color : Nat) : Bool := (t.wires >>> (color - 1).toUInt8) &&& 1 == 1

/-- Tile types whose `HitWireSingle` path is only the actuator prefix (no branch of their own) and
whose `TileFrame` in a 3 × 3 re-frame issues no further `TileFrame` calls: stone, gray brick, Lihzahrd
brick, switch, lever. Dirt (`TileID.Sets.Dirt`) is excluded on purpose: its `TileFrameCosmetic`
re-frames dirt-merging neighbours recursively, in an order that depends on the atlas merge state, and
platforms are solid, so an actuated platform re-frames itself through `TileFrameImportant`. -/
def plainWireType (id : Nat) : Bool :=
  id == 1 || id == 38 || id == 226 || id == 132 || id == 136

/-! ## The Mech sound -/

private def clamp (lo hi v : Float32) : Float32 := if v < lo then lo else if v > hi then hi else v

/-- `LegacySoundPlayer.PlaySound(28, x, y, 0)`: the common positional prefix, then — unless the previous
Mech instance is still playing — volume, pan and pitch `Next(-10, 11) · 0.01`, with the common tail
(`pitch += pitchOffset` clamped, `volume *= volumeScale`). Type 28 does not scale with ambient volume. -/
def playMech (sound : Sound) (request : OriginalTileHitEffects.SoundRequest) (random : UnifiedRandom) :
    Option OriginalTileHitEffects.PlayedSound × UnifiedRandom := Id.run do
  let c := sound.context
  if c.dedicatedServer || !c.audioSupported || c.soundVolume == 0 then return (none, random)
  let global := request.x == -1 || request.y == -1
  if !global && (c.generating || c.netMode == 2) then return (none, random)
  let offset : Vector := ⟨single request.x - c.cameraCenter.x, single request.y - c.cameraCenter.y⟩
  let distance := Float32.sqrt (offset.x * offset.x + offset.y * offset.y)
  if !global && !(distance < c.attenuationDistance) then return (none, random)
  let pan := if global then 0 else clamp (-1) 1 (offset.x / (c.maxWorldViewWidth * 0.5))
  let volume := clamp 0 1 (if global then 1 else 1 - distance / c.attenuationDistance)
  let volume := min 1 (volume * c.soundVolume)
  if !(volume > 0) then return (none, random)
  if sound.mechPlaying then return (none, random)
  let (roll, random) := random.next (-10) 11
  return (some { request, variation := 0, volume := volume * request.volumeScale, pan,
                 pitch := clamp (-1) 1 (single roll * 0.01 + request.pitchOffset) }, random)

/-! ## The phase -/

private structure S where
  st : State
  events : Array Event := #[]
  unsupported : Option String := none

private abbrev M := StateM S

private def tile (x y : Int) : M Tile := return tileAt (← get).st.terrain x y
private def setTile (x y : Int) (t : Tile) : M Unit := do
  if x < 0 || y < 0 then return
  modify fun s => match s.st.terrain.replaceTile x.toNat y.toNat t with
    | some terrain => { s with st := { s.st with terrain } }
    | none => s
private def emit (e : Event) : M Unit := modify fun s => { s with events := s.events.push e }
private def fail (reason : String) : M Unit :=
  modify fun s => if s.unsupported.isSome then s else { s with unsupported := some reason }
private def blocked : M Bool := return (← get).unsupported.isSome

/-- `WorldGen.TileFrame(x, y)` without reset, inside an actuator's or a lever's re-frame. -/
private def tileFrame (rules : Rules) (x y : Int) (allowFrameImportant : Bool := false) : M Unit := do
  if ← blocked then return
  emit (.tileFrame x y)
  if !(x > 5 && y > 5 && x < (rules.maxTilesX : Int) - 5 && y < (rules.maxTilesY : Int) - 5) then return
  let t ← tile x y
  if !active t then
    setTile x y { t with shape := .full, tilePaint := 0, invisibleBlock := false, fullbrightBlock := false }
  if t.liquid.any (·.amount > 0) && rules.netMode != 1 && !rules.generatingWorld then emit (.addWater x y)
  let some id := t.id | return
  let id := id.toNat
  if Tables.frameImportant id then
    -- a correctly supported lever re-checks its 2 × 2 and stays (verified natively); any other
    -- frame-important neighbour runs its own TileFrameImportant check
    unless allowFrameImportant && id == 132 do fail s!"TileFrame: frame-important tile {id} outside scope"
    return
  unless plainWireType id do fail s!"TileFrame: tile {id} outside scope"

private def squareTileFrame (rules : Rules) (x y : Int) : M Unit := do
  emit (.square x y)
  for dx in [0:3] do
    for dy in [0:3] do
      tileFrame rules (x - 1 + dx) (y - 1 + dy)

private def reActive (rules : Rules) (x y : Int) : M Unit := do
  emit (.reActive x y)
  let t ← tile x y
  setTile x y { t with actuated := false }
  squareTileFrame rules x y

private def deActive (rules : Rules) (x y : Int) : M Unit := do
  emit (.deActive x y)
  let t ← tile x y
  let some id := t.id | return
  let id := id.toNat
  if id == 226 && Float.ofInt y > rules.worldSurface && !rules.downedPlantBoss then return
  let mut solid := Tables.solid id && !Tables.notReallySolid id
  if id == 314 || id == 379 || (386 ≤ id && id ≤ 389) || id == 476 then solid := false
  if !solid then return
  let above ← tile x (y - 1)
  if let some a := above.id then
    if Tables.preventsActuationUnder a.toNat then return
    if !OriginalHammerForeground.canKillTile (← get).st.terrain x y then return
  setTile x y { t with actuated := true }
  squareTileFrame rules x y

/-- `Wiring.ActuateForced`. -/
private def actuateForced (rules : Rules) (x y : Int) : M Unit := do
  emit (.actuateForced x y)
  if (← tile x y).actuated then reActive rules x y else deActive rules x y

/-- `Wiring.HitWireSingle` within scope. -/
private def hitWireSingle (rules : Rules) (x y : Int) : M Unit := do
  if ← blocked then return
  let t ← tile x y
  if let some id := t.id then
    unless plainWireType id.toNat do
      fail s!"HitWireSingle: tile {id} has a wiring behaviour of its own"; return
  emit (.visit x y)
  if t.actuator then actuateForced rules x y

private structure Queue where
  cells : Array ((Int × Int) × Nat) := #[]
  head : Nat := 0

/-- `Wiring.HitWire(starts, color)`. -/
private def hitWire (rules : Rules) (starts : Array (Int × Int)) (color : Nat) : M Unit := do
  emit (.hitWire color starts.size)
  let mut skip : Std.HashSet (Int × Int) := {}
  let mut toProcess : Std.HashMap (Int × Int) Nat := {}
  let mut queue : Queue := {}
  for p in starts do
    skip := skip.insert p
    toProcess := toProcess.insert p 4
    queue := { queue with cells := queue.cells.push (p, 0) }
  while queue.head < queue.cells.size do
    if ← blocked then return
    let ((x, y), _) := queue.cells[queue.head]!
    queue := { queue with head := queue.head + 1 }
    if !skip.contains (x, y) then hitWireSingle rules x y
    if ← blocked then return
    let here ← tile x y
    if here.id == some 424 || here.id == some 445 then fail "HitWire: junction/pixel box"; return
    for k in [0:4] do
      let (nx, ny) := match k with
        | 0 => (x, y + 1) | 1 => (x, y - 1) | 2 => (x + 1, y) | _ => (x - 1, y)
      if !(nx ≥ 2 && nx < (rules.maxTilesX : Int) - 2 && ny ≥ 2 && ny < (rules.maxTilesY : Int) - 2) then continue
      let n ← tile nx ny
      if n.id == some 424 || n.id == some 445 then fail "HitWire: junction/pixel box"; return
      if !hasWire n color then continue
      match toProcess.get? (nx, ny) with
      | some c =>
        toProcess := if c - 1 == 0 then toProcess.erase (nx, ny) else toProcess.insert (nx, ny) (c - 1)
      | none =>
        queue := { queue with cells := queue.cells.push ((nx, ny), k) }
        toProcess := toProcess.insert (nx, ny) 3

/-- `Wiring.TripWire(x, y, w, h)`. -/
private def tripWire (rules : Rules) (x y w h : Int) : M Unit := do
  emit (.tripWire x y w h)
  if rules.netMode == 1 then return
  for color in [1:5] do
    let mut starts := #[]
    for i in [0:w.toNat] do
      for j in [0:h.toNat] do
        if hasWire (← tile (x + i) (y + j)) color then starts := starts.push (x + i, y + j)
    if starts.size > 0 then hitWire rules starts color
    if ← blocked then return

private def mech (sound : Sound) (x y : Int) : M Unit := do
  let request : OriginalTileHitEffects.SoundRequest := { typeId := 28, x := x * 16, y := y * 16, style := 0 }
  let s ← get
  let (played, random) := playMech sound request s.st.random
  set { s with st := { s.st with random } }
  emit (.sound request played)

/-- `Wiring.HitSwitch(x, y)` for a Switch (136) or a Lever (132). Other switch-like tiles (pressure
plates, timers, sensors, mines, detonators, geysers, …) are reported as unsupported. -/
def hitSwitch (rules : Rules) (sound : Sound) (x y : Int) (state : State) : Outcome := Id.run do
  if !(x ≥ 0 && y ≥ 0 && x < rules.maxTilesX && y < rules.maxTilesY) then return { state }
  let t := tileAt state.terrain x y
  let id := typeOf t
  if active t && !(id == 136 || id == 132) &&
      [135, 314, 423, 428, 442, 476, 440, 210, 443, 144, 441, 468, 467, 411].contains id then
    return { state, unsupported := some s!"HitSwitch: tile {id} outside the switch/lever scope" }
  if !active t || !(id == 136 || id == 132) then return { state }
  let prog : M Unit := do
    emit (.hitSwitch x y)
    if id == 136 then
      setTile x y { t with frame := some (frameX t, if frameY t == 0 then 18 else 0) }
      mech sound x y
      tripWire rules x y 1 1
    else
      let mut delta : Int := 36
      let mut lx := (-(frameX t).tdiv 18).tmod 4
      if lx < -1 then
        lx := lx + 2
        delta := -36
      lx := lx + x
      let ly := -(frameY t).tdiv 18 + y
      for i in [0:2] do
        for j in [0:2] do
          let c ← tile (lx + i) (ly + j)
          if c.id == some 132 then setTile (lx + i) (ly + j) { c with frame := some (frameX c + delta, frameY c) }
      tileFrame rules lx ly (allowFrameImportant := true)
      mech sound x y
      tripWire rules lx ly 2 2
  let (_, s) := prog.run { st := state }
  return { state := s.st, events := s.events, unsupported := s.unsupported }

end Lean4Terria.OriginalWiringSwitches
