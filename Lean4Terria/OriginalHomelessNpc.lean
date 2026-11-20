import Lean4Terria.OriginalTravelMerchant
import Lean4Terria.OriginalHousing
import Lean4Terria.OriginalHousingTables
import Lean4Terria.UnifiedRandom

/-!
# Homeless town NPCs (desktop 1.4.5.8): `WorldGen.SpawnHomelessNPC` / `UnspawnHomelessNPC`

As `Main.UpdateTime` calls them. The spot search is the one of `SpawnTravelNPC` (same tile queries, same screen test); it is
re-stated here so that `OriginalTravelMerchant` stays as released.

Typed boundaries:
* `WorldGen.SpawnTownNPC(LastFoundHouse, true)`: the caller runs it (`OriginalTownNpcSpawn.spawnTownNPC`) and passes `TownSpawn`:
  `.notSpawned` when it returned a refusal that touched nothing (result 0: no room, no score; it draws nothing from `Main.rand`),
  or `.spawned rngAfter` when it returned 1 (an arrival): the module then returns that stream with
  `homelessSpawnTimeout = 54000`. Both outcomes are differentially verified (the arrival with a valid empty room at
  `LastFoundHouse`, `OriginalTownNpcSpawn` against vanilla). Results 2 to 5 (moved in, occupied, no slot, nothing to spawn)
  are outside: they change the NPC array without a birth.
* `WorldGen.QuickFindHome(npc)`: `spawnHomelessNPCHoused` runs the read-only `OriginalHousing.quickFindHome` on the new NPC
  (a free valid room is taken: new home tile, `homeless := false`, `homelessDespawn := false`, progression event 8) and
  returns the shared `WorldGen` room statics; verified including worlds with valid, occupied, shared and broken rooms.
  The plain `spawnHomelessNPC` (no housing world) keeps the home as it is: valid only when no room check can succeed around
  the home tile.
* `NPC.NewNPC` internals as in `OriginalTravelMerchant` (two draws, three in a `getGoodWorld` world), the announcement text and
  the broadcast.
-/

namespace Lean4Terria.OriginalHomelessNpc

open Lean4Terria Lean4Terria.OriginalTravelMerchant

/-- The `NPC` fields the two callees read. -/
structure HNpc where
  type : Nat
  active : Bool := true
  townNPC : Bool := true
  homeless : Bool := false
  homelessDespawn : Bool := false
  homeX : Int := 0
  homeY : Int := 0
  cx : Float32 := 0
  cy : Float32 := 0
  deriving Inhabited

structure HWorld where
  eclipse : Bool := false
  dayTime : Bool := true
  invasionType : Int := 0
  invasionDelay : Int := 0
  invasionSize : Int := 0
  worldSurface : Float := 0
  maxTilesX : Int := 4200
  maxTilesY : Int := 1200
  getGoodWorld : Bool := false
  /-- `WorldGen.prioritizedTownNPCType`. -/
  prioritizedTownNPCType : Nat := 0
  /-- `WorldGen.homelessSpawnTimeout`. -/
  homelessSpawnTimeout : Int := 0

/-- The stream after a `SpawnTownNPC` call that spawned an NPC (the caller ran it). -/
inductive TownSpawn where
  | notSpawned
  | spawned (rngAfter : UnifiedRandom)

private def sWidth : Int := 1920
private def sHeight : Int := 1200
private def safeRangeX : Int := 62
private def safeRangeY : Int := 39

private structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int

private def Rect.intersects (a b : Rect) : Bool :=
  a.x < b.x + b.w && b.x < a.x + a.w && a.y < b.y + b.h && b.y < a.y + a.h

private def screenRect (x y : Int) : Rect :=
  { x := x * 16 + 8 - sWidth / 2 - safeRangeX, y := y * 16 + 8 - sHeight / 2 - safeRangeY,
    w := sWidth + safeRangeX * 2, h := sHeight + safeRangeY * 2 }

private def playerSees (players : Array PlayerBox) (r : Rect) : Bool :=
  players.any fun p => p.active && (Rect.intersects { x := p.x.toInt32.toInt, y := p.y.toInt32.toInt, w := p.width, h := p.height } r)

/-! ## UnspawnHomelessNPC -/

structure UnspawnResult where
  /-- The NPC whose `homelessDespawn` was cleared because it is not an active homeless town NPC. -/
  cleared : Option Nat := none
  /-- The NPC removed (`active = false`, `life = 0`, broadcast). -/
  removed : Option Nat := none

/-- `WorldGen.UnspawnHomelessNPC`: no random numbers. -/
def unspawnHomelessNPC (npcs : Array HNpc) (players : Array PlayerBox) : UnspawnResult := Id.run do
  let some index := (List.range npcs.size).find? fun i => npcs[i]!.active && npcs[i]!.homelessDespawn | return {}
  let n := npcs[index]!
  if !(n.homeless && n.active && n.townNPC) then return { cleared := some index }
  let tx := (n.cx / 16).toInt32.toInt
  let ty := (n.cy / 16).toInt32.toInt
  if playerSees players (screenRect tx ty) then return {}
  return { removed := some index }

/-! ## SpawnHomelessNPC -/

structure Birth where
  /-- The `Main.npc` slot `NewNPC` gave it (only known to `spawnHomelessNPCHoused`: the caller's `slotFor`). -/
  slot : Option Nat := none
  type : Nat
  x : Int
  y : Int
  homeTileX : Int
  homeTileY : Int
  direction : Int
  /-- `NPC.homeless` after `QuickFindHome` (it clears it when a valid free room is found). -/
  homeless : Bool := true
  /-- `NPC.homelessDespawn` after `QuickFindHome`. -/
  homelessDespawn : Bool := true
  /-- `QuickFindHome` raised `AchievementsHelper.NotifyProgressionEvent(8)`. -/
  progressionEvent : Bool := false
  /-- The `Main.rand.Next(6)` variant of the arrival line. -/
  announcement : Nat
  deriving Repr

structure SpawnResult where
  rng : UnifiedRandom
  /-- `WorldGen.homelessSpawnTimeout` after the call. -/
  timeout : Int
  birth : Option Birth := none

private def isHomedTown (n : HNpc) : Bool :=
  n.active && n.townNPC && !n.homeless && n.type != 37 && n.type != 453 && n.type != 368

/-- `WorldGen.FindAnyHomelessTownNPC(true)`. -/
def anyHomeless (npcs : Array HNpc) : Bool :=
  npcs.any fun n => n.active && n.townNPC && n.homeless && n.type != 37 && n.type != 453 && n.type != 368 && n.type != 160

/-- The gates of `SpawnHomelessNPC` that come before `WorldGen.SpawnTownNPC(LastFoundHouse)`: when `false` the call returns
without touching anything (no `SpawnTownNPC`, no draws). -/
def gatesOpen (w : HWorld) (npcs : Array HNpc) : Bool :=
  !(w.eclipse || !w.dayTime) &&
  !(w.invasionType > 0 && w.invasionDelay == 0 && w.invasionSize > 0) &&
  w.prioritizedTownNPCType != 0 && !(w.homelessSpawnTimeout > 0) &&
  npcs.any isHomedTown && !(anyHomeless npcs) &&
  !(npcs.any fun n => n.active && n.type == w.prioritizedTownNPCType)

/-- The new NPC after `NewNPC`, the home fields and `homeless`/`homelessDespawn` writes, before `QuickFindHome`. -/
structure Pending where
  /-- The stream after the `NewNPC` draws (the announcement variant is drawn after `QuickFindHome`). -/
  rng : UnifiedRandom
  timeout : Int
  type : Nat
  x : Int
  y : Int
  homeTileX : Int
  homeTileY : Int
  direction : Int

inductive Start where
  | done (r : SpawnResult)
  | pending (p : Pending)

/-- `Main.rand` draws inside `NPC.NewNPC` (`SetDefaults` and the unique town data) before the `getGoodWorld` roll: two for the
town NPCs, three for the town pets (cat 637, dog 638, bunny 656), as measured natively in `OriginalTownNpcSpawn`. -/
def newNpcDraws (type : Nat) : Nat := if type == 637 || type == 638 || type == 656 then 3 else 2

def spawnHomelessNPCStart (w : HWorld) (npcs : Array HNpc) (players : Array PlayerBox) (tiles : TileMap)
    (townSpawn : TownSpawn) (rng : UnifiedRandom) : Start := Id.run do
  let unchanged : Start := .done { rng, timeout := w.homelessSpawnTimeout }
  if w.eclipse || !w.dayTime then return unchanged
  if w.invasionType > 0 && w.invasionDelay == 0 && w.invasionSize > 0 then return unchanged
  if w.prioritizedTownNPCType == 0 then return unchanged
  if w.homelessSpawnTimeout > 0 then return unchanged
  if !(npcs.any isHomedTown) then return unchanged
  if anyHomeless npcs then return unchanged
  if npcs.any fun n => n.active && n.type == w.prioritizedTownNPCType then return unchanged
  let mut rng := rng
  match townSpawn with
  | .spawned after => return .done { rng := after, timeout := 54000 }
  | .notSpawned => pure ()
  let mut cands : Array (Int × Int) := #[]
  for n in npcs do
    if n.active && n.townNPC && n.type != 37 && !n.homeless then cands := cands.push (n.homeX, n.homeY)
  if cands.isEmpty then return .done { rng, timeout := w.homelessSpawnTimeout }
  let (pick, r) := rng.next 0 cands.size
  rng := r
  let (hx, hy) := cands[pick.toNat]!
  let mut bestX := hx
  let bestY := hy
  let columnOk (x : Int) : Bool :=
    (solidTile tiles x bestY || (match tiles.get? x bestY with | some t => TileTables.solidTopTypes.contains t.type | none => false)) &&
    [1, 2, 3].all fun k => match tiles.get? x (bestY - k) with
      | some t => !(t.active && TileTables.solidTypes.contains t.type && !TileTables.solidTopTypes.contains t.type)
      | none => true
  let mut left := hx
  let mut x := hx
  while x > hx - 10 do
    if columnOk x then
      left := x
      x := x - 1
    else break
  let mut right := hx
  x := hx
  while x < hx + 10 do
    if columnOk x then
      right := x
      x := x + 1
    else break
  for attempt in [:30] do
    let (n, r) := rng.next left (right + 1)
    rng := r
    if attempt < 20 then
      if n < bestX - 1 || n > bestX + 1 then
        bestX := n
        break
    else if n != bestX then
      bestX := n
      break
  let mut spawnX := bestX
  let mut spawnY := bestY
  let mut found := false
  if (Float.ofInt spawnY) ≤ w.worldSurface then
    for ring in [20:500] do
      if !found then
        for side in [:2] do
          if !found then
            spawnX := if side == 0 then bestX + (ring : Int) * 2 else bestX - (ring : Int) * 2
            if spawnX > 10 && spawnX < w.maxTilesX - 10 then
              let mut top := bestY - (ring : Int)
              let mut bottom : Float := Float.ofInt (bestY + (ring : Int))
              if top < 10 then top := 10
              if bottom > w.worldSurface then bottom := w.worldSurface
              let mut y := top
              while Float.ofInt y < bottom do
                spawnY := y
                match tiles.get? spawnX y with
                | some t =>
                  if t.active && !t.inactive && TileTables.solidTypes.contains t.type then
                    let liquidAbove := [3, 2, 1].any fun k => match tiles.get? spawnX (y - k) with
                      | some a => a.liquid != 0
                      | none => false
                    if !liquidAbove && !solidTiles tiles w.maxTilesX w.maxTilesY (spawnX - 1) (spawnX + 1) (y - 3) (y - 1) then
                      found := !playerSees players (screenRect spawnX y)
                    break
                | none => pure ()
                y := y + 1
  for _ in [:(newNpcDraws w.prioritizedTownNPCType + (if w.getGoodWorld then 1 else 0))] do
    let (_, r) := rng.next 0 1
    rng := r
  let direction : Int := if spawnX < bestX then 1 else if spawnX > bestX then -1 else 0
  return .pending { rng, timeout := w.homelessSpawnTimeout, type := w.prioritizedTownNPCType, x := spawnX * 16, y := spawnY * 16,
                    homeTileX := bestX, homeTileY := bestY, direction }

/-- After `QuickFindHome`: the new NPC's fields as it left them, and whether it raised progression event 8. -/
structure Quick where
  homeTileX : Int
  homeTileY : Int
  homeless : Bool
  homelessDespawn : Bool
  progressionEvent : Bool := false

/-- The arrival line variant (`Main.rand.Next(6)`) and the result. -/
def spawnHomelessNPCFinish (p : Pending) (q : Quick) (slot : Option Nat := none) : SpawnResult :=
  let (variant, rng) := p.rng.next 0 6
  { rng, timeout := p.timeout, birth := some {
      slot, type := p.type, x := p.x, y := p.y, homeTileX := q.homeTileX, homeTileY := q.homeTileY, direction := p.direction,
      homeless := q.homeless, homelessDespawn := q.homelessDespawn, progressionEvent := q.progressionEvent,
      announcement := variant.toNat } }

/-- `SpawnHomelessNPC` with `QuickFindHome` assumed to leave the new NPC's home as it is (valid only where no room check can
succeed around the home tile; see the header). -/
def spawnHomelessNPC (w : HWorld) (npcs : Array HNpc) (players : Array PlayerBox) (tiles : TileMap)
    (townSpawn : TownSpawn) (rng : UnifiedRandom) : SpawnResult :=
  match spawnHomelessNPCStart w npcs players tiles townSpawn rng with
  | .done r => r
  | .pending p =>
    spawnHomelessNPCFinish p { homeTileX := p.homeTileX, homeTileY := p.homeTileY, homeless := true, homelessDespawn := true }

/-- `SpawnHomelessNPC` with `WorldGen.QuickFindHome` run by `OriginalHousing.quickFindHome` on `house` / `room` (the shared
`WorldGen` room statics; returned updated). `npcs` are the declared `Main.npc` entries by slot; the housing view is padded to
`house.maxNPCs` with inactive NPCs. `slotFor type` is the slot `NPC.NewNPC(..., type, 1, ...)` takes in the caller's `Main.npc`
(`NPC.GetAvailableNPCSlot`: first free from 1, or from the end for the types of `SearchSpawnSlotsInReverse`). -/
def spawnHomelessNPCHoused (w : HWorld) (npcs : Array HNpc) (players : Array PlayerBox) (tiles : TileMap)
    (townSpawn : TownSpawn) (rng : UnifiedRandom) (house : OriginalHousing.World) (room : OriginalHousing.RoomState)
    (slotFor : Nat → Except String Nat) : Except String (SpawnResult × OriginalHousing.RoomState) :=
  match spawnHomelessNPCStart w npcs players tiles townSpawn rng with
  | .done r => .ok (r, room)
  | .pending p => do
    let slot ← slotFor p.type
    let view (n : HNpc) : OriginalHousing.Npc :=
      { active := n.active, type := n.type, townNPC := n.townNPC, homeless := n.homeless, homelessDespawn := n.homelessDespawn,
        homeTileX := n.homeX, homeTileY := n.homeY, housingCategory := (OriginalHousingTables.housingCategory n.type).getD 0 }
    let inactive : OriginalHousing.Npc :=
      { active := false, type := 0, townNPC := false, homeless := false, homelessDespawn := false, homeTileX := 0, homeTileY := 0, housingCategory := 0 }
    let size := max house.maxNPCs.toNat (slot + 1)
    let mut arr : Array OriginalHousing.Npc := Array.replicate size inactive
    for i in [:npcs.size] do arr := arr.set! i (view npcs[i]!)
    arr := arr.set! slot
      { active := true, type := p.type, townNPC := true, homeless := true, homelessDespawn := true,
        homeTileX := p.homeTileX, homeTileY := p.homeTileY, housingCategory := (OriginalHousingTables.housingCategory p.type).getD 0 }
    -- the spot search leaves its result in the statics `WorldGen.bestX` / `bestY` (also the room statics `QuickFindHome` reads)
    let r ← OriginalHousing.quickFindHome house arr slot { room with bestX := p.homeTileX, bestY := p.homeTileY }
    let q : Quick := { homeTileX := r.npc.homeTileX, homeTileY := r.npc.homeTileY, homeless := r.npc.homeless,
                       homelessDespawn := r.npc.homelessDespawn, progressionEvent := r.events.any fun e => e == .notifyProgressionEvent 8 }
    .ok (spawnHomelessNPCFinish p q (some slot), r.state)

end Lean4Terria.OriginalHomelessNpc
