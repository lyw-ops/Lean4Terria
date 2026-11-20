import Lean4Terria.OriginalTravelMerchant.Model
import Lean4Terria.OriginalTravelMerchant.Tables
import Lean4Terria.OriginalTravelMerchant.TileTables
import Std.Data.HashMap
import Std.Data.HashSet
import Lean4Terria.UnifiedRandom

/-!
# Travelling Merchant (desktop 1.4.5.8), stage A: `Chest.SetupTravelShop`

`SetupTravelShop` with its callees `GetItem`, `GetPainting`, `AdjustSlotRarities`, `AddToShop`, `CanAddItemToShop`,
`Player.GetPlayerWithHighestLuck` and `Player.RollLuck` (`Luck.RollLuck`). The item and painting programs are generated from
the IL (`Tables.lean`). The random stream is `Main.rand`, carried through every draw.
-/

namespace Lean4Terria.OriginalTravelMerchant

open Lean4Terria

/-- A player as `GetPlayerWithHighestLuck` sees it. -/
structure PlayerLuck where
  active : Bool := true
  luck : Float32 := 0

/-- `Player.GetPlayerWithHighestLuck`: the first active player with the strictly highest `luck`; a fresh player (luck 0)
when none is active. -/
def highestLuck (players : Array PlayerLuck) : Float32 := Id.run do
  let mut best : Option Float32 := none
  for p in players do
    if p.active then
      match best with
      | none => best := some p.luck
      | some b => if !(b ≥ p.luck) then best := some p.luck
  return best.getD 0

private def nextN (rng : UnifiedRandom) (n : Int) : Int × UnifiedRandom := rng.next 0 n

/-- `Luck.RollLuck(luck, range)`: lucky `Next(Next(range/2, range))`, unlucky `Next(Next(range, range*2))`. -/
def rollLuck (luck : Float32) (range : Int) (rng : UnifiedRandom) : Int × UnifiedRandom := Id.run do
  let mut rng := rng
  if luck > 0 then
    let (v, r) := rng.sample
    rng := r
    if v.toFloat32 < luck then
      let (inner, r) := rng.next (range / 2) range
      return nextN r inner
  if luck < 0 then
    let (v, r) := rng.sample
    rng := r
    if v.toFloat32 < -luck then
      let (inner, r) := rng.next range (range * 2)
      return nextN r inner
  return nextN rng range

private def evalCond (c : Cond) (flags : Flags) (luck : Float32) (rar : Array Int) (slot : Int)
    (rng : UnifiedRandom) : Bool × UnifiedRandom :=
  match c with
  | .roll k => let (v, r) := rollLuck luck rar[k]! rng; (v == 0, r)
  | .flag f want => (flags.get f == want, rng)
  | .slotLE k => (slot ≤ k, rng)
  | .anyFlag fs => (fs.any flags.get, rng)

/-- Run a generated statement program (`GetItem` / `GetPainting`). `none` as the returned item flag means the program
executed `return`; the item and generator are the state at that point. -/
def runProgram (prog : Array Stmt) (flags : Flags) (luck : Float32) (rar : Array Int) (slot : Int) (item : Int)
    (rng : UnifiedRandom) : Int × UnifiedRandom := Id.run do
  let mut item := item
  let mut rng := rng
  for st in prog do
    let mut ok := true
    for c in st.conds do
      if ok then
        let (b, r) := evalCond c flags luck rar slot rng
        rng := r
        ok := b
    if ok then
      match st.effect with
      | .set v => item := v
      | .setThenIf a f b => item := if flags.get f then b else a
      | .setRand base n => let (v, r) := rng.next 0 n; rng := r; item := base + v
      | .returnIfSlotGT k => if slot > k then return (item, rng)
  return (item, rng)

/-- `SetupTravelShop_AdjustSlotRarities(slot, ref rarities)`: weights above 1 drop to 1 once the attempt count passes
4200 + 100 * index. -/
def adjustSlotRarities (slot : Int) (rar : Array Int) : Array Int := Id.run do
  let mut rar := rar
  for k in [:6] do
    let i := 5 - k
    if rar[i]! > 1 && slot > 4200 + 100 * (i : Int) then rar := rar.set! i 1
  return rar

/-- `SetupTravelShop_CanAddItemToShop`. -/
def canAdd (shop : Array Int) (item : Int) : Bool := Id.run do
  if item == 0 then return false
  for e in shop do
    if e == item then return false
    if item == 3637 then
      if (e - 3621).toNat ≤ 1 && e ≥ 3621 then return false
      if e ≥ 3633 && e - 3633 ≤ 9 then return false
  return true

/-- `SetupTravelShop_AddToShop(item, ref numAdded, ref slot)`. Item 3637 (the Travelling Merchant's mystery item) draws
`Main.rand.Next(6)` for the pair it writes. -/
def addToShop (shop : Array Int) (numAdded slot : Nat) (item : Int) (rng : UnifiedRandom) :
    Array Int × Nat × Nat × UnifiedRandom :=
  if item == 0 then (shop, numAdded, slot, rng) else
  if item == 3637 then
    let (r, rng) := rng.next 0 6
    let (a, b) := Tables.pairs3637[r.toNat]!
    ((shop.set! slot a).set! (slot + 1) b, numAdded + 1, slot + 2, rng)
  else
    let items : List Int := match Tables.bundles.find? fun (b : Nat × List Nat) => (b.1 : Int) == item with
      | some b => b.2.map Int.ofNat
      | none => [item]
    let (s, k) := items.foldl (init := (shop, slot)) fun (s, k) v => (s.set! k v, k + 1)
    (s, numAdded + 1, k, rng)

structure ShopContext where
  flags : Flags := {}
  expertMode : Bool := false
  players : Array PlayerLuck := #[]

structure ShopResult where
  /-- `Main.travelShop`. -/
  shop : Array Int
  rng : UnifiedRandom

/-- `Chest.SetupTravelShop()`. -/
def setupTravelShop (ctx : ShopContext) (rng : UnifiedRandom) : ShopResult := Id.run do
  let mut shop : Array Int := Array.replicate Tables.shopLength 0
  let luck := highestLuck ctx.players
  let mut rng := rng
  let (base, r) := rng.next 4 7
  rng := r
  let mut num : Int := base
  for range in [4, 8, 16, 32] do
    let (v, r) := rollLuck luck range rng
    rng := r
    if v == 0 then num := num + 1
  -- Main.expertMode is also true in a "for the worthy" world (found by the replay: getGoodWorld alone changes the stream)
  if ctx.expertMode || ctx.flags.getGoodWorld then
    let (v, r) := rollLuck luck 2 rng
    rng := r
    if v == 0 then num := num + 1
  if ctx.flags.peddlersSatchelWasUsed then num := num + 1
  if ctx.flags.tenthAnniversaryWorld then
    if !ctx.flags.getGoodWorld then num := num + 1
    num := num + 1
  let mut numAdded : Nat := 0
  let mut slot : Nat := 0
  let mut rar := Tables.rarities
  let mut tries : Int := 0
  if ctx.flags.hardMode then
    let mut item : Int := 0
    for _ in [:5000] do
      if tries < 5000 then
        tries := tries + 1
        rar := adjustSlotRarities tries rar
        let (v, r) := runProgram Tables.getItem ctx.flags luck rar 2 item rng
        item := v
        rng := r
        if canAdd shop item then
          let (s, n, k, r) := addToShop shop numAdded slot item rng
          shop := s; numAdded := n; slot := k; rng := r
          break
  -- the main loop has no attempt cap in the original
  for _ in [:1000000] do
    if (numAdded : Int) < num then
      let (item, r) := runProgram Tables.getItem ctx.flags luck rar 0 0 rng
      rng := r
      if canAdd shop item then
        let (s, n, k, r) := addToShop shop numAdded slot item rng
        shop := s; numAdded := n; slot := k; rng := r
    else break
  tries := 0
  let mut painting : Int := 0
  for _ in [:5000] do
    if tries < 5000 then
      tries := tries + 1
      rar := adjustSlotRarities tries rar
      let (v, r) := runProgram Tables.getPainting ctx.flags luck rar 0 painting rng
      painting := v
      rng := r
      if canAdd shop painting then
        let (s, _, _, r) := addToShop shop numAdded slot painting rng
        return { shop := s, rng := r }
  return { shop, rng }


/-! ## Tiles -/

/-- The `Tile` fields the spawn code reads. -/
structure Tile where
  type : Nat := 0
  active : Bool := false
  inactive : Bool := false
  liquid : Nat := 0
  halfBrick : Bool := false
  slope : Nat := 0

/-- `Main.tile`: the tiles inside `window = (x0, x1, y0, y1)` exist (air unless overridden); every other
position (and everything outside the world) is `null`. -/
structure TileMap where
  window : Int × Int × Int × Int := (0, 0, 0, 0)
  overrides : Std.HashMap (Int × Int) Tile := {}
  /-- Positions inside the window that are `null` (holes). -/
  nulls : Std.HashSet (Int × Int) := {}

def TileMap.get? (m : TileMap) (x y : Int) : Option Tile :=
  let (x0, x1, y0, y1) := m.window
  if m.nulls.contains (x, y) then none
  else if x0 ≤ x && x ≤ x1 && y0 ≤ y && y ≤ y1 then some ((m.overrides.get? (x, y)).getD {}) else none

private def tileSolid (type : Nat) : Bool := TileTables.solidTypes.contains type
private def tileSolidTop (type : Nat) : Bool := TileTables.solidTopTypes.contains type

/-- `WorldGen.SolidTile(i, j, false)`; a missing tile counts as solid. -/
def solidTile (m : TileMap) (x y : Int) : Bool :=
  match m.get? x y with
  | none => true
  | some t => t.active && !t.inactive && tileSolid t.type && !tileSolidTop t.type && !t.halfBrick && t.slope == 0

/-- `Collision.SolidTiles(x1, x2, y1, y2)`. -/
def solidTiles (m : TileMap) (maxTilesX maxTilesY : Int) (x1 x2 y1 y2 : Int) : Bool := Id.run do
  if x1 < 0 || x2 ≥ maxTilesX || y1 < 0 || y2 ≥ maxTilesY - 40 then return true
  let mut x := x1
  while x ≤ x2 do
    let mut y := y1
    while y ≤ y2 do
      match m.get? x y with
      | none => return false
      | some t => if t.active && !t.inactive && tileSolid t.type && !tileSolidTop t.type then return true
      y := y + 1
    x := x + 1
  return false

/-! ## The world around the call -/

/-- The `NPC` fields `SpawnTravelNPC` / `UnspawnTravelNPC` read (one entry per `Main.npc` slot that matters). -/
structure Npc where
  type : Nat
  active : Bool := true
  townNPC : Bool := true
  homeless : Bool := false
  homeX : Int := 0
  homeY : Int := 0
  /-- `Entity.Center`. -/
  cx : Float32 := 0
  cy : Float32 := 0
  deriving Inhabited

/-- A player as the screen test sees it: `position` (truncated to int), `width`, `height`. -/
structure PlayerBox where
  active : Bool := true
  x : Float32 := 0
  y : Float32 := 0
  width : Int := 20
  height : Int := 42
  luck : Float32 := 0

structure SpawnWorld where
  eclipse : Bool := false
  dayTime : Bool := true
  invasionType : Int := 0
  invasionDelay : Int := 0
  invasionSize : Int := 0
  worldSurface : Float := 0
  maxTilesX : Int := 4200
  maxTilesY : Int := 1200
  flags : Flags := {}
  expertMode : Bool := false

/-- `TownRoomManager.HasRoom(type, out point)`: the room set for that NPC type. -/
abbrev Rooms := Array (Nat × Int × Int)

/-- `NPC.sWidth`, `NPC.sHeight`, `NPC.safeRangeX`, `NPC.safeRangeY` (read from the host). -/
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

/-- The rectangle `Rectangle(x*16+8 - sWidth/2 - safeRangeX, y*16+8 - sHeight/2 - safeRangeY, …)` around tile `(x, y)`. -/
private def screenRect (x y : Int) : Rect :=
  { x := x * 16 + 8 - sWidth / 2 - safeRangeX, y := y * 16 + 8 - sHeight / 2 - safeRangeY,
    w := sWidth + safeRangeX * 2, h := sHeight + safeRangeY * 2 }

private def playerRect (p : PlayerBox) : Rect :=
  { x := p.x.toInt32.toInt, y := p.y.toInt32.toInt, w := p.width, h := p.height }

/-- Any active player whose box intersects the rectangle. -/
private def playerSees (players : Array PlayerBox) (r : Rect) : Bool :=
  players.any fun p => p.active && (playerRect p).intersects r

/-! ## UnspawnTravelNPC -/

structure UnspawnResult where
  /-- Index of the Travelling Merchant that was removed (`active = false`, `life = 0`), with the arrival message. -/
  removed : Option Nat

/-- `WorldGen.UnspawnTravelNPC`: no random numbers. -/
def unspawnTravelNPC (npcs : Array Npc) (players : Array PlayerBox) : UnspawnResult := Id.run do
  let some index := (List.range npcs.size).find? fun i => npcs[i]!.active && npcs[i]!.type == 368 | return { removed := none }
  let n := npcs[index]!
  let tx := (n.cx / 16).toInt32.toInt
  let ty := (n.cy / 16).toInt32.toInt
  if playerSees players (screenRect tx ty) then return { removed := none }
  return { removed := some index }

/-! ## SpawnTravelNPC -/

/-- The `NPC.NewNPC(source, x, y, 368, 1, 0, 0, 0, 0, 255)` call and the fields `SpawnTravelNPC` writes after it. -/
structure Birth where
  /-- `NewNPC` arguments: `X`, `Y` (pixels), type 368, `Start` 1, `Target` 255. -/
  x : Int
  y : Int
  homeTileX : Int
  homeTileY : Int
  /-- `NPC.direction` (unchanged from `SetDefaults`, 0, when the spot is the home column); the NPC is also `homeless = true`, `netUpdate = true`, then the `Announcement.HasArrived` broadcast is sent. -/
  direction : Int
  deriving Repr

structure SpawnResult where
  rng : UnifiedRandom
  /-- `Main.travelShop` when `SetupTravelShop` ran. -/
  shop : Option (Array Int) := none
  birth : Option Birth := none

/-- `WorldGen.SpawnTravelNPC`. The `NewNPC` call draws exactly two numbers from `Main.rand` (the town NPC's name via
`Language.RandomFromCategory` and one more roll of the town-NPC setup; their values are not modelled); `NPC.NewNPC` itself and the chat broadcast are typed
boundaries (`Birth`). -/
def spawnTravelNPC (w : SpawnWorld) (npcs : Array Npc) (rooms : Rooms) (players : Array PlayerBox) (tiles : TileMap)
    (rng : UnifiedRandom) (newNpcDrawsOverride : Option Nat := none) : SpawnResult := Id.run do
  if w.eclipse || !w.dayTime then return { rng }
  if w.invasionType > 0 && w.invasionDelay == 0 && w.invasionSize > 0 then return { rng }
  if npcs.any fun n => n.active && n.type == 368 then return { rng }
  let shopResult := setupTravelShop { flags := w.flags, expertMode := w.expertMode, players := players.map fun p => { active := p.active, luck := p.luck } } rng
  let mut rng := shopResult.rng
  let shop := some shopResult.shop
  -- candidate homes: homed town NPCs (not the Old Man), else homeless ones that have a room
  let mut cands : Array (Int × Int) := #[]
  for n in npcs do
    if n.active && n.townNPC && n.type != 37 && !n.homeless then cands := cands.push (n.homeX, n.homeY)
  if cands.isEmpty then
    for n in npcs do
      if n.active && n.townNPC && n.type != 37 && n.homeless then
        -- a later `SetRoom` for the same type replaces the earlier one
        match rooms.reverse.find? fun r => r.1 == n.type with
        | some r => cands := cands.push (r.2.1, r.2.2)
        | none => pure ()
  if cands.isEmpty then return { rng, shop }
  let (pick, r) := rng.next 0 cands.size
  rng := r
  let (hx, hy) := cands[pick.toNat]!
  let mut bestX := hx
  let bestY := hy
  -- extent of walkable ground to the left and right of the home
  let columnOk (x : Int) : Bool :=
    (solidTile tiles x bestY || (match tiles.get? x bestY with | some t => tileSolidTop t.type | none => false)) &&
    [1, 2, 3].all fun k => match tiles.get? x (bestY - k) with
      | some t => !(t.active && tileSolid t.type && !tileSolidTop t.type)
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
  -- a new home column: up to 30 draws of Next(left, right + 1)
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
  -- the spawn spot: search outwards for ground out of every player's sight
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
                  if t.active && !t.inactive && tileSolid t.type then
                    let liquidAbove := [3, 2, 1].any fun k => match tiles.get? spawnX (y - k) with
                      | some a => a.liquid != 0
                      | none => false
                    if !liquidAbove && !solidTiles tiles w.maxTilesX w.maxTilesY (spawnX - 1) (spawnX + 1) (y - 3) (y - 1) then
                      found := !playerSees players (screenRect spawnX y)
                    break
                | none => pure ()
                y := y + 1
  -- NPC.NewNPC(368): two draws from Main.rand (measured natively: the given name and one more roll of the town-NPC
  -- setup; a slime draws none), three in a "for the worthy" world; their values are not modelled
  let newNpcDraws := newNpcDrawsOverride.getD (if w.flags.getGoodWorld then 3 else 2)
  for _ in [:newNpcDraws] do
    let (_, r) := rng.next 0 1
    rng := r
  let direction : Int := if spawnX < bestX then 1 else if spawnX > bestX then -1 else 0
  return { rng, shop, birth := some { x := spawnX * 16, y := spawnY * 16, homeTileX := bestX, homeTileY := bestY, direction } }

end Lean4Terria.OriginalTravelMerchant
