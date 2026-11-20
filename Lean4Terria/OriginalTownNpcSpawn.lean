import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalHousing
import Lean4Terria.OriginalHousingTables
import Lean4Terria.OriginalWorldTimePhase

/-!
# Town NPC arrivals (Terraria desktop 1.4.5.8, Classic, single player)

Hand transcription from the desktop CIL (`.lake/original-town-npc-spawn/il/`) of the methods that move a town NPC
into a house or make a new one arrive:

* `WorldGen.TrySpawningTownNPC(int x, int y)` — called by every overground / underground random tile update; the
  `npcSpawnDelay` / `npcSpawnPeriod` cadence, the wall-34 `Next(4)` veto, `Main.tileSolid[379]` forced solid around
  the call.
* `WorldGen.SpawnTownNPC(int x, int y, bool canSpawnNewTownNPC)` — room check, homeless move-in, the prioritized type,
  the `TownRoomManager` alternate room (one recursion), `IsRoomConsideredAlreadyOccupied`, the off-screen spot search,
  `NPC.NewNPC`, home tile / direction / `netUpdate`, the `unlocked…Spawn` flags, `LastFoundHouse`.
* Its helpers `FindNPCLookingForHomeThatCanMoveIn`, `IsThereASpawnablePrioritizedTownNPC`,
  `IsRoomConsideredAlreadyOccupied`, `NPC.AnyNPCs`, `NPC.GetAvailableNPCSlot` and the `NPC.NewNPC` slot part.
* `WorldGen.UpdatePrioritizedTownNPC()` and `WorldGen.CheckForHousesNearAPlayer()` (both run by every
  `WorldGen.UpdateWorld`, before the tile updates; the latter draws up to 600 numbers from `Main.rand` per frame and is
  the path by which a homeless NPC moves into a new house).
* A driver for `Main.UpdateTime_SpawnTownNPCs` (`runCensus`): the census itself is `OriginalWorldTimePhase`'s
  `spawnTownNpcs` (not re-stated here); this module answers its three callees (`WorldGen.QuickFindHome` through
  `OriginalHousing.quickFindHome`, the bestiary report, the Old Man's `NPC.NewNPC(37)`).

Room checks (`StartRoomCheck`, `RoomNeeds`, `ScoreRoom`, `CheckSpecialTownNPCSpawningConditions`,
`Collision.SolidTiles`, `QuickFindHome`) are `OriginalHousing`'s, with its `RoomState` threaded through every call
exactly as the `WorldGen` statics are.

Typed boundaries (declared, not modelled):
* Inside `NPC.NewNPC`: `SetDefaults` and `GiveTownUniqueDataToNPCsThatNeedIt` draw a fixed number of values from
  `Main.rand` per type (measured natively for every town type: 2, the Old Man and Santa 0, cat / dog / bunny 3; the
  given name and the variation are not modelled), after the `getGoodWorld` `Next(3)` roll which is modelled. The new
  NPC's fields are the measured defaults (`homeTile` -1, `direction` 0, `netUpdate` true, `spawnSlotProtected` 2).
* Chat broadcasts, achievements, `NetMessage.SendData` are reported as `Effect`s.
-/

namespace Lean4Terria.OriginalTownNpcSpawn

open Lean4Terria.OriginalHousing (World RoomState getTile readBool)

/-- `Main.maxNPCs`. `Main.npc` has one more element (index 200) that `NewNPC` returns when it finds no slot. -/
def maxNPCs : Nat := 200

/-! ## Inputs and state -/

/-- One `Main.npc` slot, the fields this chain reads or writes. -/
structure Slot where
  npc : OriginalHousing.Npc
  lookForHomeTimeout : Int := 0
  /-- `NPC.spawnSlotProtected[slot]`. -/
  spawnSlotProtected : Int := 0
  canBeReplacedByOtherNPCs : Bool := false
  direction : Int := 0
  netUpdate : Bool := false
  deriving Repr, Inhabited

/-- A never-used slot (`new NPC()`: home tile -1). -/
def Slot.empty : Slot :=
  { npc := { active := false, type := 0, townNPC := false, homeless := false, homelessDespawn := false,
             homeTileX := -1, homeTileY := -1, housingCategory := 0 } }

/-- `Main.player[i]`: `position`, `width`, `height` (only active players matter). -/
structure PlayerBox where
  active : Bool := true
  x : Float32
  y : Float32
  width : Int := 20
  height : Int := 42
  deriving Repr, Inhabited

/-- The `NPC.unlocked…Spawn` flags `SpawnTownNPC` sets on an arrival. -/
structure Unlocks where
  merchant : Bool := false
  nurse : Bool := false
  armsDealer : Bool := false
  dyeTrader : Bool := false
  demolitionist : Bool := false
  partyGirl : Bool := false
  princess : Bool := false
  slimeGreen : Bool := false
  truffle : Bool := false
  deriving BEq, Repr, Inhabited

/-- The statics the chain reads and writes (WorldGen, Main and NPC). -/
structure Statics where
  /-- `WorldGen.canSpawn`, room bounds, `bestX/bestY`, `hiScore`, … (OriginalHousing's view of them). -/
  room : RoomState
  /-- `Main.townNPCCanSpawn` (`NPCID.Count` entries). -/
  townNPCCanSpawn : Array Bool
  prioritizedTownNPCType : Int := 0
  npcSpawnDelay : Int := 0
  currentlyTryingToUseAlternateHousingSpot : Bool := false
  /-- `WorldGen.LastFoundHouse`. -/
  lastFoundHouse : Int × Int := (0, 0)
  unlocks : Unlocks := {}
  rng : UnifiedRandom

/-- What `NPC.NewNPC(type)` leaves behind besides the slot (measured natively per type). -/
structure NewNpcDefaults where
  /-- `Main.rand` draws inside `SetDefaults` / `GiveTownUniqueDataToNPCsThatNeedIt` (after the `getGoodWorld` roll). -/
  draws : Nat
  townNPC : Bool
  housingCategory : Int

/-- The measured table (`survey` capture): every town type draws 2, the Old Man (37) and Santa (142) 0, the cat (637),
dog (638) and bunny (656) 3; the Skeleton Merchant (453) is not a town NPC. -/
def standardNewNpc (type : Int) : NewNpcDefaults :=
  { draws := if type == 37 || type == 142 then 0 else if type == 637 || type == 638 || type == 656 then 3 else 2,
    townNPC := type != 453,
    housingCategory := (OriginalHousingTables.housingCategory type).getD 0 }

structure Context where
  /-- Tiles and housing tables; `tables.tileSolid` as it is before `Main.tileSolid[379]` is forced. -/
  world : World
  invasionType : Int := 0
  eclipse : Bool := false
  /-- `WorldGen.npcSpawnPeriod` (`UpdateWorld` sets it to `20 * GetWorldUpdateRate()`). -/
  npcSpawnPeriod : Int := 20
  /-- `WorldGen.TownManager._roomLocationPairs` in list order: `(npc type, home x, home y)`. -/
  rooms : Array (Int × Int × Int) := #[]
  /-- `NPCID.Sets.IsTownPet`. -/
  isTownPet : Array Bool
  /-- `NPCID.Sets.SearchSpawnSlotsInReverse`. -/
  searchSpawnSlotsInReverse : Array Bool
  /-- `NPCID.Sets.CannotSpawnInSlot0`. -/
  cannotSpawnInSlot0 : Array Bool
  /-- `NPCID.Count`. -/
  npcIdCount : Int := 697
  getGoodWorld : Bool := false
  /-- `Main.player` (all 255 slots or only the active ones; inactive ones are skipped). -/
  players : Array PlayerBox := #[]
  /-- `Main.npc[200].type`. -/
  extraSlotType : Int := 0
  newNpc : Int → NewNpcDefaults := standardNewNpc
  /-- `NPC.sWidth`, `NPC.sHeight` (constant getters), `NPC.safeRangeX/Y`. -/
  sWidth : Int := 1920
  sHeight : Int := 1200
  safeRangeX : Int := 62
  safeRangeY : Int := 39

/-- Side effects outside the modelled state, in call order. -/
inductive Effect where
  /-- `ChatHelper.BroadcastChatMessage("Announcement.HasArrived", npc.GetFullNetName())`. -/
  | arrived (slot : Nat)
  /-- `AchievementsHelper.NotifyProgressionEvent(id)`. -/
  | progressionEvent (id : Nat)
  /-- `NetMessage.SendData(7)` after the Truffle's first arrival. -/
  | worldSync
  /-- `WorldGen.CheckAchievement_RealEstateAndTownSlimes()`. -/
  | realEstateCheck
  deriving BEq, Repr

/-- An `NPC.NewNPC(source, X, Y, type, 1, 0, 0, 0, 0, 255)` call that found a slot, and what the caller wrote after it. -/
structure Birth where
  slot : Nat
  type : Int
  /-- The `X`, `Y` arguments in pixels (the new NPC's `Bottom`). -/
  x : Int
  y : Int
  homeTileX : Int
  homeTileY : Int
  direction : Int
  deriving BEq, Repr

structure Sim where
  st : Statics
  slots : Array Slot
  births : Array Birth := #[]
  effects : Array Effect := #[]

abbrev S := StateT Sim (Except String)

private def liftE (x : Except String α) : S α := match x with | .ok a => pure a | .error e => throw e

/-- Run an `OriginalHousing` computation on the threaded `WorldGen` statics. -/
def room (m : OriginalHousing.M α) : S α := do
  let s ← get
  match m.run s.st.room with
  | .ok (a, r) => set { s with st := { s.st with room := r } }; pure a
  | .error e => throw e

private def npcs : S (Array OriginalHousing.Npc) := return (← get).slots.map (·.npc)

private def slotAt (site : String) (i : Int) : S Slot := do
  let s ← get
  if 0 ≤ i && i < s.slots.size then return s.slots[i.toNat]! else throw s!"{site}: Main.npc[{i}] not supplied"

private def setSlot (i : Nat) (f : Slot → Slot) : S Unit :=
  modify fun s => { s with slots := s.slots.modify i f }

private def setRoom (f : RoomState → RoomState) : S Unit :=
  modify fun s => { s with st := { s.st with room := f s.st.room } }

private def emit (e : Effect) : S Unit := modify fun s => { s with effects := s.effects.push e }

private def draw (lo hi : Int) : S Int := do
  let s ← get
  let (v, r) := s.st.rng.next lo hi
  set { s with st := { s.st with rng := r } }
  return v

private def setCanSpawnFlag (site : String) (type : Int) (v : Bool) : S Unit := do
  let s ← get
  let a := s.st.townNPCCanSpawn
  if !(0 ≤ type && type < a.size) then throw s!"{site}: IndexOutOfRangeException Main.townNPCCanSpawn[{type}]"
  set { s with st := { s.st with townNPCCanSpawn := a.set! type.toNat v } }

/-- `Main.tileSolid[379] = true` for the duration of a call (TrySpawningTownNPC IL_004e..0065,
CheckForHousesNearAPlayer IL_00b6..00cd). -/
def force379 (w : World) : World :=
  { w with tables := { w.tables with tileSolid := w.tables.tileSolid.set! 379 true } }

/-! ## Helpers -/

/-- `NPC.AnyNPCs(int type)`. -/
def anyNPCs (type : Int) : S Bool := do
  let s ← get
  for i in [:maxNPCs] do
    let n := (s.slots[i]?.getD Slot.empty).npc
    if n.active && n.type == type then return true
  return false

private def checkSpecial (ctx : Context) (type : Int) : S Bool :=
  room (OriginalHousing.checkSpecialTownNPCSpawningConditions ctx.world type)

/-- `WorldGen.FindNPCLookingForHomeThatCanMoveIn(int type)`. -/
def findNPCLookingForHome (ctx : Context) (type : Int) : S Int := do
  for i in [:maxNPCs] do
    let sl ← slotAt "FindNPCLookingForHomeThatCanMoveIn IL_0004" i
    let n := sl.npc
    if n.active && n.homeless && n.type == type && sl.lookForHomeTimeout == 0 then
      if ← checkSpecial ctx n.type then return i
  return -1

/-- `TownRoomManager.HasRoomQuick(type)` (`_hasRoom[type]`, kept in step with the pair list by `SetRoom`). -/
def hasRoomQuick (ctx : Context) (type : Int) : Bool := ctx.rooms.any (·.1 == type)

/-- `TownRoomManager.HasRoom(type, out point)`: the first pair of that type. -/
def hasRoom (ctx : Context) (type : Int) : Option (Int × Int) :=
  (ctx.rooms.find? (·.1 == type)).map fun (_, x, y) => (x, y)

/-- `WorldGen.IsThereASpawnablePrioritizedTownNPC(int x, int y)`. -/
def isThereASpawnablePrioritizedTownNPC (ctx : Context) (x y : Int) : S Int := do
  -- IL_0000..0045: the TownRoomManager occupants of (x, y), list order
  for (t, px, py) in ctx.rooms do
    if px != x || py != y then continue
    if (← liftE (readBool "IsThereASpawnablePrioritizedTownNPC IL_0025" "Main.townNPCCanSpawn" (← get).st.townNPCCanSpawn t)) then
      if !(← anyNPCs t) then
        if ← checkSpecial ctx t then return t
  -- IL_0047..00af
  let mut result : Int := -1
  for k in [:ctx.npcIdCount.toNat] do
    let i : Int := k
    if !(← liftE (readBool "IsThereASpawnablePrioritizedTownNPC IL_0055" "Main.townNPCCanSpawn" (← get).st.townNPCCanSpawn i)) then continue
    if !(← checkSpecial ctx i) then continue
    if ← anyNPCs i then
      setCanSpawnFlag "IsThereASpawnablePrioritizedTownNPC IL_0072" i false
      continue
    if hasRoomQuick ctx i then return i
    if ← liftE (readBool "IsThereASpawnablePrioritizedTownNPC IL_008d" "NPCID.Sets.IsTownPet" ctx.isTownPet i) then return i
    if i == (← get).st.prioritizedTownNPCType then result := i
  return result

/-- `WorldGen.IsRoomConsideredAlreadyOccupied(int x, int y, int npcType)`. -/
def isRoomConsideredAlreadyOccupied (ctx : Context) (x y npcType : Int) : S Bool := do
  for i in [:maxNPCs] do
    let n := (← slotAt "IsRoomConsideredAlreadyOccupied IL_0006" i).npc
    if n.active && n.townNPC && !n.homeless && n.homeTileX == x && n.homeTileY == y then
      if !OriginalHousing.canLiveWithType ctx.world npcType n then return true
  return false

private structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int

/-- `Rectangle.Intersects`. -/
private def Rect.intersects (a b : Rect) : Bool :=
  b.x < a.x + a.w && a.x < b.x + b.w && b.y < a.y + a.h && a.y < b.y + b.h

/-- The rectangle `SpawnTownNPC` IL_01c1..0207 / IL_036b..03b1 builds around a tile. -/
private def screenRect (ctx : Context) (x y : Int) : Rect :=
  { x := x * 16 + 8 - ctx.sWidth / 2 - ctx.safeRangeX, y := y * 16 + 8 - ctx.sHeight / 2 - ctx.safeRangeY,
    w := ctx.sWidth + ctx.safeRangeX * 2, h := ctx.sHeight + ctx.safeRangeY * 2 }

/-- Any active player whose `(int)position` hitbox intersects the rectangle. -/
private def playerSees (ctx : Context) (r : Rect) : Bool :=
  ctx.players.any fun p =>
    p.active && Rect.intersects { x := p.x.toInt32.toInt, y := p.y.toInt32.toInt, w := p.width, h := p.height } r

/-- `NPC.GetAvailableNPCSlot(type, start)` for a positive `type` (`NPCID.FromNetId` is the identity there). -/
def getAvailableNPCSlot (ctx : Context) (type start : Int) : S Int := do
  let reverse ← liftE (readBool "GetAvailableNPCSlot IL_000e" "NPCID.Sets.SearchSpawnSlotsInReverse" ctx.searchSpawnSlotsInReverse type)
  -- IL_000f..001c
  let mut first := start
  if start == 0 then
    if ← liftE (readBool "GetAvailableNPCSlot IL_0018" "NPCID.Sets.CannotSpawnInSlot0" ctx.cannotSpawnInSlot0 type) then
      first := 1
  let mut last : Int := maxNPCs
  let mut step : Int := 1
  if reverse then
    last := maxNPCs - 1
    (first, last) := (last, first)
    step := -1
  let s ← get
  let inUse (i : Int) : Bool :=
    let sl := s.slots[i.toNat]?.getD Slot.empty
    sl.npc.active || sl.spawnSlotProtected > 0
  let mut i := first
  let mut fuel := 512
  while i != last && fuel > 0 do
    if !(0 ≤ i && i ≤ maxNPCs) then throw s!"GetAvailableNPCSlot: index {i} outside Main.npc"
    if !inUse i then return i
    i := i + step
    fuel := fuel - 1
  i := first
  fuel := 512
  while i != last && fuel > 0 do
    if (s.slots[i.toNat]?.getD Slot.empty).canBeReplacedByOtherNPCs then return i
    i := i + step
    fuel := fuel - 1
  return -1

/-- `NPC.NewNPC(source, X, Y, type, start, 0, 0, 0, 0, Target = 255)`: the slot (or `Main.maxNPCs` when none is
free) and the stream; the new NPC gets the measured defaults. `X`, `Y` (its `Bottom`) are the caller's to record. -/
def newNPC (ctx : Context) (type : Int) (start : Int := 1) : S Nat := do
  let mut type := type
  -- IL_0000..0027: "for the worthy" swaps 46 → 614 and 62 → 66 two times in three
  if ctx.getGoodWorld then
    if (← draw 0 3) != 0 then
      if type == 46 then type := 614
      if type == 62 then type := 66
  let slot ← getAvailableNPCSlot ctx type start
  if slot < 0 then return maxNPCs
  let d := ctx.newNpc type
  for _ in [:d.draws] do
    let _ ← draw 0 1
  let fresh : Slot := {
    npc := { active := true, type, townNPC := d.townNPC, homeless := false, homelessDespawn := false,
             homeTileX := -1, homeTileY := -1, housingCategory := d.housingCategory },
    lookForHomeTimeout := 0, spawnSlotProtected := 2, canBeReplacedByOtherNPCs := false, direction := 0,
    netUpdate := true }
  modify fun s => { s with slots := if slot.toNat < s.slots.size then s.slots.set! slot.toNat fresh else s.slots }
  return slot.toNat

/-! ## `WorldGen.SpawnTownNPC` -/

private def unlockFor (u : Unlocks) (type : Int) : Unlocks :=
  match type with
  | 17 => { u with merchant := true }
  | 18 => { u with nurse := true }
  | 19 => { u with armsDealer := true }
  | 38 => { u with demolitionist := true }
  | 160 => { u with truffle := true }
  | 207 => { u with dyeTrader := true }
  | 208 => { u with partyGirl := true }
  | 663 => { u with princess := true }
  | 678 => { u with slimeGreen := true }
  | _ => u

/-- The off-screen spot search (IL_029a..0465). Returns `(found, x, y)` with the locals' final values. -/
private def searchSpot (ctx : Context) (bestX bestY x0 y0 : Int) : S (Bool × Int × Int) := do
  let w := ctx.world
  let mut x := x0
  let mut y := y0
  let mut found := false
  for k in [1:500] do
    let k : Int := k
    for side in [0:2] do
      x := if side == 0 then bestX + k else bestX - k
      if x > 10 && x < w.maxTilesX - 10 then
        let mut top := bestY - k
        let mut bottom : Float := Float.ofInt (bestY + k)
        if top < 10 then top := 10
        if bottom > w.worldSurface then bottom := w.worldSurface
        let mut yy := top
        while Float.ofInt yy < bottom do
          y := yy
          let t ← liftE (getTile w "SpawnTownNPC IL_0321" x y)
          if let some ty := OriginalHousing.nactiveType? t then
            if ← liftE (readBool "SpawnTownNPC IL_0348" "Main.tileSolid" w.tables.tileSolid ty) then
              if !(← liftE (OriginalHousing.solidTiles w (x - 1) (x + 1) (y - 3) (y - 1))) then
                found := !playerSees ctx (screenRect ctx x y)
              break
          yy := yy + 1
      if found then break
    if found then break
  return (found, x, y)

/-- `WorldGen.SpawnTownNPC(int x, int y, bool canSpawnNewTownNPC)`; returns its result
(0 no room, 1 arrival, 2 a homeless NPC moved in, 3 occupied, 4 no slot, 5 nothing to spawn). `fuel` bounds the
alternate-room recursion, which the `currentlyTryingToUseAlternateHousingSpot` guard stops after one level. -/
def spawnTownNPCCore (ctx : Context) : Nat → Int → Int → Bool → S Int
  | 0, _, _, _ => throw "SpawnTownNPC: recursion below the alternate-housing guard"
  | fuel + 1, x, y, canSpawnArg => do
    let w := ctx.world
    let mut canSpawnNew := canSpawnArg
    -- IL_0000..0027
    let t ← liftE (getTile w "SpawnTownNPC IL_000c" x y)
    if ← liftE (readBool "SpawnTownNPC IL_0016" "Main.wallHouse" w.tables.wallHouse t.wall.toNat) then
      setRoom fun r => { r with canSpawn := true }
    if !(← get).st.room.canSpawn then return 0
    -- IL_0028..003c
    if !(← room (OriginalHousing.startRoomCheck w x y)) then return 0
    if !(← room (OriginalHousing.roomNeeds w)) then return 0
    -- IL_003d..0069
    let mut loc0 := (← get).st.prioritizedTownNPCType
    let mut loc1 ← findNPCLookingForHome ctx loc0
    let pet ← liftE (readBool "SpawnTownNPC IL_0056" "NPCID.Sets.IsTownPet" ctx.isTownPet loc0)
    let loc3 := pet && (hasRoom ctx loc0).isSome
    -- IL_006a..007c
    let r := (← get).st.room
    if (r.roomHasStinkbug || r.roomHasEchoStinkbug) && !loc3 then return 0
    -- IL_007d..008e
    room (OriginalHousing.scoreRoom w (← npcs) (-1) loc0)
    if (← get).st.room.hiScore ≤ 0 then return 0
    -- IL_008f..00c5
    let pri := (← get).st.prioritizedTownNPCType
    let mut keepHomeless := false
    if ← checkSpecial ctx pri then
      if ← anyNPCs pri then
        keepHomeless := true
    if keepHomeless then
      canSpawnNew := false
    else
      loc1 := -1
      if canSpawnNew then
        let r := (← get).st.room
        loc0 ← isThereASpawnablePrioritizedTownNPC ctx r.bestX r.bestY
        canSpawnNew := loc0 > 0
    -- IL_00c7..0120: a homeless NPC of the prioritized type moves in
    if loc1 != -1 then
      setCanSpawnFlag "SpawnTownNPC IL_00d6" (← get).st.prioritizedTownNPCType false
      let r := (← get).st.room
      setSlot loc1.toNat fun sl =>
        { sl with npc := { sl.npc with homeTileX := r.bestX, homeTileY := r.bestY, homeless := false, homelessDespawn := false } }
      emit (.progressionEvent 8)
      modify fun s => { s with st := { s.st with prioritizedTownNPCType := 0 } }
      return 2
    -- IL_0121..0135
    if ← anyNPCs (← get).st.prioritizedTownNPCType then canSpawnNew := false
    if !canSpawnNew then
      modify fun s => { s with st := { s.st with lastFoundHouse := (x, y) } }
      return 5
    -- IL_0136..018f: the room this type had before (TownRoomManager), tried once
    if let some (px, py) := hasRoom ctx loc0 then
      if !(← get).st.currentlyTryingToUseAlternateHousingSpot then
        let r := (← get).st.room
        let (bx, by_) := (r.bestX, r.bestY)
        modify fun s => { s with st := { s.st with currentlyTryingToUseAlternateHousingSpot := true } }
        let res ← spawnTownNPCCore ctx fuel px (py - 2) true
        modify fun s => { s with st := { s.st with currentlyTryingToUseAlternateHousingSpot := false } }
        setRoom fun r => { r with bestX := bx, bestY := by_ }
        if res == 1 then return res
    -- IL_0190..01b3
    let r := (← get).st.room
    let (bestX, bestY) := (r.bestX, r.bestY)
    if ← isRoomConsideredAlreadyOccupied ctx bestX bestY (← get).st.prioritizedTownNPCType then return 3
    -- IL_01b4..0286: is the home on screen?
    let mut x := bestX
    let mut y := bestY
    let mut found := !playerSees ctx (screenRect ctx x y)
    -- IL_0286..0465: on screen and at or above the surface → search outwards for an unseen standing spot
    if !found && !(Float.ofInt y > w.worldSurface) then
      (found, x, y) ← searchSpot ctx bestX bestY x y
    -- IL_046a..04b5
    let slot ← newNPC ctx loc0
    if slot == maxNPCs then
      if ctx.extraSlotType != loc0 then return 4
      throw "SpawnTownNPC IL_04b6: NewNPC found no slot and Main.npc[200] has the requested type (writes into the extra slot are outside this module)"
    -- IL_04b6..0550
    setCanSpawnFlag "SpawnTownNPC IL_04bd" loc0 false
    let dir? : Option Int := if x < bestX then some 1 else if x > bestX then some (-1) else none
    setSlot slot fun sl =>
      { sl with npc := { sl.npc with homeTileX := bestX, homeTileY := bestY }, direction := dir?.getD sl.direction, netUpdate := true }
    let dir := ((← get).slots[slot]?.getD Slot.empty).direction
    let birth : Birth := { slot, type := loc0, x := x * 16, y := y * 16, homeTileX := bestX, homeTileY := bestY, direction := dir }
    modify fun s => { s with births := s.births.push birth }
    emit (.arrived slot)
    emit (.progressionEvent 8)
    -- IL_0551..0630: first-arrival flags
    let type := ((← get).slots[slot]?.getD Slot.empty).npc.type
    if type == 160 then emit (.progressionEvent 18)
    modify fun s => { s with st := { s.st with unlocks := unlockFor s.st.unlocks type } }
    if type == 160 then emit .worldSync
    emit .realEstateCheck
    modify fun s => { s with st := { s.st with prioritizedTownNPCType := 0 } }
    return 1

/-- `WorldGen.SpawnTownNPC(x, y, canSpawnNewTownNPC)` with the world as given (the callers below force tile 379). -/
def spawnTownNPC (ctx : Context) (x y : Int) (canSpawnNewTownNPC : Bool) : S Int :=
  spawnTownNPCCore ctx 2 x y canSpawnNewTownNPC

/-! ## Callers -/

/-- `WorldGen.TrySpawningTownNPC(int x, int y)`; the `SpawnTownNPC` result when it was called. -/
def trySpawningTownNPC (ctx : Context) (x y : Int) : S (Option Int) := do
  -- IL_0000..000f
  if ctx.invasionType > 0 || ctx.eclipse then return none
  -- IL_0010..0026
  let delay := OriginalHousing.i32 ((← get).st.npcSpawnDelay + 1)
  modify fun s => { s with st := { s.st with npcSpawnDelay := delay } }
  if delay < ctx.npcSpawnPeriod then return none
  modify fun s => { s with st := { s.st with npcSpawnDelay := 0 } }
  -- IL_002b..004d: in a wall-34 room three tries in four stop here
  let t ← liftE (getTile ctx.world "TrySpawningTownNPC IL_0032" x y)
  if t.wall == 34 then
    if (← draw 0 4) != 0 then return none
  -- IL_004e..007b
  some <$> spawnTownNPC { ctx with world := force379 ctx.world } x y true

/-- `WorldGen.UpdatePrioritizedTownNPC()`. -/
def updatePrioritizedTownNPC : S Unit := do
  if (← get).st.prioritizedTownNPCType == 37 then return
  for i in [:maxNPCs] do
    let sl ← slotAt "UpdatePrioritizedTownNPC IL_0010" i
    let n := sl.npc
    if n.active && n.homeless && n.townNPC && sl.lookForHomeTimeout == 0 && n.type != 368 && n.type != 160 then
      modify fun s => { s with st := { s.st with prioritizedTownNPCType := n.type } }
      return

/-- The frame inputs of `CheckForHousesNearAPlayer`. -/
structure Frame where
  /-- `Main.GameUpdateCount` (uint). -/
  gameUpdateCount : Nat
  /-- `Main.CurrentFrameFlags.ActivePlayersCount`. -/
  activePlayersCount : Int
  /-- `Main.MaxWorldViewSize`. -/
  maxWorldViewSize : Int × Int

/-- `WorldGen.CheckForHousesNearAPlayer()`: one active player per frame (round robin on the update count), up to 300
random tiles around it; the first with a house wall gets `SpawnTownNPC(x, y, false)`. Returns that tile and result. -/
def checkForHousesNearAPlayer (ctx : Context) (frame : Frame) : S (Option (Int × Int × Int)) := do
  let count := if frame.activePlayersCount ≤ 0 then 1 else frame.activePlayersCount
  let mut num : Int := (frame.gameUpdateCount % count.toNat : Nat)
  let w := ctx.world
  for p in ctx.players do
    if !p.active then continue
    let old := num
    num := num - 1
    if old != 0 then continue
    -- IL_003b..007c: CenteredRectangle(Center.ToTileCoordinates(), 0×0), Inflate, ClampToWorld(10)
    let cx := p.x + Float32.ofInt p.width / 2
    let cy := p.y + Float32.ofInt p.height / 2
    let tx := cx.toInt32.toInt >>> 4
    let ty := cy.toInt32.toInt >>> 4
    let (ax, ay) := (frame.maxWorldViewSize.1.tdiv 32, frame.maxWorldViewSize.2.tdiv 32)
    let (l, t, rgt, b) := (tx - ax, ty - ay, tx - ax + 2 * ax, ty - ay + 2 * ay)
    let clamp (v hi : Int) : Int := max 10 (min v (hi - 10))
    let (left, top, right, bottom) := (clamp l w.maxTilesX, clamp t w.maxTilesY, clamp rgt w.maxTilesX, clamp b w.maxTilesY)
    for _ in [:300] do
      let px ← draw left right
      let py ← draw top bottom
      let tile ← liftE (getTile w "CheckForHousesNearAPlayer IL_00a9" px py)
      if ← liftE (readBool "CheckForHousesNearAPlayer IL_00b3" "Main.wallHouse" w.tables.wallHouse tile.wall.toNat) then
        let res ← spawnTownNPC { ctx with world := force379 w } px py false
        return some (px, py, res)
    return none
  return none

/-! ## `Main.UpdateTime_SpawnTownNPCs` driver -/

/-- What the census callees need beyond `Context`. -/
structure CensusInputs where
  /-- `Main.GetBestiaryProgressReport().CompletionPercent` (see `OriginalBestiaryProgress`). -/
  bestiaryCompletion : Float32
  dungeonX : Int
  dungeonY : Int

/-- Runs `OriginalWorldTimePhase.spawnTownNpcs` (the census transcription) to completion and returns the phase state
(with `checkForSpawns`). `Main.rand`, `prioritizedTownNPCType` and `townNPCCanSpawn` are this module's statics: they are
copied into the phase state first and back after every step. `QuickFindHome` runs `OriginalHousing.quickFindHome` on the threaded room state, the Old Man arrives
through `newNPC`. -/
def runCensus (ctx : Context) (phase : OriginalWorldTimePhase.State) (phaseCtx : OriginalWorldTimePhase.Context)
    (inputs : CensusInputs) : S OriginalWorldTimePhase.State := do
  let syncIn (ph : OriginalWorldTimePhase.State) : S OriginalWorldTimePhase.State :=
    return { ph with world := { ph.world with random := (← get).st.rng } }
  -- the census reads and writes the same statics this module threads
  let st := (← get).st
  let phase := { phase with prioritizedTown := st.prioritizedTownNPCType.toNat, townCanSpawn := st.townNPCCanSpawn }
  let syncOut (ph : OriginalWorldTimePhase.State) : S Unit :=
    modify fun s =>
      { s with st := { s.st with rng := ph.world.random, prioritizedTownNPCType := ph.prioritizedTown, townNPCCanSpawn := ph.townCanSpawn } }
  let mut step := OriginalWorldTimePhase.spawnTownNpcs (← syncIn phase) phaseCtx 0 .finish
  for _ in [:1024] do
    syncOut step.state
    let some pending := step.pending | return step.state
    match pending.call with
    | .quickFindHome slot =>
      let res ← liftE (OriginalHousing.quickFindHome ctx.world (← npcs) slot (← get).st.room)
      modify fun s => { s with st := { s.st with room := res.state } }
      setSlot slot fun sl => { sl with npc := res.npc }
      step ← liftE (OriginalWorldTimePhase.resume pending step.state phaseCtx .unit)
    | .bestiaryReport =>
      step ← liftE (OriginalWorldTimePhase.resume pending step.state phaseCtx (.bestiary inputs.bestiaryCompletion))
    | .spawnOldMan x y =>
      -- UpdateTime_SpawnTownNPCs IL_0599..05cc: NewNPC(dungeonX * 16 + 8, dungeonY * 16, 37, Start 0)
      let slot ← newNPC ctx 37 0
      if slot < maxNPCs then
        setSlot slot fun sl =>
          { sl with npc := { sl.npc with homeless := false, homeTileX := inputs.dungeonX, homeTileY := inputs.dungeonY } }
        let birth : Birth := { slot, type := 37, x, y, homeTileX := inputs.dungeonX, homeTileY := inputs.dungeonY, direction := 0 }
        modify fun s => { s with births := s.births.push birth }
      let ph ← syncIn step.state
      step ← liftE (OriginalWorldTimePhase.resume pending ph phaseCtx (.npcSlot slot))
    | other => throw s!"runCensus: unexpected callee {repr other}"
  throw "runCensus: more than 1024 callees"

end Lean4Terria.OriginalTownNpcSpawn
