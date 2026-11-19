import Lean4Terria.OriginalLiquid
import Lean4Terria.OriginalItems
import Lean4Terria.OriginalItemUse

/-!
# Buckets and sponges: `Player.ItemCheck_UseBuckets` (Terraria 1.4.5.8, single player)

Hand port of `Player.ItemCheck_UseBuckets` with `Player.PutItemInInventoryFromItemUsage`,
`Player.GetOrDropItem` and `Player.ApplyItemTime`, from the IL of `TerrariaServer.exe` 1.4.5.8
(`.lake/original-liquid-buckets/probe/il/`). Items: empty bucket 205; water 206, lava 207, honey 1128
buckets; bottomless water 3031, lava 4820, honey 5302, shimmer 5364 buckets; super absorbent 3032,
lava absorbent 4872, honey absorbent 5303, ultra absorbent 5304 sponges.

* Reach: `Player.IsInTileInteractionRange(TileReachCheckSettings.Simple, item.tileBoost)` — the player's
  `tileRangeX` 5 / `tileRangeY` 3, i.e. `OriginalMining.inToolRange body cell 5 3 tileBoost`.
* Use: after `ItemCheck` renews the swing (`OriginalItemUse.Timing.advance`, `useAnimation`,
  auto-reuse), a frame with `itemTime == 0`, `itemAnimation > 0` and the button held uses the item.
* Pick-up (empty bucket off shimmer, sponges on their liquids): the 3 × 3 sum of the target's liquid type
  must exceed 100 for a bucket; the target is emptied, then topped up from its eight neighbours in column
  order (each `SquareTileFrame(i, j, false)` + `AddWater`). The empty bucket becomes honey / lava / water;
  shimmer cannot be picked up with it. Sponges keep their stack.
* Pour: target liquid below 200, not a solid non-platform tile (grate 546 allowed), empty or the same liquid;
  255 of the liquid, `SquareTileFrame(x, y, true)` (its `TileFrame` queues the cell). A normal bucket gives
  back an empty bucket; bottomless buckets stay.
* Every use plays `SoundEngine.PlaySound(19, (int)position.X, (int)position.Y, 1)` (five call sites, IL_03f0,
  IL_071d, IL_082e, IL_0940, IL_0a45) and sets `itemTime = useTime`. A client that really starts the splash
  rolls its pitch on the player stream (`Input.soundStarts`).

Liquid and framing go through the shared `OriginalLiquid.Runtime`; random draws made by the framing (a
plant killed by poured lava, a `resetFrame` roll) come from the player-update stream the caller passes,
as `Main.SwapRandom("UpdatePlayers")` does in vanilla. The liquid stream is not touched.
-/

namespace Lean4Terria.OriginalLiquidBuckets

open OriginalItems (Inventory Stack)
open OriginalItemUse (Timing)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)
open OriginalCollision (single)

/-- `Item.useTime`, `useAnimation`, `tileBoost` of the buckets (read from the runtime,
`.lake/original-liquid-buckets/out/item_stats.json`). -/
structure Stats where
  useTime : Nat
  useAnimation : Nat
  tileBoost : Int

def stats (item : UInt16) : Option Stats :=
  match item with
  | 205 | 206 | 207 | 1128 => some ⟨10, 15, 0⟩
  | 3031 | 3032 | 4820 | 4872 | 5302 | 5303 | 5364 => some ⟨5, 12, 2⟩
  | 5304 => some ⟨3, 8, 3⟩
  | _ => none

def isBucket (item : UInt16) : Bool := (stats item).isSome

def isSponge (item : UInt16) : Bool := [3032, 4872, 5303, 5304].contains item.toNat

/-- `SoundEngine.PlaySound(type, x, y, style, volume 1, pitch 0)` (asset `Sounds/Splash_1`). `pitchDraw` is
the `Main.rand.Next(-10, 11)` that `LegacySoundPlayer.PlaySound` takes for a type 19 style 1 sound it actually
starts (pitch `pitchDraw · 0.01`); see `Input.soundStarts`. -/
structure Sound where
  type : Nat := 19
  style : Nat := 1
  x : Int
  y : Int
  pitchDraw : Option Int := none
  pickupBefore : Bool := false
  pickupAfter : Bool := false
  deriving BEq, Repr

/-- The items a player carries: 58 slots (`Player.inventory[0..57]`: 50 main, 4 coins, 4 ammo), the
mouse item, and the slot in use — a hotbar index, or 58 when the mouse item is used. -/
structure Carry where
  inventory : Inventory
  mouse : Option Stack := none
  selected : Nat
  deriving BEq, Repr

namespace Carry
def held (c : Carry) : Option Stack := if c.selected == 58 then c.mouse else c.inventory[c.selected]?.join
def setHeld (c : Carry) (s : Option Stack) : Carry :=
  if c.selected == 58 then { c with mouse := s } else { c with inventory := c.inventory.set! c.selected s }
end Carry

/-- `Item.IsAir`: no item or a used-up stack -/
def air (s : Option Stack) : Bool := s.all (·.count == 0)

structure Input where
  /-- `Player.controlUseItem` -/
  held : Bool
  /-- `Player.tileTargetX / tileTargetY` -/
  target : Option Cell
  body : Body
  noBuilding : Bool := false
  /-- Whether a client's `LegacySoundPlayer.PlaySound(19, x, y, 1)` gets past its early returns and rolls the
  splash pitch: not `Main.dedServ`, audio supported, `Main.soundVolume ≠ 0`, the sound within
  `SoundAttenuationDistance` of the camera centre, and the previous `Splash_1` instance not playing. The roll is
  `Main.rand.Next(-10, 11)` on the player stream, at the point the sound is played. The dedicated-server probe
  never rolls it (false). -/
  soundStarts : Bool := false

/-- An item `GetOrDropItem` could not store: `Item.RequestNewItem` at the player's centre, with the two
velocity draws already taken from the player stream (`velocity = (drawX · 0.1, drawY · 0.1)`). -/
structure Overflow where
  stack : Stack
  drawX : Int
  drawY : Int
  deriving BEq, Repr

structure Result where
  timing : Timing
  carry : Carry
  liquids : OriginalLiquid.Runtime
  playerRandom : UnifiedRandom
  sound : Option Sound := none
  overflow : Option Overflow := none
  /-- the bucket was used this frame (a liquid edit happened) -/
  used : Bool := false
  /-- draws taken from `playerRandom` (framing during the edit, overflow velocity) -/
  playerDraws : Nat := 0

/-- `Player.PutItemInInventoryFromItemUsage(type)`: top up the first non-full stack in slots 0–57, else
fill the slot in use when it is air, else `GetOrDropItem` (`GetItem` with `ItemCreatedFromItemUsage`,
then `RequestNewItem` for what is left). Only the `GetItem` fallback plays the pickup sound. -/
def putFromUsage (carry : Carry) (item : UInt16) (rng : UnifiedRandom) :
    Carry × Option Overflow × UnifiedRandom × Bool := Id.run do
  let max := OriginalItemStats.maxStack item
  for i in [:min 58 carry.inventory.size] do
    if let some s := carry.inventory[i]! then
      if s.count > 0 && s.item == item && s.count < max then
        return ({ carry with inventory := carry.inventory.set! i (some { s with count := s.count + 1 }) },
          none, rng, false)
  if air carry.held then
    return (carry.setHeld (some (.ofItem item)), none, rng, false)
  let (inventory, rest) := OriginalItems.store carry.inventory (.ofItem item)
  match rest with
  | none => return ({ carry with inventory }, none, rng, true)
  | some stack =>
    let (vx, rng) := rng.next (-30) 31
    let (vy, rng) := rng.next (-40) (-15)
    return ({ carry with inventory }, some ⟨stack, vx, vy⟩, rng, false)

open OriginalLiquid (M Cell getCell setCell squareTileFrame addWater) in
/-- The liquid edit of a pick-up: empty the target, frame it, then top up from its neighbours. -/
def pickUp (hk : OriginalLiquid.Hooks) (x y : Int) (kind : Nat) : M Unit := do
  let t ← getCell x y
  let mut taken := t.liquid
  setCell x y { t with liquid := 0, kind := 0 }
  squareTileFrame hk x y false
  addWater hk x y
  if taken ≥ 255 then return
  for i in [x - 1, x, x + 1] do
    for j in [y - 1, y, y + 1] do
      if i == x && j == y then continue
      let c ← getCell i j
      if c.liquid > 0 && c.kind == kind then
        let n := if c.liquid + taken > 255 then 255 - taken else c.liquid
        taken := taken + n
        let left := c.liquid - n
        setCell i j { c with liquid := left, kind := if left == 0 then 0 else kind }
        squareTileFrame hk i j false
        addWater hk i j

open OriginalLiquid (M getCell setCell squareTileFrame) in
def pour (hk : OriginalLiquid.Hooks) (x y : Int) (kind : Nat) : M Unit := do
  let t ← getCell x y
  setCell x y { t with kind, liquid := 255 }
  squareTileFrame hk x y true

/-- One `ItemCheck` frame of a held bucket or sponge (everything `ItemCheck_UseBuckets` does, plus the
swing timing). `playerRandom` is the `UpdatePlayers` stream. -/
def advance (liquids : OriginalLiquid.Runtime) (playerRandom : UnifiedRandom) (timing : Timing)
    (carry : Carry) (input : Input) : Result := Id.run do
  let some stack := carry.held
    | return { timing := timing.advance false 0 false, carry, liquids, playerRandom }
  let some st := stats stack.item
    | return { timing := timing.advance false 0 false, carry, liquids, playerRandom }
  let timing := timing.advance input.held st.useAnimation (stack.count != 0)
  let idle : Result := { timing, carry, liquids, playerRandom }
  let some cell := input.target | return idle
  if input.noBuilding || stack.count == 0 then return idle
  if !inToolRange input.body cell 5 3 st.tileBoost then return idle
  if !(timing.itemTime == 0 && timing.itemAnimation > 0 && input.held) then return idle
  let x : Int := cell.x
  let y : Int := cell.y
  let s := liquids.liquid
  let t := s.cell x y
  let item := stack.item.toNat
  let sound : Sound := { x := (input.body.position.x.toInt32.toInt), y := (input.body.position.y.toInt32.toInt) }
  let hk := liquids.hooks
  -- run a liquid edit on the player stream (vanilla is inside SwapRandom("UpdatePlayers"))
  let edit (m : OriginalLiquid.M Unit) (rng : UnifiedRandom) : OriginalLiquid.Runtime × UnifiedRandom × Nat :=
    let liquidRng := liquids.liquid.rng
    let draws := liquids.liquid.draws
    let ((), st') := (m.run { liquids.liquid with rng, draws := 0 } : Unit × OriginalLiquid.State)
    ({ liquids with liquid := { st' with rng := liquidRng, draws } }, st'.rng, st'.draws)
  let pickupCase := (item == 205 && !t.shimmer) || (item == 3032 && t.kind == 0) || (item == 3032 && t.shimmer) ||
    (item == 4872 && t.lava) || (item == 5303 && t.honey) || item == 5304
  if pickupCase then
    let kind := t.kind
    let mut sum := 0
    for i in [x - 1, x, x + 1] do
      for j in [y - 1, y, y + 1] do
        let c := s.cell i j
        if c.kind == kind then sum := sum + c.liquid
    if t.liquid == 0 then return idle
    if sum ≤ 100 && !isSponge stack.item then return idle
    let mut carry := carry
    let mut overflow := none
    let mut rng := playerRandom
    let mut extra := 0
    let mut sound := sound
    if !isSponge stack.item then
      let result : UInt16 := if t.honey then 1128 else if t.lava then 207 else if t.shimmer then 0 else 206
      if result == 0 then return idle
      carry := carry.setHeld (some { stack with count := stack.count - 1 })
      let (c, o, r, pickedUp) := putFromUsage carry result rng
      carry := c
      overflow := o
      rng := r
      sound := { sound with pickupBefore := pickedUp }
      if overflow.isSome then extra := 2
    -- PlaySound (IL_03f0) comes after PutItemInInventoryFromItemUsage and before the liquid edit
    if input.soundStarts then
      let (p, r) := rng.next (-10) 11
      rng := r
      sound := { sound with pitchDraw := some p }
      extra := extra + 1
    let (liquids, rng1, n) := edit (pickUp hk x y kind) rng
    return { timing := timing.setTime st.useTime, carry, liquids, playerRandom := rng1, sound := some sound,
             overflow, used := true, playerDraws := n + extra }
  -- pouring
  if t.liquid ≥ 200 then return idle
  if t.nactive && OriginalLiquid.solid false t.type && !OriginalLiquid.solidTop t.type && t.type != 546 then return idle
  let pourKind : Option Nat := match item with
    | 207 | 4820 => some 1
    | 206 | 3031 => some 0
    | 1128 | 5302 => some 2
    | 5364 => some 3
    | _ => none
  let some kind := pourKind | return idle
  if t.liquid != 0 && t.kind != kind then return idle
  -- PlaySound comes before the liquid is set, PutItemInInventoryFromItemUsage after it
  let (sound, rngS, soundDraws) := if input.soundStarts then
      let (p, r) := playerRandom.next (-10) 11
      ({ sound with pitchDraw := some p }, r, 1)
    else (sound, playerRandom, 0)
  let (liquids, rng0, n) := edit (pour hk x y kind) rngS
  let mut carry := carry
  let mut overflow := none
  let mut rng := rng0
  let mut sound := sound
  if [206, 207, 1128].contains item then
    carry := carry.setHeld (some { stack with count := stack.count - 1 })
    let (c, o, r, pickedUp) := putFromUsage carry 205 rng
    carry := c
    overflow := o
    rng := r
    sound := { sound with pickupAfter := pickedUp }
  return { timing := timing.setTime st.useTime, carry, liquids, playerRandom := rng, sound := some sound,
           overflow, used := true, playerDraws := soundDraws + n + (if overflow.isSome then 2 else 0) }

end Lean4Terria.OriginalLiquidBuckets
