import Lean4Terria.OriginalLiquid.Core
import Lean4Terria.OriginalItems

/-!
# `WorldGen.KillTile` for tiles destroyed by liquid (1.4.5.8)

Random calls of `KillTile(x, y, fail = false, effectOnly = false, noItem = false)` for the single-cell
tiles liquids destroy, in vanilla order (IL: `integration/probe/il/WorldGen.KillTile*.il`,
`WorldGen.KillTile_DropBait.il`, `WorldGen.KillTile_GetItemDrops.short`):

1. `KillTile` IL_0a30: a cobweb (51) on spider wall (62) keeps its item only on `genRand.Next(4) == 0`.
2. `KillTile_DropBait`: per-type critter chances (worm / maggot, grasshopper, the 5574 bait item,
   ladybug-family, 653/654), each a `genRand.Next(chance)` when fewer than 5 (8) such critters live.
3. `KillTile_DropItems` → `KillTile_GetItemDrops`: mushroom / seed / jungle drops and their
   `Main.rand` rolls; a dropped item is `Item.NewItem`, whose velocity takes two draws
   (`rng.next(-30, 31)`, `rng.next(-40, -15)`, as `OriginalItems.spawnStackAt`).
4. the tile is cleared and `SquareTileFrame(x, y, true)` runs.

Before 1., the dust loop (`KillTile_GetTileDustAmount` × `KillTile_MakeTileDust`) takes one draw per
particle for the types `dustDraws` lists (the dust-type choice); dust itself is not simulated.

Inside `Liquid.UpdateLiquid`, `genRand` and `Main.rand` are both the swapped `UpdateLiquid` stream.
Dust and sounds take no draws on the probe (dedicated-server build, measured). Items are not created
here: `State.drops` records them with the two velocity draws already taken, for the caller to place.
A critter that spawns (a chance roll of 0) takes the gold-variant luck roll (worm, grasshopper), and
`NPC.NewNPC` is followed by two velocity draws; `State.spawns` records it for the caller.
Measured on the probe for short-grass plants: exactly three draws per kill (`Next(400)`, `Next(100)`,
`Next(2)`), `integration/trace/killtile_3*.json`.
-/

namespace Lean4Terria.OriginalLiquid.Kill

open Lean4Terria.OriginalLiquid

/-- What the nearest player and the live critters look like (`Player.FindClosest`, `NPC.CountNPCS`). -/
structure Env where
  flowerBoots : Bool := false
  graveyard : Bool := false
  /-- holds a Blowpipe (281) or Blowgun (986): `KillTile_ShouldDropSeeds` -/
  seedWeapon : Bool := false
  cordage : Bool := false
  /-- `Item.axe` of that player's selected item (`KillTile_GetTreeDrops`; 0 when it is not an axe) -/
  heldAxe : Nat := 0
  /-- `Player.luck` of that player (`Luck.RollLuck`) -/
  luck : Float32 := 0
  /-- `NPC.goldCritterChance` (400 unless an event changed it) -/
  goldCritterChance : Nat := 400
  /-- `CountNPCS(357 or 606)`, `CountNPCS(377)`, `CountNPCS(485..487)`, `CountNPCS(653..654)` -/
  worms : Nat := 0
  grasshoppers : Nat := 0
  ladybugs : Nat := 0
  critters653 : Nat := 0

def drawRange (lo hi : Int) : M Int := do
  let s ← get
  let (v, rng) := s.rng.next lo hi
  set { s with rng, draws := s.draws + 1 }
  return v

/-- `Utils.NextFloat(Main.rand)`: `(float)NextDouble()` -/
def nextFloat : M Float32 := do
  let s ← get
  let (v, rng) := s.rng.sample
  set { s with rng, draws := s.draws + 1 }
  return v.toFloat32

/-- `GameContent.Luck.RollLuck(luck, range)` (`Player.GetClosestRollLuck`) -/
def rollLuck (env : Env) (range : Nat) : M Int := do
  if env.luck > 0 then
    if (← nextFloat) < env.luck then
      let r ← drawRange (range / 2) range
      return (← draw r.toNat)
  if env.luck < 0 then
    if (← nextFloat) < -env.luck then
      let r ← drawRange range (range * 2)
      return (← draw r.toNat)
  return (← draw range)

/-- `NPC.NewNPC` at the tile (no draws on the probe) and the two velocity draws that follow. -/
def spawn (x y : Int) (npc : Nat) : M Unit := do
  let vy ← drawRange (-50) (-21)
  let vx ← drawRange 0 26
  modify fun s => { s with spawns := s.spawns.push { x, y, npc, drawX := vx, drawY := vy } }

/-- `Item.NewItem` at the tile: the two velocity draws, recorded for the caller. -/
def newItem (x y : Int) (item stack : Nat) : M Unit := do
  let vx ← drawRange (-30) 31
  let vy ← drawRange (-40) (-15)
  modify fun s =>
    let grid : OriginalCollision.Grid := {
      width := s.width, height := s.height, rules := OriginalTileRules.collision
      tileAt := fun x y => (s.cell x y).applyTo default }
    let position : OriginalCollision.Vector := ⟨OriginalCollision.single (x * 16),
      OriginalCollision.single (y * 16)⟩
    let wet := OriginalItems.spawnWet grid position
    { s with drops := s.drops.push { x, y, item, stack, drawX := vx, drawY := vy, wet } }

/-- `KillTile_DropBait` -/
def dropBait (env : Env) (x y : Int) (c : Cell) : M Unit := do
  let (fx, fy) := c.frame.getD (0, 0)
  let boost (n : Int) : Int := if env.flowerBoots && fx ≥ 108 then n * 10000 else n
  let t := c.type
  let mut n0 : Int := -1
  let mut n1 : Int := -1
  let mut n2 : Int := -1
  let mut n3 : Int := -1
  let mut n4 : Int := -1
  if t == 3 then n0 := boost 400; n1 := boost 100
  if t == 73 then
    if fx < 108 then n2 := 1000
    n0 := boost 200; n1 := boost 50
  if t == 637 then n4 := boost 100
  if t == 61 then n3 := boost 80
  if t == 74 then n3 := boost 40
  if t == 62 then n3 := 250
  if t == 185 then
    if fy == 0 && fx < 214 then n0 := 6
    if fy == 18 && (fx < 214 || fx ≥ 1368) then n0 := 6
  else if t == 186 then
    if fx ≥ 378 && fx ≤ 700 then n0 := 6
  else if t == 187 then
    if fx ≥ 756 && fx ≤ 916 then n0 := 6
    if fx ≤ 322 then n0 := 6
  else if t == 233 then n3 := 10
  let worm := if env.graveyard then 606 else 357
  if n0 > 0 && env.worms < 5 then
    if (← draw n0.toNat) == 0 then
      let mut npc := worm
      if worm == 357 then
        if (← rollLuck env env.goldCritterChance) == 0 then npc := 448
      spawn x y npc
  if n2 > 0 then
    if (← draw n2.toNat) == 0 then newItem x y 5574 1
  if n1 > 0 && env.grasshoppers < 5 then
    if (← draw n1.toNat) == 0 then
      let npc := if (← rollLuck env env.goldCritterChance) == 0 then 446 else 377
      spawn x y npc
  if n3 > 0 && env.ladybugs < 8 then
    -- `genRand.Next((int)((double)num4 * 1.25))`
    if (← draw (n3.toNat * 5 / 4)) == 0 then
      let mut npc := 485
      if (← draw 4) == 0 then npc := 486
      if (← draw 12) == 0 then npc := 487
      spawn x y npc
  if n4 > 0 && env.critters653 < 8 then
    if (← draw n4.toNat) == 0 then
      let npc := if (← draw 2) == 0 then 654 else 653
      spawn x y npc

/-- `KillTile_ShouldDropSeeds`: `Main.rand.Next(2) == 0` and the player carries a seed weapon. -/
def shouldDropSeeds (env : Env) : M Bool := do
  let v ← draw 2
  return v == 0 && env.seedWeapon

/-- `KillTile_GetItemDrops` for the single-cell types below; `(item, stack)`, item 0 = none. -/
def itemDrop (env : Env) (c : Cell) : M (Nat × Nat) := do
  let (fx, fy) := c.frame.getD (0, 0)
  match c.type with
  | 3 =>
    if fx == 144 then return (5, 1)
    return (if (← shouldDropSeeds env) then 283 else 0, 1)
  | 24 => return (if fx == 144 then 60 else 0, 1)
  | 110 => return (if fx == 144 then 5 else 0, 1)
  | 201 => return (if fx == 270 then 2887 else 0, 1)
  | 113 => return (0, 1)
  | 73 => return (if (← shouldDropSeeds env) then 283 else 0, 1)
  | 71 =>
    if (← draw 40) == 0 then return (194, 1)
    return (if (← draw 2) == 0 then 183 else 0, 1)
  | 61 | 74 =>
    if fx == 144 && c.type == 61 then
      let n ← drawRange 2 4
      return (331, n.toNat)
    if fx == 162 && c.type == 61 then return (223, 1)
    if fx ≥ 108 && fx ≤ 126 && c.type == 61 then
      if (← draw 20) == 0 then return (208, 1)
    return (if (← draw 100) == 0 then 195 else 0, 1)
  | 4 =>
    let style := (fy / 22).toNat
    let item := match style with
      | 0 => 8 | 8 => 523 | 9 => 974 | 10 => 1245 | 11 => 1333 | 12 => 2274 | 13 => 3004 | 14 => 3045
      | 15 => 3114 | 16 => 4383 | 17 => 4384 | 18 => 4385 | 19 => 4386 | 20 => 4387 | 21 => 4388
      | 22 => 5293 | 23 => 5353 | s => 426 + s
    return (item, 1)
  | 51 => return (150, 1)
  | 52 | 62 | 382 =>
    -- IL_1703: `Main.rand.Next(2) == 0` and the player has Cordage
    let v ← draw 2
    return (if v == 0 && env.cordage then 2996 else 0, 1)
  | 528 => return (if (← draw 2) == 0 then 183 else 0, 1)
  | _ => return (0, 1)

/-- `KillTile_GetTileDustAmount(fail = false, tile)`: 10 for every type here except the listed ones. -/
def dustAmount (c : Cell) : Nat :=
  let t := c.type
  if [125, 138, 172, 287, 461, 484, 499, 617, 654, 699, 709].contains t || (300 ≤ t && t ≤ 308) ||
      (354 ≤ t && t ≤ 355) || (373 ≤ t && t ≤ 376) || (564 ≤ t && t ≤ 565) || (593 ≤ t && t ≤ 594) ||
      (625 ≤ t && t ≤ 631) || (664 ≤ t && t ≤ 665) || (711 ≤ t && t ≤ 716) then 0
  else if t == 541 then 1 else if t == 549 then 2 else if 481 ≤ t && t ≤ 483 then 3
  else if t == 231 then 6 else if [534, 536, 539].contains t then 5
  else if t == 184 then (if (c.frame.getD (0, 0)).1 / 22 ≥ 5 then 5 else 10)
  else 10

/-- Random calls one `KillTile_MakeTileDust` makes for the tile (its dust-type choice), measured per type
on the probe (`integration/trace/dust_draws.json`): one for these types, none for the others measured. -/
def dustDraws (type : Nat) : Nat :=
  if [2, 23, 24, 27, 32, 109, 352].contains type then 1 else 0

/-- single-cell tiles this module kills completely -/
def singleCell : List Nat := [3, 4, 24, 51, 61, 71, 73, 74, 110, 113, 201, 52, 62, 115, 205, 382, 528, 636, 638]

/-- `WorldGen.KillTile(x, y)` for a single-cell tile (see the module comment). -/
def killSingleCell (hk : Hooks) (env : Env) (x y : Int) : M Unit := do
  let c ← getCell x y
  -- KillTile IL_057b: the dust loop (dust itself is not simulated; its random choice is)
  for _ in [:dustAmount c * dustDraws c.type] do
    let _ ← draw 2
  let mut noItem := false
  if c.type == 51 && c.wall == 62 then
    if (← draw 4) != 0 then noItem := true
  if !noItem then
    dropBait env x y c
    let (item, stack) ← itemDrop env c
    if item > 0 then newItem x y item stack
  setCell x y { c with id := none, actuated := false, shape := .full, frame := none }
  squareTileFrame hk x y true

end Lean4Terria.OriginalLiquid.Kill
