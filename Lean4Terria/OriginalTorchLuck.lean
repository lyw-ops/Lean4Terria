import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalWorld

/-! `Player.torchLuck` of desktop 1.4.5.8: `Player.TryRecalculatingTorchLuck` and
`Player.UpdateTorchLuck_ConsumeCountersAndCalculate`, called for the local player from
`UpdateLuckFactors` in every `UpdateLuck` (`Player.Update` IL_24cb).

Below the surface the method scans one tile row per update, from 40 rows above the centre it
remembered at the last calculation to 40 rows below it, 40 columns each side, and notes every
torch style it sees. When the scan is done — or at once while that remembered centre is above
the surface or the player is dead — the noted styles are weighed against the player's zones and
the result (never below 0) becomes `torchLuck`. A fresh player remembers (0, 0), above the
surface, so its first update calculates.

The Torch God event (`happyFunTorchTime`, `TorchAttack`) is not ported: when it would start, the
step reports a boundary. IL: `.lake/forest-desktop-integration/death-runtime/il/`. -/
namespace Lean4Terria.OriginalTorchLuck

open OriginalCollision (Vector)

/-- `TorchID.Count` in 1.4.5.8 (`TorchID..cctor`). -/
def torchCount : Nat := 24

structure State where
  torchLuck : Float32 := 0
  luckyTorchCounter : Int := 0
  /-- `_nextTorchLuckCheckCenter`: the player's Center at the last calculation. -/
  nextCheckCenter : Vector := ⟨0, 0⟩
  nearbyTorch : Array Bool := Array.replicate torchCount false
  nearbyTorches : Int := 0
  dryCoralTorch : Bool := false
  torchGodCooldown : Int := 0
  happyFunTorchTime : Bool := false
  deriving BEq, Repr

/-- The player zones `UpdateBiomes` left in this update. -/
structure Zones where
  dungeon : Bool := false
  lihzhardTemple : Bool := false
  snow : Bool := false
  desert : Bool := false
  jungle : Bool := false
  glowshroom : Bool := false
  corrupt : Bool := false
  crimson : Bool := false
  hallow : Bool := false

structure Input where
  center : Vector
  position : Vector
  dead : Bool
  /-- The torch style of `inventory[selectedItem]` when its `createTile` is 4 (a torch item). -/
  selectedTorchStyle : Option Nat := none
  zones : Zones
  /-- `WorldGen.oceanDepths(Center.X / 16, Center.Y / 16)`. -/
  oceanDepths : Bool
  unlockedBiomeTorches : Bool
  /-- An item 5043 (Torch God's Favor) anywhere in `inventory`. -/
  hasTorchGodsFavor : Bool
  maxTilesX : Int
  maxTilesY : Int
  worldSurface : Float
  tileAt : Int → Int → Option OriginalWorld.Tile

private def clampI (lo hi v : Int) : Int := if v < lo then lo else if v > hi then hi else v

/-- `UpdateTorchLuck_ConsumeCountersAndCalculate`. A `some` boundary: the Torch God event starts. -/
def consume (s : State) (i : Input) : State × Option String := Id.run do
  let mut nearby := s.nearbyTorch
  if let some style := i.selectedTorchStyle then
    if style < torchCount then nearby := nearby.set! style true
  let has := fun (k : Nat) => nearby[k]?.getD false
  let z := i.zones
  let mut bad : Float32 := 0
  let mut good : Float32 := 0
  if !z.dungeon && !z.lihzhardTemple then
    -- (style, the zone where it is the right torch); a wrong place counts as bad
    for (style, right) in [(9, z.snow), (16, z.desert)] do
      if has style then
        if right then good := good + 1 else bad := bad + 1
    if has 17 then
      if i.oceanDepths then good := good + 1
      else if s.dryCoralTorch then bad := bad + 1
    for (style, right) in [(21, z.jungle), (22, z.glowshroom), (18, z.corrupt)] do
      if has style then
        if right then good := good + 1 else bad := bad + 1
    if has 8 && z.corrupt then good := good + 1
    if has 19 then
      if z.crimson then good := good + 1 else bad := bad + 1
    if has 11 && z.crimson then good := good + 1
    if has 20 then
      if z.hallow then good := good + 1 else bad := bad + 1
    if has 13 && !z.snow && !z.desert && !z.corrupt && !z.crimson && !z.hallow && !z.jungle then
      good := good + 0.5
    if has 0 then
      if z.snow then bad := bad + 2
      else if z.desert || z.corrupt || z.crimson || z.hallow then bad := bad + 0.5
  let mut luck : Float32 := 0
  if good ≥ 1 then luck := luck + 1
  else if good > 0 then luck := luck + 0.5
  if bad ≥ 2 then luck := luck + -1.5
  else if bad ≥ 1 then luck := luck + -1
  else if bad > 0 then luck := luck + -0.5
  if luck < 0 then luck := 0
  let godStarts := s.torchGodCooldown ≤ 0 && !i.unlockedBiomeTorches && s.nearbyTorches > 100 &&
    !s.happyFunTorchTime && i.position.y.toFloat > i.worldSurface * 16 && !i.hasTorchGodsFavor
  let next : State := { s with
    luckyTorchCounter := 0, torchLuck := luck, nextCheckCenter := i.center
    dryCoralTorch := false, nearbyTorch := Array.replicate torchCount false, nearbyTorches := 0
    happyFunTorchTime := s.happyFunTorchTime || godStarts }
  return (next, if godStarts then some "Torch God event (TorchAttack) not ported" else none)

/-- `TryRecalculatingTorchLuck`, one call. -/
def step (s : State) (i : Input) : State × Option String := Id.run do
  if s.happyFunTorchTime then return (s, some "Torch God event (TorchAttack) not ported")
  let s := { s with torchGodCooldown := if s.torchGodCooldown > 0 then s.torchGodCooldown - 1 else s.torchGodCooldown }
  let c := s.nextCheckCenter
  if c.y.toFloat < i.worldSurface * 16 || i.dead then return consume s i
  let range : Int := 40
  let cy := c.y.toInt32.toInt.tdiv 16
  let cx := c.x.toInt32.toInt.tdiv 16
  let top := cy - range
  let x0 := clampI 10 (i.maxTilesX - 10) (cx - range)
  let x1 := clampI 10 (i.maxTilesX - 10) (cx + range)
  let mut s := s
  let y := top + s.luckyTorchCounter
  if 10 ≤ y && y ≤ i.maxTilesY - 10 then
    let mut x := x0
    while x ≤ x1 do
      if let some tile := i.tileAt x y then
        if tile.id == some 4 then
          let (fx, fy) := tile.frame.getD (0, 0)
          if fx ≥ 0 && fy ≥ 0 then
            if fx < 66 then s := { s with nearbyTorches := s.nearbyTorches + 1 }
            let style := (fy.tdiv 22).toNat
            if style < torchCount then
              s := { s with nearbyTorch := s.nearbyTorch.set! style true }
              if style == 17 then
                let dry : Bool := match tile.liquid with
                  | none => true
                  | some liquid => liquid.amount == 0 || liquid.kind != OriginalWorld.LiquidKind.water
                if dry then s := { s with dryCoralTorch := true }
      x := x + 1
    if y ≥ cy + range then return consume s i
  return ({ s with luckyTorchCounter := s.luckyTorchCounter + 1 }, none)

end Lean4Terria.OriginalTorchLuck
