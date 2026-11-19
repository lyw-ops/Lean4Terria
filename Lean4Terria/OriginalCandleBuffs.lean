/-! Terraria 1.4.5.8 environmental candle Buffs. The caller owns the ordinary
Buff effects and timer phase; this module supplies the preceding candle refresh.
Source and native continuous comparisons: `.lake/original-candle-buffs/REPORT.md`. -/
namespace Lean4Terria.OriginalCandleBuffs

structure Zones where
  water : Bool := false
  peace : Bool := false
  shadow : Bool := false
  deriving BEq, Repr, Inhabited

structure Counts where
  water : Nat := 0
  peace : Nat := 0
  shadow : Nat := 0
  deriving BEq, Repr, Inhabited

/-- SceneMetrics.ScanTiles IL_00d3..014b. Feed each tile from the caller's
existing 169 × 124 zone scan. Null/inactive tiles do not count; active candle
tiles count when frameX < 18. Shape, actuation, liquid, wall and frameY do not
participate in this condition. No second scan is needed. -/
def Counts.addTile (counts : Counts) (active : Bool) (tileType : Nat) (frameX : Int) : Counts :=
  if !active || frameX ≥ 18 then counts else
    { water := counts.water + if tileType == 49 then 1 else 0
      peace := counts.peace + if tileType == 372 then 1 else 0
      shadow := counts.shadow + if tileType == 646 then 1 else 0 }

/-- CalculateZones IL_03a4..03cc compares the three independent counts to zero.
Water and Peace do not suppress each other's zone or icon. -/
def Counts.zones (counts : Counts) : Zones :=
  { water := counts.water > 0, peace := counts.peace > 0, shadow := counts.shadow > 0 }

structure SpawnNumbers where
  rate : Int
  maximum : Int
  deriving BEq, Repr, Inhabited

/-- Spawner.GetSpawnRate IL_0892..08e3. Call at this point in the original
pipeline, before later effects and final clamps. Both flags keep these numbers
unchanged. Rate uses Double; maximum uses Single before truncation to Int32. -/
def spawnFactors (zones : Zones) (numbers : SpawnNumbers) : SpawnNumbers :=
  if zones.water && !zones.peace then
    { rate := (numbers.rate.toFloat * 0.75).toInt32.toInt
      maximum := ((Int32.ofInt numbers.maximum).toFloat32 * 1.5).toInt32.toInt }
  else if zones.peace && !zones.water then
    { rate := (numbers.rate.toFloat * 1.3).toInt32.toInt
      maximum := ((Int32.ofInt numbers.maximum).toFloat32 * 0.7).toInt32.toInt }
  else numbers

/-- The immediately following GetSpawnRate IL_08e4..091e water/sky test is
independent of Peace. Player.position.Y / 16 is Single; the worldSurface
threshold and rate multiplication are Double. -/
def waterSkyFactor (zones : Zones) (playerY : Float32) (worldSurface : Float)
    (rate : Int) : Int :=
  if zones.water && (playerY / 16).toFloat < worldSurface * 0.3499999940395355 then
    (rate.toFloat * 0.5).toInt32.toInt
  else rate

structure SpawnFlags where
  townNPCs : Int
  noWorms : Bool
  noGroundWorms : Bool
  deriving BEq, Repr, Inhabited

/-- Spawner.SetSpawnFlags IL_03c2..03de, after copying the player's zone bits
and establishing the ordinary town/worm flags. -/
def shadowSpawnFlags (zones : Zones) (flags : SpawnFlags) : SpawnFlags :=
  if zones.shadow then {townNPCs := 0, noWorms := false, noGroundWorms := false}
  else flags

/-- SetSpawnFlagsForChosenTile IL_0000..0016, later in the spawn pipeline.
Use the stored noGroundWorms after Shadow's reset; do not recompute it from AFK. -/
def SpawnFlags.forChosenTile (flags : SpawnFlags) (skyMob : Bool) : SpawnFlags :=
  if !skyMob && flags.noGroundWorms then { flags with noWorms := true }
  else flags

/-- SceneMetrics.AddPlayerEffects runs after the placed-tile zone calculation.
Only the selected item's type matters; its stack, holdStyle and buffType do not. -/
def Zones.withHeld (zones : Zones) (selectedItemType : Nat) : Zones :=
  { water := zones.water || selectedItemType == 148
    peace := zones.peace || selectedItemType == 3117
    shadow := zones.shadow || selectedItemType == 5322 }

structure Slots where
  types : Array Nat := Array.replicate 44 0
  times : Array Int := Array.replicate 44 0
  deriving BEq, Repr, Inhabited

structure Input where
  /-- Final PlayerSceneMetrics zones, including AddPlayerEffects / held items. -/
  zones : Zones
  localPlayer : Bool := true
  /-- Immunities left by the previous update, before Update clears the array. -/
  immune : Array Bool := #[]
  deriving Repr, Inhabited

/-- Main.debuff from the installed 1.4.5.8 desktop image. AddBuff uses this
classification to evict an ordinary Buff when all 44 slots are occupied. -/
def desktopDebuff (id : Nat) : Bool :=
  (#[20,21,22,23,24,25,28,30,31,32,33,34,35,36,37,38,39,43,44,46,47,
     67,68,69,70,72,80,86,87,88,89,94,103,119,120,137,144,145,146,147,
     148,149,153,156,157,158,160,163,164,169,183,186,189,194,195,196,
     197,199,203,204,215,320,321,323,324,332,333,334,344,350,353,395,
     397,398,399,400] : Array Nat).contains id

/-- Player.DelBuff compacts only indices 0 through maxBuffs-2. Slot 43 remains
in place, including when an earlier expired slot is removed. -/
def Slots.remove (slots : Slots) (slot : Nat) : Slots := Id.run do
  let mut result : Slots := {
    types := slots.types.set! slot 0
    times := slots.times.set! slot 0 }
  let mut free := 0
  for index in [:43] do
    if result.types[index]! != 0 && result.times[index]! != 0 then
      if free < index then
        result := {
          types := (result.types.set! free result.types[index]!).set! index 0
          times := (result.times.set! free result.times[index]!).set! index 0 }
      free := free + 1
  return result

private def add (slots : Slots) (id : Nat) (isDebuff : Nat → Bool) : Slots := Id.run do
  -- The three candle IDs have no additive cap, pet/melee exclusivity,
  -- difficulty multiplier, or potion-station duration multiplier.
  if let some index := slots.types.findIdx? (· == id) then
    return { slots with times := slots.times.set! index (max slots.times[index]! 2) }
  let mut result := slots
  -- Each failed insertion evicts one ordinary Buff. The bounded loop also
  -- handles the native last-slot compaction behavior without partial recursion.
  for _ in [:45] do
    let some first := result.types.findIdx? (fun t => !isDebuff t) | return result
    if let some free := (List.range 44).find? (fun i => i ≥ first && result.types[i]! == 0) then
      return { types := result.types.set! free id, times := result.times.set! free 2 }
    result := result.remove first
  return result

/-- Player.Update IL_2155..21ad, after ResetEffects and before UpdateBuffs.
This refreshes 86, 157, 350 in source order and consumes no random samples.
The ordinary Buff phase must subsequently decrement timers exactly once. -/
def refresh (slots : Slots) (input : Input)
    (isDebuff : Nat → Bool := desktopDebuff) : Except String Slots := do
  unless slots.types.size == 44 && slots.times.size == 44 do
    throw "Candle refresh requires the 44 native Buff slots"
  if !input.localPlayer then return slots
  let mut result := slots
  for (id,active) in #[(86,input.zones.water),(157,input.zones.peace),(350,input.zones.shadow)] do
    if active && !input.immune[id]?.getD false then result := add result id isDebuff
  return result

/-- Player.Update IL_23c5..2406, after Buff effects. Call only if the caller
does not already perform native expired-slot cleanup. The loop intentionally
increments after compaction, as the original does. It never refreshes timers. -/
def clearExpired (slots : Slots) (localPlayer : Bool := true) : Slots := Id.run do
  if !localPlayer then return slots
  let mut result := slots
  for index in [:44] do
    if result.types[index]! > 0 && result.times[index]! ≤ 0 then
      result := result.remove index
  return result

end Lean4Terria.OriginalCandleBuffs
