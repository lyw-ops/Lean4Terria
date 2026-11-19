/-!
# `Player.luck` (Terraria desktop 1.4.5.8)

The value every `Luck.Roll*` consumer reads. Order inside one `Player.Update`:
`ResetEffects` (IL_2006) clears `equipmentBasedLuckBonus` (IL_0f33) and the five `hasLuck_*` flags;
`UpdateEquips` (IL_248f) sets the flags from the worn accessories (`ApplyEquipFunctional`) and then,
for the local player only (`whoAmI == Main.myPlayer`), adds the bonuses in a fixed order
(IL_04f2 … IL_05a3); `UpdateLuck` (IL_24cb) runs `UpdateLuckFactors` (ladybug timer, coin luck,
torch luck, broken mirror) and `RecalculateLuck`. The natural spawner copies the result in
`Spawner.SetSpawnFlags` IL_002c.

Owner boundary: `torchLuck` (`TryRecalculatingTorchLuck` / `UpdateTorchLuck_ConsumeCountersAndCalculate`,
fed by the `nearbyTorch` tile scan), `ladyBugLuckTimeLeft`, `coinLuck`, `luckPotion`,
`kiteLuckLevel`, `usedGalaxyPearl`, `LanternNight.LanternsUp`, `HasGardenGnomeNearby`, `stinky` and
`brokenMirrorBadLuck` are player / world state the caller owns; this module only combines them
exactly. Evidence: `.lake/original-spawn-luck/`.
-/
namespace Lean4Terria.OriginalLuck

/-- The worn-accessory flags `UpdateEquips` reads (`Player.hasLuck_*`). -/
structure Accessories where
  luckyClover : Bool := false
  wiltedClover : Bool := false
  ravenFeather : Bool := false
  luckyHorseshoe : Bool := false
  luckyCoin : Bool := false
  deriving Repr, BEq, Inhabited

/-- `UpdateEquips` IL_04f2 … IL_05a3, starting from the 0 `ResetEffects` left: +0.03 clover,
−0.1 wilted clover, −0.1 raven feather, +0.05 horseshoe, +0.05 lucky coin, each a Single add in
this order and only for the local player. -/
def equipmentBonus (isLocal : Bool) (a : Accessories) : Float32 := Id.run do
  let mut b : Float32 := 0
  if isLocal && a.luckyClover then b := b + 0.03
  if isLocal && a.wiltedClover then b := b - 0.1
  if isLocal && a.ravenFeather then b := b - 0.1
  if isLocal && a.luckyHorseshoe then b := b + 0.05
  if isLocal && a.luckyCoin then b := b + 0.05
  return b

/-- `Player.GetLadyBugLuck`: `(float)t / (float)goodTime` for `t > 0`, `-(float)t / (float)badTime`
for `t < 0`. The statics are `NPC.ladyBugGoodLuckTime = 43200` and `NPC.ladyBugBadLuckTime = -10800`
in 1.4.5.8 (read natively; the negative divisor makes bad ladybug luck negative). -/
def ladyBugLuck (timeLeft goodTime badTime : Int) : Float32 :=
  if timeLeft > 0 then (Float.ofInt timeLeft).toFloat32 / (Float.ofInt goodTime).toFloat32
  else if timeLeft < 0 then -(Float.ofInt timeLeft).toFloat32 / (Float.ofInt badTime).toFloat32
  else 0

/-- `Player.CalculateCoinLuck`. -/
def coinLuckBonus (coinLuck : Float32) : Float32 :=
  if coinLuck == 0 then 0
  else if coinLuck > 249000 then 0.2
  else if coinLuck > 24900 then 0.175
  else if coinLuck > 2490 then 0.15
  else if coinLuck > 249 then 0.125
  else if coinLuck.toFloat > 24.9 then 0.1
  else if coinLuck.toFloat > 2.49 then 0.075
  else if coinLuck.toFloat > 0.249 then 0.05
  else 0.025

/-- Everything `RecalculateLuck` reads, as the player / world holds it at that point. -/
structure Factors where
  ladyBugLuck : Float32 := 0
  torchLuck : Float32 := 0
  luckPotion : UInt8 := 0
  kiteLuckLevel : UInt8 := 0
  usedGalaxyPearl : Bool := false
  lanternsUp : Bool := false
  hasGardenGnomeNearby : Bool := false
  stinky : Bool := false
  equipmentBasedLuckBonus : Float32 := 0
  coinLuck : Float32 := 0
  brokenMirrorBadLuck : Bool := false
  deriving Repr, Inhabited

/-- `Player.RecalculateLuck` with every Single operation in CIL order (no clamp is applied here). -/
def recalculate (f : Factors) : Float32 := Id.run do
  let mut luck : Float32 := f.ladyBugLuck * 0.2 + f.torchLuck * 0.2
  luck := luck + (Float.ofNat f.luckPotion.toNat).toFloat32 * 0.1
  luck := luck + (Float.ofNat f.kiteLuckLevel.toNat).toFloat32 * 0.1 / 3
  if f.usedGalaxyPearl then luck := luck + 0.03
  if f.lanternsUp then luck := luck + 0.3
  if f.hasGardenGnomeNearby then luck := luck + 0.2
  if f.stinky then luck := luck - 0.25
  luck := luck + f.equipmentBasedLuckBonus
  luck := luck + coinLuckBonus f.coinLuck
  if f.brokenMirrorBadLuck then luck := luck - 0.25
  return luck

end Lean4Terria.OriginalLuck
