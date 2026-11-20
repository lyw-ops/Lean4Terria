import Lean4Terria.OriginalEyeOfCthulhu

/-!
# Boss-spawner items (main B): Suspicious Looking Eye (item 43)

The caller recipe the released `OriginalEyeOfCthulhu` (source 662a5c75…, leafkit ACCEPT-WITH-NOTES) verified on its native
summon cases, as one entry:

1. `Player.SummonItemCheck` (`ItemCheck_CheckCanUse_Inner` gate): false while an Eye is active — the item is not used and
   not consumed.
2. When the generic `ItemCheck` reaches `ItemCheck_UseBossSpawners` (`ItemTimeIsZero && itemAnimation > 0`): the item-43
   branch (`useSummonItem`). By day nothing happens (no item time, so nothing is consumed). By night: the roar request
   (through `Env.playSound`, which may draw `Main.rand`), `NPC.SpawnOnPlayer(plr, 4)`, `WorldGen.spawnEye = false`.
3. `Player.ApplyItemTime(item 43)`: `itemTime = itemTimeMax = 45` (no speed modifiers on this path). The consumption itself
   is the generic `ItemCheck`'s (`itemTime == itemTimeMax`).

NPC slot placement (`spawnSlotProtected = 2`), the Eye's AI and the natural spawn belong to main A.
-/
namespace Lean4Terria.OriginalPlayerActions.BossSpawners

open Lean4Terria.OriginalEyeOfCthulhu

/-- `Item.useTime` of item 43 (`ApplyItemTime`). -/
def suspiciousEyeUseTime : Nat := 45

structure EyeUse where
  /-- `SummonItemCheck`: false → the item is not used this frame (nothing below happened). -/
  canUse : Bool
  /-- `ApplyItemTime`: `some 45` by night, `none` by day / when refused. -/
  itemTime : Option Nat
  /-- `WorldGen.spawnEye = false`. -/
  clearsSpawnEye : Bool
  /-- The roar request (already passed through `Env.playSound`). -/
  sound : Option Sound
  /-- `Main.rand` after the branch, the Eye's slot/actor (caller places it, protected = 2) and the "has awoken" chat. -/
  spawn : SpawnResult

/-- Gate + item-43 branch + `ApplyItemTime`, for a frame where the generic `ItemCheck` reaches the boss-spawner call.
`activeTypes` = types of the active NPCs (`SummonItemCheck` reads them). -/
def useSuspiciousEye (env : Env) (tiles : SpawnTiles) (plr : Nat) (activeTypes : Array Nat) (random : UnifiedRandom) : EyeUse :=
  if !summonItemCheck activeTypes then
    { canUse := false, itemTime := none, clearsSpawnEye := false, sound := none, spawn := { random } }
  else
    let r := useSummonItem env tiles plr random
    { canUse := true, itemTime := if r.applyItemTime then some suspiciousEyeUseTime else none
      clearsSpawnEye := r.clearsSpawnEye, sound := r.sound, spawn := r.spawn }

end Lean4Terria.OriginalPlayerActions.BossSpawners
