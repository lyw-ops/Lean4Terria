import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalRecipeCraft

namespace Lean4Terria.OriginalCombat.Loot

open OriginalCollision (Grid Vector single)
open OriginalGreenSlime (Slime)
open OriginalItems (Drop Stack)

/-- Ordinary local Classic player at zero luck. The preview has no mana-consuming
weapon, but the native drop rule still draws its mana rolls. -/
structure Context where
  grid : Option Grid := none
  needsLife : Bool := false
  needsMana : Bool := false
  bloodMoon : Bool := false
  /-- `Player.luck` of the player the drop rules ask. `rollLuck`, `dropMoneyAt`, `dropHealsAt` and the
  slime rules use it; NPC-specific rules elsewhere that roll at luck 0 make the caller stop. -/
  luck : Float32 := 0
  /-- `Conditions.EyeOfCthulhuDefeatedAndNoAltarsInWorld`: `NPC.downedBoss1 && Skyblock.noAltars`.
  `none` when the caller cannot tell (Eye defeated, altar scan not tracked). -/
  eyeDefeatedNoAltars : Option Bool := some false

structure Result where
  drops : Array Drop := #[]
  random : UnifiedRandom

private def Result.next (result : Result) (lo hi : Int) : Int × Result :=
  let (value, random) := result.random.next lo hi
  (value, { result with random })

/-- `Utils.NextFloat`: `(float)NextDouble()`. -/
private def Result.nextFloat (result : Result) : Float32 × Result :=
  let (value, random) := result.random.sample
  (value.toFloat32, { result with random })

/-- `Player.RollLuck(range)` = `Luck.RollLuck(luck, range)` (IL `death-runtime/il/Luck.RollLuck.il`): with
`luck > 0` and `NextFloat() < luck`, `Next(Next(range / 2, range))`; with `luck < 0` and
`NextFloat() < -luck`, `Next(Next(range, range * 2))`; otherwise `Next(range)` (a failed float draw stays
taken). At luck 0 this is the plain `Next(range)` the rules used before. -/
def rollLuck (luck : Float32) (range : Int) (result : Result) : Int × Result := Id.run do
  let mut result := result
  if luck > 0 then
    let (f, next) := result.nextFloat
    result := next
    if f < luck then
      let (inner, next) := result.next (range.tdiv 2) range
      return next.next 0 inner
  else if luck < 0 then
    let (f, next) := result.nextFloat
    result := next
    if f < -luck then
      let (inner, next) := result.next range (range * 2)
      return next.next 0 inner
  return result.next 0 range

def dropPosition (position : Vector) (width height : Nat) : Vector :=
  ⟨single (position.x.toInt32.toInt + (width / 2 : Nat) - 8),
   single (position.y.toInt32.toInt + (height / 2 : Nat) - 8)⟩

private def push (result : Result) (env : Context) (position : Vector) (item : UInt16)
    (count : Nat) (naturalPrefix : Bool := false) (color : Option UInt32 := none) : Result :=
  let (prefixId, random) := if naturalPrefix then OriginalRecipeCatalog.rollNaturalPrefix item result.random
                           else (0, result.random)
  let (drop, random) := OriginalItems.rollDropAt position
    { Stack.ofItem item count with prefixId, color } random env.grid
  { drops := result.drops.push drop, random }

/-- SlimeBodyItemDropRule.GetDropInfo. Endpoints are inclusive here. -/
private def bodyRange (item : Int) : Int × Int :=
  if #[2, 3, 9, 150, 593, 751, 1103, 3081, 3086, 3609, 3610, 5395].contains item then (10, 25)
  else if #[11, 12, 13, 14, 174, 364, 365, 366, 699, 700, 701, 702, 1104, 1105, 1106, 3347].contains item then (3, 13)
  else if #[147, 314, 1124, 1125, 1345, 3736, 3737, 3738].contains item then (2, 5)
  else match item with
    | 8 => (5, 10)
    | 166 => (2, 6)
    | 965 => (20, 45)
    | 71 => (50, 99)
    | 72 => (20, 99)
    | 73 => (1, 2)
    | 4343 | 4344 => (2, 5)
    | _ => (1, 1)

def dropItems (env : Context) (slime : Slime) (result : Result) : Result := Id.run do
  let mut result := result
  let position := dropPosition slime.position slime.width slime.height
  let held := slime.ai1.toInt32.toInt
  if held > 0 && held < 6196 then
    let (lo, hi) := bodyRange held
    let (stack, next) := result.next lo (hi + 1)
    result := push next env position held.toNat.toUInt16 stack.toNat true
  -- DropBasedOnExtraGel (Classic: its first rule): gel 1..2, or 2..5 for netID -6..-9
  -- (native drop-rule survey `.lake/forest-desktop-integration/slime-variants/npc-drops-slimes.json`).
  let (lo, hi) : Int × Int := if slime.netId ≤ -6 && slime.netId ≥ -9 then (2, 5) else (1, 2)
  -- both rules are `CommonDrop` (`RollLuck(denominator) < 1`)
  let (_, next) := rollLuck env.luck 1 result
  let (stack, next) := next.next lo (hi + 1)
  result := push next env position 23 stack.toNat false (some slime.color.rgba)
  let (staff, next) := rollLuck env.luck 10000 result
  if staff != 0 then return next
  let (_, next) := next.next 1 2
  return push next env position 1309 1 true

private def coinPercentage (value : Float32) (percent : Int) : Float32 :=
  value * (1 + single percent * 0.01)

/-- Ordinary NPC coin variance followed by the native denomination and stack splits.
`NPCLoot_DropMoney` IL_0000..01d5: `NextFloat() < |luck|` rolls the value twice and keeps the larger
(the smaller with bad luck); the float is drawn at luck 0 too. No Midas here. -/
def dropMoneyAt (env : Context) (position : Vector) (baseValue : Float32) (result : Result) : Result := Id.run do
  let (f, next) := result.nextFloat
  let mut result := next
  let rolls : Nat := if f < Float32.abs env.luck then 2 else 1
  let mut value : Float32 := 0
  for i in [:rolls] do
    let (variance, next) := result.next (-20) 76
    result := next
    let mut rolled := coinPercentage baseValue variance
    for (denominator, lo, hi) in #[(2, 5, 11), (4, 10, 21), (8, 15, 31),
          (16, 20, 41), (32, 25, 51), (64, 50, 101)] do
      let (roll, next) := result.next 0 denominator
      result := next
      if roll == 0 then
        let (extra, next) := result.next lo hi
        result := next
        rolled := coinPercentage rolled extra
    if env.bloodMoon then
      let (extra, next) := result.next 0 101
      result := next
      rolled := coinPercentage rolled extra
    if i == 0 then value := rolled
    else if env.luck < 0 then
      if rolled < value then value := rolled
    else if rolled > value then value := rolled
  while value.toInt32 > 0 do
    let (item, unit, lastDivisor) : UInt16 × Int × Int :=
      if value > 1000000 then (74, 1000000, 4)
      else if value > 10000 then (73, 10000, 4)
      else if value > 100 then (72, 100, 4)
      else (71, 1, 5)
    let mut count := (value / single unit).toInt32.toInt
    if count > 50 then
      let (split, next) := result.next 0 5
      result := next
      if split == 0 then
        let (divisor, next) := result.next 1 4
        result := next
        count := count / divisor
    let (split, next) := result.next 0 5
    result := next
    if split == 0 then
      let (divisor, next) := result.next 1 lastDivisor
      result := next
      count := count / divisor
    if item == 71 then count := max 1 count
    value := value - single (count * unit)
    if count > 0 then result := push result env position item count.toNat
  return result

def dropMoney (env : Context) (slime : Slime) (result : Result) : Result :=
  dropMoneyAt env (dropPosition slime.position slime.width slime.height) slime.value result

/-- `NPCLoot_DropCommonLifeAndMana` for an ordinary NPC (not 16 / 81 / 121): `RollLuck(6)`, then the
plain `Next(2)` mana / heart choice, then `RollLuck(2)` for the extra star. -/
def dropHealsAt (env : Context) (position : Vector) (eligible : Bool) (result : Result) : Result := Id.run do
  let (first, next) := rollLuck env.luck 6 result
  let mut result := next
  if first == 0 && eligible then
    let (mana, next) := result.next 0 2
    if mana == 0 && env.needsMana then
      result := push next env position 184 1
    else
      let (heart, next) := next.next 0 2
      result := if heart == 0 && env.needsLife then push next env position 58 1 else next
  let (mana, next) := rollLuck env.luck 2 result
  return if mana == 0 && eligible && env.needsMana then push next env position 184 1 else next

def dropHeals (env : Context) (slime : Slime) (result : Result) : Result :=
  dropHealsAt env (dropPosition slime.position slime.width slime.height) (slime.lifeMax > 1 && slime.damage > 0) result

def roll (env : Context) (slime : Slime) (random : UnifiedRandom) : Result :=
  dropHeals env slime (dropMoney env slime (dropItems env slime { random }))

private def dropCommon (env : Context) (position : Vector) (item : UInt16)
    (denominator : Int) (result : Result) : Bool × Result :=
  let (roll, next) := result.next 0 denominator
  if roll != 0 then (false, next) else
  let (_, next) := next.next 1 2
  (true, push next env position item 1 true)

/-- Ordinary Classic `NPC.NPCLoot` for type 35 or 36, after `StrikeNPC_Inner`
and its hit effects. The head's trophy rule precedes the boss-specific
rules. Mask, hook and spellbook are a chain on failed rolls. -/
def skeletron (env : Context) (position : Vector) (head healsEligible : Bool)
    (random : UnifiedRandom) : Result := Id.run do
  let mut result : Result := { random }
  if head then
    result := (dropCommon env position 1363 10 result).2
    for item in #[1281, 1273, 1313] do
      let (dropped, next) := dropCommon env position item 7 result
      result := next
      if dropped then break
    result := (dropCommon env position 4993 7 result).2
    let (potions, next) := result.next 5 16
    result := push next env position 188 potions.toNat
    let (hearts, next) := result.next 5 10
    result := next
    for _ in [:hearts.toNat] do
      result := push result env position 58 1
  result := dropMoneyAt env position (if head then 50000 else 0) result
  return dropHealsAt env position healsEligible result

/-- Type 68 has a guaranteed Bone Key rule and goes through the ordinary
coin and recovery phases. -/
def dungeonGuardian (env : Context) (position : Vector) (random : UnifiedRandom) : Result :=
  let key := (dropCommon env position 1169 1 { random }).2
  dropHealsAt env position true (dropMoneyAt env position 0 key)

end Lean4Terria.OriginalCombat.Loot
