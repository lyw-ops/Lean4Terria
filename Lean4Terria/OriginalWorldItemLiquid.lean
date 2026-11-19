import Lean4Terria.OriginalItems
import Lean4Terria.OriginalPlayerWet

/-!
# World items in liquid (Terraria 1.4.5.8)

Handwritten from `WorldItem.UpdateItem`, `WorldItem.MoveInWorld`, `WorldItem.ApplyMovement`,
`WorldItem.CheckLavaDeath`, `WorldItem.CheckInWorld` and `Collision.LavaCollision` of
`TerrariaServer.exe` 1.4.5.8. Liquid flags travel with the world item through spawning,
merging, and pickup.

`UpdateItem`, for an item that is not being grabbed:

1. The physics constants come from last frame's flags, and are fixed before the item combines.

   | Flag | gravity | maxFall | wetVelocity |
   |---|---|---|---|
   | `shimmerWet` | 0.065 | 4 | `velocity * 0.375` |
   | `honeyWet` | 0.05 | 3 | `velocity * 0.25` |
   | `wet` | 0.08 | 5 | `velocity * 0.5` |
   | dry | 0.1 | 7 | `velocity * 0.5` |

2. `TryCombiningIntoNearbyItems`, the same as in `updateDry`.
3. `MoveInWorld`:
   * gravity, capped at `maxFall`, then `vx *= 0.95` with the 0.1 dead zone;
   * `LavaCollision` (full hitbox, lava cells only) sets `lavaWet`;
   * `WetCollision` (the centred probe of `OriginalPlayerWet.wetCollision`, any liquid) sets
     `honeyWet` and `shimmerWet`;
   * entering or leaving sets `wetCount = 20` when it was 0, then `wetCount -= 1`;
   * `TileCollision`. While wet, an axis the collision changed overrides `wetVelocity`;
   * `ApplyMovement`: wet items move by `wetVelocity`, dry ones by `velocity`;
   * `SlopeCollision`.
4. With `lavaWet`, `CheckLavaDeath` removes an item whose rarity is 0 or -1, unless it is
   in `ItemID.Sets.IsLavaImmuneRegardlessOfRarity`.
5. `CheckInWorld`: an item more than 20 tiles outside the world disappears.

An item being grabbed clears every liquid flag and `wetCount`, then moves by `velocity`.

`Item.NewItem` sets only `wet = Collision.WetCollision(...)` at spawn.

The splash dust and `PlaySound(19)` on entering and leaving draw from `Main.rand`. They change
no item state, and this module does not draw them.

Not modelled:
* the shimmer transmutation (`shimmered`, `shimmerTime`, `GetShimmered`). With `shimmerWet` only
  the constants of step 1 are used;
* `TryFillBucket` (item 205), the Guide Voodoo Doll (267);
* conveyors, `RecoverableImportantItem`;
* despawn rules (75, 4143, 3822, ...), and items picked up by enemies.
-/

namespace Lean4Terria.OriginalWorldItemLiquid

open OriginalCollision (Grid Vector single)
open OriginalItems (Drop WorldItems)

/-- World items are 16 × 16 (`WorldItem` keeps the `Entity` size, whatever the item's own size). -/
def itemSize : Nat := 16

/-! ## Item tables (vanilla 1.4.5.8, `ItemID.Count = 6196`) -/

/-- Items with `rare` 0 or -1 that are not in `IsLavaImmuneRegardlessOfRarity`: 3,260 types,
read from the running game (`.lake/original-world-item-liquid/1.4.5.8/traces/item_lava_table.json`). -/
def burnRanges : Array (Nat × Nat) := #[
  (1, 16), (19, 28), (30, 42), (48, 48), (52, 52), (58, 63), (66, 69), (71, 74), (76, 83), (87, 87),
  (89, 94), (97, 97), (99, 99), (105, 108), (110, 110), (116, 116), (118, 118), (126, 126), (129, 147), (149, 150),
  (154, 154), (161, 161), (166, 166), (168, 172), (176, 184), (192, 192), (194, 196), (208, 210), (214, 214), (221, 222),
  (224, 225), (235, 236), (239, 255), (259, 264), (267, 267), (269, 271), (275, 276), (278, 283), (286, 287), (306, 311),
  (313, 317), (319, 356), (358, 363), (370, 370), (392, 392), (398, 398), (408, 409), (412, 421), (424, 424), (427, 433),
  (438, 480), (488, 488), (498, 498), (511, 513), (529, 530), (538, 543), (577, 577), (580, 580), (583, 598), (604, 661),
  (664, 664), (673, 673), (677, 677), (680, 681), (687, 708), (710, 722), (727, 740), (745, 747), (750, 752), (762, 771),
  (775, 775), (789, 791), (803, 820), (824, 831), (833, 838), (845, 847), (849, 849), (852, 853), (858, 859), (881, 884),
  (894, 896), (911, 927), (929, 929), (931, 931), (937, 937), (949, 949), (952, 952), (954, 955), (965, 968), (970, 974),
  (978, 981), (985, 985), (988, 988), (995, 1000), (1071, 1103), (1120, 1120), (1124, 1126), (1130, 1130), (1134, 1140), (1142, 1154),
  (1168, 1168), (1173, 1177), (1245, 1246), (1263, 1263), (1267, 1272), (1282, 1283), (1292, 1292), (1298, 1298), (1304, 1304), (1315, 1315),
  (1330, 1330), (1337, 1339), (1344, 1348), (1372, 1429), (1431, 1443), (1447, 1448), (1451, 1502), (1509, 1512), (1528, 1532), (1538, 1545),
  (1551, 1551), (1573, 1577), (1589, 1594), (1614, 1614), (1702, 1723), (1725, 1773), (1775, 1781), (1783, 1783), (1785, 1786), (1788, 1796),
  (1808, 1809), (1812, 1824), (1828, 1828), (1836, 1836), (1838, 1843), (1846, 1854), (1867, 1868), (1872, 1908), (1912, 1913), (1917, 1917),
  (1922, 1922), (1924, 1926), (1932, 1945), (1948, 1957), (1966, 1968), (1970, 1976), (1988, 1989), (1991, 1991), (1993, 1994), (1998, 1998),
  (2003, 2003), (2005, 2006), (2008, 2103), (2114, 2155), (2157, 2160), (2162, 2175), (2178, 2187), (2190, 2198), (2203, 2208), (2210, 2213),
  (2222, 2222), (2224, 2265), (2271, 2272), (2274, 2274), (2278, 2278), (2281, 2283), (2288, 2291), (2297, 2302), (2316, 2316), (2333, 2333),
  (2337, 2339), (2352, 2353), (2357, 2358), (2376, 2419), (2431, 2435), (2439, 2449), (2490, 2490), (2495, 2500), (2503, 2534), (2536, 2550),
  (2552, 2583), (2586, 2586), (2591, 2606), (2610, 2610), (2612, 2620), (2625, 2672), (2677, 2700), (2702, 2738), (2740, 2741), (2743, 2748),
  (2751, 2755), (2772, 2773), (2775, 2775), (2777, 2778), (2780, 2780), (2782, 2783), (2785, 2785), (2787, 2794), (2803, 2855), (2860, 2861),
  (2865, 2868), (2881, 2881), (2886, 2887), (2995, 2996), (3003, 3005), (3046, 3050), (3055, 3059), (3066, 3067), (3077, 3083), (3086, 3089),
  (3091, 3092), (3094, 3094), (3100, 3101), (3111, 3116), (3125, 3182), (3184, 3189), (3214, 3222), (3229, 3234), (3238, 3240), (3242, 3244),
  (3246, 3248), (3254, 3257), (3263, 3265), (3270, 3278), (3338, 3348), (3453, 3455), (3462, 3463), (3465, 3465), (3478, 3521), (3563, 3563),
  (3565, 3565), (3573, 3576), (3584, 3584), (3596, 3596), (3617, 3617), (3627, 3627), (3644, 3662), (3665, 3720), (3722, 3724), (3732, 3741),
  (3743, 3746), (3748, 3748), (3751, 3756), (3760, 3762), (3782, 3782), (3822, 3822), (3847, 3851), (3853, 3853), (3861, 3862), (3884, 3920),
  (3931, 3978), (3988, 3988), (4010, 4010), (4041, 4048), (4050, 4053), (4058, 4058), (4071, 4073), (4083, 4088), (4090, 4091), (4096, 4127),
  (4139, 4143), (4145, 4236), (4238, 4241), (4260, 4261), (4265, 4268), (4277, 4280), (4298, 4316), (4319, 4320), (4326, 4332), (4342, 4344),
  (4349, 4353), (4355, 4355), (4359, 4360), (4364, 4364), (4373, 4376), (4380, 4380), (4383, 4388), (4390, 4391), (4395, 4398), (4401, 4402),
  (4404, 4404), (4415, 4416), (4420, 4420), (4424, 4424), (4445, 4449), (4457, 4459), (4461, 4462), (4464, 4466), (4473, 4475), (4480, 4481),
  (4484, 4540), (4547, 4548), (4554, 4557), (4564, 4586), (4626, 4647), (4655, 4661), (4667, 4668), (4673, 4673), (4682, 4682), (4685, 4686),
  (4689, 4700), (4702, 4702), (4704, 4706), (4708, 4710), (4712, 4713), (4717, 4729), (4743, 4743), (4831, 4844), (4846, 4846), (4848, 4848),
  (4850, 4867), (4871, 4871), (4880, 4880), (4882, 4895), (4907, 4907), (4915, 4915), (4962, 4964), (4978, 4978), (4986, 4986), (5013, 5013),
  (5066, 5067), (5085, 5087), (5121, 5124), (5132, 5133), (5148, 5210), (5212, 5275), (5279, 5284), (5293, 5293), (5299, 5301), (5307, 5308),
  (5310, 5321), (5327, 5328), (5344, 5344), (5351, 5351), (5353, 5353), (5356, 5357), (5363, 5363), (5365, 5376), (5378, 5380), (5388, 5389),
  (5395, 5397), (5399, 5399), (5417, 5436), (5438, 5450), (5471, 5472), (5483, 5483), (5486, 5494), (5497, 5498), (5511, 5512), (5516, 5516),
  (5518, 5518), (5520, 5522), (5527, 5531), (5544, 5546), (5548, 5572), (5601, 5623), (5630, 5636), (5668, 5668), (5687, 5687), (5689, 5734),
  (5739, 5927), (5929, 6136), (6143, 6143), (6160, 6160), (6170, 6171)]

def burnsInLava (item : UInt16) : Bool :=
  burnRanges.any fun (lo, hi) => lo ≤ item.toNat && item.toNat ≤ hi

/-- `ItemID.Sets.ItemNoGravity`. -/
def noGravity (item : UInt16) : Bool :=
  #[520, 521, 547, 548, 549, 575, 3453, 3454, 3455, 3456, 3457, 3458, 3459, 3580, 3581, 4143].contains item.toNat

/-! ## Collision.LavaCollision -/

private def clampInt (v lo hi : Int) : Int := if v < lo then lo else if v > hi then hi else v

/-- `Collision.LavaCollision(position, width, height)`. It tests the full hitbox against
lava cells, whose surface lies `(256 - liquid) / 32 * 2` px below the tile top. -/
def lavaCollision (grid : Grid) (position : Vector) (width height : Nat) : Bool := Id.run do
  let x0 := clampInt ((position.x / 16).toInt32.toInt - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (((position.x + single width) / 16).toInt32.toInt + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt ((position.y / 16).toInt32.toInt - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (((position.y + single height) / 16).toInt32.toInt + 2) 0 ((grid.height : Int) - 40)
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      let some liquid := tile.liquid | continue
      if liquid.amount == 0 || liquid.kind != .lava then continue
      let left := single (x * 16 : Nat)
      let depth : Float32 := single (256 - liquid.amount.toNat : Nat) / 32
      let top := single (y * 16 : Nat) + depth * 2
      let tall : Int := 16 - (depth * 2).toInt32.toInt
      if position.x + single width > left && position.x < left + 16 &&
          position.y + single height > top && position.y < top + single tall then
        return true
  return false

/-! ## UpdateItem -/

/-- `WorldItem.MoveInWorld` after the gravity step: the liquid probes, entering and leaving,
the collision, `ApplyMovement` and `SlopeCollision`. -/
private def moveInWorld (grid : Grid) (drop : Drop) (gravity maxFall : Float32)
    (wetVelocity : Vector) : Drop := Id.run do
  let mut v := drop.velocity
  if noGravity drop.stack.item then
    let x := v.x * 0.95
    let x := if x.toFloat < 0.1 && x.toFloat > -0.1 then 0 else x
    let y := v.y * 0.95
    let y := if y.toFloat < 0.1 && y.toFloat > -0.1 then 0 else y
    v := ⟨x, y⟩
  else
    let y := v.y + gravity
    let y := if y > maxFall then maxFall else y
    let x := v.x * 0.95
    let x := if x.toFloat < 0.1 && x.toFloat > -0.1 then 0 else x
    v := ⟨x, y⟩
  let mut l := drop.liquid
  if lavaCollision grid drop.position itemSize itemSize then l := { l with lavaWet := true }
  let hit := OriginalPlayerWet.wetCollision grid drop.position itemSize itemSize
  if hit.honey then l := { l with honeyWet := true }
  if hit.shimmer then l := { l with shimmerWet := true }
  if hit.wet then
    if !l.wet then
      if l.wetCount == 0 then l := { l with wetCount := 20 }
      l := { l with wet := true }
  else if l.wet then
    l := { l with wet := false }
    if l.wetCount == 0 then l := { l with wetCount := 20 }
  if !l.wet then l := { l with lavaWet := false, honeyWet := false, shimmerWet := false }
  if l.wetCount > 0 then l := { l with wetCount := l.wetCount - 1 }
  let contact := OriginalCollision.tileCollision grid drop.position v itemSize itemSize
    { ignoreAetheriumPlatforms := true, hoik := true }
  let after := contact.displacement
  let wetVelocity : Vector := ⟨if after.x != v.x then after.x else wetVelocity.x,
                               if after.y != v.y then after.y else wetVelocity.y⟩
  let position := drop.position + (if l.wet then wetVelocity else after)
  let slope := OriginalCollision.slopeCollision grid position after itemSize itemSize false true
  return { drop with position := slope.position, velocity := slope.velocity, liquid := l }

/-- `Main.UpdateWorld_Items` for slots in index order. -/
def update (grid : Grid) (initial : WorldItems) (fallenStarsExpire : Bool := false) : WorldItems := Id.run do
  let mut items := initial
  for index in [:items.size] do
    let some drop := items[index]?.join | continue
    let l := drop.liquid
    if drop.beingGrabbed then
      items := items.set! index (some ({ drop with
        beingGrabbed := false, liquid := {}, position := drop.position + drop.velocity } : Drop).advanceTimers)
      continue
    let (gravity, maxFall, wetScale) : Float32 × Float32 × Float32 :=
      if l.shimmerWet then (0.065, 4, 0.375)
      else if l.honeyWet then (0.05, 3, 0.25)
      else if l.wet then (0.08, 5, 0.5)
      else (0.1, 7, 0.5)
    let wetVelocity : Vector := ⟨drop.velocity.x * wetScale, drop.velocity.y * wetScale⟩
    let (combined, drop) := OriginalItems.combineNearby items index drop
    let drop := moveInWorld grid drop gravity maxFall wetVelocity
    items := combined.set! index <|
      if (drop.liquid.lavaWet && burnsInLava drop.stack.item) || !OriginalItems.inWorld grid drop.position ||
          (fallenStarsExpire && drop.stack.item == 75)
      then none else some drop.advanceTimers
  return items

end Lean4Terria.OriginalWorldItemLiquid
