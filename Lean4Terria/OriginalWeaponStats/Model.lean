import Init

namespace Lean4Terria.OriginalWeaponStats

/-- The ten outputs of `Item.TryGetPrefixStatMultipliersForItem` other than the final value
multiplier, in the order of the original's `ref` parameters: damage, knockback, use time,
scale, shoot speed, mana, then critical chance, tag damage and armour penetration. -/
structure Mult where
  dmg : Float32 := 1
  kb : Float32 := 1
  useTime : Float32 := 1
  scale : Float32 := 1
  shootSpeed : Float32 := 1
  mana : Float32 := 1
  crit : Int := 0
  tag : Int := 0
  armorPen : Int := 0
  deriving Inhabited

end Lean4Terria.OriginalWeaponStats
