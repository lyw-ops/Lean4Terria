/-! The six `IBestiaryUICollectionInfoProvider` kinds of the installed 1.4.5.8 desktop image
(`Terraria.GameContent.Bestiary.*UICollectionInfoProvider`). Values are copied from the real
`Main.BestiaryDB` by `.lake/original-bestiary-progress/gen_db.py`. -/
namespace Lean4Terria.OriginalBestiaryProgress

inductive Provider where
  /-- CommonEnemy: persistent id, `_quickUnlock`, `_killCountNeededToFullyUnlock` -/
  | common (id : String) (quick : Bool) (needed : Int)
  /-- Critter: unlocked by a sighting -/
  | critter (id : String)
  /-- TownNPC: unlocked by a chat -/
  | town (id : String)
  /-- GoldCritter: the gold critter's id and the normal critters' ids -/
  | gold (goldId : String) (normal : Array String)
  /-- HighestOfMultiple: main provider index and the nested providers -/
  | highest (main : Nat) (providers : Array Provider)
  /-- SalamanderShellyDad: persistent id and kills needed -/
  | salamander (id : String) (needed : Int)
  deriving Repr, Inhabited

end Lean4Terria.OriginalBestiaryProgress
