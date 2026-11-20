import Init

namespace Lean4Terria.OriginalTravelMerchant

/-- The world flags `Chest.SetupTravelShop` and its callees read. -/
inductive Flag where
  | dontStarveWorld | downedBoss1 | downedBoss2 | downedBoss3 | downedDeerclops | downedFrost | downedMartians
  | downedMechBoss1 | downedMechBoss2 | downedMechBoss3 | downedMechBossAny | downedMoonlord | downedQueenBee
  | downedSlimeKing | getGoodWorld | hardMode | peddlersSatchelWasUsed | shadowOrbSmashed | tenthAnniversaryWorld
  deriving BEq, Repr

/-- One boolean per `Flag`. -/
structure Flags where
  dontStarveWorld : Bool := false
  downedBoss1 : Bool := false
  downedBoss2 : Bool := false
  downedBoss3 : Bool := false
  downedDeerclops : Bool := false
  downedFrost : Bool := false
  downedMartians : Bool := false
  downedMechBoss1 : Bool := false
  downedMechBoss2 : Bool := false
  downedMechBoss3 : Bool := false
  downedMechBossAny : Bool := false
  downedMoonlord : Bool := false
  downedQueenBee : Bool := false
  downedSlimeKing : Bool := false
  getGoodWorld : Bool := false
  hardMode : Bool := false
  peddlersSatchelWasUsed : Bool := false
  shadowOrbSmashed : Bool := false
  tenthAnniversaryWorld : Bool := false

def Flags.get (f : Flags) : Flag → Bool
  | .dontStarveWorld => f.dontStarveWorld | .downedBoss1 => f.downedBoss1 | .downedBoss2 => f.downedBoss2
  | .downedBoss3 => f.downedBoss3 | .downedDeerclops => f.downedDeerclops | .downedFrost => f.downedFrost
  | .downedMartians => f.downedMartians | .downedMechBoss1 => f.downedMechBoss1 | .downedMechBoss2 => f.downedMechBoss2
  | .downedMechBoss3 => f.downedMechBoss3 | .downedMechBossAny => f.downedMechBossAny | .downedMoonlord => f.downedMoonlord
  | .downedQueenBee => f.downedQueenBee | .downedSlimeKing => f.downedSlimeKing | .getGoodWorld => f.getGoodWorld
  | .hardMode => f.hardMode | .peddlersSatchelWasUsed => f.peddlersSatchelWasUsed | .shadowOrbSmashed => f.shadowOrbSmashed
  | .tenthAnniversaryWorld => f.tenthAnniversaryWorld

/-- A condition atom, evaluated in order with short-circuit semantics. `roll k` draws
`Player.RollLuck(rarity[k])` and holds when it returns 0. -/
inductive Cond where
  | roll (k : Nat)
  | flag (f : Flag) (want : Bool)
  | slotLE (k : Int)
  | anyFlag (fs : List Flag)

inductive Effect where
  | set (item : Int)
  /-- `item = a; if (flag) item = b`. -/
  | setThenIf (a : Int) (f : Flag) (b : Int)
  /-- `item = base + Main.rand.Next(n)`. -/
  | setRand (base : Int) (n : Int)
  /-- `if (slotRarity > k) return;` -/
  | returnIfSlotGT (k : Int)

structure Stmt where
  conds : List Cond
  effect : Effect

end Lean4Terria.OriginalTravelMerchant
