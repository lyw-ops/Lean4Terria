import Init

namespace Lean4Terria

/-- The subtractive generator used by Terraria's `Main.rand` and `WorldGen.genRand`. The
table has 55 working entries and keeps index zero unused, as in the game. -/
structure UnifiedRandom where
  private table : Array Int32
  private index : Nat

namespace UnifiedRandom

private def limit : Int32 := 2147483647

def seed (value : Int) : UnifiedRandom := Id.run do
  let value := Int32.ofInt value
  let magnitude := if value == -2147483648 then limit else if value < 0 then -value else value
  let mut previous := 161803398 - magnitude
  let mut next : Int32 := 1
  let mut table := (Array.replicate 56 (0 : Int32)).set! 55 previous
  for i in [1:55] do
    let index := (21 * i) % 55
    table := table.set! index next
    next := previous - next
    if next < 0 then next := next + limit
    previous := table[index]!
  for _ in [:4] do
    for i in [1:56] do
      let value := table[i]! - table[1 + (i + 30) % 55]!
      table := table.set! i (if value < 0 then value + limit else value)
  return ⟨table, 0⟩

def fromEntropy (entropy : UInt64) : UnifiedRandom :=
  seed (Int32.ofInt entropy.toNat).toInt

/-- Read the state for a simulation phase that stores the same generator in its own state type. -/
def state (rng : UnifiedRandom) : Array Int32 × Nat :=
  (rng.table, rng.index)

def nextInt (rng : UnifiedRandom) : Int × UnifiedRandom :=
  let index := rng.index % 55 + 1
  let other := (index + 20) % 55 + 1
  let value := rng.table[index]! - rng.table[other]!
  let value := if value == limit then value - 1 else value
  let value := if value < 0 then value + limit else value
  (value.toInt, ⟨rng.table.set! index value, index⟩)

def sample (rng : UnifiedRandom) : Float × UnifiedRandom :=
  let (value, next) := rng.nextInt
  (value.toFloat * 4.656612875245797e-10, next)

/-- Native integer range sampling for ordered Int32 bounds. Equal bounds
return that value and still advance the generator. -/
def next (rng : UnifiedRandom) (lower upper : Int) : Int × UnifiedRandom :=
  if upper < lower then (lower, rng) else
    let span := upper - lower
    let (value, next) :=
      if span ≤ 2147483647 then rng.sample
      else
        let (value, rng) := rng.nextInt
        let (sign, rng) := rng.nextInt
        let signed := if sign % 2 == 0 then -value else value
        ((signed.toFloat + 2147483646) / 4294967293, rng)
    (lower + (value * span.toFloat).toInt64.toInt, next)

end UnifiedRandom

end Lean4Terria
