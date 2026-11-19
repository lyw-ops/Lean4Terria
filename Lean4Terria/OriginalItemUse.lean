import Init

namespace Lean4Terria.OriginalItemUse

structure Timing where
  itemAnimation : Nat := 0
  itemTime : Nat := 0
  itemAnimationMax : Nat := 0
  itemTimeMax : Nat := 0
  deriving BEq, Repr

/-- The ordinary start/renew gate, before ItemCheck decrements its counters. -/
def Timing.willStart (timing : Timing) (held : Bool) (canAutoReuse : Bool := true) : Bool :=
  let finished := timing.itemAnimation == 0 || (canAutoReuse && timing.itemAnimation == 1)
  held && canAutoReuse && finished

/-- ItemCheck starts or renews the swing, then decrements both counters
before applying the use effect. -/
def Timing.advance (timing : Timing) (held : Bool) (animationLength : Nat)
    (canAutoReuse : Bool := true) : Timing :=
  let finished := timing.itemAnimation == 0 || (canAutoReuse && timing.itemAnimation == 1)
  let starts := timing.willStart held canAutoReuse
  { timing with
    itemAnimation := if starts then animationLength - 1 else timing.itemAnimation - 1
    itemAnimationMax := if starts then animationLength
      else if finished then 0 else timing.itemAnimationMax
    itemTime := timing.itemTime - 1 }

def Timing.setTime (timing : Timing) (length : Nat) : Timing :=
  { timing with itemTime := length, itemTimeMax := length }

end Lean4Terria.OriginalItemUse
