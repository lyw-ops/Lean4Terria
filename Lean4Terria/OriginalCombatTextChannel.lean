import Lean4Terria.OriginalCombatText

/-! A caller-owned view of the one shared CombatText pool. The owning world
passes the actual current body rectangle, local-player gravity and real font
measurement, then commits the returned pool and stream at that call site. -/
namespace Lean4Terria.OriginalCombatTextChannel

structure Channel where
  state : OriginalCombatText.State
  context : OriginalCombatText.Context
  rect : OriginalForestChatLayout.Rect
  measure : OriginalCombatText.Measure

structure Output where
  channel : Channel
  random : UnifiedRandom
  draws : Nat
  created : Option OriginalCombatText.Created

/-- Execute the real pure NewText(int) implementation. No static skip replaces
allocation. A full pool/server gate returns unchanged pool/RNG and draws=0;
font errors remain errors, before allocation or random consumption. -/
def Channel.number (channel : Channel) (amount : Int) (color : OriginalForestChatLayout.Color)
    (dramatic dot : Bool) (random : UnifiedRandom) : Except String Output := do
  let out ← OriginalCombatText.newNumber channel.state channel.context
    {rect:=channel.rect,color,text:="",dramatic,dot}
    (Int32.ofInt amount) channel.measure random
  let draws := if out.created.isSome then (if dramatic then 3 else 2) else 0
  return ⟨{channel with state:=out.state},out.random,draws,out.created⟩

end Lean4Terria.OriginalCombatTextChannel
