import Lean4Terria.OriginalCombatTextChannel

/-! The text portion of ordinary `NPC.StrikeNPC_Inner`. The world owns one
pool and commits this state before any later hit, HitEffect or death owner.
`none` is the explicit legacy, assumed-free-pool contract. -/
namespace Lean4Terria.OriginalNpcCombatText

structure State where
  channel : Option OriginalCombatTextChannel.Channel := none
  fromNet : Bool := false
  fromPlayer : Int := 0
  /-- Used only by the legacy path; a real channel owns its netMode. -/
  netMode : Int := 0
  legacyAvailable : Bool := true
  boundary : Option String := none

instance : Repr State where
  reprPrec s _ := repr (s.channel.isSome, s.fromNet, s.fromPlayer, s.netMode, s.legacyAvailable, s.boundary)

def color (friendly crit fromNet : Bool) : OriginalForestChatLayout.Color :=
  if friendly then
    if crit then OriginalCombatText.damagedFriendlyCrit else OriginalCombatText.damagedFriendly
  else if fromNet then
    if crit then OriginalCombatText.othersDamagedHostileCrit else OriginalCombatText.othersDamagedHostile
  else if crit then OriginalCombatText.damagedHostileCrit else OriginalCombatText.damagedHostile

/-- Original IL 0000..0144. `inputDamage` is the pre-defense argument; amount
is the final integer damage. The caller supplies the NPC's pre-strike body.
No DOT text is requested here. Font failures retain the pre-allocation pool
and stream and stop the caller before justHit/life/HitEffect/loot. -/
def State.strike (s : State) (rect : OriginalForestChatLayout.Rect)
    (friendly : Bool) (lifeMax inputDamage amount : Int) (crit : Bool)
    (random : UnifiedRandom) : State × UnifiedRandom := Id.run do
  if s.boundary.isSome || lifeMax ≤ 1 || (inputDamage ≥ 9999 && s.fromPlayer == 255) then
    return (s, random)
  match s.channel with
  | some channel =>
    match ({channel with rect}).number amount (color friendly crit s.fromNet) crit false random with
    | .ok out => return ({s with channel := some out.channel}, out.random)
    | .error why => return ({s with boundary := some s!"NPC CombatText.NewText: {why}"}, random)
  | none =>
    if s.netMode == 2 || !s.legacyAvailable then return (s, random)
    let mut random := random
    for _ in [:if crit then 3 else 2] do random := (random.next 0 1).2
    return (s, random)

end Lean4Terria.OriginalNpcCombatText
