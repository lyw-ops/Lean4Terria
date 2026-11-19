import Lean4Terria.OriginalCombatTextChannel
import Lean4Terria.OriginalCombat.Dust

/-! Original desktop Player.Hurt's ordinary local General-damage prefix.
Text allocation is before life subtraction and blood. Special accessory/dodge,
retaliation, multiplayer and death continuations remain caller-owned. -/
namespace Lean4Terria.OriginalPlayerHurtText

open OriginalCollision (Vector single)
open OriginalForestGuideArrow (DustPool DustParticle DustRequest)
open OriginalCombatTextChannel (Channel)

inductive Difficulty where
  | classic | expert | master
  deriving BEq, Repr

/-- Main.CalculateDamagePlayersTake, then Hurt's Float32 endurance multiplier,
Int32 truncation and second minimum. Crit does not double this local amount:
Hurt doubles a different local used by its final PVP return calculation. -/
def actualDamage (damage defense : Int) (endurance : Float32 := 0)
    (difficulty : Difficulty := .classic) : Int :=
  let factor : Float := match difficulty with
    | .classic => 0.5 | .expert => 0.75 | .master => 1
  let base := max 1 (Float.ofInt damage - Float.ofInt defense * factor)
  max 1 ((1 - endurance).toFloat * base).toInt32.toInt

structure TextResult where
  text : Option Channel
  random : UnifiedRandom
  draws : Nat := 0
  created : Option OriginalCombatText.Created := none
  boundary : Option String := none

/-- At original Hurt IL076e. `amount` is the actual post-defense/endurance
damage, not NPC base damage or clamped remaining life. Player.Hurt always uses
dot=false. Some allocates from the actual shared pool; None is explicitly the
legacy headless fixed-sample contract. Measurement errors stop before blood. -/
def emit (text : Option Channel) (amount : Int) (crit : Bool)
    (random : UnifiedRandom) : TextResult := Id.run do
  let color := if crit then OriginalCombatText.damagedFriendlyCrit
    else OriginalCombatText.damagedFriendly
  if let some channel := text then
    match channel.number amount color crit false random with
    | .ok out => return {text:=some out.channel,random:=out.random,draws:=out.draws,created:=out.created}
    | .error why => return {text,random,boundary:=some s!"Player.Hurt CombatText.NewText: {why}"}
  let draws := if crit then 3 else 2
  let mut random := random
  for _ in [:draws] do random := random.sample.2
  return {text,random,draws}

structure BloodInput where
  position : Vector
  width : Nat
  height : Nat
  damage : Int
  lifeAfter : Int
  lifeMax : Int
  hitDirection : Int
  /-- Original stoned takes the earlier dust1 branch; frost/bone armor require
  shader assignment and must be declared through unsupported instead. -/
  stoned : Bool := false
  unsupported : Option String := none

structure BloodResult where
  pool : DustPool
  random : UnifiedRandom
  particles : Array DustParticle := #[]
  requests : Array DustRequest := #[]
  prefixDraws : Nat := 0
  boundary : Option String := none

/-- Nonlethal Hurt blood: each iteration samples NextFloat BEFORE NewDust,
even though its coefficient is zero. Pool/viewport failures do not remove that
prefix draw. NewDust itself retains its real allocation/pressure draws.
The caller invokes PlayHurtSound before this function; audio is not synthesized.
Lethal Hurt has no blood loop here and must continue into the real death owner. -/
def blood (input : BloodInput) (pool : DustPool) (random : UnifiedRandom) : BloodResult := Id.run do
  if let some why := input.unsupported then
    return {pool,random,boundary:=some s!"Player.Hurt blood: {why}"}
  if input.lifeAfter ≤ 0 then return {pool,random}
  if input.lifeMax ≤ 0 then
    return {pool,random,boundary:=some "Player.Hurt blood: nonpositive statLifeMax2"}
  let count := (Float.ofInt input.damage / Float.ofInt input.lifeMax * 100).ceil.toUInt64.toNat
  let request : DustRequest := {
    position:=input.position,width:=input.width,height:=input.height
    typeId:=if input.stoned then 1 else 5
    speed:=⟨single (2*input.hitDirection),-2⟩}
  let mut out : BloodResult := {pool,random}
  for _ in [:count] do
    -- Utils.NextFloat casts NextDouble to Single even when multiplied by0.
    let random := out.random.sample.2
    let (pool,particle,random) := out.pool.create request random
    out := { out with
      pool := pool
      random := random
      particles := particle.elim out.particles out.particles.push
      requests := out.requests.push request
      prefixDraws := out.prefixDraws + 1 }
  return out

structure Input where
  damage : Int
  defense : Int := 0
  endurance : Float32 := 0
  difficulty : Difficulty := .classic
  crit : Bool := false
  creativeGodMode : Bool := false
  immune : Bool := false
  immuneTime : Int := 0
  /-- The General Hurt method itself does not check dead. The outer Player
  update/contact caller must avoid calling it for dead players. -/
  dead : Bool := false
  life : Int
  lifeMax : Int
  longInvince : Bool := false
  /-- Fields/branches outside the ordinary local General contract must be
  surfaced by the caller, never silently treated as ordinary equipment. -/
  unsupported : Option String := none

structure Prefix where
  accepted : Bool := false
  damage : Int := 0
  /-- `statLife` once the whole Hurt has returned: the death branch (IL_0fa1..0fa3) has already
  written 0 over a nonpositive value. This is NOT what PlayHurtSound and the blink see. -/
  lifeAfter : Int
  /-- `statLife` as IL_077e writes it: the signed subtraction. It stays in the field through
  knockback, PlayHurtSound (IL_0dde) and the blink, and decides blood or death at IL_0dee.
  Native: 5 − 9 is −4 at the sound request and 0 on return
  (`.lake/original-player-hurt-effects/prefix-life-boundary.json`). -/
  rawLifeAfter : Int
  immune : Bool
  immuneTime : Int := 0
  text : TextResult
  needsDeathContinuation : Bool := false
  boundary : Option String := none

/-- Ordinary local-player General prefix through text/life/immunity. It makes
no DamageVar roll: Update_NPCCollision owns that earlier roll. `dead` is retained
as a declaration, not an invented Hurt gate. The caller owns knockback, actual
PlayHurtSound, blood, status effects and death in original order: it continues from
`rawLifeAfter` (`OriginalPlayerHurt.finish`), never from the already clamped `lifeAfter`. -/
def ordinaryPrefix (input : Input) (text : Option Channel) (random : UnifiedRandom) : Prefix := Id.run do
  let initial : Prefix := {lifeAfter:=input.life,rawLifeAfter:=input.life,immune:=input.immune,immuneTime:=input.immuneTime,text:={text,random}}
  if input.creativeGodMode then return initial
  if let some why := input.unsupported then
    return {initial with boundary:=some s!"Player.Hurt ordinary prefix: {why}"}
  if input.immune then return initial
  let damage := actualDamage input.damage input.defense input.endurance input.difficulty
  let popup := emit text damage input.crit random
  if popup.boundary.isSome then return {initial with text:=popup,boundary:=popup.boundary}
  let life := input.life-damage
  return {
    accepted := true
    damage := damage
    lifeAfter := max 0 life
    rawLifeAfter := life
    immune := true
    immuneTime := (if damage == 1 then 20 else 40) * (if input.longInvince then 2 else 1)
    text := popup
    needsDeathContinuation := life ≤ 0 }

end Lean4Terria.OriginalPlayerHurtText
