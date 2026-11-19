import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalPlayerLiquidHealth
import Lean4Terria.OriginalCombatTextChannel
import Lean4Terria.OriginalManaHeat

/-!
# Quick Heal / Quick Mana potions (Terraria 1.4.5.8 desktop, single player, Classic)

Hand port of the installed 1.4.5.8 CIL of `Player.QuickHeal`, `QuickHeal_GetItemToUse`,
`QuickHeal_GetItemToUse_TryChoosingItem`, `QuickMana`, `QuickMana_GetItemToUse`,
`ApplyPotionDelay`, `ApplyManaPotionManaHeat`, plus the callees they need to be
observable (`ApplyLifeAndOrMana`, `TryToResetHungerToNeutral`, the `AddBuff` chain, `HealEffect`
and `ManaEffect`). Evidence: `.lake/original-healing-potions/`.

Order inside `QuickHeal` (CIL IL_0000..012f): pending inventory action → cursed / CCed / dead →
full life or `potionDelay > 0` → choose item → `ItemCheck_TryStartUse` → sound request
`PlaySound(item.UseSound, position, 0, 1)` → `ApplyPotionDelay` (potions) → `ApplyLifeAndOrMana`
→ Mushroom hunger reset → item buff `AddBuff(buffType, buffTime || 3600)` → stack − 1 and
`TurnToAir` → achievement event 25 for item 126 at breath 0.
`QuickMana` is the same shape without a full-mana gate (ManaV2 is on in the shipped game), without
the item buff, and chooses the FIRST inventory slot (then the Void Bag) with `healMana > 0`.

Findings from the installed binary (all checked against native runs):
* `ApplyManaPotionManaHeat` has **no caller anywhere** in the 1.4.5.8 assembly; `QuickMana` does not
  apply mana heat. Only `PickupItem` calls `TryApplyManaHeat`. The method is implemented and verified
  as an orphan with its own entry (`applyManaPotionManaHeat`).
* `ItemCheck_TryStartUse` for these potions reduces to `!noItems` (the Cursed buff): every other
  sub-check (ammo, mana cost, dirt block, bobbers, summons, pulley, ...) is true for them.
* Philosopher's Stone multiplies only the Strange Brew delay inside `ApplyPotionDelay`; the other
  delays (`potionDelayTime`, ...) are multiplied by the `Update` copy IL_1f8b..1ff1 (`Player.tick`).

Not modelled (outside this leaf, recorded in INTERFACE.md): mounts that dismount on item use,
multiplayer / `netMode`, expert-mode debuff scaling, `buffImmune`, pet / melee / fed-state buff
exclusions in `AddBuff` for buff types other than the ones potions produce (21, 48, 94, 396), audible
`LegacySoundPlayer` pitch draws (the host runs with audio unsupported, so only the request is checked).
-/

namespace Lean4Terria.OriginalHealingPotions

open OriginalCollision (Vector single)
open OriginalPlayerLiquidHealth (Health Rng)
open OriginalForestChatLayout (Color Rect)

/-- One inventory / Void Bag slot. `TurnToAir` leaves type 0 and stack 0. -/
structure Slot where
  type : Nat := 0
  stack : Int := 0
  deriving BEq, Repr, Inhabited

/-- Item fields the quick potion code reads. Values come from `Item.SetDefaults` of the installed
desktop executable (native table in `.lake/original-healing-potions/`); every other item has
no heal fields and is not a potion. `soundStyle` is the `LegacySoundStyle` style of `UseSound`
(its `SoundId` is 2, the Item sounds). -/
structure ItemInfo where
  healLife : Int := 0
  healMana : Int := 0
  potion : Bool := false
  buffType : Nat := 0
  buffTime : Int := 0
  soundStyle : Nat := 3
  deriving BEq, Repr, Inhabited

def itemInfo (type : Nat) : ItemInfo :=
  match type with
  | 5 => { healLife := 15, potion := true, soundStyle := 2 }
  | 28 => { healLife := 50, potion := true }
  | 110 => { healMana := 50 }
  | 126 => { healLife := 30, potion := true }
  | 188 => { healLife := 100, potion := true }
  | 189 => { healMana := 100 }
  | 226 => { healLife := 90, potion := true }
  | 227 => { healLife := 90, potion := true }
  | 499 => { healLife := 150, potion := true }
  | 500 => { healMana := 200 }
  | 1134 => { healLife := 80, potion := true, buffType := 48, buffTime := 900 }
  | 1912 => { healLife := 80, potion := true }
  | 2209 => { healMana := 400 }
  | 2314 => { healLife := 120, potion := true }
  | 3001 => { healLife := 70, potion := true }
  | 3544 => { healLife := 200, potion := true }
  | 5496 => { healLife := 180, potion := true }
  | _ => {}

/-- Main.debuff from the installed desktop image (read from the native table). -/
def isDebuff (id : Nat) : Bool :=
  #[20,21,22,23,24,25,28,30,31,32,33,34,35,36,37,38,39,43,44,46,47,67,68,69,70,72,80,86,87,88,89,94,
    103,119,120,137,144,145,146,147,148,149,153,156,157,158,160,163,164,169,183,186,189,194,195,196,
    197,199,203,204,215,320,321,323,324,332,333,334,344,350,353,395,397,398,399,400].contains id

/-- Static values from `Item.potionDelay`, `restorationDelay`, `mushroomDelay`, `Player.manaSickTime`
and `PhilosopherStoneDurationMultiplier` (0.75). -/
def potionDelayBase : Int := 3600
def restorationDelayBase : Int := 2700
def mushroomDelayBase : Int := 1800
def manaSickTime : Int := 300
def philosopherMultiplier : Float32 := 0.75

structure Player where
  health : Health
  statManaMax2 : Int := 20
  potionDelay : Int := 0
  manaPotionDelay : Int := 0
  potionDelayTime : Int := 3600
  restorationDelayTime : Int := 2700
  mushroomDelayTime : Int := 1800
  eggnogDelayTime : Int := 2400
  /-- Philosopher's Stone result of the accessory phase of the current update. -/
  pStone : Bool := false
  frozen : Bool := false
  webbed : Bool := false
  stoned : Bool := false
  /-- Set by a live buff 94 in `UpdateBuffs`, cleared by `ResetEffects`; no potion code reads it. -/
  manaSick : Bool := false
  /-- `Main.LocalPlayerHasPendingInventoryActions()` (fake cursor item or crafting requests). -/
  pending : Bool := false
  /-- 59 slots; the quick loops read 0..57. -/
  inventory : Array Slot := Array.replicate 59 {}
  bank4 : Array Slot := Array.replicate 40 {}
  manaHeat : OriginalManaHeat.State := {}
  position : Vector := ⟨0, 0⟩
  width : Nat := 20
  height : Nat := 42
  deriving Repr, Inhabited

/-- Requests in the order the original emits them. `soundId`/`style` identify the
`LegacySoundStyle`; the last two floats are the volume and pitch arguments. `addBuff` is the
request passed to `Player.AddBuff` (type, time, quiet), not the resulting timer. -/
inductive Effect where
  | sound (soundId style : Nat) (position : Vector) (volumeScale pitchOffset : Float32)
  | addBuff (type : Nat) (time : Int) (quiet : Bool)
  | text (rect : Rect) (color : Color) (amount : Int) (dramatic dot : Bool)
  | achievement (event : Nat)
  deriving Repr

structure Outcome where
  player : Player
  rng : Rng
  effects : Array Effect := #[]

/-- A chosen item: an inventory index (0..57) or a Void Bag (`bank4`) index. -/
inductive SlotRef where
  | inventory (index : Nat)
  | bank (index : Nat)
  deriving BEq, Repr

def Player.slot (p : Player) : SlotRef → Slot
  | .inventory i => p.inventory[i]!
  | .bank i => p.bank4[i]!

def Player.setSlot (p : Player) : SlotRef → Slot → Player
  | .inventory i, s => { p with inventory := p.inventory.set! i s }
  | .bank i, s => { p with bank4 := p.bank4.set! i s }

/-- `Player.get_CCed`: frozen, webbed or stoned. -/
def Player.ccd (p : Player) : Bool := p.frozen || p.webbed || p.stoned

/-- `Player.useVoidBag`: any inventory slot 0..57 with stack > 0 holding item 4131. -/
def Player.useVoidBag (p : Player) : Bool :=
  (List.range 58).any fun i => p.inventory[i]!.stack > 0 && p.inventory[i]!.type == 4131

/-! ## Buffs: the real `AddBuff` chain for potion buff types -/

def maxBuffs : Nat := OriginalPlayerLiquidHealth.maxBuffs

/-- `BuffID.Sets.AddBuffTimeAdditivelyToCap` (non-zero entries of the installed image): Mana Sickness (94) stacks
up to 600 ticks, Shimmer (383) up to 43200. Potion sickness (21), honey (48) and mana heat (396) refresh to the longer time. -/
def additiveCap (type : Nat) : Int := if type == 94 then 600 else if type == 383 then 43200 else 0

/-- AddBuff_ActuallyTryToAddTheBuff: the first slot holding a non-debuff (empty counts) fixes where
the search for an empty slot starts; with no empty slot at or after it, that non-debuff buff is deleted
(compacting) and the whole attempt restarts. All slots debuffs: not added. -/
def actuallyAddBuff (h : Health) (type : Nat) (time : Int) : Health := Id.run do
  let mut h := h
  for _ in [:maxBuffs + 1] do
    let some first := (List.range maxBuffs).find? (fun i => !isDebuff h.buffType[i]!) | return h
    match (List.range maxBuffs).find? (fun i => i ≥ first && h.buffType[i]! == 0) with
    | some free => return { h with buffType := h.buffType.set! free type, buffTime := h.buffTime.set! free time }
    | none => h := h.delBuff first
  return h

/-- `Player.AddBuff(type, time, quiet)` for potion buff types in Classic: non-quiet requests pass through
`AddBuff_DetermineBuffTimeToAdd` unchanged; an existing slot of that type keeps the longer time, except
additive buffs (see `additiveCap`). -/
def addBuffChain (h : Health) (type : Nat) (time : Int) : Health :=
  match (List.range maxBuffs).find? (fun i => h.buffType[i]! == type) with
  | some i =>
    let cap := additiveCap type
    if cap > 0 then
      -- BuffID.Sets.AddBuffTimeAdditivelyToCap: the new time is added, then capped
      let sum := h.buffTime[i]! + time
      { h with buffTime := h.buffTime.set! i (if sum > cap then cap else sum) }
    else if h.buffTime[i]! < time then { h with buffTime := h.buffTime.set! i time } else h
  | none => actuallyAddBuff h type time

structure Step where
  player : Player
  effects : Array Effect

private def Step.addBuff (s : Step) (type : Nat) (time : Int) : Step :=
  { player := { s.player with health := addBuffChain s.player.health type time }
    effects := s.effects.push (.addBuff type time false) }

/-! ## Text requests (`HealEffect` / `ManaEffect`) -/

private def bodyRect (p : Player) : Rect :=
  ⟨p.position.x.toInt32.toInt, p.position.y.toInt32.toInt, p.width, p.height⟩

/-- `CombatText.NewText(body rectangle, color, amount, false, false)` through the shared channel. The
channel rectangle is the current body rectangle; a font failure is recorded as `rng.boundary`. -/
private def emitText (p : Player) (rng : Rng) (effects : Array Effect) (color : Color) (amount : Int) :
    Rng × Array Effect :=
  let rect := bodyRect p
  let rng := { rng with text := rng.text.map (fun (c : OriginalCombatTextChannel.Channel) => { c with rect := rect }) }
  (rng.combatNumber amount color false false, effects.push (.text rect color amount false false))

/-! ## ApplyPotionDelay -/

/-- `Player.TryToResetHungerToNeutral`: every live 332/333/334 buff gets time 0 (the type stays); the
following `UpdateHungerBuffs` then finds no live hunger buff and does nothing. -/
def resetHunger (p : Player) : Player := Id.run do
  let h := p.health
  let mut tm := h.buffTime
  for i in [:maxBuffs] do
    if h.buffType[i]! > 0 && h.buffTime[i]! > 0 && (h.buffType[i]! == 332 || h.buffType[i]! == 333 || h.buffType[i]! == 334) then
      tm := tm.set! i 0
  return { p with health := { h with buffTime := tm } }

/-- `Player.ApplyPotionDelay(item)`. Strange Brew draws `Main.rand.Next(2400, 4201)` and scales it by the
Philosopher's Stone multiplier with a float multiply and `conv.i4`. -/
def applyPotionDelay (p : Player) (type : Nat) (rng : Rng) : Outcome := Id.run do
  let mut p := p
  let mut rng := rng
  let mut delay : Int := 0
  let mut hunger := false
  if type == 3001 then
    let (d, next) := rng.stream.next 2400 4201
    rng := { rng with stream := next, draws := rng.draws + 1 }
    delay := d
    if p.pStone then delay := (Float32.ofInt d * philosopherMultiplier).toInt32.toInt
  else if type == 227 || type == 126 then delay := p.restorationDelayTime
  else if type == 1912 then
    delay := p.eggnogDelayTime
    hunger := true
  else if type == 5 then
    delay := p.mushroomDelayTime
    hunger := true
  else delay := p.potionDelayTime
  p := { p with potionDelay := delay }
  let mut s : Step := { player := p, effects := #[] }
  s := s.addBuff 21 delay
  p := s.player
  if hunger then p := resetHunger p
  return { player := p, rng, effects := s.effects }

/-! ## ApplyLifeAndOrMana -/

/-- `Player.ApplyLifeAndOrMana(item)` for the local player: heal numbers, caps, `HealEffect` text, mana
sickness buff 94 and `ManaEffect` text. Strange Brew rolls `Next(healLife, 121)` and a `NextFloat` for the
immunity seconds (240 / 120 / 60 / none, then `SetImmuneTimeForAllTypes`). -/
def applyLifeAndOrMana (p : Player) (type : Nat) (rng : Rng) (effects : Array Effect := #[]) : Outcome := Id.run do
  let info := itemInfo type
  let mut p := p
  let mut rng := rng
  let mut effects := effects
  let mut healLife := info.healLife
  let mut healMana := info.healMana
  if type == 3001 then
    let (life, next) := rng.stream.next info.healLife 121
    rng := { rng with stream := next, draws := rng.draws + 1 }
    healLife := life
    let (sample, next) := rng.stream.sample
    rng := { rng with stream := next, draws := rng.draws + 1 }
    let f : Float32 := sample.toFloat32
    let seconds : Int := if !(f > 0.1) then 240 else if !(f > 0.3) then 120 else if !(f > 0.6) then 60 else 0
    if seconds > 0 then
      p := { p with health := { p.health with
        immune := true, immuneTime := seconds,
        hurtCooldowns := p.health.hurtCooldowns.map fun _ => seconds } }
  healMana := (Float32.ofInt healMana * 1).toInt32.toInt
  let mut h := p.health
  h := { h with statLife := h.statLife + healLife, statMana := h.statMana + healMana }
  if h.statLife > h.statLifeMax2 then h := { h with statLife := h.statLifeMax2 }
  if h.statMana > p.statManaMax2 then h := { h with statMana := p.statManaMax2 }
  p := { p with health := h }
  if healLife > 0 then
    (rng, effects) := emitText p rng effects OriginalCombatText.healLife healLife
    if rng.boundary.isSome then return { player := p, rng, effects }
  if healMana > 0 then
    p := { p with manaPotionDelay := 0 }
    let mut s : Step := { player := p, effects }
    s := s.addBuff 94 manaSickTime
    p := s.player
    effects := s.effects
    (rng, effects) := emitText p rng effects OriginalCombatText.healMana healMana
  return { player := p, rng, effects }

/-! ## Mana heat -/

/-- `Player.ApplyManaPotionManaHeat(item)`: Mana Potion 189 → (0.05, 20 s), Greater 500 → (0.1, 20 s),
2209 → (0.15, 20 s), anything else (0, 0). `TryApplyManaHeat` records both latest fields and requests
`AddBuff(396, ticks, false)` only when both numbers are nonzero. -/
def applyManaPotionManaHeat (p : Player) (type : Nat) : Player × Array Effect :=
  let (bonus, seconds) : Float32 × Float32 :=
    if type == 189 then (0.05000000074505806, 20)
    else if type == 500 then (0.10000000149011612, 20)
    else if type == 2209 then (0.15000000596046448, 20)
    else (0, 0)
  let applied := p.manaHeat.apply bonus seconds
  let p := { p with manaHeat := applied.state }
  match applied.buffTime with
  | some time =>
    let s : Step := { player := p, effects := #[] }
    let s := s.addBuff 396 time
    (s.player, s.effects)
  | none => (p, #[])

/-! ## Choosing the item -/

/-- `QuickHeal_GetItemToUse_TryChoosingItem(missing, ref chosen, ref best, item)`. `best` starts at
`-statLifeMax2`. A negative `best` (every candidate so far under-heals) takes any candidate that is
closer to zero from below, or any non-negative one; once `best ≥ 0` only the smallest non-negative
candidate wins. Restoration Potion (227) moves an under-healing difference by +30, capped at 0. -/
def tryChoosingHeal (missing : Int) (chosen : Option SlotRef) (best : Int) (ref : SlotRef) (slot : Slot) :
    Option SlotRef × Int :=
  let info := itemInfo slot.type
  if slot.stack > 0 && slot.type > 0 && info.potion && info.healLife > 0 then
    let d0 := info.healLife - missing
    let d :=
      if slot.type == 227 && d0 < 0 then
        let d1 := d0 + 30
        if d1 > 0 then 0 else d1
      else d0
    if best < 0 then
      if d > best then (some ref, d) else (chosen, best)
    else if d < best && d ≥ 0 then (some ref, d)
    else (chosen, best)
  else (chosen, best)

/-- `QuickHeal_GetItemToUse`: inventory 0..57, then the Void Bag items (only with a Void Bag). -/
def Player.quickHealItem (p : Player) : Option SlotRef := Id.run do
  let missing := p.health.statLifeMax2 - p.health.statLife
  let mut chosen : Option SlotRef := none
  let mut best : Int := -p.health.statLifeMax2
  for i in [:58] do
    (chosen, best) := tryChoosingHeal missing chosen best (.inventory i) p.inventory[i]!
  if p.useVoidBag then
    for i in [:p.bank4.size] do
      (chosen, best) := tryChoosingHeal missing chosen best (.bank i) p.bank4[i]!
  return chosen

private def manaCandidate (p : Player) (slot : Slot) : Bool :=
  let info := itemInfo slot.type
  slot.stack > 0 && slot.type > 0 && info.healMana > 0 && (p.potionDelay == 0 || !info.potion)

/-- `QuickMana_GetItemToUse`: first matching inventory slot, then the first matching Void Bag slot. -/
def Player.quickManaItem (p : Player) : Option SlotRef := Id.run do
  for i in [:58] do
    if manaCandidate p p.inventory[i]! then return some (.inventory i)
  if p.useVoidBag then
    for i in [:p.bank4.size] do
      if manaCandidate p p.bank4[i]! then return some (.bank i)
  return none

/-! ## QuickHeal / QuickMana -/

/-- `Item.UseSound` request at `PlaySound(item.UseSound, position, 0, 1)`. -/
private def soundEffect (p : Player) (type : Nat) : Effect :=
  .sound 2 (itemInfo type).soundStyle p.position 0 1

/-- `ItemCheck_TryStartUse(item, false)` for these potions: only the Cursed buff's `noItems` can refuse. -/
private def canStartUse (p : Player) : Bool := !p.health.noItems

/-- Consume one: `stack - 1`, then `TurnToAir` at stack ≤ 0. -/
private def consume (p : Player) (ref : SlotRef) : Player :=
  let slot := p.slot ref
  let stack := slot.stack - 1
  p.setSlot ref (if stack > 0 then { slot with stack } else {})

/-- `Player.QuickHeal()` as the local player. -/
def quickHeal (p : Player) (rng : Rng) : Outcome := Id.run do
  let none' : Outcome := { player := p, rng }
  if p.pending then return none'
  if p.health.cursed || p.ccd || p.health.dead then return none'
  if p.health.statLife == p.health.statLifeMax2 || p.potionDelay > 0 then return none'
  let some ref := p.quickHealItem | return none'
  let type := (p.slot ref).type
  let info := itemInfo type
  if !canStartUse p then return none'
  let mut effects : Array Effect := #[soundEffect p type]
  let mut p := p
  let mut rng := rng
  if info.potion then
    let o := applyPotionDelay p type rng
    p := o.player
    rng := o.rng
    effects := effects ++ o.effects
  let o := applyLifeAndOrMana p type rng effects
  p := o.player
  rng := o.rng
  effects := o.effects
  if rng.boundary.isSome then return { player := p, rng, effects }
  if type == 5 then p := resetHunger p
  if info.buffType > 0 then
    let time := if info.buffTime == 0 then 3600 else info.buffTime
    let s : Step := { player := p, effects }
    let s := s.addBuff info.buffType time
    p := s.player
    effects := s.effects
  p := consume p ref
  if type == 126 && p.health.breath == 0 then effects := effects.push (.achievement 25)
  return { player := p, rng, effects }

/-- `Player.QuickMana()` as the local player (ManaV2 on: no full-mana gate). -/
def quickMana (p : Player) (rng : Rng) : Outcome := Id.run do
  let none' : Outcome := { player := p, rng }
  if p.pending then return none'
  if p.health.cursed || p.ccd || p.health.dead then return none'
  if p.manaPotionDelay > 0 then return none'
  let some ref := p.quickManaItem | return none'
  let type := (p.slot ref).type
  let info := itemInfo type
  if !canStartUse p then return none'
  let mut effects : Array Effect := #[soundEffect p type]
  let mut p := p
  let mut rng := rng
  if info.potion then
    let o := applyPotionDelay p type rng
    p := o.player
    rng := o.rng
    effects := effects ++ o.effects
  let o := applyLifeAndOrMana p type rng effects
  p := o.player
  rng := o.rng
  effects := o.effects
  if rng.boundary.isSome then return { player := p, rng, effects }
  p := consume p ref
  return { player := p, rng, effects }

/-! ## Time -/

/-- The slice of one `Player.Update` that carries the potion state to the next press: delay decrements
(IL_0a7a..0aa8), `UpdateImmunity` and `ResetEffects` (existing `Health.resetEffects`), the delay-time copies
with the Philosopher's Stone multiplier (IL_1f8b..1ff1), and `UpdateBuffs` writing `potionDelay` from buff 21
(IL_3f89..3fa3) and the expiry cleanup (existing `Health.updateBuffs`). `ResetEffects` clears `pStone`,
which the accessory phase then restores; the declared `pStone` is carried. -/
def Player.tick (p : Player) : Player := Id.run do
  let mut p := p
  if p.potionDelay > 0 then p := { p with potionDelay := p.potionDelay - 1 }
  if p.manaPotionDelay > 0 then p := { p with manaPotionDelay := p.manaPotionDelay - 1 }
  let scale (v : Int) : Int := if p.pStone then (Float32.ofInt v * philosopherMultiplier).toInt32.toInt else v
  p := { p with potionDelayTime := scale potionDelayBase, restorationDelayTime := scale restorationDelayBase,
                mushroomDelayTime := scale mushroomDelayBase }
  let h := p.health.resetEffects
  -- ResetEffects also clears the crowd-control flags, mana sickness and statManaMax2 (= statManaMax)
  p := { p with frozen := false, webbed := false, stoned := false, manaSick := false, statManaMax2 := p.health.statManaMax }
  -- UpdateBuffs: tick every live buff; buff 21 mirrors its remaining time into potionDelay
  let mut potionDelay := p.potionDelay
  let mut hh := h
  for i in [:hh.buffType.size] do
    if hh.buffType[i]! > 0 && hh.buffTime[i]! > 0 then
      -- buff 396 (mana heat) reads its entry time; a zero latestDuration becomes 1 (OriginalManaHeat.updateEffects)
      if hh.buffType[i]! == 396 then p := { p with manaHeat := p.manaHeat.updateEffects hh.buffTime[i]! }
      hh := { hh with buffTime := hh.buffTime.set! i (hh.buffTime[i]! - 1) }
      if hh.buffType[i]! == 23 then hh := { hh with noItems := true, cursed := true }
      if hh.buffType[i]! == OriginalPlayerLiquidHealth.onFireBuff then hh := { hh with onFire := true }
      if hh.buffType[i]! == OriginalPlayerLiquidHealth.honeyBuff then hh := { hh with honey := true }
      if hh.buffType[i]! == 21 then potionDelay := hh.buffTime[i]!
      if hh.buffType[i]! == 47 || hh.buffType[i]! == 353 then p := { p with frozen := true }
      if hh.buffType[i]! == 149 then p := { p with webbed := true }
      if hh.buffType[i]! == 156 then p := { p with stoned := true }
      if hh.buffType[i]! == 94 then p := { p with manaSick := true }
  for i in [:hh.buffType.size] do
    if hh.buffType[i]! > 0 && hh.buffTime[i]! ≤ 0 then hh := hh.delBuff i
  return { p with health := hh, potionDelay }

end Lean4Terria.OriginalHealingPotions
