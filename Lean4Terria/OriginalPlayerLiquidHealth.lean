import Lean4Terria.OriginalPlayerWet
import Lean4Terria.OriginalTileRules
import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalCombatTextChannel
import Lean4Terria.OriginalPlayerHurt

/-!
# Breath, drowning and lava contact (Terraria 1.4.5.8, single player, Classic, no equipment)

Hand port from the IL of `TerrariaServer.exe` 1.4.5.8 (`.lake/original-player-liquid-health/probe/il/`) of the
parts of `Player.Update` that settle life in liquids:

* `Player.UpdateImmunity` (`Update` IL_1f08): `immuneTime − 1` (`immune` off at 0); every `hurtCooldowns[i] > 0`
  counts down.
* `Player.ResetEffects` (IL_2006): `lifeRegen 0`, `statLifeMax2 = statLifeMax`, `onFire`, `honey`, `lavaMax`,
  `lavaImmune` off.
* `Player.UpdateBuffs` (IL_2281): each live buff loses a tick, then sets its flag (24 On Fire! → `onFire`,
  48 Honey → `honey`); IL_23c5 then deletes buffs whose time reached 0 (`DelBuff`, compacting).
* `Player.UpdateLifeRegen` (IL_3228): On Fire! (`lifeRegenTime 0`, `lifeRegen` capped at 0, −8), Honey
  (+4 towards 0 when negative; `lifeRegenTime + 2`, `lifeRegen + 2`), `lifeRegenTime + 1`, the natural regen
  steps at 300 … 3000 ticks (+1 at 3600, capped), ×1.25 standing / ×0.5 moving, × `(statLifeMax2 / 400 · 0.85 +
  0.15)`, `Math.Round` (to even) into `lifeRegen`; `lifeRegenCount += lifeRegen`; +1 life per +120,
  `HurtLifeRegen(min(count / −120, 4))` per −120 (a `CombatText` request), death `KillMe(ByOther(8))`.
* `Player.CheckDrowning` (IL_7270): `Collision.DrownCollision` (head box ≤ 10 × 12 px from `position.Y − 2`, water
  or honey); drowning: `breathCD + 1`, at `breathCDMax` 7: `breath − 1`; at 0: `lifeRegenTime 0`, `statLife − 2`,
  death `KillMe(ByOther(1))`; otherwise `breath + 3` (capped at `breathMax` 200), `breathCD 0`. A drowning frame
  draws `Main.rand.Next(20)` (a bubble on 0, client only).
* The lava block (IL_72c1–IL_7418): `lavaWet = Collision.LavaCollision`; in lava without `lavaImmune` and with
  `hurtCooldowns[Lava] ≤ 0`: `lavaTime − 1` if positive, else `Hurt(ByOther(2), 80, cooldown Lava)` and On Fire!
  for 420 ticks when it hurt; out of lava `lavaTime` grows to `lavaMax` (0 without equipment).
* `Player.Hurt` for that call: `CalculateDamagePlayersTake` (Classic: `80 − defense · 0.5`, at least 1), a
  `CombatText`, `statLife − damage`, `hurtCooldowns[Lava] = 40` (20 for 1 damage), `lifeRegenTime 0`,
  the blood dust loop (`ceil(damage / statLifeMax2 · 100)` `NextFloat` draws while alive), or at ≤ 0 life
  `statLife 0` and `KillMe(ByOther(2))`.
* After `Collision.WetCollision` (IL_7445, `OriginalPlayerWet.wetCollision`): honey → Honey buff 1800; any
  non-lava liquid while On Fire! deletes it (IL_7542).
* A dead player runs `Player.UpdateDead` (IL_0b4e) instead: non-persistent buffs cleared in place, life 0,
  `respawnTimer − 1` (respawn at 0).

The drowning, lava and wet probes all read the position the frame started with (the collision step that moves the
player comes later, IL_8ab8); `UpdateLifeRegen` reads the velocity the frame started with.

An optional client TextChannel now executes those popup requests against the
shared100-slot pool, real font measurement and actual caller body rectangle.
Full/server requests consume no samples. With no channel, the legacy API still
assumes an available ordinary popup and consumes its former two samples.
Signed negative regeneration division follows the desktop CIL's truncation.
Current desktop full-method evidence: `.lake/original-health-combat-text/`.

Not modelled: `KillMe` itself (gore, dust, tombstone, sounds and their draws; it is reported as `Event.died`),
`Player.Spawn` at respawn (`Event.respawn`), the client-only bubble / blood `Dust.NewDust` draws and sound pitch
draws (a dedicated host runs none of them), accessories (Breathing Reed, Diving Helmet, Gills, lava charms,
Obsidian Rose, Shiny Stone …), armour, Expert / Master, shimmer and mounts.
-/

namespace Lean4Terria.OriginalPlayerLiquidHealth

open OriginalCollision (Grid Vector single)
open OriginalWorld (Tile LiquidKind)

/-- `ImmunityCooldownID.Lava` -/
def lavaCooldown : Nat := 4
def onFireBuff : Nat := 24
def honeyBuff : Nat := 48
def maxBuffs : Nat := 44
def respawnSkipDelay : Int := 90

/-- Life and liquid fields of `Player` this module settles. -/
structure Health where
  statLife : Int := 100
  statLifeMax : Int := 100
  statLifeMax2 : Int := 100
  statMana : Int := 0
  statManaMax : Int := 20
  manaRegen : Int := 0
  manaRegenCount : Int := 0
  usedArcaneCrystal : Bool := false
  breath : Int := 200
  breathMax : Int := 200
  breathCD : Int := 0
  lifeRegen : Int := 0
  lifeRegenCount : Int := 0
  lifeRegenTime : Float32 := 0
  immune : Bool := false
  immuneTime : Int := 0
  immuneNoBlink : Bool := false
  hurtCooldowns : Array Int := Array.replicate 6 0
  lavaTime : Int := 0
  lavaMax : Int := 0
  buffType : Array Nat := Array.replicate maxBuffs 0
  buffTime : Array Int := Array.replicate maxBuffs 0
  /-- ResetEffects / UpdateBuffs flags for this tick, including an expiring Cursed buff. -/
  noItems : Bool := false
  cursed : Bool := false
  onFire : Bool := false
  honey : Bool := false
  lavaWet : Bool := false
  dead : Bool := false
  deadTime : Int := 0
  respawnTimer : Int := 0
  deathsPVE : Int := 0
  lostCoins : Nat := 0
  deriving Repr, Inhabited

inductive Cause where
  | drowned | lava | burned | npc | fall
  deriving BEq, Repr

inductive Event where
  /-- `KillMe` ran (its gore, dust, items and draws are the caller's) -/
  | died (cause : Cause)
  /-- `respawnTimer` reached 0: `Player.Spawn` is the caller's -/
  | respawn
  /-- `Hurt` from lava: damage dealt -/
  | lavaHit (damage : Int)
  | npcHit (damage : Int)
  /-- `Hurt` from a fall: damage dealt -/
  | fallHit (damage : Int)
  deriving BEq, Repr

/-- The player-stage random stream (`Main.SwapRandom("UpdatePlayers")`) with a draw counter. -/
structure Rng where
  stream : UnifiedRandom
  draws : Nat := 0
  /-- none is the explicit legacy headless contract: each modeled popup keeps
  its original fixed skip. A client supplies the real shared pool/font/rect. -/
  text : Option OriginalCombatTextChannel.Channel := none
  /-- An allocation/measurement failure stops the current health operation.
  The owning caller must surface this boundary before advancing more phases. -/
  boundary : Option String := none

def Rng.skip (r : Rng) (n : Nat) : Rng := Id.run do
  let mut s := r.stream
  for _ in [:n] do s := (s.sample).2
  return { r with stream := s, draws := r.draws + n }

/-- A health owner creates text at the original call site. Legacy callers keep
the old fixed skip; client callers commit actual shared allocation and RNG. -/
def Rng.combatNumber (r : Rng) (amount : Int) (color : OriginalForestChatLayout.Color)
    (dramatic dot : Bool) : Rng :=
  if r.boundary.isSome then r else
  match r.text with
  | none => r.skip (if dramatic then 3 else 2)
  | some text =>
    match text.number amount color dramatic dot r.stream with
    | .ok out => {r with stream:=out.random,draws:=r.draws+out.draws,text:=some out.channel}
    | .error why => {r with boundary:=some s!"health CombatText.NewText: {why}"}

/-! ## Collision probes -/

private def amount (t : Tile) : Nat := (t.liquid.map (·.amount.toNat)).getD 0
private def kind (t : Tile) : Option LiquidKind := t.liquid.map (·.kind)
private def clampI (v lo hi : Int) : Int := if v < lo then lo else if v > hi then hi else v
private def trunc (f : Float32) : Int := f.toInt32.toInt

/-- `Collision.DrownCollision(position, width, height, gravDir 1, false)` -/
def drownCollision (grid : Grid) (position : Vector) (width height : Nat) : Bool := Id.run do
  let cx := position.x + single (width / 2 : Nat)
  let w : Nat := if 10 > width then width else 10
  let h : Nat := if 12 > height then height else 12
  let bx := cx - single (w / 2 : Nat)
  let by_ := position.y + (-2 : Float32)
  let maxY : Int := (grid.height : Int) - 40
  let x0 := clampI (trunc (position.x / 16) - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampI (trunc ((position.x + single width) / 16) + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampI (trunc (position.y / 16) - 1) 0 maxY
  let y1 := clampI (trunc ((position.y + single height) / 16) + 2) 0 maxY
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let t := grid.tileAt x y
      let a := amount t
      if a == 0 || kind t == some .lava || kind t == some .shimmer then continue
      if (y : Int) == y0 then
        if let some id := t.id then
          let r := grid.rules id
          if r.solid && !r.solidTop then continue
      let d : Float32 := (single (256 - a : Nat)) / 32
      let tx := single (x * 16 : Nat)
      let ty := single (y * 16 : Nat) + d * 2
      let th : Int := 16 - trunc (d * 2)
      if bx + single w > tx && bx < tx + 16 && by_ + single h > ty && by_ < ty + (Float32.ofInt th) then return true
  return false

/-- `Collision.LavaCollision(position, width, height)` -/
def lavaCollision (grid : Grid) (position : Vector) (width height : Nat) : Bool := Id.run do
  let maxY : Int := (grid.height : Int) - 40
  let x0 := clampI (trunc (position.x / 16) - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampI (trunc ((position.x + single width) / 16) + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampI (trunc (position.y / 16) - 1) 0 maxY
  let y1 := clampI (trunc ((position.y + single height) / 16) + 2) 0 maxY
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let t := grid.tileAt x y
      let a := amount t
      if a == 0 || kind t != some .lava then continue
      let d : Float32 := (single (256 - a : Nat)) / 32
      let tx := single (x * 16 : Nat)
      let ty := single (y * 16 : Nat) + d * 2
      let th : Int := 16 - trunc (d * 2)
      if position.x + single width > tx && position.x < tx + 16 &&
          position.y + single height > ty && position.y < ty + (Float32.ofInt th) then return true
  return false

/-! ## Buffs -/

/-- `Player.DelBuff`: clear the slot, then compact the first `maxBuffs - 1` slots.
The final slot is cleared when requested but is not moved by compaction. -/
def Health.delBuff (h : Health) (i : Nat) : Health := Id.run do
  let mut ty := h.buffType.set! i 0
  let mut tm := h.buffTime.set! i 0
  let mut free := 0
  for j in [:ty.size - 1] do
    if tm[j]! != 0 && ty[j]! != 0 then
      if free < j then
        ty := (ty.set! free ty[j]!).set! j 0
        tm := (tm.set! free tm[j]!).set! j 0
      free := free + 1
  return { h with buffType := ty, buffTime := tm }

/-- `Player.AddBuff(type, time)` for a buff that is neither additive nor a pet / melee buff: an existing one keeps
the longer time, a new one takes the first empty slot (no debuff in the way here). -/
def Health.addBuff (h : Health) (type : Nat) (time : Int) : Health := Id.run do
  for i in [:h.buffType.size] do
    if h.buffType[i]! == type then
      return if h.buffTime[i]! < time then { h with buffTime := h.buffTime.set! i time } else h
  for i in [:h.buffType.size] do
    if h.buffType[i]! == 0 then
      return { h with buffType := h.buffType.set! i type, buffTime := h.buffTime.set! i time }
  return h

/-! ## Life regeneration -/

/-- `Math.Round(double)`: half to even. -/
def roundHalfEven (x : Float) : Int :=
  let f := x.floor
  let r := x - f
  let fi : Int := if f < 0 then -((-f).toUInt64.toNat : Int) else (f.toUInt64.toNat : Int)
  if r > 0.5 then fi + 1 else if r < 0.5 then fi else if fi % 2 == 0 then fi else fi + 1

private def lifeRegenFactor (statLifeMax2 : Int) : Float32 :=
  (Float32.ofInt statLifeMax2) / 400 * (0.85 : Float32) + (0.15 : Float32)

/-- HurtLifeRegen subtracts life BEFORE requesting LifeRegen-colored DOT text.
The rectangle is the owner's current integer body rectangle in rng.text. -/
def Health.hurtLifeRegen (h : Health) (damage : Int) (rng : Rng) : Health × Rng :=
  if rng.boundary.isSome then (h,rng) else
  let h := {h with statLife:=h.statLife-damage}
  (h,rng.combatNumber damage OriginalCombatText.lifeRegen false true)

/-- `Player.UpdateLifeRegen` for a player without equipment; `velocityX` is `velocity.X` when it runs.
Returns the death (if any), actual client text allocation and stream. -/
def Health.updateLifeRegen (h : Health) (velocityX : Float32) (rng : Rng) : Health × Option Event × Rng := Id.run do
  if rng.boundary.isSome then return (h,none,rng)
  let mut h := h
  let mut rng := rng
  let mut event := none
  if h.onFire then
    if h.lifeRegen > 0 then h := { h with lifeRegen := 0 }
    h := { h with lifeRegenTime := 0, lifeRegen := h.lifeRegen - 8 }
  if h.honey && h.lifeRegen < 0 then
    h := { h with lifeRegen := h.lifeRegen + 4 }
    if h.lifeRegen > 0 then h := { h with lifeRegen := 0 }
  h := { h with lifeRegenTime := h.lifeRegenTime + 1 }
  if h.honey then h := { h with lifeRegenTime := h.lifeRegenTime + 2, lifeRegen := h.lifeRegen + 2 }
  let mut num : Float32 := 0
  for step in [300, 600, 900, 1200, 1500, 1800, 2400, 3000] do
    if h.lifeRegenTime ≥ (Float32.ofNat step) then num := num + 1
  if h.lifeRegenTime ≥ 3600 then
    num := num + 1
    h := { h with lifeRegenTime := 3600 }
  num := if velocityX == 0 then num * (1.25 : Float32) else num * (0.5 : Float32)
  num := num * lifeRegenFactor h.statLifeMax2
  h := { h with lifeRegen := h.lifeRegen + roundHalfEven num.toFloat }
  h := { h with lifeRegenCount := h.lifeRegenCount + h.lifeRegen }
  while h.lifeRegenCount ≥ 120 do
    h := { h with lifeRegenCount := h.lifeRegenCount - 120 }
    if h.statLife < h.statLifeMax2 then h := { h with statLife := h.statLife + 1 }
    if h.statLife > h.statLifeMax2 then h := { h with statLife := h.statLifeMax2 }
  while h.lifeRegenCount ≤ -120 do
    -- CIL div truncates toward zero, including non-multiple negative counts.
    let n := min (h.lifeRegenCount.tdiv (-120)) 4
    h := { h with lifeRegenCount := h.lifeRegenCount + 120 * n }
    let out := h.hurtLifeRegen n rng
    h := out.1
    rng := out.2
    if rng.boundary.isSome then return (h,event,rng)
    if h.statLife ≤ 0 && !h.dead then
      h := { h with dead := true, respawnTimer := 600 }
      event := some (.died .burned)
  return (h, event, rng)

/-! ## The frame -/

/-- What the frame needs besides the health fields. -/
structure Input where
  /-- `position` when the frame starts (the probes run before this frame's movement collision) -/
  position : Vector
  /-- `velocity.X` when the frame starts -/
  velocityX : Float32 := 0
  width : Nat := 20
  height : Nat := 42
  statDefense : Int := 0

/-- The existing immunity and ResetEffects phase. Environmental Buff refresh
runs after this phase and before `updateBuffs`. -/
def Health.resetEffects (h : Health) : Health := Id.run do
  let mut h := h
  if h.immune then
    h := { h with immuneTime := h.immuneTime - 1 }
    if h.immuneTime ≤ 0 then h := { h with immune := false, immuneNoBlink := false }
  h := { h with hurtCooldowns := h.hurtCooldowns.map fun c => if c > 0 then c - 1 else c }
  h := { h with lifeRegen := 0, statLifeMax2 := h.statLifeMax, onFire := false, honey := false, lavaMax := 0, noItems := false, cursed := false }
  return h

/-- `UpdateBuffs` and the ordinary expired-slot cleanup, once per living update. -/
def Health.updateBuffs (h : Health) : Health := Id.run do
  let mut h := h
  for i in [:h.buffType.size] do
    if h.buffType[i]! > 0 && h.buffTime[i]! > 0 then
      h := { h with buffTime := h.buffTime.set! i (h.buffTime[i]! - 1) }
      if h.buffType[i]! == 23 then h := { h with noItems := true, cursed := true }
      if h.buffType[i]! == onFireBuff then h := { h with onFire := true }
      if h.buffType[i]! == honeyBuff then h := { h with honey := true }
  for i in [:h.buffType.size] do
    if h.buffType[i]! > 0 && h.buffTime[i]! ≤ 0 then h := h.delBuff i
  return h

/-- Early living update for callers without an intervening environmental refresh. -/
def Health.beginFrame (h : Health) (velocityX : Float32) (rng : Rng) : Health × Option Event × Rng :=
  h.resetEffects.updateBuffs.updateLifeRegen velocityX rng

/-- Ordinary `UpdateManaRegen` with no equipment bonus or spell-use delay.
With the installed ManaV2 behavior, a still-running item animation or reuse
delay suppresses the five refill dust-scale rolls, while mana still recovers.
The caller supplies this gate before the item phase decrements its timers. -/
def Health.advanceMana (h : Health) (stillOrGrappling regenBuff : Bool) (rng : Rng)
    (isUsingItem : Bool := false) (consumeRefillScaleRolls : Bool := true)
    (evenWhenDead : Bool := false) : Health × Rng :=
  -- `UpdateManaRegen` itself does not test `dead`: a player killed earlier in the same update (the
  -- fall block) still regenerates in it. The default keeps the earlier contract of direct callers.
  if rng.boundary.isSome || (h.dead && !evenWhenDead) || h.statManaMax ≤ 0 then (h, rng) else
  let rate := h.statManaMax / 3 + 1
  let standingBonus := if stillOrGrappling || regenBuff then rate else 0
  let upgrade := if h.usedArcaneCrystal then h.statManaMax / 50 else 0
  let fraction : Float32 := Float32.ofInt h.statMana / Float32.ofInt h.statManaMax
  let scale : Float32 := if regenBuff then 1 else 0.5 + 0.5 * fraction
  let regen := max 2 ((Float32.ofInt (rate + standingBonus + upgrade) * scale).toInt32.toInt)
  let count := h.manaRegenCount + regen
  let restored := count / 120
  let mana := min h.statManaMax (h.statMana + restored)
  let full := h.statMana < h.statManaMax && mana == h.statManaMax
  ({ h with statMana := mana, manaRegen := regen, manaRegenCount := count % 120 },
    if consumeRefillScaleRolls && full && !isUsingItem then rng.skip 5 else rng)

/-- What a window client adds to the lava `Hurt`: its sound side and the real shared dust pool
with the player's rectangle. Without it the legacy headless contract applies: no PlayHurtSound
call, and the blood loop only consumes its `NextFloat` prefix draws. -/
structure HurtClient where
  hurt : OriginalPlayerHurt.Client := {}
  pool : OriginalForestGuideArrow.DustPool
  position : Vector
  width : Nat
  height : Nat

structure LiquidResult where
  health : Health
  events : Array Event := #[]
  rng : Rng
  /-- Blood particles a client's lava hit created, in birth order. -/
  dust : Array OriginalForestGuideArrow.DustParticle := #[]
  hurt : Option OriginalPlayerHurt.Record := none
  /-- The client's sound side after the hit; none when no lava hit reached the sound. -/
  sound : Option OriginalPlayerHurt.SoundClient := none

/-- `CheckDrowning` and the lava block: after horizontal movement, before `WetCollision`.
The lava hit is `Hurt(ByOther(2), 80, 0, …, cooldown Lava)`: text, signed life, cooldown,
no knockback (hitDirection 0), PlayHurtSound, then blood or death (`OriginalPlayerHurt.finish`).
A sound or blood boundary is reported through `rng.boundary` with everything before it kept. -/
def Health.liquidChecksWith (h : Health) (drowning lava : Bool) (statDefense : Int) (rng : Rng)
    (client : Option HurtClient := none) : LiquidResult := Id.run do
  if rng.boundary.isSome then return {health:=h,rng}
  let mut h := h
  let mut rng := rng
  let mut events : Array Event := #[]
  let mut dust : Array OriginalForestGuideArrow.DustParticle := #[]
  let mut hurt : Option OriginalPlayerHurt.Record := none
  let mut sound : Option OriginalPlayerHurt.SoundClient := none
  -- CheckDrowning
  if drowning then
    h := { h with breathCD := h.breathCD + 1 }
    if h.breathCD ≥ 7 then
      h := { h with breathCD := 0, breath := h.breath - 1 }
      if h.breath ≤ 0 then
        h := { h with lifeRegenTime := 0, breath := 0, statLife := h.statLife - 2 }
        if h.statLife ≤ 0 then
          h := { h with statLife := 0, dead := true, respawnTimer := 600 }
          events := events.push (.died .drowned)
  else
    h := { h with breath := min h.breathMax (h.breath + 3), breathCD := 0 }
  if drowning then rng := rng.skip 1                -- Main.rand.Next(20): the bubble roll
  -- lava
  h := { h with lavaWet := lava }
  let mut away := true
  if lava then
    away := false
    if h.hurtCooldowns[lavaCooldown]! ≤ 0 then
      if h.lavaTime > 0 then h := { h with lavaTime := h.lavaTime - 1 }
      else
        -- Hurt(ByOther(2), 80, 0, …, cooldown Lava, dodgeable)
        let dmgF : Float := max 1 ((80 : Float) - (Float.ofInt statDefense) * 0.5)
        -- Hurt IL_03da..03eb: `num = (int)((1 - endurance) * num)`. From here on the amount is
        -- integral: the text, the life write, the cooldown test and the blood count all use it.
        let dmg : Int := (dmgF.floor.toUInt64.toNat : Int)
        -- Hurt emits ordinary friendly-damage text BEFORE subtracting life.
        rng := rng.combatNumber dmg OriginalCombatText.damagedFriendly false false
        if rng.boundary.isSome then return {health:=h,events,rng}
        h := { h with statLife := h.statLife - dmg }
        let cd : Int := if dmg == 1 then 20 else 40
        if h.hurtCooldowns[lavaCooldown]! == 0 then
          h := { h with hurtCooldowns := h.hurtCooldowns.set! lavaCooldown cd }
        h := { h with lifeRegenTime := 0 }
        events := events.push (.lavaHit dmg)
        match client with
        | none =>
          if h.statLife > 0 then
            -- the blood dust loop: one NextFloat per iteration before Dust.NewDust
            let n := (Float.ofInt dmg / (Float.ofInt h.statLifeMax2) * 100).ceil.toUInt64.toNat
            rng := rng.skip n
          else
            h := { h with statLife := 0, dead := true, respawnTimer := 600 }
            events := events.push (.died .lava)
        | some client =>
          let tail := OriginalPlayerHurt.finish {
            position := client.position, width := client.width, height := client.height
            damage := dmg, rawLife := h.statLife, lifeMax2 := h.statLifeMax2
            hitDirection := 0 } client.hurt.sound client.pool rng.stream
          rng := { rng with stream := tail.random, boundary := tail.boundary }
          h := { h with statLife := tail.life }
          dust := tail.particles
          hurt := some tail.record
          sound := tail.sound
          if tail.boundary.isSome then return {health:=h,events,rng,dust,hurt,sound}
          if tail.record.lethal then
            h := { h with dead := true, respawnTimer := 600 }
            events := events.push (.died .lava)
        if dmg > 0 then h := h.addBuff onFireBuff 420
  if away && h.lavaTime < h.lavaMax then h := { h with lavaTime := h.lavaTime + 1 }
  if h.lavaTime > h.lavaMax then h := { h with lavaTime := h.lavaMax }
  return {health:=h,events,rng,dust,hurt,sound}

/-- The headless form of `liquidChecksWith`: no sound call and no blood allocation. -/
def Health.liquidChecks (h : Health) (drowning lava : Bool) (statDefense : Int) (rng : Rng) :
    Health × Array Event × Rng :=
  let out := h.liquidChecksWith drowning lava statDefense rng
  (out.health, out.events, out.rng)

/-- After `Collision.WetCollision` (`hit` from `OriginalPlayerWet.wetCollision` at the frame's start position):
the Honey buff, water putting out On Fire!, and `lavaWet` cleared when the wet probe misses. -/
def Health.afterWet (h : Health) (hit : OriginalPlayerWet.WetHit) : Health := Id.run do
  let mut h := h
  if hit.honey then h := h.addBuff honeyBuff 1800
  if hit.wet && h.onFire && !h.lavaWet then
    for i in [:maxBuffs] do
      if h.buffType[i]! == onFireBuff then h := h.delBuff i
  if !hit.wet then h := { h with lavaWet := false }
  return h

/-- `Player.UpdateDead` for the fields here. -/
def Health.deadFrame (h : Health) : Health × Option Event := Id.run do
  let mut h := { h with onFire := false, honey := false, statLife := 0 }
  for i in [:h.buffType.size] do
    let buff := h.buffType[i]!
    if buff > 0 && buff != 71 && !(73 ≤ buff && buff ≤ 79) then
      h := { h with buffType := h.buffType.set! i 0, buffTime := h.buffTime.set! i 0 }
  let t := min 3600 (max 0 (h.respawnTimer - 1))
  h := { h with respawnTimer := t, deadTime := h.deadTime + 1 }
  return (h, if t == 0 then some .respawn else none)

/-- The life and immunity fields set by `Player.Spawn` for a living local Classic player. -/
def Health.spawn (h : Health) (joining : Bool) : Health :=
  let revived := h.statLife ≤ 0
  { h with
    statLife := if revived then max 100 (min h.statLifeMax h.statLifeMax2 / 2) else h.statLife
    breath := if revived then h.breathMax else h.breath
    dead := false
    deadTime := 0
    lavaWet := false
    immune := true
    immuneTime := if joining then 60 else 180
    immuneNoBlink := true }

/-- One `Player.Update` of this module's fields, in the vanilla order, on a map: the drowning, lava and wet probes
at the frame's start position. -/
def Health.step (h : Health) (grid : Grid) (input : Input) (rng : Rng) : Health × Array Event × Rng :=
  if rng.boundary.isSome then (h,#[],rng) else if h.dead then
    let (h, e) := h.deadFrame
    (h, e.toArray, rng)
  else
    let (h, e1, rng) := h.beginFrame input.velocityX rng
    if rng.boundary.isSome then (h,e1.toArray,rng) else
    let drowning := drownCollision grid input.position input.width input.height
    let lava := lavaCollision grid input.position input.width input.height
    let (h, es, rng) := h.liquidChecks drowning lava input.statDefense rng
    if rng.boundary.isSome then (h,e1.toArray ++ es,rng) else
    let hit := OriginalPlayerWet.wetCollision grid input.position input.width input.height
    (h.afterWet hit, e1.toArray ++ es, rng)

/-- `step` on a decoded map, for a session: `start` is the player's body when the frame began (before the movement
step), `stream` the player-stage random stream (`OriginalSession.State.itemRandom`). Returns the advanced stream. -/
def Health.stepOn (h : Health) (terrain : OriginalWorld.Terrain) (start : OriginalPlayerCollision.Body)
    (stream : UnifiedRandom) (statDefense : Int := 0) : Health × Array Event × UnifiedRandom :=
  let grid := OriginalCollision.Grid.ofTerrain terrain OriginalTileRules.collision
  let (h, events, rng) := h.step grid
    { position := start.position, velocityX := start.velocity.x, width := start.width, height := start.height,
      statDefense } { stream }
  (h, events, rng.stream)

end Lean4Terria.OriginalPlayerLiquidHealth
