import Lean4Terria.OriginalForestGuide.Resident
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalStarterMelee
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalTownCombat

namespace Lean4Terria.OriginalDungeonMechanic

open OriginalCollision (Grid single tileIndex)
open OriginalForestGuide (Resident)
open OriginalGreenSlime (Player Slime World)

def boundType : Nat := 123

/-- The common NPC fields used by style 0 in a dry dungeon. -/
structure Bound where
  body : Slime
  deriving Repr

def spawn (world : World) (x y : Int) : Bound :=
  ⟨{ position := ⟨single (x - 8), single (y - 30)⟩
     width := 16, height := 30, timeLeft := world.activeTime * 5 / 4
     life := 250, lifeMax := 250, damage := 10, defense := 15, defDamage := 10, defDefense := 15
     knockBackResist := 0.5, value := 0, alpha := 0, color := ⟨0, 0, 0, 0⟩ }⟩

private def move (world : World) (gravity maxFall : Float32) (body : Slime) : Slime :=
  let vx := body.velocity.x * 0.93
  let vx := if vx.toFloat > -0.1 && vx.toFloat < 0.1 then 0 else vx
  let body := { body with velocity := ⟨vx, min maxFall (body.velocity.y + gravity)⟩ }
  OriginalGreenSlime.updateCollision world.grid gravity body

def advance (world : World) (player : Player) (bound : Bound) : Bound × Float32 :=
  let body := bound.body
  if !body.active then (bound, 0) else
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world body
  let body := OriginalGreenSlime.targetClosest #[player]
    { body with oldTarget := body.target, oldDirection := body.direction, oldDirectionY := body.directionY }
  let body := move world gravity maxFall { body with spriteDirection := body.direction }
  let (body, nearby) := OriginalGreenSlime.checkActive world #[player] body
  (⟨{ body with netUpdate := false }⟩, nearby.foldl (fun count (_, slots) => count + slots) 0)

/-! ## Hurt by hostile NPCs

`NPC.UpdateNPC` gives every friendly NPC (the bound Mechanic is `friendly`, not a town NPC) a
`CheckLifeRegen` and a `GetHurtByOtherNPCs(AllNPCs)` step after the AI, the `immune[]`
countdown and gravity, and before `UpdateCollision`. The first active, non-friendly NPC with
damage whose hitbox intersects the Mechanic's, in slot order, calls `BeHurtByOtherNPC`:
`Main.DamageVar(attacker.damage)`, then `StrikeNPCNoInteraction(damage, 6, direction)` (no player
credit) with the direction pointing away from the attacker's centre, and `immune[255] = 30`.
The strike rolls the two combat-text draws, runs the type 123 `HitEffect` (type 5 blood) and
plays `NPCHit1`. `justHit` is cleared again at the end of `UpdateNPC`. -/

open OriginalForestGuideArrow (DustPool DustRequest DustParticle)

/-- The bound Mechanic with the two per-NPC counters this path reads and writes:
`friendlyRegen` and `immune[255]` (kept in `body.immune`). -/
structure HurtBound where
  bound : Bound
  friendlyRegen : Int := 0

/-- One other NPC slot, as `GetHurtByOtherNPCs` reads it at the Mechanic's update: slots below
the Mechanic already updated this tick, higher slots still hold last tick's state. -/
abbrev Other := OriginalTownCombat.Other
abbrev Gore := OriginalTownCombat.Gore
/-- The hit of this update (shared with town NPCs): attacker slot, damage, direction, type 5
dust; when `killed`, `NPCHit1` then `NPCDeath1`, the gores, the `NPCLoot` drops, and for the
rescued (town) Mechanic the panic timer, the announcement and an optional tombstone. -/
abbrev Hit := OriginalTownCombat.Hit

def customMeleeHitbox (typeId : Int) : Bool := OriginalTownCombat.customMeleeHitbox typeId

structure HurtStep where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  bound : HurtBound
  nearby : Float32
  rng : OriginalGreenSlime.Rng
  pool : DustPool
  /-- The hit this update, if any; the caller plays `NPCHit1` at the Mechanic. -/
  hit : Option Hit := none
  unsupported : Option String := none

private def target (h : HurtBound) (typeId : Int) (townNPC : Bool) : OriginalTownCombat.Target :=
  let body := h.bound.body
  { typeId, townNPC, position := body.position, velocity := body.velocity, width := body.width
    height := body.height, life := body.life, lifeMax := body.lifeMax, damage := body.damage
    defense := body.defense, knockBackResist := body.knockBackResist, direction := body.direction
    immune255 := body.immune[255]?.getD 0, friendlyRegen := h.friendlyRegen }

private def absorb (h : HurtBound) (t : OriginalTownCombat.Target) (hit : Option Hit) : HurtBound :=
  let body := h.bound.body
  { bound := ⟨{ body with
      life := t.life, velocity := t.velocity, direction := t.direction
      immune := body.immune.set! 255 t.immune255
      justHit := body.justHit || hit.any (·.text.boundary.isNone)
      active := body.active && !hit.any (·.killed) }⟩
    friendlyRegen := t.friendlyRegen }

/-- `CheckLifeRegen` for type 123 (no Dryad's Ward): +1 per update while hurt, one life at 181. -/
def lifeRegen (h : HurtBound) : HurtBound :=
  absorb h (OriginalTownCombat.lifeRegen (target h 123 false)) none

/-- `GetHurtByOtherNPCs` and `BeHurtByOtherNPC` for the bound Mechanic in `slot`. -/
def hurtByOthers (slot : Nat) (others : Array Other) (pool : DustPool) (rng : UnifiedRandom)
    (h : HurtBound) (loot : OriginalCombat.Loot.Context := {}) (townNPC : Bool := false)
    (goreContext : Option (Grid × OriginalGore.Pool) := none)
    (text : OriginalNpcCombatText.State := {fromPlayer := 255})
    (playSound : Option OriginalTownCombat.SoundHook := none) :
    Except String (HurtBound × Option Hit × DustPool × UnifiedRandom) := do
  let typeId := if townNPC then 124 else 123
  let (t, hit, pool, rng) ← OriginalTownCombat.hurt slot others { dustPool := pool, loot, goreContext,text,playSound } pool rng (target h typeId townNPC)
  return (absorb h t hit, hit, pool, rng)

/-- One `UpdateNPC` of the bound Mechanic in `slot`, including regeneration and hostile-NPC
damage. `others` are the other NPC slots as the update sees them. Thread `rng` (the
`UpdateNPCs` stream) and `pool` like the other NPC updates; a disabled pool reproduces a
host without dust (the headless single-player host), where gores still draw. -/
def advanceAmong (world : World) (player : Player) (slot : Nat) (others : Array Other)
    (rng : OriginalGreenSlime.Rng) (h : HurtBound) (pool : DustPool := { enabled := false })
    (loot : OriginalCombat.Loot.Context := {})
    (goreContext : Option (Grid × OriginalGore.Pool) := none)
    (text : OriginalNpcCombatText.State := {fromPlayer := 255})
    (playSound : Option OriginalTownCombat.SoundHook := none) : HurtStep := Id.run do
  let body := h.bound.body
  if !body.active || text.boundary.isSome then return { bound := h, nearby := 0, rng, pool,text }
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world body
  let body := OriginalGreenSlime.targetClosest #[player]
    { body with oldTarget := body.target, oldDirection := body.direction, oldDirectionY := body.directionY }
  let body := { body with spriteDirection := body.direction }
  let vx : Float32 := body.velocity.x * 0.93
  let vx : Float32 := if vx.toFloat > -0.1 && vx.toFloat < 0.1 then 0 else vx
  let body := { body with
    velocity := ⟨vx, min maxFall (body.velocity.y + gravity)⟩
    immune := body.immune.map fun t => if t > 0 then t - 1 else t }
  let h := lifeRegen { h with bound := ⟨body⟩ }
  match hurtByOthers slot others pool ⟨rng.table, rng.inext⟩ h loot false goreContext text playSound with
  | .error why => return { bound := h, nearby := 0, rng, pool,text, unsupported := some why }
  | .ok (h, hit, pool, random) =>
    let (table, index) := random.state
    let text := (hit.map (·.text)).getD text
    if text.boundary.isSome then
      return {bound := h,nearby := 0,rng := ⟨table,index⟩,pool,hit,text}
    -- A Mechanic killed this update still moves; `CheckActive` returns at once for it.
    let body := OriginalGreenSlime.updateCollision world.grid gravity h.bound.body
    let (body, nearby) := if body.active then OriginalGreenSlime.checkActive world #[player] body else (body, #[])
    return { bound := { h with bound := ⟨{ body with netUpdate := false, justHit := false }⟩ }
             nearby := nearby.foldl (fun count (_, slots) => count + slots) 0
             rng := ⟨table, index⟩, pool, hit,text }

/-- `AI_000_TransformBoundNPC` runs during the NPC phase after the draw pass
sets the player's talk slot. Style 0 finishes the current update at type 124's
dimensions, with the old bottom, velocity and proportional life. -/
def rescue (world : World) (player : Player) (bound : Bound) (names : Array String)
    (rng : OriginalGreenSlime.Rng) : Resident × OriginalGreenSlime.Rng := Id.run do
  let before := bound.body
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world before
  let position := { before.position with y := before.position.y + single before.height - 40 }
  let body := OriginalGreenSlime.targetClosest #[player]
    { before with width := 18, height := 40, position, direction := 0, directionY := 1, target := 255 }
  let homeTile := (tileIndex (position.x + 9), tileIndex (position.y + 40))
  -- GiveTownUniqueData first assigns a regular name, then LegacyNPCProfile
  -- replaces it with its own pick from the same localization category.
  let (_, rng) := rng.next names.size
  let (nameIndex, rng) := rng.next names.size
  let body := move world gravity maxFall body
  let airborne := body.velocity.y != 0
  let walking := body.velocity.x != 0
  return ({ kind := .mechanic, name := names[nameIndex]?.getD "", homeTile, homeless := true,
            life := max 1 (before.life * 250 / before.lifeMax), position := body.position, velocity := body.velocity,
            oldPosition := body.oldPosition, oldVelocity := body.oldVelocity, direction := body.direction,
            directionY := body.directionY, spriteDirection := body.direction, target := body.target,
            collideX := body.collideX, collideY := body.collideY, stairFall := body.stairFall,
            gfxOffY := body.gfxOffY, stepSpeed := body.stepSpeed,
            frameRow := if airborne then 1 else if walking then 2 else 0,
            frameCounter := if walking && !airborne then (2 * body.velocity.x.abs).toFloat + 1 else 0 }, rng)

structure RescueStep where
  text : OriginalNpcCombatText.State := {fromPlayer := 255}
  resident : Resident
  rng : OriginalGreenSlime.Rng
  pool : DustPool
  /-- A hit taken by the new type 124 in the rescue update itself. When `killed`, the resident
  is inactive (a town NPC death: announcement, optional tombstone, `NPCLoot`). -/
  hit : Option Hit := none
  unsupported : Option String := none

/-- The rescue update with the counters the original keeps. `Transform(124)` leaves
`immune[255]` and `friendlyRegen` as they were; the rest of the same `UpdateNPC` then counts
`immune[]` down, runs `CheckLifeRegen` (+1 for type 124) and `GetHurtByOtherNPCs` on the new
18 x 40 town NPC before the collision, so an expired immunity next to a hostile NPC is hit (and
possibly killed) in the rescue update. -/
def rescueAmong (world : World) (player : Player) (slot : Nat) (others : Array Other) (names : Array String)
    (rng : OriginalGreenSlime.Rng) (h : HurtBound) (pool : DustPool := { enabled := false })
    (loot : OriginalCombat.Loot.Context := {}) (allHardcore : Bool := false)
    (goreContext : Option (Grid × OriginalGore.Pool) := none)
    (text : OriginalNpcCombatText.State := {fromPlayer := 255})
    (playSound : Option OriginalTownCombat.SoundHook := none) : RescueStep := Id.run do
  let before := h.bound.body
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world before
  let position := { before.position with y := before.position.y + single before.height - 40 }
  let body := OriginalGreenSlime.targetClosest #[player]
    { before with width := 18, height := 40, position, direction := 0, directionY := 1, target := 255
                  life := max 1 (before.life * 250 / before.lifeMax), lifeMax := 250 }
  let homeTile := (tileIndex (position.x + 9), tileIndex (position.y + 40))
  let (_, rng) := rng.next names.size
  let (nameIndex, rng) := rng.next names.size
  let vx : Float32 := body.velocity.x * 0.93
  let vx : Float32 := if vx.toFloat > -0.1 && vx.toFloat < 0.1 then 0 else vx
  let body := { body with
    velocity := ⟨vx, min maxFall (body.velocity.y + gravity)⟩
    immune := body.immune.map fun t => if t > 0 then t - 1 else t }
  let h := absorb { h with bound := ⟨body⟩ } (OriginalTownCombat.lifeRegen (target { h with bound := ⟨body⟩ } 124 true)) none
  let effects : OriginalTownCombat.Effects := { dustPool := pool, loot, allHardcore, goreContext,text,playSound }
  let (h, hit, pool, rng, problem) ← match OriginalTownCombat.hurt slot others effects pool ⟨rng.table, rng.inext⟩ (target h 124 true) with
    | .ok (t, hit, pool, random) =>
      let (table, index) := random.state
      pure (absorb h t hit, hit, pool, (⟨table, index⟩ : OriginalGreenSlime.Rng), (none : Option String))
    | .error why => pure (h, none, pool, rng, some why)
  let text := (hit.map (·.text)).getD text
  let body := if text.boundary.isSome then h.bound.body
    else OriginalGreenSlime.updateCollision world.grid gravity h.bound.body
  let airborne := body.velocity.y != 0
  let walking := body.velocity.x != 0
  let resident : Resident := {
    kind := .mechanic, name := names[nameIndex]?.getD "", homeTile, homeless := true, active := body.active,
    life := body.life, position := body.position, velocity := body.velocity,
    oldPosition := body.oldPosition, oldVelocity := body.oldVelocity, direction := body.direction,
    directionY := body.directionY, spriteDirection := body.direction, target := body.target,
    collideX := body.collideX, collideY := body.collideY, stairFall := body.stairFall,
    gfxOffY := body.gfxOffY, stepSpeed := body.stepSpeed,
    friendlyRegen := h.friendlyRegen.toNat, hostileImmunity := (body.immune[255]?.getD 0).toNat,
    frameRow := if airborne then 1 else if walking then 2 else 0,
    frameCounter := if walking && !airborne then (2 * body.velocity.x.abs).toFloat + 1 else 0 }
  -- A hit in the rescue update leaves the town AI walking away on the panic timer, as the
  -- Guide's `hostileContact` writes it.
  let resident := match hit.bind (·.panic) with
    | some timer => { resident with activity := .walking, sitting := false, talkTarget := 0
                                    timer := single timer, turnTimer := 0 }
    | none => resident
  return { resident, rng, pool, hit,text, unsupported := problem }

end Lean4Terria.OriginalDungeonMechanic
