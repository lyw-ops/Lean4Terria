import Lean4Terria.OriginalPlayerArrows

/-!
# `Main.projectile` pool (desktop 1.4.5.8): one owner for slots, keys and the update order

Main B's unified projectile pool. It owns what is shared between projectile kinds natively:

* the 1000 update slots plus the extra `Main.projectile[1000]` that `NewProjectileSetup` can write
  when every slot is `netImportant` (the update loop never visits it);
* `Projectile.slotGenerations` (`ProjectileKey` generation per slot; never reset by the game);
* `NewProjectile`'s slot choice (released `OriginalPlayerArrows.allocate`: first inactive slot, else
  `FindOldestProjectile`) — it reads every kind's `active`/`netImportant`/`timeLeft`;
* `Main.UpdateProjectiles` order: ascending slot `0..999`, one `Projectile.Update` each, the
  `UpdateProjectiles` random stream, the dust pool and the combat-text state threaded through.

Kinds: wooden arrows (`OriginalPlayerArrows`, Root ACCEPT, `81105bb4…`) are owned here. Every other
kind stays with its current module (Session/Combat stab, Guide arrow, gravestone, …) and is carried
through the type parameter `κ` with its own `SlotInfo` and update callback, so those modules can move
in without an import cycle (socket request). A slot reused while live is overwritten without `Kill`,
as `SetDefaults` does natively.
-/
namespace Lean4Terria.OriginalProjectiles

open OriginalPlayerArrows (SlotInfo Key Launch Arrow Target Hit Step EndReason)
open OriginalForestGuideArrow (DustPool DustParticle)

/-- `Main.projectile.Length` (1001: the update loop covers `0..999`). -/
def physicalSlots : Nat := OriginalPlayerArrows.maxProjectiles + 1

/-- What lives in one `Main.projectile[i]`. -/
inductive Entry (κ : Type) where
  | woodenArrow (arrow : Arrow)
  /-- A kind owned by another module, with the fields `NewProjectileSetup` reads. -/
  | other (info : SlotInfo) (value : κ)
  /-- A projectile `NewProjectile` really created but no module owns yet (e.g. Flaming Arrow, type 2).
  It keeps slot allocation and `slotGenerations` exact; its `Projectile.Update` is a boundary. -/
  | unowned (info : SlotInfo) (type : Nat)

def Entry.info {κ : Type} : Entry κ → SlotInfo
  | .woodenArrow a => { active := a.active, netImportant := false, timeLeft := a.timeLeft }
  | .other info _ => info
  | .unowned info _ => info

structure Pool (κ : Type) where
  slots : Array (Option (Entry κ)) := Array.replicate physicalSlots none
  /-- `Projectile.slotGenerations` (indexed like `Main.projectile`). -/
  generations : Array Nat := Array.replicate physicalSlots 0

def Pool.infos {κ : Type} (p : Pool κ) : Array SlotInfo :=
  p.slots.map fun e => (e.map Entry.info).getD {}

/-- `NewProjectile` slot setup: allocate, `new ProjectileKey(myPlayer, slot, ++slotGenerations[slot])`,
then the entry built for that slot/key overwrites whatever was there. No random draws. -/
def Pool.newProjectile {κ : Type} (p : Pool κ) (myPlayer : Nat) (make : Nat → Key → Entry κ) :
    Pool κ × Nat × Key :=
  let slot := OriginalPlayerArrows.allocate p.infos
  let (key, gen) := OriginalPlayerArrows.nextKey myPlayer slot (p.generations.getD slot 0)
  ({ slots := p.slots.set! slot (some (make slot key)), generations := p.generations.set! slot gen }, slot, key)

/-- `Projectile.NewProjectile` of a wooden arrow (type 1) from a typed launch (bow `ItemCheck_Shoot`). -/
def Pool.launchArrow {κ : Type} (p : Pool κ) (myPlayer : Nat) (launch : Launch) : Pool κ × Arrow :=
  let (p, slot, _) := p.newProjectile myPlayer fun slot key => .woodenArrow (OriginalPlayerArrows.spawn launch slot key)
  let arrow := match p.slots.getD slot none with
    | some (.woodenArrow a) => a
    | _ => OriginalPlayerArrows.spawn launch slot default
  (p, arrow)

/-- `NewProjectile` of a projectile type no module owns yet. `timeLeft` is the value `SetDefaults` would
leave (the caller states it; it only matters to `FindOldestProjectile` on a full pool). -/
def Pool.newUnowned {κ : Type} (p : Pool κ) (myPlayer type : Nat) (timeLeft : Int) : Pool κ × Nat :=
  let (p, slot, _) := p.newProjectile myPlayer fun _ _ => .unowned { active := true, timeLeft } type
  (p, slot)

/-! ## `Main.UpdateProjectiles` -/

/-- What one slot's update produced, in slot order. -/
inductive Event where
  | arrow (slot : Nat) (dust : Array DustParticle) (hit : Option Hit) (ended : Option EndReason)
  | other (slot : Nat)
  /-- An active `unowned` slot reached the update loop: its `Projectile.Update` is not modelled. The
  caller must stop at this boundary (the rest of the phase still ran in slot order). -/
  | unowned (slot : Nat) (type : Nat)

/-- The caller-owned world one projectile phase reads and writes (NPC slots, loot). -/
structure World (κ σ : Type) where
  /-- `Main.npc` as arrow targets, read fresh before every projectile. -/
  targets : σ → Array (Option Target)
  /-- Write a strike's NPC back to its slot and take its drops, before the next projectile. -/
  applyHit : σ → Hit → σ
  /-- `Projectile.Update` of a kind owned elsewhere; `none` frees the slot. -/
  updateOther : Nat → SlotInfo → κ → σ → UnifiedRandom → DustPool → Option (SlotInfo × κ) × σ × UnifiedRandom × DustPool

structure PhaseResult (κ σ : Type) where
  pool : Pool κ
  world : σ
  random : UnifiedRandom
  dustPool : DustPool
  text : OriginalNpcCombatText.State
  events : Array Event

/-- `Main.UpdateProjectiles`: slots `0..999` ascending; an ended arrow stays in its slot inactive
(natively `active = false`, so the next `NewProjectile` may reuse it). -/
def Pool.update {κ σ : Type} (p : Pool κ) (ctx : OriginalPlayerArrows.Context) (w : World κ σ) (world : σ)
    (random : UnifiedRandom) (dustPool : DustPool) : PhaseResult κ σ := Id.run do
  let mut p := p
  let mut world := world
  let mut random := random
  let mut dustPool := dustPool
  let mut text := ctx.text
  let mut events : Array Event := #[]
  for i in [:OriginalPlayerArrows.maxProjectiles] do
    match p.slots.getD i none with
    | some (.woodenArrow a) =>
      if !a.active then continue
      let s := OriginalPlayerArrows.step { ctx with text } a (w.targets world) random dustPool
      random := s.random; dustPool := s.dustPool; text := s.text
      if let some hit := s.hit then world := w.applyHit world hit
      p := { p with slots := p.slots.set! i (some (.woodenArrow s.arrow)) }
      events := events.push (.arrow i s.dust s.hit s.ended)
    | some (.other info v) =>
      if !info.active then continue
      let (next, world', random', dust') := w.updateOther i info v world random dustPool
      world := world'; random := random'; dustPool := dust'
      p := { p with slots := p.slots.set! i (next.map fun (info, v) => .other info v) }
      events := events.push (.other i)
    | some (.unowned info type) =>
      if info.active then events := events.push (.unowned i type)
    | none => pure ()
  return { pool := p, world, random, dustPool, text, events }

end Lean4Terria.OriginalProjectiles
