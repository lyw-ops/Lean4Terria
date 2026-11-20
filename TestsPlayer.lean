import Lean4Terria.OriginalLightingRuntime
import Lean4Terria.OriginalPlayerActions
import Lean4Terria.OriginalProjectiles
import Lean4Terria.OriginalPlayerDrawRuntime

/-! Main B regression entry (`tests-player`). Quick checks whose expected values come from the recorded native
captures; the full native replays (lighting joint test, leaf corpora through the B modules, the multi-arrow corpus)
are the drivers under `.lake/main-b/` (see each INTERFACE.md for commands and counts). -/
open Lean4Terria

def check {α} [BEq α] [Repr α] (name : String) (actual expected : α) : StateT Nat IO Unit := do
  if actual == expected then modify (· + 1)
  else throw (IO.userError s!"{name}: got {repr actual}, expected {repr expected}")

def lighting : StateT Nat IO Unit := do
  -- native `Main.GetAreaToLight()` for screenPosition (32832, 4462), 1600×900 (lighting joint v1, record `A`)
  match OriginalLightingRuntime.areaToLight ⟨(32832, 4462), (1600, 900)⟩ with
  | .ok r => check "GetAreaToLight" (r.x, r.y, r.width, r.height) ((2051 : Int32), (277 : Int32), (103 : Int32), (60 : Int32))
  | .error e => throw (IO.userError s!"GetAreaToLight fault {repr e}")
  -- ApplyColorOfTheSkiesToTiles (MethodDef 3922): noon sky stays white; the dusk sky of joint v1
  check "tileColor noon" (OriginalLightingRuntime.tileColorOfSkies 255 255 255).r 255
  let d := OriginalLightingRuntime.tileColorOfSkies 160 96 112
  check "tileColor dusk" (d.r, d.g, d.b, d.a) ((148 : UInt8), (104 : UInt8), (115 : UInt8), (255 : UInt8))
  -- static tables: sizes of the native arrays
  check "tables sizes" (OriginalLightingRuntime.Tables.tileLighted.size, OriginalLightingRuntime.Tables.wallLight.size) (754, 367)

def actions : StateT Nat IO Unit := do
  let picks := [3509, 1, 3521, 103].map fun t => match OriginalPlayerActions.actionOf t with
    | .pickaxe p => p.pick | _ => 0
  check "pickaxe powers" picks [35, 40, 55, 65]
  check "bow dispatch" (match OriginalPlayerActions.actionOf 39 with | .bow _ => true | _ => false) true
  match OriginalPlayerActions.bowFor 39 17 (UnifiedRandom.seed 0) with
  | some (b, _) => check "Wooden Bow prefix 17 useTime (native 26)" b.stats.useTime 26
  | none => throw (IO.userError "bowFor 39 17")

def projectiles : StateT Nat IO Unit := do
  -- every slot netImportant: NewProjectileSetup returns 1000 (leaf case slot-full-all-netimportant)
  let full : OriginalProjectiles.Pool Unit :=
    { slots := (Array.range OriginalProjectiles.physicalSlots).map fun (i : Nat) =>
        if i < 1000 then some (.other { active := true, netImportant := true, timeLeft := (100 + i : Nat) } ()) else none }
  let (_, slot, key) := full.newProjectile 0 fun _ _ => .unowned {} 1
  check "all netImportant → slot 1000" (slot, key.generation) (1000, 1)
  let (p, a) := ({} : OriginalProjectiles.Pool Unit).launchArrow 0 { center := ⟨2202, 2379⟩, velocity := ⟨9.1, 0⟩, damage := 11, knockback := 0 }
  let (_, b) := p.launchArrow 0 { center := ⟨2202, 2379⟩, velocity := ⟨9.1, 0⟩, damage := 11, knockback := 0 }
  check "two launches take slots 0, 1" (a.slot, b.slot) (0, 1)

def playerDraw : StateT Nat IO Unit := do
  -- native H itemSlots: Iron Helmet-like head 37 → head 10; 729 → legs 31; 954 → head 72
  let s (t : UInt16) := (OriginalPlayerDrawRuntime.itemSlots? t).map fun x => (x.headSlot, x.bodySlot, x.legSlot)
  check "item slots" [s 37, s 729, s 954] [some (10, -1, -1), some (-1, -1, 31), some (72, -1, -1)]
  check "probe-unknown items refused" (s 269) none
  check "non-armor item" (s 2) (some (-1, -1, -1))

def main : IO UInt32 := do
  let ((), n) ← (do lighting; actions; projectiles; playerDraw : StateT Nat IO Unit).run 0
  IO.println s!"tests-player: {n} checks passed"
  return 0
