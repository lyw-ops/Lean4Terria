import Lean4Terria.OriginalPlayerArmorDraw
import Lean4Terria.OriginalLightingRuntime
import Lean4Terria.OriginalPlayerDrawRuntime.Tables

/-!
# Player draw runtime (main B): armor/hair/body layers lit by the lighting runtime

Wires the released `OriginalPlayerArmorDraw` (Root ACCEPT-WITH-NOTES; sources `1c557f8b…`/`99866acd…`/`e9afc4e9…`)
to the session's real state:

* `Context.light x y` is `Lighting._activeEngine.GetColor(x, y)` of `OriginalLightingRuntime` (the active map the
  renderer reads), and `globalBrightness` is the runtime's `Lighting.GlobalBrightness`. A light-map fault (unreachable
  for a consistent engine: `GetColor` is zero outside the processed area) becomes a NaN sample, which the layer code
  turns into an explicit `runtimeDependentConversion` out-of-scope result rather than a colour.
* Static tables come from `Tables` (native H lines of the leaf's capture). `TextureAssets.Players[skin, 15]` (eyelid)
  sizes are NOT in the tables (the probe used stand-in assets): the caller passes the loaded texture sizes.
* The 8 item ids whose `SetDefaults` threw in the probe (269–271 familiar vanity, 5104–5106, 5136, 5305) have no known
  equip slots: an equipment set containing one is refused as out of scope before any layer runs.

The draw order of the rest of `PlayerDrawLayers` (arms, accessories, held item, wings…) is not in the leaf; this
module returns the leaf's `DrawResult` (leggings, torso, head `DrawData` in `DrawDataCache` order).
-/
namespace Lean4Terria.OriginalPlayerDrawRuntime

open Lean4Terria.OriginalPlayerArmorDraw
open Lean4Terria.OriginalPlayerEquipment (Equipment)

/-- `Item.headSlot/bodySlot/legSlot`; `none` for ids whose slots the native probe could not read. -/
def itemSlots? (t : UInt16) : Option ItemSlots :=
  if Tables.unknownItems.contains t.toNat then none else
  match Tables.itemSlotList.find? (·.1 == t.toNat) with
  | some (_, h, b, l) => some ⟨h, b, l⟩
  | none => some ⟨-1, -1, -1⟩

/-- Native `-1` slots for every item id not in `itemSlotList` (the probe listed every id with a slot ≥ 0). -/
private def itemSlotsTotal (t : UInt16) : ItemSlots := (itemSlots? t).getD ⟨-1, -1, -1⟩

def offsetsPlayerHeadgear : Array Vector2 :=
  Tables.offsetsPlayerHeadgearBits.map fun (x, y) => ⟨Float32.ofBits x, Float32.ofBits y⟩

/-- `Lighting._activeEngine.GetColor(x, y)` as the layer code samples it. -/
def lightOf (rt : OriginalLightingRuntime.Runtime) (x y : Int32) : Vector3 :=
  match OriginalLightingEngine.getColor rt.lighting x y with
  | .ok v => ⟨v.x, v.y, v.z⟩
  | .error _ => let n : Float32 := 0 / 0; ⟨n, n, n⟩

/-- The layer `Context` of one world draw (not `Main.gameMenu`). -/
def context (rt : OriginalLightingRuntime.Runtime) (screenPosition : Vector2)
    (eyelidTexture : Int32 → Option (Int32 × Int32)) (drawnIsLocalPlayer : Bool := true) : Context :=
  let (h, b, l) := Tables.counts
  { screenPosition, gameMenu := false, globalBrightness := rt.globalBrightness, light := lightOf rt
    offsetsPlayerHeadgear, headCount := h, bodyCount := b, legsCount := l
    usesNewFramingCode := Tables.usesNewFramingCode, useSkinColor := Tables.useSkinColor
    hidesHead := Tables.hidesHead, facePreventHairDraw := Tables.facePreventHairDraw
    maleVariant := Tables.maleVariant, eyelidTexture, itemSlots := itemSlotsTotal
    dualEquipArmor := Tables.dualEquipArmor, accessoryIncompatibilityType := Tables.accessoryIncompatibilityType
    drawnIsLocalPlayer }

/-- Every item the armor selection may read: armor/vanity slots and every loadout's armor slots. -/
def equipmentItems (e : Equipment) : Array UInt16 :=
  (e.armor ++ e.loadouts.foldl (fun acc (lo : OriginalPlayerEquipment.Loadout) => acc ++ lo.armor) #[]).map
    (fun (s : OriginalPlayerEquipment.Slot) => s.item)

/-- `PlayerDrawSet.BoringSetup` + leggings / torso / head layers of one player draw. -/
def drawArmor (c : Context) (e : Equipment) (p : Player) (drawPosition : Vector2) (shadow : Float32) : M DrawResult := do
  if let some t := (equipmentItems e).find? fun t => (itemSlots? t).isNone then
    throw (.guard s!"item {t}: equip slots unknown (SetDefaults failed in the native probe)")
  OriginalPlayerArmorDraw.drawArmor c e p drawPosition shadow

end Lean4Terria.OriginalPlayerDrawRuntime
