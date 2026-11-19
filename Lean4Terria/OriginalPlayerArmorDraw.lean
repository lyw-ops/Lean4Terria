import Lean4Terria.OriginalPlayerArmorDraw.Basic
import Lean4Terria.OriginalPlayerArmorDraw.Tables

/-!
# OriginalPlayerArmorDraw — ordinary unmounted player armor / body draw commands

Hand transcription of desktop 1.4.5.8 (`Terraria.exe` d52c7ea7…):

| native | token | Lean |
|---|---|---|
| `Player.PlayerFrame` armor/vanity selection IL_0026‥IL_0196 (+ `GetEffectiveArmor`, `CanShareArmor`, `ItemSlot.CanEquipInArmorSlot/HasSameItemInSlot`, `SetMatch`) | 100665864 | `selectArmor` |
| `PlayerDrawSet.BoringSetup` (ordinary branch) + `CreateCompositeData` | 100678349, 100678355 | `boringSetup` |
| `PlayerDrawLayers.DrawPlayer_13_Leggings` | 100678391 | `drawLeggings` |
| `PlayerDrawLayers.DrawPlayer_17_Torso` / `_TorsoComposite` (+ `DrawCompositeArmorPiece`) | 100678399 / 100678400 / 100678363 | `drawTorso` |
| `PlayerDrawLayers.DrawPlayer_21_Head` (+ `_TheFace`, `_TheFace_Eyelid`) | 100678405 / 100678409 / 100678410 | `drawHead` |

Every branch outside the ordinary scope (mounts, sitting/sleeping, display dolls, buff/debuff visual
effects and their `Main.rand` dust, stealth/ghost/opacity, glow-mask armor, special hats, face/beard/
coat accessories, hair dyes) returns an explicit `OutOfScope` instead of a value.
-/

namespace Lean4Terria.OriginalPlayerArmorDraw

open Lean4Terria.OriginalPlayerEquipment (Slot Equipment)

/-! ## Inputs -/

/-- The real native state the five methods read (pose = frames/positions/rotations, appearance,
dyes, scope flags).  `head/body/legs` are produced by `selectArmor`. -/
structure Player where
  position : Vector2            -- Entity.position
  width : Int32 := 20
  height : Int32 := 42
  direction : Int32
  gravDir : Float32 := 1
  skinVariant : Int32
  hair : Int32
  head : Int32 := 0
  body : Int32 := 0
  legs : Int32 := 0
  cHead : Int32 := 0
  cBody : Int32 := 0
  cLegs : Int32 := 0
  wearsRobe : Bool := false
  invis : Bool := false
  dead : Bool := false
  bodyFrame : Rect
  legFrame : Rect
  bodyPosition : Vector2 := Vector2.zero
  legPosition : Vector2 := Vector2.zero
  headPosition : Vector2 := Vector2.zero
  bodyRotation : Float32 := 0
  legRotation : Float32 := 0
  headRotation : Float32 := 0
  immuneAlpha : Int32 := 0
  shimmerTransparency : Float32 := 0
  hairDye : UInt8 := 0
  skinDyePacked : Int32 := 0
  hairColor : Color
  skinColor : Color
  eyeColor : Color
  shirtColor : Color
  underShirtColor : Color
  pantsColor : Color
  shoeColor : Color
  eyeFrameToShow : Int32 := 0
  shoe : Int32 := 0
  face : Int32 := -1
  faceHead : Int32 := -1
  faceMask : Int32 := -1
  beard : Int32 := -1
  front : Int32 := -1
  handon : Int32 := -1
  handoff : Int32 := -1
  coat : Int32 := 0
  /-- Scope flags: any `true` (or a non-ordinary value) leaves the transcribed branch. -/
  mountActive : Bool := false
  sitting : Bool := false
  sleeping : Bool := false
  portableStoolInUse : Bool := false
  isDisplayDollOrInanimate : Bool := false
  isHatRackDoll : Bool := false
  socialGhost : Bool := false
  socialIgnoreLight : Bool := false
  opacityForAnimation : Float32 := 1
  heldItemType : Int32 := 0
  shroomiteStealth : Bool := false
  setVortex : Bool := false
  merman : Bool := false
  forceMerman : Bool := false
  hasFloatingTube : Bool := false
  babyBird : Bool := false
  yoraiz0rDarkness : Bool := false
  /-- Buff/debuff visual flags read by BoringSetup IL_2a2c‥IL_4907 (all spawn dust via `Main.rand`). -/
  effectFlags : List (String × Bool) := []
  deriving Inhabited

/-- `Item.headSlot/bodySlot/legSlot` of an item type (`Item.SetDefaults`), supplied by the caller. -/
structure ItemSlots where
  headSlot : Int32
  bodySlot : Int32
  legSlot : Int32
  deriving Repr, Inhabited

/-- Static tables and frame globals. -/
structure Context where
  screenPosition : Vector2
  gameMenu : Bool := false
  globalBrightness : Float32 := 1
  /-- `Lighting._activeEngine.GetColor(x, y)` (sampled light, a typed input). -/
  light : Int32 → Int32 → Vector3
  /-- `Main.OffsetsPlayerHeadgear`. -/
  offsetsPlayerHeadgear : Array Vector2
  headCount : Int32
  bodyCount : Int32
  legsCount : Int32
  /-- `ArmorIDs.Body.Sets.UsesNewFramingCode`, `ArmorIDs.Head.Sets.UseSkinColor/HidesHead`,
  `ArmorIDs.Face.Sets.PreventHairDraw`, `PlayerVariantID.Sets.Male`. -/
  usesNewFramingCode : Array Bool
  useSkinColor : Array Bool
  hidesHead : Array Bool
  facePreventHairDraw : Array Bool
  maleVariant : Array Bool
  /-- `TextureAssets.Players[skinVar, 15]`: `IsLoaded` and its texture size (eyelid frame). -/
  eyelidTexture : Int32 → Option (Int32 × Int32)
  itemSlots : UInt16 → ItemSlots
  dualEquipArmor : Array Bool
  accessoryIncompatibilityType : Array Int32
  /-- `Main.LocalPlayer` is the drawn player (`CanEquipInArmorSlot` reads the local player's armor). -/
  drawnIsLocalPlayer : Bool := true

def lookup {α} [Inhabited α] (name : String) (a : Array α) (i : Int32) : M α :=
  if i < 0 || i.toNatClampNeg ≥ a.size then throw (.indexOutOfRange name) else pure a[i.toNatClampNeg]!

def Context.male (c : Context) (p : Player) : M Bool := lookup "PlayerVariantID.Sets.Male" c.maleVariant p.skinVariant

/-! ## Armor / vanity selection (`Player.PlayerFrame` IL_0026‥IL_01f7) -/

def Slot.isAir (s : Slot) : Bool := s.item == 0 || s.stack == 0

/-- `ItemSlot.HasSameItemInSlot(item, segment)`. -/
def hasSameItemInSlot (c : Context) (item : Slot) (segment : List Slot) : M Bool := do
  if item.isAir then return false
  let k ← lookup "ItemID.Sets.AccessoryIncompatibilityType" c.accessoryIncompatibilityType item.item.toNat.toInt32
  for it in segment do
    if it.item == item.item then return true
    let k2 ← lookup "ItemID.Sets.AccessoryIncompatibilityType" c.accessoryIncompatibilityType it.item.toNat.toInt32
    if k != -1 && k == k2 then return true
  return false

/-- `ItemSlot.CanEquipInArmorSlot(Main.LocalPlayer, item, slot)` for the armor slots 0‥2 / 10‥12. -/
def canEquipInArmorSlot (c : Context) (e : Equipment) (item : Slot) (slot : Nat) : M Bool := do
  if !c.drawnIsLocalPlayer then oos "CanEquipInArmorSlot reads a different Main.LocalPlayer"
  let s := c.itemSlots item.item
  let ok := match slot % 10 with | 0 => s.headSlot >= 0 | 1 => s.bodySlot >= 0 | _ => s.legSlot >= 0
  if !ok then return false
  if ← lookup "ItemID.Sets.DualEquipArmor" c.dualEquipArmor item.item.toNat.toInt32 then return true
  let base := if slot < 10 then 0 else 10
  return !(← hasSameItemInSlot c item ((List.range 3).map fun k => e.armor[base + k]!))

/-- `Player.GetEffectiveArmor(slot)`: an empty slot borrows the first favorited item of the loadouts when
`CanShareArmor` allows it (armor slots: just `CanEquipInArmorSlot`), otherwise stays `armor[slot]`. -/
def effectiveArmor (c : Context) (e : Equipment) (slot : Nat) : M Slot := do
  let own := e.armor[slot]!
  if !own.isAir then return own
  for l in e.loadouts do
    let it := l.armor[slot]!
    if !it.isAir && it.favorited then
      return (if ← canEquipInArmorSlot c e it slot then it else own)
  return own

/-- `Player.SetMatch(request, ref somethingSpecial)`: returns the override and the new `ref` flag. -/
def setMatch (slot : Nat) (head body legs : Int32) (male : Bool) (flag : Bool) : Int32 × Bool :=
  match slot with
  | 0 => (if head == 201 then (if male then 201 else 202) else -1, flag)   -- mount 54 is out of scope
  | 1 =>
    let (r, v4) : Int32 × Bool := match body.toInt with
      | 15 => (88, true) | 36 => (89, true) | 41 => (97, true) | 42 => (90, true)
      | 58 => (91, true) | 59 => (92, true) | 60 => (93, true) | 61 => (94, true) | 62 => (95, true) | 63 => (96, true)
      | 77 => (121, true) | 165 => (if male then 118 else 99, true) | 166 => (if male then 119 else 100, false)
      | 167 => (if male then 101 else 102, true) | 180 => (115, true) | 181 => (116, true)
      | 183 => (if male then 136 else 123, true) | 191 => (131, true) | 93 => (165, true) | 90 => (166, true)
      | 88 => (168, true) | 81 => (if legs == -1 || legs == 0 then 169 else -1, true)
      | 213 => (187, true) | 215 => (189, true) | 219 => (196, true) | 221 => (199, true) | 223 => (204, true)
      | 231 => (214, true) | 232 => (215, true) | 233 => (216, true) | 241 => (229, true) | 256 => (244, true)
      | _ => (-1, true)
    (r, if r != -1 then v4 else flag)
  | _ =>
    let r : Int32 := match legs.toInt with
      | 57 => if male then 137 else -1 | 83 => if male then 117 else -1 | 84 => if male then 120 else -1
      | 132 => if male then 135 else -1 | 180 => if male then -1 else 179 | 184 => if male then -1 else 183
      | 146 => if male then 146 else 147 | 154 => if male then 155 else 154 | 158 => if male then 157 else -1
      | 191 => if male then -1 else 192 | 193 => if male then -1 else 194 | 197 => if male then -1 else 198
      | 203 => if male then -1 else 202 | 208 => if male then -1 else 207 | 219 => if male then -1 else 220
      | 232 => if male then -1 else 233 | 236 => if male then -1 else 248 | 249 => if male then -1 else 250
      | _ => -1
    (r, flag)

/-- `Player.PlayerFrame` IL_0026‥IL_01f7: vanity-over-armor equip slots, then the robe/variant matches.
Returns the player with `head/body/legs/wearsRobe` set (accessory and cape bookkeeping is out of scope). -/
def selectArmor (c : Context) (e : Equipment) (p : Player) : M Player := do
  if p.mountActive then oos "mount (SetMatch head 201 / PlayerFrame mount paths)"
  let pick (vanity armor : Nat) (f : ItemSlots → Int32) : M Int32 := do
    let v := f (c.itemSlots (← effectiveArmor c e vanity).item)
    if v >= 0 then return v
    return f (c.itemSlots (← effectiveArmor c e armor).item)
  let head ← pick 10 0 (·.headSlot)
  let body ← pick 11 1 (·.bodySlot)
  let legs ← pick 12 2 (·.legSlot)
  let male ← c.male p
  let (l1, robe) := setMatch 1 head body legs male false
  let legs := if l1 != -1 then l1 else legs
  let (l2, _) := setMatch 2 head body legs male false
  let legs := if l2 != -1 then l2 else legs
  let (h0, _) := setMatch 0 head body legs male false
  let head := if h0 != -1 then h0 else head
  return { p with head, body, legs, wearsRobe := robe }

/-! ## `PlayerDrawSet` after `BoringSetup` (the fields the four layers read) -/

structure DrawSet where
  position : Vector2
  shadow : Float32
  cHead : Int32
  cBody : Int32
  cLegs : Int32
  skinVar : Int32
  missingHand : Bool
  armorAdjust : Int32
  fullHair : Bool
  hatHair : Bool
  hideHair : Bool
  backHairDraw : Bool
  hairDyePacked : Int32
  skinDyePacked : Int32
  playerEffect : UInt32
  colorHair : Color
  colorEyeWhites : Color
  colorEyes : Color
  colorHead : Color
  colorBodySkin : Color
  colorLegs : Color
  colorShirt : Color
  colorUnderShirt : Color
  colorPants : Color
  colorShoes : Color
  colorArmorHead : Color
  colorArmorBody : Color
  colorArmorLegs : Color
  headGlowMask : Int32
  bodyGlowMask : Int32
  legsGlowMask : Int32
  bodyGlowColor : Color
  legVect : Vector2
  bodyVect : Vector2
  headVect : Vector2
  hairFrontFrame : Rect
  hairOffset : Vector2
  helmetOffset : Vector2
  legsOffset : Vector2
  usesCompositeTorso : Bool
  compTorsoFrame : Rect
  compBackShoulderFrame : Rect
  deriving Inhabited

/-- `Player.Directions`: `new Vector2(direction, gravDir)`. -/
def directions (p : Player) : Vector2 := ⟨p.direction.toFloat32, p.gravDir⟩

/-- `Player.GetImmuneAlpha(newColor, alphaReduction)`. -/
def immuneAlpha (p : Player) (c : Color) (shadow : Float32) : M Color := do
  let mut a : Float32 := (255 - p.immuneAlpha).toFloat32 / 255
  if shadow > 0 then a := a * (1 - shadow)
  if p.shimmerTransparency > 0 then
    if p.shimmerTransparency.toFloat >= 0.8 then return Color.transparent
    a := a * (1 - p.shimmerTransparency)
    a := a * (1 - p.shimmerTransparency)
    a := a * (1 - p.shimmerTransparency)
  if p.immuneAlpha > 125 then return Color.transparent
  c.multiply a

/-- `Player.GetImmuneAlphaPure(newColor, alphaReduction)`. -/
def immuneAlphaPure (p : Player) (c : Color) (shadow : Float32) : M Color := do
  let mut a : Float32 := (255 - p.immuneAlpha).toFloat32 / 255
  if shadow > 0 then a := a * (1 - shadow)
  if p.shimmerTransparency > 0 then a := a * (1 - p.shimmerTransparency)
  c.multiply a

/-- `Lighting.GetColorClamped(x, y, oldColor)`. -/
def getColorClamped (c : Context) (x y : Int32) (old : Color) : M Color := do
  if c.gameMenu then return old
  let v := c.light x y
  let v : Vector3 := ⟨minF 1 v.x, minF 1 v.y, minF 1 v.z⟩
  let o := old.toVector3
  Color.ofVector3 ⟨v.x * o.x, v.y * o.y, v.z * o.z⟩

/-- `Lighting.GetColor(int x, int y)`: `(int)(v * GlobalBrightness * 255)`, only upper-clamped, packed. -/
def getColor (c : Context) (x y : Int32) : M Color := do
  if c.gameMenu then return Color.white
  let v := c.light x y
  let s := c.globalBrightness * 255
  let r ← convI4 (v.x * s)
  let g ← convI4 (v.y * s)
  let b ← convI4 (v.z * s)
  let r := if r > 255 then 255 else r
  let g := if g > 255 then 255 else g
  let b := if b > 255 then 255 else b
  let packed : UInt32 := ((r ||| (g <<< 8)) ||| (b <<< 16) ||| (-16777216 : Int32)).toUInt32
  pure ⟨(packed &&& 255).toUInt8, ((packed >>> 8) &&& 255).toUInt8, ((packed >>> 16) &&& 255).toUInt8,
        (packed >>> 24).toUInt8⟩

/-- `(int)((double)X + width * 0.5) / 16`. -/
def tileX (pos : Vector2) (width : Int32) : M Int32 := do
  let d := pos.x.toFloat + width.toFloat * 0.5
  if d.isNaN || d >= 2147483648.0 || d <= -2147483649.0 then throw .runtimeDependentConversion
  pure (d.toInt32 / 16)

/-- `(int)(((double)Y + height * k) / 16.0)` (skin/cloth samples). -/
def tileYDouble (pos : Vector2) (height : Int32) (k : Float) : M Int32 := do
  let d := (pos.y.toFloat + height.toFloat * k) / 16
  if d.isNaN || d >= 2147483648.0 || d <= -2147483649.0 then throw .runtimeDependentConversion
  pure d.toInt32

/-- `(int)((double)Y + height * k) / 16` (armour samples, integer division). -/
def tileYInt (pos : Vector2) (height : Int32) (k : Float) : M Int32 := do
  let d := pos.y.toFloat + height.toFloat * k
  if d.isNaN || d >= 2147483648.0 || d <= -2147483649.0 then throw .runtimeDependentConversion
  pure (d.toInt32 / 16)

/-- `Player.GetHairColor(useLighting: true)` with `hairDye == 0` (`HairShaderDataSet.GetColor` undyed). -/
def hairColor (c : Context) (p : Player) : M Color := do
  if p.hairDye != 0 then oos "hairDye"
  let light ← getColor c (← tileX p.position p.width) (← tileYDouble p.position p.height 0.25)
  Color.mulVector4 light p.hairColor

/-- `Player.GetHairSettings` (head categories from `Tables`, then face/faceHead, then back hair). -/
def hairSettings (c : Context) (p : Player) : M (Bool × Bool × Bool × Bool) := do
  let cat := if p.head < -1 || p.head ≥ 399 then 0 else Tables.hairSettingsByHead[(p.head + 1).toNatClampNeg]!
  if p.head ≥ 399 then oos "head id beyond the extracted GetHairSettings table"
  let fullHair := cat == 1
  let hatHair := cat == 2
  let mut hideHair := false
  if p.face > -1 then
    if ← lookup "Face.Sets.PreventHairDraw" c.facePreventHairDraw p.face then hideHair := true
  if p.faceHead > -1 && p.head != 0 then hideHair := true
  let h := p.hair
  let mut back := !(h <= 50 || (h >= 56 && h <= 63) || (h >= 74 && h <= 77) || (h >= 88 && h <= 89)
    || h == 94 || h == 100 || h == 104 || h == 112) && h < 116
  if h == 133 || h == 134 || h == 146 || h == 162 || h == 6 then back := true
  return (fullHair, hatHair, hideHair, back)

/-- `Player.GetHelmetDrawOffset(false)` for the in-scope heads (no face-head add-on, no mount). -/
def helmetDrawOffset (p : Player) : Vector2 :=
  let d := directions p
  let base : Vector2 :=
    if p.head == 270 then
      let v := Vector2.mul ⟨-10, 0⟩ d
      if p.direction == -1 then { v with x := v.x - 2 } else v
    else if p.head == 268 then Vector2.mul ⟨0, -6⟩ d
    else if p.head == 222 then
      let y : Float32 := if p.hair == 15 || p.hair == 76 || p.hair == 108 then 4 else 0
      Vector2.mul ⟨0, y⟩ d
    else if p.head == 272 then
      if p.hair == 15 || p.hair == 76 || p.hair == 108 then Vector2.mul ⟨0, 4⟩ d else Vector2.zero
    else if p.head == 275 then Vector2.mul ⟨0, -4⟩ d
    else Vector2.zero
  base + Vector2.zero + Vector2.zero   -- GetHelmetOffsetAddonFromFaceHead (faceHead ≤ 0), …FromMount (no mount)

/-- `PlayerDrawSet.BoringSetup(player, …, drawPosition, shadowOpacity, rotation, rotationOrigin)` for the
ordinary branch; returns the draw set and the player as mutated by the method (`legPosition.Y`,
`headPosition.Y`, `bodyPosition.Y`, `headRotation`). -/
def boringSetup (c : Context) (p : Player) (drawPosition : Vector2) (shadow : Float32) : M (DrawSet × Player) := do
  -- scope guards (each is a branch of the native method this leaf does not transcribe)
  if p.mountActive then oos "mount"
  if p.sitting then oos "sitting"
  if p.sleeping then oos "sleeping"
  if p.portableStoolInUse then oos "portable stool"
  if p.isDisplayDollOrInanimate then oos "display doll"
  if p.isHatRackDoll then oos "hat rack doll"
  if p.socialGhost then oos "socialGhost"
  if p.socialIgnoreLight then oos "socialIgnoreLight"
  if p.opacityForAnimation != 1 then oos "opacityForAnimation"
  if p.heldItemType == 3106 then oos "held item 3106 stealth"
  if p.shroomiteStealth then oos "shroomiteStealth"
  if p.setVortex then oos "setVortex"
  if p.merman || p.forceMerman then oos "merman"
  if p.hasFloatingTube then oos "floating tube"
  if p.effectFlags.any (·.2) then oos "buff/debuff visual effect"
  if Tables.glowHeads.contains p.head then oos "head glow mask"
  if Tables.glowBodies.contains p.body then oos "body glow mask"
  if Tables.glowLegs.contains p.legs then oos "legs glow mask"
  if (p.head == 78 || p.head == 79 || p.head == 80 || p.head == 283) && p.body == 51 && p.legs == 47 then
    oos "special armor set colour"
  if p.face > 0 || p.faceHead > 0 || p.faceMask > 0 || p.beard > 0 || p.front > 0 then oos "face/front accessory"
  if p.handon > 0 || p.handoff > 0 then oos "hand accessory"
  if p.coat > 0 then oos "coat"
  -- IL_0070‥IL_00bb
  let cHead := p.cHead
  let cBody := p.cBody
  let cLegs := if p.wearsRobe then p.cBody else p.cLegs
  -- IL_0387: Position = drawPosition + new Vector2(MountXOffset * direction, 0) (MountXOffset = 0)
  let position := drawPosition + ⟨(0 : Float32) * p.direction.toFloat32, 0⟩
  let missingHand := Tables.missingHandBodies.contains p.body
  let (fullHair, hatHair, hideHair, backHairDraw) ← hairSettings c p
  let hairDyePacked : Int32 := if p.head == 0 && p.hairDye == 0 then 1 + 1000 else p.hairDye.toNat.toInt32 + 1000
  -- IL_0bec: Position.Y -= mountOffSet (HeightOffsetVisual = 0)
  let position := { position with y := position.y - 0 }
  let colorHair ← immuneAlpha p (← hairColor c p) shadow
  let tx ← tileX position p.width
  let sample (k : Float) (base : Color) : M Color := do getColorClamped c tx (← tileYDouble position p.height k) base
  let sampleI (k : Float) : M Color := do getColorClamped c tx (← tileYInt position p.height k) Color.white
  let colorEyeWhites ← immuneAlpha p (← sample 0.25 Color.white) shadow
  let colorEyes ← immuneAlpha p (← sample 0.25 p.eyeColor) shadow
  let colorHead ← immuneAlpha p (← sample 0.25 p.skinColor) shadow
  let colorBodySkin ← immuneAlpha p (← sample 0.5 p.skinColor) shadow
  let colorLegs ← immuneAlpha p (← sample 0.75 p.skinColor) shadow
  let colorShirt ← immuneAlphaPure p (← sample 0.5 p.shirtColor) shadow
  let colorUnderShirt ← immuneAlphaPure p (← sample 0.5 p.underShirtColor) shadow
  let colorPants ← immuneAlphaPure p (← sample 0.75 p.pantsColor) shadow
  let colorShoes ← immuneAlphaPure p (← sample 0.75 p.shoeColor) shadow
  let colorArmorHead ← immuneAlphaPure p (← sampleI 0.25) shadow
  let colorArmorBody ← immuneAlphaPure p (← sampleI 0.5) shadow
  let colorArmorLegs ← immuneAlphaPure p (← sampleI 0.75) shadow
  let _floatingTube ← immuneAlphaPure p (← sampleI 0.75) shadow
  -- IL_12ca: no glow mask in scope; IL_2077 glow colours = GetImmuneAlphaPure(Transparent, shadow)
  let bodyGlowColor ← immuneAlphaPure p Color.transparent shadow
  -- IL_29c1
  let (colorLegs, colorBodySkin, colorHead, colorHair, colorEyes, colorEyeWhites) :=
    if shadow > 0 then (Color.transparent, Color.transparent, Color.transparent, Color.transparent,
                        Color.transparent, Color.transparent)
    else (colorLegs, colorBodySkin, colorHead, colorHair, colorEyes, colorEyeWhites)
  -- IL_59b5: facing, and the player's part positions
  let (playerEffect, partY) : UInt32 × Float32 :=
    if p.gravDir == 1 then (if p.direction == 1 then 0 else 1, 0) else (if p.direction == 1 then 2 else 3, 6)
  let p := if p.dead then p else
    { p with legPosition := { p.legPosition with y := partY }, headPosition := { p.headPosition with y := partY },
             bodyPosition := { p.bodyPosition with y := partY } }
  -- IL_5b42 (all three from legFrame)
  let lw := p.legFrame.w.toFloat32
  let lh := p.legFrame.h.toFloat32
  let legVect : Vector2 := ⟨lw * 0.5, lh * 0.75⟩
  let bodyVect : Vector2 := ⟨lw * 0.5, lh * 0.5⟩
  let headVect : Vector2 := ⟨lw * 0.5, lh * 0.4⟩
  let p := if p.dead then p else { p with headRotation := 0 }
  -- IL_5cb2
  let r0 := p.bodyFrame
  let r := { r0 with h := r0.h - 2, y := r0.y - 336 }
  let r := if r.y < 0 then { r with y := 0 } else r
  let hairFrontFrame := if hideHair then { r with h := 0 } else if backHairDraw then { r with h := 26 } else r
  -- IL_5f11
  let d := directions p
  let hairOffset : Vector2 :=
    if !hatHair && p.hair == 163 then Vector2.mul ⟨0, -2⟩ d
    else if p.hair == 164 then Vector2.mul ⟨-2, 0⟩ d else Vector2.zero
  let helmetOffset := helmetDrawOffset p
  let legsOffset := if p.legs == 226 then Vector2.mul ⟨-6, 0⟩ d else Vector2.zero
  -- CreateCompositeData
  let composite ← if p.body > 0 && p.body < c.bodyCount then
      lookup "Body.Sets.UsesNewFramingCode" c.usesNewFramingCode p.body else pure false
  let composite := if p.body < 1 then true else composite
  let frame := if p.bodyFrame.h == 0 then 0 else p.bodyFrame.y / p.bodyFrame.h
  if composite && p.bodyFrame.h == 0 then oos "bodyFrame.Height == 0 (native DivideByZeroException)"
  let torsoX : Int32 := if frame == 5 then 1 else 0
  -- IL_042e: only a female body shifts the composite rows down by 2
  let rowShift : Int32 := if ← c.male p then 0 else 2
  let ds : DrawSet := {
    position, shadow, cHead, cBody, cLegs, skinVar := p.skinVariant, missingHand, armorAdjust := 0,
    fullHair, hatHair, hideHair, backHairDraw, hairDyePacked, skinDyePacked := p.skinDyePacked, playerEffect,
    colorHair, colorEyeWhites, colorEyes, colorHead, colorBodySkin, colorLegs, colorShirt, colorUnderShirt,
    colorPants, colorShoes, colorArmorHead, colorArmorBody, colorArmorLegs,
    headGlowMask := -1, bodyGlowMask := -1, legsGlowMask := -1, bodyGlowColor,
    legVect, bodyVect, headVect, hairFrontFrame, hairOffset, helmetOffset, legsOffset,
    usesCompositeTorso := composite, compTorsoFrame := ⟨torsoX * 40, rowShift * 56, 40, 56⟩,
    compBackShoulderFrame := ⟨40, (1 + rowShift) * 56, 40, 56⟩ }
  return (ds, p)

/-! ## Layers -/

/-- The recurring `new Vector2((int)(Position.X - screen.X - frame.W/2 + width/2) [+ adj], (int)(Position.Y -
screen.Y + height - frame.H + 4))`. -/
def anchor (c : Context) (d : DrawSet) (p : Player) (frame : Rect) (adj : Int32 := 0) : M Vector2 := do
  let x ← convI4 (d.position.x - c.screenPosition.x - (frame.w / 2).toFloat32 + (p.width / 2).toFloat32)
  let y ← convI4 (d.position.y - c.screenPosition.y + p.height.toFloat32 - frame.h.toFloat32 + 4)
  pure ⟨(x + adj).toFloat32, y.toFloat32⟩

def dd (tex : Tex) (pos : Vector2) (src : Option Rect) (color : Color) (rot : Float32) (origin : Vector2)
    (effect : UInt32) (shader : Int32 := 0) : DrawData :=
  { texture := tex, position := pos, sourceRect := src, color, rotation := rot, origin, scale := 1, effect,
    layerDepth := 0, shader }

/-- `DrawPlayer_13_Leggings`. -/
def drawLeggings (c : Context) (d : DrawSet) (p : Player) : M (Array DrawData) := do
  if p.legs == 169 then return #[]
  if p.legs == 140 then oos "legs 140"
  let overrideShoes := p.shoe == 15   -- ShouldOverrideLegs_CheckShoes
  if p.legs > 0 && p.legs < c.legsCount && (!overrideShoes || p.wearsRobe) then
    if p.invis then return #[]
    let pos := d.legsOffset + (← anchor c d p p.legFrame) + p.legPosition + d.legVect
    let mut out := #[dd (.armorLeg p.legs) pos (some p.legFrame) d.colorArmorLegs p.legRotation d.legVect d.playerEffect d.cLegs]
    if p.legs == 60 then
      out := out.push (dd (.extra 278) pos (some p.legFrame) d.colorArmorLegs p.legRotation d.legVect d.playerEffect 0)
    return out   -- legsGlowMask == -1 in scope
  if p.invis || overrideShoes then return #[]
  let pos := (← anchor c d p p.legFrame) + p.legPosition + d.legVect
  return #[dd (.players d.skinVar 11) pos (some p.legFrame) d.colorPants p.legRotation d.legVect d.playerEffect,
           dd (.players d.skinVar 12) pos (some p.legFrame) d.colorShoes p.legRotation d.legVect d.playerEffect]

/-- `DrawPlayer_17_TorsoComposite` (+ `DrawCompositeArmorPiece` for context 2, no glow). -/
def drawTorsoComposite (c : Context) (d : DrawSet) (p : Player) : M (Array DrawData) := do
  let base := (← anchor c d p p.bodyFrame) + p.bodyPosition + ⟨(p.bodyFrame.w / 2).toFloat32, (p.bodyFrame.h / 2).toFloat32⟩
  let idx := p.bodyFrame.y / p.bodyFrame.h
  let off ← lookup "Main.OffsetsPlayerHeadgear" c.offsetsPlayerHeadgear idx
  let off := { off with y := off.y - 2 }
  let sign : Int32 := if d.playerEffect &&& 2 != 0 then 1 else -1   -- Utils.ToDirectionInt
  let v0 := base + off.scale (-sign).toFloat32
  let mut out : Array DrawData := #[]
  let drawn := p.body > 0 && p.body < c.bodyCount
  if drawn && (!p.invis || !(p.body == 21 || p.body == 22)) then
    out := out.push (dd (.armorBodyComposite p.body) v0 (some d.compTorsoFrame) d.colorArmorBody p.bodyRotation d.bodyVect d.playerEffect d.cBody)
    if d.bodyGlowColor.packedPositive then oos "body glow colour"
    if p.body == 251 || p.body == 259 then oos "composite piece 251/259 extra layer"
    if p.body == 71 then
      out := out.push (dd (.extra 277) v0 (some d.compTorsoFrame) d.colorArmorBody p.bodyRotation d.bodyVect d.playerEffect 0)
  if !drawn && !p.invis then
    out := out.push (dd (.players d.skinVar 4) v0 (some d.compBackShoulderFrame) d.colorUnderShirt p.bodyRotation d.bodyVect d.playerEffect)
    out := out.push (dd (.players d.skinVar 6) v0 (some d.compBackShoulderFrame) d.colorShirt p.bodyRotation d.bodyVect d.playerEffect)
    out := out.push (dd (.players d.skinVar 4) v0 (some d.compTorsoFrame) d.colorUnderShirt p.bodyRotation d.bodyVect d.playerEffect)
    out := out.push (dd (.players d.skinVar 6) v0 (some d.compTorsoFrame) d.colorShirt p.bodyRotation d.bodyVect d.playerEffect)
  return out

/-- `DrawPlayer_17_Torso`. -/
def drawTorso (c : Context) (d : DrawSet) (p : Player) (male : Bool) : M (Array DrawData) := do
  if d.usesCompositeTorso then return ← drawTorsoComposite c d p
  let half : Vector2 := ⟨(p.bodyFrame.w / 2).toFloat32, (p.bodyFrame.h / 2).toFloat32⟩
  if p.body > 0 && p.body < c.bodyCount then
    let src := { p.bodyFrame with x := p.bodyFrame.x + d.armorAdjust, w := p.bodyFrame.w - d.armorAdjust }
    let adj := if p.direction == -1 then 0 else d.armorAdjust
    let mut out : Array DrawData := #[]
    if !(p.invis && (p.body == 21 || p.body == 22)) then
      let tex := if male then Tex.armorBody p.body else Tex.femaleBody p.body
      let pos := (← anchor c d p p.bodyFrame adj) + p.bodyPosition + half
      out := out.push (dd tex pos (some src) d.colorArmorBody p.bodyRotation d.bodyVect d.playerEffect d.cBody)
    if d.missingHand && !p.invis then
      let pos := (← anchor c d p p.bodyFrame) + p.bodyPosition + half
      out := out.push (dd (.players d.skinVar 5) pos (some p.bodyFrame) d.colorBodySkin p.bodyRotation d.bodyVect d.playerEffect d.skinDyePacked)
    return out
  if p.invis then return #[]
  let pos := (← anchor c d p p.bodyFrame) + p.bodyPosition + half
  return #[dd (.players d.skinVar 4) pos (some p.bodyFrame) d.colorUnderShirt p.bodyRotation d.bodyVect d.playerEffect,
           dd (.players d.skinVar 6) pos (some p.bodyFrame) d.colorShirt p.bodyRotation d.bodyVect d.playerEffect,
           dd (.players d.skinVar 5) pos (some p.bodyFrame) d.colorBodySkin p.bodyRotation d.bodyVect d.playerEffect d.skinDyePacked]

/-- `DrawPlayer_21_Head_TheFace` (+ `_TheFace_Eyelid`) for the ordinary branch (no face accessories). -/
def drawFace (c : Context) (d : DrawSet) (p : Player) : M (Array DrawData) := do
  if p.yoraiz0rDarkness then oos "yoraiz0rDarkness"
  let hides ← if p.head >= 0 then lookup "Head.Sets.HidesHead" c.hidesHead p.head else pure false
  if p.invis || hides then return #[]
  let pos := (← anchor c d p p.bodyFrame) + p.headPosition + d.headVect
  let mut out := #[
    dd (.players d.skinVar 0) pos (some p.bodyFrame) d.colorHead p.headRotation d.headVect d.playerEffect d.skinDyePacked,
    dd (.players d.skinVar 1) pos (some p.bodyFrame) d.colorEyeWhites p.headRotation d.headVect d.playerEffect,
    dd (.players d.skinVar 2) pos (some p.bodyFrame) d.colorEyes p.headRotation d.headVect d.playerEffect]
  -- DrawPlayer_21_Head_TheFace_Eyelid
  match c.eyelidTexture d.skinVar with
  | none => pure ()
  | some (tw, th) =>
    let off ← lookup "Main.OffsetsPlayerHeadgear" c.offsetsPlayerHeadgear (p.bodyFrame.y / p.bodyFrame.h)
    let off := { off with y := off.y - 2 }
    let sign : Int32 := if d.playerEffect &&& 2 != 0 then 1 else -1
    let off := off.scale (-sign).toFloat32
    let fw := tw / 1
    let fh := th / 3
    let rect : Rect := ⟨fw * 0, fh * p.eyeFrameToShow, fw + 0, fh + 0⟩   -- Utils.Frame(tex, 1, 3, 0, eyeFrame, 0, 0)
    let epos := Vector2.zero + (← anchor c d p p.bodyFrame) + p.headPosition + d.headVect + off
    out := out.push (dd (.players d.skinVar 15) epos (some rect) d.colorHead p.headRotation d.headVect d.playerEffect d.skinDyePacked)
  return out

/-- `DrawPlayer_21_Head` for the ordinary branch. -/
def drawHead (c : Context) (d : DrawSet) (p : Player) : M (Array DrawData) := do
  let mountOffset := Vector2.zero                    -- ApplyHeadOffsetFromMount: no mount
  let helmetPos := d.helmetOffset + mountOffset
  let mut out ← drawFace c d p
  -- IL_0123: hair position
  let v7 : Vector2 := ⟨(-(p.bodyFrame.w / 2) + p.width / 2).toFloat32, (p.height - p.bodyFrame.h + 4).toFloat32⟩
  let hairPos := (← (d.position.sub c.screenPosition + v7).floor) + p.headPosition + d.headVect + mountOffset
  let hairPos := if d.playerEffect &&& 2 != 0 then { hairPos with y := hairPos.y + (p.bodyFrame.h - d.hairFrontFrame.h).toFloat32 } else hairPos
  let hairPos := hairPos + d.hairOffset
  let headAnchor := helmetPos + (← anchor c d p p.bodyFrame) + p.headPosition + d.headVect
  let skin ← if p.head >= 0 && p.head < c.headCount then lookup "Head.Sets.UseSkinColor" c.useSkinColor p.head else pure false
  let (armorCol, armorShader) := if skin then (d.colorHead, d.skinDyePacked) else (d.colorArmorHead, d.cHead)
  -- IL_03cc fullHair
  if d.fullHair then
    out := out.push (dd (.armorHead p.head) headAnchor (some p.bodyFrame) armorCol p.headRotation d.headVect d.playerEffect armorShader)
    if !p.invis then
      out := out.push (dd (.playerHair p.hair) hairPos (some d.hairFrontFrame) d.colorHair p.headRotation d.headVect d.playerEffect d.hairDyePacked)
  -- IL_05a2 hatHair
  if d.hatHair && !p.invis then
    out := out.push (dd (.playerHairAlt p.hair) hairPos (some d.hairFrontFrame) d.colorHair p.headRotation d.headVect d.playerEffect d.hairDyePacked)
  let v3 := p.head == 14 || p.head == 56 || p.head == 114 || p.head == 158 || p.head == 69 || p.head == 180
  let v4 := p.head == 28
  let v5 := p.head == 39 || p.head == 38
  if p.head == 270 || p.head == 282 || v3 || p.head == 259 then oos "special hat"
  if p.head > 0 && p.head < c.headCount && !v4 then
    if !(p.invis && v5) then
     if p.head == 13 || p.head == 265 then oos "stacked hat"
     let (src, hv) := if p.gravDir == 1 then ({ p.bodyFrame with h := p.bodyFrame.h - 4 }, d.headVect)
      else ({ p.bodyFrame with h := p.bodyFrame.h - 4 }, { d.headVect with y := d.headVect.y - 4 })
     out := out.push (dd (.armorHead p.head) headAnchor (some src) armorCol p.headRotation hv d.playerEffect armorShader)
     if p.head == 292 || p.head == 109 || p.head == 211 then oos "head extra layer"
  else if !p.invis then
    let prevent ← if p.face >= 0 then lookup "Face.Sets.PreventHairDraw" c.facePreventHairDraw p.face else pure false
    if !prevent then
      out := out.push (dd (.playerHair p.hair) hairPos (some d.hairFrontFrame) d.colorHair p.headRotation d.headVect d.playerEffect d.hairDyePacked)
  -- IL_1d7b: beard (beard ≤ 0 in scope), head 205/214/137, babyBird
  if p.head == 205 || p.head == 214 || p.head == 137 then oos "head extra layer"
  if p.babyBird then oos "babyBird"
  return out

/-- The ordinary composition Root asked for: selection, setup, then the four layers in order. -/
structure DrawResult where
  player : Player
  drawSet : DrawSet
  leggings : Array DrawData
  torso : Array DrawData
  head : Array DrawData

def drawArmor (c : Context) (e : Equipment) (p : Player) (drawPosition : Vector2) (shadow : Float32) : M DrawResult := do
  let p ← selectArmor c e p
  let (d, p) ← boringSetup c p drawPosition shadow
  let male ← c.male p
  return { player := p, drawSet := d, leggings := ← drawLeggings c d p, torso := ← drawTorso c d p male,
           head := ← drawHead c d p }

end Lean4Terria.OriginalPlayerArmorDraw
