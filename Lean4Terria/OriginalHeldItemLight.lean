import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalCombat.Dust

/-! `Player.ItemCheck_EmitHeldItemLight` of desktop Terraria 1.4.5.8, together with the two
`Player.AnimatePlayerAndGetItemFrame` branches that place `Player.itemLocation` for the items it
covers (hold style 1 and the use style 1 swing), and the `Dust.UpdateDust` branches of the dust
those items create.

`ItemCheck` calls the method once per update for the item in `inventory[selectedItem]`, after the
item-use start and the `itemAnimation` decrement of the same call (IL_0852, behind
`!JustDroppedAnItem && IsAllowedToHoldItems`). The random draws are `Main.rand.Next(bound)`, the
draws of `Dust.NewDust` when that roll is zero and a particle is really created, and
`Main.rand.Next(3)` after `NewDust` returned (also when it returned its sentinel). -/
namespace Lean4Terria.OriginalHeldItemLight

open OriginalCollision (Vector single tileIndex)
open OriginalForestGuideArrow (DustPool DustParticle DustRequest DustFrame Viewport)

/-- The `Player` fields the method and the two location branches read. No mount, not sleeping,
not sitting, not petting, not a display doll: those paths are a declared boundary. -/
structure Holder where
  position : Vector
  width : Nat := 20
  height : Nat := 42
  velocity : Vector := ⟨0, 0⟩
  direction : Int := 1
  gravDir : Float32 := 1
  gfxOffY : Float32 := 0
  fullRotation : Float32 := 0
  wet : Bool := false
  pulley : Bool := false
  happyFunTorchTime : Bool := false
  stealth : Float32 := 1
  /-- `Player.itemAnimation` as `ItemCheck` left it before IL_0852: after a start in this update
  and after this update's decrement. -/
  itemAnimation : Int := 0
  itemLocation : Vector
  /-- `ApplyItemPositionOffsetFromMount` applied to zero. It is zero without a mount; other
  values are outside the checked range. -/
  mountOffset : Vector := ⟨0, 0⟩
  spelunkerTimer : Nat := 0

/-- The `Main` statics read through `TorchID.TorchColor` and `Player.BiomeTorchHoldStyle`. -/
structure Ambient where
  discoR : Nat := 255
  discoG : Nat := 0
  discoB : Nat := 0
  demonTorch : Float32 := 0
  mouseTextColor : Nat := 255
  /-- `Main.worldSurface`. -/
  worldSurface : Float := 0
  /-- `Main.UnderworldLayer` (`Main.maxTilesY - 200`). -/
  underworldLayer : Int := 0
  remixWorld : Bool := false

/-- `Player.UsingBiomeTorches` and the zone flags `BiomeTorchHoldStyle` tests, in its order. -/
structure Biome where
  usingBiomeTorches : Bool := false
  shimmer : Bool := false
  dungeon : Bool := false
  lihzhardTemple : Bool := false
  glowshroom : Bool := false
  hallow : Bool := false
  corrupt : Bool := false
  crimson : Bool := false
  snow : Bool := false
  jungle : Bool := false
  desert : Bool := false
  undergroundDesert : Bool := false

/-- `ItemID.Sets.Torches`. -/
def torches : List Nat :=
  [8, 427, 428, 429, 430, 431, 432, 433, 523, 974, 1245, 1333, 2274, 3004, 3045, 3114,
   4383, 4384, 4385, 4386, 4387, 4388, 5293, 5353]

/-- `ItemID.Sets.WaterTorches`. -/
def waterTorches : List Nat := [523, 1333, 4384]

/-- `TorchID.Dust`. -/
def torchDust : Array Nat :=
  #[6, 59, 60, 61, 62, 63, 64, 65, 75, 135, 158, 169, 156, 234, 66, 242, 293, 294, 295, 296,
    297, 298, 307, 310]

/-- `Player.BiomeTorchHoldStyle`. -/
def biomeTorchHoldStyle (itemType : Nat) (p : Holder) (biome : Biome) (ambient : Ambient) : Nat :=
  if !biome.usingBiomeTorches || itemType != 8 then itemType
  else if biome.shimmer then 5353
  else if biome.dungeon then 3004
  else if biome.lihzhardTemple then 4388
  else if p.position.y > single (ambient.underworldLayer * 16) then 433
  else if biome.glowshroom then 5293
  else if biome.hallow then 4387
  else if biome.corrupt then 4385
  else if biome.crimson then 4386
  else if biome.snow then 974
  else if biome.jungle then 4388
  else if (biome.desert && p.position.y.toFloat < ambient.worldSurface * 16) || biome.undergroundDesert then 4383
  else if biome.desert && ambient.remixWorld then 4383
  else itemType

/-- The torch style of a held torch item (IL_008f..01bf), clamped to `TorchID.Count - 1`. -/
def torchStyle (holdStyle : Nat) : Nat :=
  let style := match holdStyle with
    | 523 => 8 | 974 => 9 | 1245 => 10 | 1333 => 11 | 2274 => 12 | 3004 => 13 | 3045 => 14
    | 3114 => 15 | 4383 => 16 | 4384 => 17 | 4385 => 18 | 4386 => 19 | 4387 => 20 | 4388 => 21
    | 5293 => 22 | 5353 => 23
    | other => if other ≥ 427 then other - 426 else 0
  min style 23

/-- `TorchID.TorchColor`: the constant providers, the Demon (7), Disco (14) and Shimmer (23)
providers, and zero outside the table. -/
def torchColor (style : Int) (ambient : Ambient) : Float32 × Float32 × Float32 :=
  match style with
  | 0 => (1, 0.95, 0.8) | 1 => (0, 0.1, 1.3) | 2 => (1, 0.1, 0.1) | 3 => (0, 1, 0.1)
  | 4 => (0.9, 0, 0.9) | 5 => (1.4, 1.4, 1.4) | 6 => (0.9, 0.9, 0)
  | 7 =>
    let d := ambient.demonTorch
    (0.5 * d + (1 - d), 0.3, d + 0.5 * (1 - d))
  | 8 => (1, 1.6, 0.5) | 9 => (0.75, 0.85, 1.4) | 10 => (1, 0.5, 0) | 11 => (1.4, 1.4, 0.7)
  -- The green of style 12 is the single 1.3499999046325684 (0x3FACCCCC), one ulp below 1.35f.
  | 12 => (0.75, Float32.ofBits 0x3FACCCCC, 1.5) | 13 => (0.95, 0.75, 1.3)
  | 14 => (single ambient.discoR / 255, single ambient.discoG / 255, single ambient.discoB / 255)
  | 15 => (1, 0, 1) | 16 => (1.4, 0.85, 0.55) | 17 => (0.25, 1.3, 0.8) | 18 => (0.95, 0.4, 1.4)
  | 19 => (1.4, 0.7, 0.5) | 20 => (1.25, 0.6, 1.2) | 21 => (0.75, 1.45, 0.9) | 22 => (0.3, 0.78, 1.2)
  | 23 =>
    let clamp := fun (x : Float32) => if x > 1 then 1 else if x < 0 then 0 else x
    let fade := single (270 - (ambient.mouseTextColor : Int))
    let a := clamp (0.9 + fade / 900)
    let b := clamp (0.9 + fade / 125)
    (a * 0.9, b * 0.55, a * 1.2)
  | _ => (0, 0, 0)

/-- `Utils.RotatedBy(v, radians)` around the origin. -/
private def rotatedBy (v : Vector) (radians : Float32) : Vector :=
  let cos := (Float.cos radians.toFloat).toFloat32
  let sin := (Float.sin radians.toFloat).toFloat32
  ⟨0 + (v.x * cos - v.y * sin), 0 + (v.x * sin + v.y * cos)⟩

/-- `Player.RotatedRelativePoint(point, reverseRotation := false, addGfxOffY := true)`
without a mount, bed or chair. -/
def rotatedRelativePoint (p : Holder) (point : Vector) : Vector :=
  let pivot : Vector := ⟨p.position.x + single p.width / 2 + 0, p.position.y + single p.height + p.gfxOffY⟩
  let lift : Int := 4
  let shift := (⟨0, single (-lift)⟩ : Vector) + rotatedBy ⟨0, single lift⟩ p.fullRotation
  let point : Vector := ⟨point.x, point.y + p.gfxOffY⟩
  pivot + rotatedBy ⟨point.x - pivot.x, point.y - pivot.y⟩ p.fullRotation + shift

/-- `Lighting.AddLight(Vector2, r, g, b)` as the method requests it. The tile it lights is
`(Int)(position / 16)` on both axes. -/
structure Light where
  position : Vector
  r : Float32
  g : Float32
  b : Float32
  /-- The Fairy Glowstick also spreads this colour with `Utils.FloodFillTile` (radius 4.5,
  `DelegateMethods.SpreadLightOpen_StopForSolids`); that spread is not modelled here. -/
  spread : Bool := false
  deriving BEq, Repr

structure Emitted where
  random : UnifiedRandom
  pool : DustPool
  /-- The particle created by this call, with the caller-side edits applied. -/
  dust : Array DustParticle := #[]
  light : Option Light := none
  /-- The Peace Candle moves `Player.itemLocation` four pixels toward the body. -/
  itemLocation : Vector
  spelunkerTimer : Nat
  /-- `SpelunkerProjectileHelper.AddSpotToCheck(Center)` every tenth Spelunker Glowstick update. -/
  spelunkerSpot : Option Vector := none

/-- Dust types whose `Dust.NewDust` tail redraws the vertical speed (IL_04a7..0623). -/
def redrawsSpeed (typeId : Nat) : Bool :=
  typeId == 228 || typeId == 279 || typeId == 269 || typeId == 135 || typeId == 6 || typeId == 242 ||
  typeId == 75 || typeId == 169 || typeId == 29 || (typeId ≥ 59 && typeId ≤ 65) || typeId == 158 ||
  (typeId ≥ 293 && typeId ≤ 298) || typeId == 302 || typeId == 307 || typeId == 310

/-- `Dust.NewDust` for the dust these items create: the shared creation, the atlas row of
types above 99, and the extra `Main.rand.Next(-10, 6)` of the flame types. -/
def newDust (pool : DustPool) (request : DustRequest) (random : UnifiedRandom) :
    DustPool × Option DustParticle × UnifiedRandom :=
  let (pool, created, random) := pool.create request random
  match created with
  | none => (pool, none, random)
  | some dust =>
    let bank : Int := request.typeId / 100
    let dust := { dust with frame := { dust.frame with x := dust.frame.x - 1000 * bank, y := dust.frame.y + 30 * bank } }
    if redrawsSpeed request.typeId then
      let (speed, random) := random.next (-10) 6
      (pool, some { dust with velocity := ⟨dust.velocity.x * 0.3, single speed * 0.1⟩, scale := dust.scale * 0.7 }, random)
    else (pool, some dust, random)

private structure Draw where
  random : UnifiedRandom
  pool : DustPool
  dust : Array DustParticle := #[]

/-- `loc1 && Main.rand.Next(bound) == 0`, `Dust.NewDust(position, 4, 4, type, 0, 0, 100)`,
`Main.rand.Next(3)`, and the edits of the returned particle. With a sentinel result the
third draw still happens and nothing is kept. -/
private def flame (p : Holder) (ambient : Ambient) (bound : Nat) (typeId : Nat) (position : Vector)
    (pool : DustPool) (random : UnifiedRandom) : Draw :=
  if !(p.stealth ≥ 1) then { random, pool } else
  let (roll, random) := random.next 0 bound
  if roll != 0 then { random, pool } else
  let (pool, created, random) := newDust pool { position, width := 4, height := 4, typeId, alpha := 100 } random
  let (gravity, random) := random.next 0 3
  match created with
  | none => { random, pool }
  | some dust =>
    let dust := if gravity != 0 then { dust with noGravity := true } else dust
    let dust := { dust with
      velocity := ⟨dust.velocity.x * 0.3, dust.velocity.y * 0.3 - 1.5⟩
      position := rotatedRelativePoint p dust.position }
    let dust := if typeId == 66 then
        let color : UInt32 := (ambient.discoR.toUInt32 <<< 24) ||| (ambient.discoG.toUInt32 <<< 16) |||
          (ambient.discoB.toUInt32 <<< 8) ||| 255
        { dust with request := { dust.request with color }, noGravity := true }
      else dust
    { random, pool, dust := #[dust] }

/-- Which part of the method an item type reaches. -/
inductive Coverage where
  /-- No branch of the method names this item. -/
  | untouched
  /-- Ported: every draw, particle edit and light request. -/
  | exact
  /-- Ported except for the stated part. -/
  | partially (missing : String)
  /-- A branch exists and is not ported; `emit` does nothing for it. -/
  | unsupported (branch : String)
  deriving Repr, BEq

def coverage (itemType : Nat) : Coverage :=
  if torches.contains itemType || itemType == 105 || itemType == 713 || itemType == 148 ||
      itemType == 3117 || itemType == 5322 || itemType == 282 || itemType == 286 ||
      itemType == 3112 || itemType == 3002 || itemType == 5643 then .exact
  else if itemType == 4776 then .partially "Utils.FloodFillTile light spread"
  else if itemType == 3542 then .unsupported "IL_150b: four NewDust 242 at Player.HandPosition"
  else if itemType == 4952 then .unsupported "IL_163b: light, Next(40), NextVector2Circular, NewDustPerfect 43"
  else .untouched

/-- One `Player.ItemCheck_EmitHeldItemLight(item)` call. `pool.enabled := false` is a host on
which `Dust.NewDust` returns its sentinel (dedicated server, game menu, pause). -/
def emit (itemType : Nat) (p : Holder) (ambient : Ambient) (biome : Biome) (pool : DustPool)
    (random : UnifiedRandom) : Emitted := Id.run do
  let idle : Emitted := { random, pool, itemLocation := p.itemLocation, spelunkerTimer := p.spelunkerTimer }
  let left := p.direction == -1
  let location := p.itemLocation
  let animating := p.itemAnimation > 0
  -- The glowstick light position, shared by every branch that adds the mount offset.
  let glow : Vector := rotatedRelativePoint p
    ((⟨(if left then location.x - 16 else location.x + 6) + p.velocity.x, location.y - 14⟩ : Vector) + p.mountOffset)
  if (torches.contains itemType && !p.wet) || waterTorches.contains itemType then
    if p.pulley || p.happyFunTorchTime then return idle
    let style := torchStyle (biomeTorchHoldStyle itemType p biome ambient)
    let (r, g, b) := torchColor style ambient
    let typeId := torchDust[style]!
    let bound := if animating then 7 else 30
    let position : Vector :=
      (⟨if left then location.x - 16 else location.x + 6, location.y - 14 * p.gravDir⟩ : Vector) + p.mountOffset
    let made := flame p ambient bound typeId position pool random
    let lit : Vector := rotatedRelativePoint p
      ((⟨(if left then location.x - 12 else location.x + 12) + p.velocity.x, location.y - 14 + p.velocity.y⟩ : Vector) +
        p.mountOffset)
    return { idle with random := made.random, pool := made.pool, dust := made.dust
                       light := some { position := lit, r := r * 1, g := g * 1, b := b * 1 } }
  if itemType == 105 || itemType == 713 || itemType == 148 then
    let candle := itemType != 148
    if (candle && p.wet) || p.pulley then return idle
    let bound := if animating then 7 else if candle then 20 else 10
    let position : Vector :=
      (⟨if left then location.x - 12 else location.x + 4, location.y - 20 * p.gravDir⟩ : Vector) + p.mountOffset
    let made := flame p ambient bound (if candle then 6 else 172) position pool random
    let (r, g, b) : Float32 × Float32 × Float32 := if candle then (1 * 1, 0.95 * 1, 0.8 * 1) else (0, 0.5 * 1, 1 * 1)
    return { idle with random := made.random, pool := made.pool, dust := made.dust
                       light := some { position := glow, r, g, b } }
  if itemType == 3117 || itemType == 5322 then
    if p.wet || p.pulley then return idle
    if itemType == 5322 then
      let lit := rotatedRelativePoint p ⟨(if left then location.x - 16 else location.x + 6) + p.velocity.x, location.y - 14⟩
      return { idle with light := some { position := lit, r := 0.2 * 1, g := 0.3 * 1, b := 0.32 * 1 } }
    let location : Vector := ⟨location.x - single (p.direction * 4), location.y⟩
    let bound := if animating then 7 else 10
    let position : Vector := ⟨if left then location.x - 10 else location.x + 6, location.y - 20 * p.gravDir⟩
    let made := flame p ambient bound 242 position pool random
    let lit := rotatedRelativePoint p ⟨(if left then location.x - 16 else location.x + 6) + p.velocity.x, location.y - 14⟩
    return { idle with random := made.random, pool := made.pool, dust := made.dust, itemLocation := location
                       light := some { position := lit, r := 0.9 * 1, g := 0.1 * 1, b := 0.75 * 1 } }
  if p.pulley then return idle
  match itemType with
  | 282 => return { idle with light := some { position := glow, r := 0.7 * 1, g := 1 * 1, b := 0.8 * 1 } }
  | 3002 =>
    let timer := (p.spelunkerTimer + 1) % 256
    let centre : Vector := ⟨p.position.x + single p.width / 2, p.position.y + single p.height / 2⟩
    return { idle with
      light := some { position := glow, r := 1.05 * 1, g := 0.95 * 1, b := 0.55 * 1 }
      spelunkerTimer := if timer ≥ 10 then 0 else timer
      spelunkerSpot := if timer ≥ 10 then some centre else none }
  | 286 => return { idle with light := some { position := glow, r := 0.7 * 1, g := 0.8 * 1, b := 1 * 1 } }
  | 3112 => return { idle with light := some { position := glow, r := 1 * 1, g := 0.6 * 1, b := 0.85 * 1 } }
  | 4776 => return { idle with light := some { position := glow, r := 0.9 * 1, g := 0.35 * 1, b := 1 * 1, spread := true } }
  | 5643 =>
    return { idle with light := some { position := glow, r := single ambient.discoR / 255 * 1,
                                       g := single ambient.discoG / 255 * 1, b := single ambient.discoB / 255 * 1 } }
  | _ => return idle

/-- `Item.GetDrawHitbox` for the covered items: the whole item texture (`Images/Item_<type>`).
On a dedicated server the original returns `Rectangle.Empty` and the frame is not read. -/
structure ItemFrame where
  width : Nat
  height : Nat

/-- `ItemCheck_ApplyHoldStyle` for hold style 1 with the covered items (IL_00e3..0a8a).
`heightOffset` is `Player.HeightOffsetHitboxCenter`, zero without a mount or portable stool. -/
def holdLocation (dedServ : Bool) (itemType : Nat) (frame : ItemFrame) (p : Holder)
    (heightOffset : Float32 := 0) : Vector :=
  if p.pulley then p.itemLocation else
  let middle := p.position.x + single p.width * 0.5
  let direction := single p.direction
  let x :=
    if dedServ then middle + 20 * direction
    else
      let x := middle + (single frame.width * 0.5 + 2) * direction
      if itemType == 282 || itemType == 286 || itemType == 3112 || itemType == 4776 || itemType == 5643 then
        x - single (p.direction * 2)
      else if itemType == 3002 then x - single (4 * p.direction)
      else x
  let y := p.position.y + 24 + heightOffset
  let y := if p.gravDir == -1 then p.position.y + single p.height + (p.position.y - y) + heightOffset else y
  ⟨x, y⟩

/-- `ItemCheck_ApplyUseStyle` for use style 1 with the covered items (IL_025f..0776): the three
phases of the placement swing. The original returns at once on a dedicated server. -/
def swingLocation (dedServ : Bool) (frame : ItemFrame) (p : Holder) (itemAnimationMax : Int)
    (heightOffset : Float32 := 0) : Vector :=
  if dedServ then p.itemLocation else
  let animation := Float.ofInt p.itemAnimation
  let full := Float.ofInt itemAnimationMax
  let middle := p.position.x + single p.width * 0.5
  let half := single frame.width * 0.5
  let direction := single p.direction
  let width := frame.width
  let height := frame.height
  let location : Vector :=
    if animation < full * 0.333 then
      let reach : Float32 := if width ≥ 92 then 38 else if width ≥ 64 then 28 else if width ≥ 52 then 24
        else if width > 32 then 14 else 10
      ⟨middle + (half - reach) * direction, p.position.y + 24 + heightOffset⟩
    else if animation < full * 0.666 then
      let reach : Float32 := if width ≥ 92 then 38 else if width ≥ 64 then 28 else if width ≥ 52 then 24
        else if width > 32 then 18 else 10
      let drop : Float32 := if height > 64 then 14 else if height > 52 then 12 else if height > 32 then 8 else 10
      ⟨middle + (half - reach) * direction, p.position.y + drop + heightOffset⟩
    else
      let reach : Float32 := if width ≥ 92 then 38 else if width ≥ 64 then 28 else if width ≥ 52 then 24
        else if width ≥ 48 then 18 else if width > 32 then 14 else 6
      let drop : Float32 := if height > 64 then 14 else if height > 52 then 12 else 10
      ⟨middle - (half - reach) * direction, p.position.y + drop + heightOffset⟩
  if p.gravDir == -1 then ⟨location.x, p.position.y + single p.height + (p.position.y - location.y)⟩
  else location

/-- `Player.AnimatePlayerAndGetItemFrame` for the covered items: where `Player.itemLocation` is
when `ItemCheck_EmitHeldItemLight` runs. `p.itemLocation` is the value the previous update left. -/
def locate (dedServ : Bool) (itemType : Nat) (frame : ItemFrame) (p : Holder) (itemAnimationMax : Int)
    (heightOffset : Float32 := 0) : Vector :=
  if p.itemAnimation > 0 then swingLocation dedServ frame p itemAnimationMax heightOffset
  else holdLocation dedServ itemType frame p heightOffset

/-- The part of one `ItemCheck` this module owns for a covered item: `AnimatePlayerAndGetItemFrame`
(IL_0833) placing `Player.itemLocation`, then `ItemCheck_EmitHeldItemLight` (IL_0852).
`p.itemLocation` is the value the previous update left; the result carries the new one. Items the
method does not name, and the two unsupported items, are left alone. -/
def update (dedServ : Bool) (itemType : Nat) (frame : ItemFrame) (p : Holder) (itemAnimationMax : Int)
    (ambient : Ambient) (biome : Biome) (pool : DustPool) (random : UnifiedRandom)
    (heightOffset : Float32 := 0) : Emitted :=
  match coverage itemType with
  | .untouched | .unsupported _ =>
    { random, pool, itemLocation := p.itemLocation, spelunkerTimer := p.spelunkerTimer }
  | _ =>
    let itemLocation := locate dedServ itemType frame p itemAnimationMax heightOffset
    emit itemType { p with itemLocation } ambient biome pool random

/-- Dust types `emit` can create, plus tile-break flame29 from Water/Shadow
Candles. `advance` and `dustAlpha` cover their ordinary no-custom-data states. -/
def handlesDust (typeId : Nat) : Bool :=
  torchDust.contains typeId || typeId == 172 || typeId == 29

/-- `Lighting.AddLight(x, y, r, g, b)` of one `Dust.UpdateDust` step. -/
structure DustLight where
  x : Int
  y : Int
  r : Float32
  g : Float32
  b : Float32
  deriving BEq, Repr

structure Advanced where
  particle : DustParticle
  active : Bool
  light : Option DustLight := none

/-- The torch style `Dust.UpdateDust` lights a flame dust type with, and its brightness cap. -/
private def flameLight (typeId : Nat) : Option (Int × Float32 × Float32) :=
  if typeId == 75 then some (8, 1, 1) else if typeId == 169 then some (11, 1, 1)
  else if typeId == 135 then some (9, 1, 1) else if typeId == 158 then some (10, 1, 1)
  else if typeId == 242 then some (15, 1, 1) else if typeId == 293 then some (16, 1, 0.95)
  else if typeId == 294 then some (17, 1, 1)
  else if typeId ≥ 59 && typeId ≤ 65 then some (1 + (typeId : Int) - 59, 0.8, 1)
  else if typeId == 295 then some (18, 1, 1) else if typeId == 296 then some (19, 1, 1)
  else if typeId == 297 then some (20, 1, 1) else if typeId == 298 then some (21, 1, 1)
  else if typeId == 307 then some (22, 1, 1) else if typeId == 310 then some (23, 1, 1)
  else none

/-- One `Dust.UpdateDust` step for a particle `emit` or candle tile destruction created:
types 6, 29, 59..65, 66, 75, 135, 156,
158, 169, 172, 234, 242, 293..298, 307 and 310 with zero `fadeIn`, no `customData`, and
`ChildSafety.Disabled`. It draws no random number. The density branches at the end of the
original loop compare `Dust.dCount`, which is at least one inside the loop, and never run. -/
def advance (dust : DustParticle) (view : Viewport) (ambient : Ambient := {}) : Advanced :=
  let typeId := dust.request.typeId
  let oversized := dust.scale > 10
  let position := dust.position + dust.velocity
  let x := tileIndex position.x
  let y := tileIndex position.y
  let torch := fun (style : Int) (amount : Float32) =>
    let (r, g, b) := torchColor style ambient
    some ({ x, y, r := r * amount, g := g * amount, b := b * amount } : DustLight)
  let cap := fun (limit value : Float32) => if value > limit then limit else value
  -- Independent light-only branches (IL_0a73, IL_122e, IL_129c).
  let early : Option DustLight :=
    if typeId == 172 then
      let level := cap 1 (dust.scale * 0.5)
      some { x, y, r := level * (level * 0), g := level * (level * 0.25), b := level * (level * 1) }
    else if typeId == 156 then torch 12 (dust.scale * 0.6)
    else if typeId == 234 then torch 13 (dust.scale * 0.6)
    else none
  let flameType := redrawsSpeed typeId
  let velocity := dust.velocity
  let (velocity, scale, rotation, light) : Vector × Float32 × Float32 × Option DustLight :=
    if flameType then
      let velocity : Vector := if dust.noGravity then velocity else ⟨velocity.x, velocity.y + 0.05⟩
      let amount := dust.scale * 1.4
      let light := if typeId == 29 then
          -- UpdateDust IL_169f..16f4: this flame has its own blue light,
          -- independent of the orange default and TorchID providers.
          let amount := cap 1 amount
          some { x, y, r := amount * 0.1, g := amount * 0.4, b := amount }
        else match flameLight typeId with
        | some (style, limit, factor) =>
          let amount := cap limit amount
          torch style (if typeId == 293 then amount * factor else amount)
        | none =>
          let amount := cap 0.6 amount
          some { x, y, r := amount, g := amount * 0.65, b := amount * 0.4 }
      (velocity, dust.scale, dust.rotation, light)
    else if typeId == 66 then
      let rotation := if velocity.x < 0 then dust.rotation - 1 else dust.rotation + 1
      let velocity : Vector := ⟨velocity.x * 0.98, velocity.y * 0.98⟩
      let scale := dust.scale + 0.02
      let level := cap 1 (scale * 0.8)
      let channel := fun (shift : UInt32) => ((dust.request.color >>> shift) &&& 255).toNat
      let light : DustLight := { x, y, r := level * (single (channel 24) / 255),
                                 g := level * (single (channel 16) / 255), b := level * (single (channel 8) / 255) }
      (velocity, scale, rotation, some light)
    else
      (if dust.noGravity then velocity else ⟨velocity.x, velocity.y + 0.1⟩, dust.scale, dust.rotation, early)
  let velocity : Vector := ⟨velocity.x * 0.99, velocity.y⟩
  let rotation := rotation + velocity.x * 0.5
  let scale := scale - 0.01
  let velocity : Vector := if dust.noGravity then ⟨velocity.x * 0.92, velocity.y * 0.92⟩ else velocity
  let scale := if dust.noGravity then scale - 0.04 else scale
  { particle := { dust with position, velocity, rotation, scale }
    active := !oversized && position.y ≤ view.position.y + single view.height && scale ≥ 0.1
    light }

/-- `Dust.GetAlpha(newColor)` for the held-light and candle-break dust types (not `fullBright`). Colours are
the RGBA words `DustRequest.color` uses. `Main.DrawDust` passes the tile light under the particle. -/
def dustAlpha (dust : DustParticle) (light : UInt32) : UInt32 :=
  let typeId := dust.request.typeId
  if typeId == 172 then 0xfafafa96
  else if typeId == 156 || typeId == 234 then 0xffffff00
  else if typeId == 66 then light &&& 0xffffff00
  else if typeId != 29 && redrawsSpeed typeId && !(typeId ≥ 59 && typeId ≤ 65) then (light &&& 0xffffff00) ||| 25
  else
    let opacity := single (255 - dust.request.alpha) / 255
    -- GetAlpha IL_0c95: RGB is softened toward full opacity for29;
    -- the output alpha still subtracts dust.alpha from the input alpha.
    let opacity := if typeId == 29 then (opacity + 3) / 4 else opacity
    let channel := fun (shift : UInt32) =>
      let value := (single ((light >>> shift) &&& 255).toNat * opacity).toInt32.toInt
      let value := if typeId == 29 then max 0 (min 255 value) else value
      (value.toNat.toUInt32 &&& 255) <<< shift
    let alpha : Int := ((light &&& 255).toNat : Int) - dust.request.alpha
    let alpha := if alpha < 0 then 0 else if alpha > 255 then 255 else alpha
    channel 24 ||| channel 16 ||| channel 8 ||| alpha.toNat.toUInt32

end Lean4Terria.OriginalHeldItemLight
