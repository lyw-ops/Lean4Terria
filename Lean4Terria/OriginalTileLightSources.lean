import Lean4Terria.OriginalTileLightSources.Basic
import Lean4Terria.OriginalTileLightSources.Arms

/-!
# OriginalTileLightSources — `Terraria.Graphics.Light.TileLightScanner` light sources

Readable hand transcription of the desktop 1.4.5.8 methods (exe
d52c7ea7e4e312d8937fd20d1f2609f0d6bd26daddf5bfab2d106ca9077103e0):

| native | token | Lean |
|---|---|---|
| `GetTileLight(int, int, out Vector3)` | 100672130 | `getTileLight` |
| `ApplySurfaceLight` | 100672135 | `applySurfaceLight` |
| `ApplyHellLight` | 100672136 | `applyHellLight` |
| `ApplyWallLight` | 100672133 | `applyWallLight` |
| `ApplyTileLight` (5,124 IL lines, IL_0000‥IL_4623) | 100672134 | `applyTileLight` |
| `ApplyLiquidLight` | 100672131 | `applyLiquidLight` |
| `Update()` | 100672127 | `Scanner.update` |

`LightIsBlocked` is the released `OriginalTileLightMask.lightIsBlocked` (not duplicated).
The five large frame switches of `ApplyTileLight` are provenance-bound data (`Arms.lean`,
generated from the CIL); every other arm and all control flow are written out below with the IL
label of each block.  The local `FastRandom` (`_random.WithModifier(x, y)`) and `Main.rand` are
threaded explicitly in native call order.
-/

namespace Lean4Terria.OriginalTileLightSources

open Lean4Terria.OriginalLightMap (Vector3 FastRandom)

/-- `lightColor.c < v ? v : lightColor.c` — the native `bge.un` (assign only when ordered-less). -/
@[inline] def raise (cur v : Float32) : Float32 := if cur < v then v else cur

/-- The final `if (lightColor.X < r) lightColor.X = r; …` of every Apply method. -/
def raiseInto (light : Vector3) (r g b : Float32) : Vector3 :=
  ⟨raise light.x r, raise light.y g, raise light.z b⟩

/-- `TileLightScanner.LightIsBlocked(tile)` = released mask leaf, with its fault classes. -/
def lightIsBlocked (c : Context) (tile : Tile) : M Bool :=
  match OriginalTileLightMask.lightIsBlocked c.mask (some tile.toMaskTile) with
  | .ok b => .ok b
  | .error .nullReference => .error .nullReference
  | .error .indexOutOfRange => .error .indexOutOfRange

/-- `(float)(270 - Main.mouseTextColor)`. -/
@[inline] def mouseWave (c : Context) : Float32 := (270 - c.mouseTextColor.toNat.toInt32).toFloat32

/-- `(float)Main.DiscoR / 255f` etc. -/
@[inline] def disco (c : Context) : Float32 × Float32 × Float32 :=
  (c.discoR.toFloat32 / 255, c.discoG.toFloat32 / 255, c.discoB.toFloat32 / 255)

/-! ## `ApplyLiquidLight` (100672131) -/

/-- `ApplyLiquidLight(Tile tile, ref Vector3 lightColor)`. -/
def applyLiquidLight (c : Context) (tile : Tile) (light : Vector3) : Vector3 :=
  if tile.liquid == 0 then light
  else if tile.lava then
    let v : Float32 := 0.55 + mouseWave c / 900
    ⟨raise light.x v, if light.y < v then v * 0.6 else light.y, if light.z < v then v * 0.2 else light.z⟩
  else if tile.shimmer then
    let v1 : Float32 := 0.7 + mouseWave c / 900
    let v2 : Float32 := 0.7 + mouseWave c / 125
    ⟨if light.x < v1 then v1 * 0.6 else light.x, if light.y < v2 then v2 * 0.25 else light.y,
     if light.z < v1 then v1 * 0.9 else light.z⟩
  else light

/-! ## `ApplyWallLight` (100672133) -/

/-- Locals `V_0‥V_3` (`r, g, b`, paint flag) shared by `ApplyWallLight`/`ApplyTileLight`. -/
structure Locals where
  r : Float32 := 0
  g : Float32 := 0
  b : Float32 := 0
  paint : Bool := false

/-- `(r, g, b) := constant`. -/
@[inline] def rgb (L : Locals) (r g b : Float32) : Locals := { L with r, g, b }

/-- `ApplyWallLight(Tile tile, int x, int y, ref FastRandom localRandom, ref Vector3 lightColor)`.
`LightIsBlocked(tile)` is evaluated only in the arms that call it natively. -/
def applyWallLight (c : Context) (tile : Tile) (rnd : FastRandom) (light : Vector3) :
    M (FastRandom × Vector3) := do
  let mut L : Locals := {}
  let mut rnd := rnd
  match tile.wall.toNat with
  | 33 =>   -- IL_0132
    if !(← lightIsBlocked c tile) then L := rgb L 0.089999996 0.052500002 0.24
  | 44 =>   -- IL_0221
    if !(← lightIsBlocked c tile) then
      let (r, g, b) := disco c
      L := rgb L (r * 0.15) (g * 0.15) (b * 0.15)
  | 137 =>  -- IL_01c1
    if !(← lightIsBlocked c tile) then
      let v : Float32 := 0.4 + mouseWave c / 1500
      let (n, rnd') := rnd.nextRange 0 50
      rnd := rnd'
      let v := v + n.toFloat32 * 0.0005
      L := rgb L (1 * v) (0.5 * v) (0.1 * v)
  | 153 => L := { L with r := 0.6, g := 0.3 }               -- IL_02c5
  | 154 => L := { L with r := 0.6, b := 0.6 }               -- IL_026b
  | 155 => L := rgb L 0.6 0.6 0.6                           -- IL_02ae
  | 156 => L := { L with g := 0.6 }                         -- IL_0298
  | 164 => L := { L with r := 0.6 }                         -- IL_02a3
  | 165 => L := { L with b := 0.6 }                         -- IL_028d
  | 166 => L := { L with r := 0.6, g := 0.6 }               -- IL_027c
  | 174 =>  -- IL_0155
    if !(← lightIsBlocked c tile) then L := { L with r := 0.2975 }
  | 175 =>  -- IL_016c
    if !(← lightIsBlocked c tile) then
      if tile.wallColor == 0 then L := rgb L 0.075 0.15 0.4 else L := { L with paint := true }
  | 176 =>  -- IL_019e
    if !(← lightIsBlocked c tile) then L := rgb L 0.1 0.1 0.1
  | 182 =>  -- IL_010f
    if !(← lightIsBlocked c tile) then L := rgb L 0.24 0.12 0.089999996
  | 341 => if !(← lightIsBlocked c tile) then L := rgb L 0.25 0.1 0      -- IL_02d6
  | 342 => if !(← lightIsBlocked c tile) then L := rgb L 0.3 0 0.17      -- IL_033c
  | 343 => if !(← lightIsBlocked c tile) then L := rgb L 0 0.25 0        -- IL_02f9
  | 344 => if !(← lightIsBlocked c tile) then L := rgb L 0 0.16 0.34     -- IL_031c
  | 345 => if !(← lightIsBlocked c tile) then L := rgb L 0.3 0 0.35      -- IL_0359
  | 346 =>  -- IL_0376
    if !(← lightIsBlocked c tile) then
      let (r, g, b) := disco c
      L := rgb L (r * 0.25) (g * 0.25) (b * 0.25)
  | 357 =>  -- IL_00ea
    if !(← lightIsBlocked c tile) then L := { rgb L 0.15 0.27 0.3 with paint := true }
  | _ => pure ()
  -- IL_03b8: paint override `WorldGen.paintColor(wallColor) / 765f`.
  if L.paint && tile.wallColor != 0 then
    let col := paintColor tile.wallColor.toNat.toInt32
    L := rgb L (col.r.toFloat32 / 765) (col.g.toFloat32 / 765) (col.b.toFloat32 / 765)
  return (rnd, raiseInto light L.r L.g L.b)

/-! ## `ApplySurfaceLight` (100672135) and `ApplyHellLight` (100672136) -/

/-- `Main.wallLight[wall] || wall == 73 || wall == 227 || (invisibleWall() && !_drawInvisibleWalls)`. -/
def wallLetsSkyIn (c : Context) (tile : Tile) : M Bool := do
  if ← lookupBool c.wallLight tile.wall.toNat then return true
  if tile.wall == 73 || tile.wall == 227 then return true
  return tile.invisibleWall && !c.mask.drawInvisibleWalls

/-- `Main.tile[x, y-1/y+1/x-1/x+1].liquid == 0`, in that native order (short-circuit). -/
def neighboursDry (grid : Grid) (x y : Int32) : M Bool := do
  if (← grid.get! x (y - 1)).liquid != 0 then return false
  if (← grid.get! x (y + 1)).liquid != 0 then return false
  if (← grid.get! (x - 1) y).liquid != 0 then return false
  return (← grid.get! (x + 1) y).liquid == 0

/-- Surface IL_00c5‥0167 / hell IL_0031‥00bd: `!active || !tileNoSunLight[type] ||
((slope != 0 || halfBrick || extra) && the four neighbours hold no liquid)`; the hell variant has no
`extra` (surface: `invisibleBlock() && !_drawInvisibleWalls`). -/
def skyOpen (c : Context) (grid : Grid) (tile : Tile) (x y : Int32) (extra : Bool) : M Bool := do
  if !tile.active then return true
  if !(← lookupBool c.tileNoSunLight tile.type.toNat) then return true
  if tile.slope != 0 || tile.halfBrick || extra then neighboursDry grid x y else return false

/-- `tile.halfBrick() && Main.tile[x, y - 1].liquid >= 200` (the neighbour is read only for half bricks). -/
def halfBrickUnderLiquid (grid : Grid) (tile : Tile) (x y : Int32) : M Bool := do
  if !tile.halfBrick then return false
  return (← grid.get! x (y - 1)).liquid >= 200

/-- The wall tint shared by surface (IL_0253‥03ca) and hell (IL_015e‥02df): walls 88‥93 and 241
(index 6), starting from `(r, g, b)`. -/
def wallTint (c : Context) (wall : UInt16) (r g b : Float32) : Float32 × Float32 × Float32 :=
  let index : Int32 := if wall == 241 then 6 else wall.toNat.toInt32 - 88
  match (index).toInt with
  | 0 => (r * 0.9, g * 0.15, b * 0.9)
  | 1 => (r * 0.9, g * 0.9, b * 0.15)
  | 2 => (r * 0.15, g * 0.15, b * 0.9)
  | 3 => (r * 0.15, g * 0.9, b * 0.15)
  | 4 => (r * 0.9, g * 0.15, b * 0.15)
  | 5 =>
    let v9 : Float32 := 0.2
    let v10 : Float32 := 0.7 - v9
    let (dr, dg, db) := disco c
    (r * (v10 + dr * v9), g * (v10 + dg * v9), b * (v10 + db * v9))
  | 6 => (r * 0.9, g * 0.5, b * 0)
  | _ => (r, g, b)

/-- `ApplySurfaceLight(Tile tile, int x, int y, ref Vector3 lightColor)`; may draw one
`Utils.NextFloat(Main.rand)` (IL_01f5‥0230). -/
def applySurfaceLight {σ : Type} (c : Context) (grid : Grid) (rand : MainRand σ) (tile : Tile)
    (x y : Int32) (light : Vector3) : M (Vector3 × MainRand σ) := do
  let tr : Float32 := c.tileColor.r.toFloat32 / 255
  let tg : Float32 := c.tileColor.g.toFloat32 / 255
  let tb : Float32 := c.tileColor.b.toFloat32 / 255
  let avg : Float32 := (tr + tg + tb) / 3
  let mut r : Float32 := 0
  let mut g : Float32 := 0
  let mut b : Float32 := 0
  let mut rand := rand
  let inWater ← if tile.active then lookupBool c.allowLightInWater tile.type.toNat else pure false
  if inWater then
    -- IL_0072‥00c0
    if light.x < avg then
      if ← wallLetsSkyIn c tile then
        r := tr; g := tg; b := tb
  else
    -- IL_00c5: sky reaches the cell unless a solid no-sunlight block covers it
    let open_ ← skyOpen c grid tile x y (tile.invisibleBlock && !c.mask.drawInvisibleWalls)
    if open_ && light.x < avg then
     if ← wallLetsSkyIn c tile then
      -- IL_01b0
      if tile.liquid < 200 then
        let blockedAbove ← halfBrickUnderLiquid grid tile x y
        if !blockedAbove then
          r := tr; g := tg; b := tb
      else
        -- IL_01e9
        let alpha ← lookupF32 c.liquidAlpha 13
        if alpha > 0 then
          let s ← match rand.state with
            | none => throw Fault.mainRandNullTimeSeeded
            | some s => pure s
          let (d, s) := rand.nextDouble s
          rand := { rand with state := some s }
          let nf : Float32 := d.toFloat32
          b := tb * 0.175 * (1 + nf * 0.13) * (← lookupF32 c.liquidAlpha 13)
  -- IL_0232
  let solidNoSun ← if tile.active && !tile.halfBrick then lookupBool c.tileNoSunLight tile.type.toNat
    else pure false
  if !solidNoSun && ((tile.wall >= 88 && tile.wall <= 93) || tile.wall == 241) && tile.liquid < 255 then
    let (r', g', b') := wallTint c tile.wall tr tg tb
    r := r'; g := g'; b := b'
  -- IL_03ca
  let d : Float32 := 1 - c.shimmerDarken
  return (raiseInto light (r * d) (g * d) (b * d), rand)

/-- `ApplyHellLight(Tile tile, int x, int y, ref Vector3 lightColor)`. -/
def applyHellLight (c : Context) (grid : Grid) (tile : Tile) (x y : Int32) (light : Vector3) :
    M Vector3 := do
  let v : Float32 := 0.55 + (Float.sin (c.globalTimeWrappedHourly * 2).toFloat).toFloat32 * 0.08
  let mut r : Float32 := 0
  let mut g : Float32 := 0
  let mut b : Float32 := 0
  -- IL_0031
  let open_ ← skyOpen c grid tile x y false
  if open_ && light.x < v then
    if (← wallLetsSkyIn c tile) && tile.liquid < 200 then
      -- IL_0109
      let blockedAbove ← halfBrickUnderLiquid grid tile x y
      if !blockedAbove then
        r := v; g := v * 0.6; b := v * 0.2
  -- IL_013d
  let solidNoSun ← if tile.active && !tile.halfBrick then lookupBool c.tileNoSunLight tile.type.toNat
    else pure false
  if !solidNoSun && ((tile.wall >= 88 && tile.wall <= 93) || tile.wall == 241) && tile.liquid < 255 then
    let (r', g', b') := wallTint c tile.wall v (v * 0.6) (v * 0.2)
    r := r'; g := g'; b := b'
  return raiseInto light r g b

/-! ## `ApplyTileLight` (100672134) -/

/-- Recurring `Next(28, 42) * 0.005f + (270 - mouseTextColor) / K` flicker. -/
def flicker (c : Context) (rnd : FastRandom) (scale divisor : Float32) : Float32 × FastRandom :=
  let (n, rnd) := rnd.nextRange 28 42
  (n.toFloat32 * scale + mouseWave c / divisor, rnd)

/-- Evaluate one arm of the generated frame switches. -/
def evalArm (c : Context) (tile : Tile) (x y : Int32) (L : Locals) : Arm → M Locals
  | .const r g b paint =>
    pure { r := r.getD L.r, g := g.getD L.g, b := b.getD L.b, paint := L.paint || paint }
  | .paintedOr r g b => pure (if tile.color == 0 then { L with r, g, b } else { L with paint := true })
  | .demonBlend ka kb kc kg kd ke kf =>
    let d := c.demonTorch
    pure { L with r := ka * d + kb * (kc - d), g := kg, b := kd * d + ke * (kf - d) }
  | .demonHsl k h s l m => do
    let col ← hslToRgb (c.demonTorch * k + h) s l 255
    let v := col.toVector3
    pure { L with r := v.x * m, g := v.y * m, b := v.z * m }
  | .shimmerClamped m =>
    let v := (getShimmerBaseColor c x.toFloat32 y.toFloat32).scale m
    pure { L with r := clampF32 v.x 0 1, g := clampF32 v.y 0 1, b := clampF32 v.z 0 1 }
  | .torch id => let (r, g, b) := torchColor c id; pure { L with r, g, b }
  | .special .lantern3a36 =>   -- IL_3a36
    let v := c.demonTorch * 0.2
    pure { L with r := 0.9 - v, g := 0.9 - v, b := 0.7 + v }
  | .special .lantern3a7a =>   -- IL_3a7a
    let r : Float32 := 1
    let g : Float32 := 0.3
    let b : Float32 := 0.5
    let b := b + c.demonTorch * 0.2
    let r := r - c.demonTorch * 0.1
    let g := g - c.demonTorch * 0.2
    pure { L with r, g, b }

/-- Native `switch (index)` over a generated table. -/
def switchArm (c : Context) (tile : Tile) (x y : Int32) (L : Locals) (arms : Array Arm)
    (default : Arm) (index : Int32) : M Locals :=
  if index < 0 || index.toNatClampNeg ≥ arms.size then evalArm c tile x y L default
  else evalArm c tile x y L arms[index.toNatClampNeg]!

/-- `rem` with the native exceptions. -/
def remChecked (a b : Int32) : M Int32 :=
  if b == 0 then throw .divideByZero
  else if b == -1 && a == -2147483648 then throw .arithmeticOverflow
  else pure (a % b)

/-- The `switch (tile.type)` body of `ApplyTileLight` (IL_0025‥IL_4596), returning the locals
`V_0‥V_3` and the advanced `localRandom`; may mutate `lightColor` directly (types 125, 235, 238). -/
def tileSwitch (c : Context) (grid : Grid) (tile : Tile) (x y : Int32) (rnd : FastRandom)
    (light : Vector3) : M (Locals × FastRandom × Vector3) := do
  let L : Locals := {}
  let fx : Int32 := tile.frameX.toInt32
  let fy : Int32 := tile.frameY.toInt32
  let mw := mouseWave c
  let none_ : M (Locals × FastRandom × Vector3) := pure (L, rnd, light)
  let ret (L : Locals) : M (Locals × FastRandom × Vector3) := pure (L, rnd, light)
  match tile.type.toNat with
  | 658 =>  -- IL_06cb
    if tile.invisibleBlock then none_ else
    let (r, g, b) := torchColor c 23
    let s : Float32 := match (fy / 54).toInt with | 1 => 0.3 | 2 => 0.1 | _ => 0.2  -- default falls into case 0
    ret (rgb L (r * s) (g * s) (b * s))
  | 356 => if c.sundialCooldown == 0 then ret (rgb L 0.45 0.25 0) else none_   -- IL_0758
  | 663 => if c.moondialCooldown == 0 then ret (rgb L 0 0.25 0.45) else none_  -- IL_0779
  | 656 => ret (rgb L 0.2 0.55 0.5)                                            -- IL_079a
  | 20 =>  -- IL_07b1
    let v := fx / 18
    if v < 30 || v > 32 then none_ else ret (rgb L 0.325 0.15 0.05)
  | 634 => ret (rgb L 0.65 0.3 0.1)                                            -- IL_07e5
  | 633 | 637 | 638 => ret (rgb L 0.325 0.15 0.05)                             -- IL_07fc
  | 463 => ret (rgb L 0.2 0.4 0.8)                                             -- IL_0813
  | 491 => ret (rgb L 0.5 0.4 0.7)                                             -- IL_082a
  | 209 =>  -- IL_0841
    let portal (k : Int32) : M (Locals × FastRandom × Vector3) := do
      let v := ((← getPortalColor c c.myPlayer k).toVector3).scale 0.65
      ret (rgb L v.x v.y v.z)
    if fx == 234 || fx == 252 then portal 0
    else if fx == 306 || fx == 324 then portal 1
    else none_
  | 415 => ret (rgb L 0.7 0.5 0.1)                                             -- IL_08e6
  | 500 => ret (rgb L 0.525 0.375 0.075)                                       -- IL_08fd
  | 416 => ret (rgb L 0 0.6 0.7)                                               -- IL_0914
  | 501 => ret (rgb L 0 0.45 0.525)                                            -- IL_092b
  | 417 => ret (rgb L 0.6 0.2 0.6)                                             -- IL_0942
  | 502 => ret (rgb L 0.45 0.15 0.45)                                          -- IL_0959
  | 418 => ret (rgb L 0.6 0.6 0.9)                                             -- IL_0970
  | 503 => ret (rgb L 0.45 0.45 0.675)                                         -- IL_0987
  | 390 => ret (rgb L 0.4 0.2 0.1)                                             -- IL_099e
  | 597 =>  -- IL_09b5; the default keeps the zero locals, then all are scaled
    let L := match (fx / 54).toInt with
      | 0 => rgb L 0.05 0.8 0.3
      | 1 => rgb L 0.7 0.8 0.05
      | 2 => rgb L 0.7 0.5 0.9
      | 3 => rgb L 0.6 0.6 0.8
      | 4 => rgb L 0.4 0.4 1.15
      | 5 => rgb L 0.85 0.45 0.1
      | 6 => rgb L 0.8 0.8 1
      | 7 => rgb L 0.5 0.8 1.2
      | _ => L
    ret (rgb L (L.r * 0.75) (L.g * 0.75) (L.b * 0.75))
  | 564 => if fx < 36 then ret (rgb L 0.05 0.3 0.55) else none_               -- IL_0aaa
  | 568 => ret (rgb L 1 0.61 0.65)                                             -- IL_0ace
  | 569 => ret (rgb L 0.12 1 0.66)                                             -- IL_0ae5
  | 570 => ret (rgb L 0.57 0.57 1)                                             -- IL_0afc
  | 580 => ret (rgb L 0.7 0.3 0.2)                                             -- IL_0b13
  | 391 => ret (rgb L 0.3 0.1 0.25)                                            -- IL_0b2a
  | 381 | 517 | 687 => ret (rgb L 0.25 0.1 0)                                  -- IL_0b41
  | 534 | 535 | 689 => ret (rgb L 0 0.25 0)                                    -- IL_0b58
  | 536 | 537 | 690 => ret (rgb L 0 0.16 0.34)                                 -- IL_0b6f
  | 539 | 540 | 688 => ret (rgb L 0.3 0 0.17)                                  -- IL_0b86
  | 625 | 626 | 691 => ret (rgb L 0.3 0 0.35)                                  -- IL_0b9d
  | 627 | 628 | 692 =>                                                         -- IL_0bb4
    let (r, g, b) := disco c; ret (rgb L (r * 0.25) (g * 0.25) (b * 0.25))
  | 184 =>  -- IL_0bf2: independent frameX tests (at most one matches)
    let L := if fx == 110 then rgb L 0.25 0.1 0 else L
    let L := if fx == 132 then rgb L 0 0.25 0 else L
    let L := if fx == 154 then rgb L 0 0.16 0.34 else L
    let L := if fx == 176 then rgb L 0.3 0 0.17 else L
    let L := if fx == 198 then rgb L 0.3 0 0.35 else L
    if fx != 220 then ret L else
    let (r, g, b) := disco c; ret (rgb L (r * 0.25) (g * 0.25) (b * 0.25))
  | 370 => ret (rgb L 0.32 0.16 0.12)                                          -- IL_0cd8
  | 659 | 667 | 708 =>                                                         -- IL_0cef
    let v := getShimmerBaseColor c x.toFloat32 y.toFloat32
    ret (rgb L v.x v.y v.z)
  | 711 => ret (rgb L 0.01 0.01 0.01)                                          -- IL_0d11
  | 27 => if fy < 36 then ret { L with r := 0.3, g := 0.27 } else none_      -- IL_0d28
  | 336 => ret (rgb L 0.85 0.5 0.3)                                           -- IL_0d46
  | 340 => ret (rgb L 0.45 1 0.45)                                            -- IL_0d5d
  | 341 =>                                                                    -- IL_0d74
    let d := c.demonTorch
    ret (rgb L (0.4 * d + 0.6 * (1 - d)) 0.35 (1 * d + 0.6 * (1 - d)))
  | 342 => ret (rgb L 0.5 0.5 1.1)                                            -- IL_0dbb
  | 343 => ret (rgb L 0.85 0.85 0.3)                                          -- IL_0dd2
  | 344 => ret (rgb L 0.6 1.026 0.96000004)                                   -- IL_0de9
  | 327 =>                                                                    -- IL_0e00
    let v : Float32 := 0.5 + mw / 1500
    let (n, rnd) := rnd.nextRange 0 50
    let v := v + n.toFloat32 * 0.0005
    pure (rgb L (1 * v) (0.5 * v) (0.1 * v), rnd, light)
  | 316 | 317 | 318 =>                                                       -- IL_0e54 jellyfish jars
    let v17 := y - fy / 18
    let frame ← remChecked ((x - fx / 18) / 3 * (v17 / 3)) c.cageFrames
    let mode2 := (← c.jellyfishCage (tile.type.toNat.toInt32 - 316) frame) == 2
    match tile.type.toNat with
    | 316 => ret (if mode2 then rgb L 0.2 0.3 0.8 else rgb L 0.1 0.2 0.5)
    | 317 => ret (if mode2 then rgb L 0.2 0.7 0.3 else rgb L 0.05 0.45 0.1)
    | _ => ret (if mode2 then rgb L 0.7 0.2 0.5 else rgb L 0.4 0.1 0.25)
  | 719 =>                                                                    -- IL_0f50
    let k ← convI4F32 (c.globalTimeWrappedHourly * 15)
    let (r, g, b) : Float32 × Float32 × Float32 := match ((x + y + k) % 14).toInt with
      | 0 => (255, 171, 183) | 1 => (255, 170, 220) | 2 => (252, 171, 255) | 3 => (224, 171, 255)
      | 4 => (192, 171, 255) | 5 => (174, 178, 255) | 6 => (168, 195, 255) | 7 => (167, 224, 255)
      | 8 => (168, 255, 252) | 9 => (162, 255, 233) | 10 => (158, 255, 198) | 11 => (207, 255, 173)
      | 12 => (255, 213, 186) | 13 => (255, 192, 182) | _ => (0, 0, 0)
    ret (rgb L (r / 255) (g / 255) (b / 255))
  | 718 =>                                                                    -- IL_1136
    if c.dayTime then none_ else
    if ← solidTile3 c grid x (y - 1) then none_ else
    let (dr, dg, db) := (c.discoR.toFloat32, c.discoG.toFloat32, c.discoB.toFloat32)
    let (f1, rnd) := rnd.nextFloat
    let r := f1 * 0.04 + 0.1 + dr / 800
    let (f2, rnd) := rnd.nextFloat
    let g := f2 * 0.04 + 0.1 + dg / 800
    let (f3, rnd) := rnd.nextFloat
    let b := f3 * 0.04 + 0.1 + db / 800
    pure (rgb L r g b, rnd, light)
  | 717 =>                                                                    -- IL_11b6
    let v : Float32 := 0.55 + mw / 800
    let (f, rnd) := rnd.nextFloat
    let v := v + f * 0.03
    let v := v * 0.5
    pure (rgb L (v * 1.1) (v * 0.4) (v * 0.1), rnd, light)
  | 429 =>                                                                    -- IL_1210
    let n := fx / 18
    let L := if n % 2 >= 1 then { L with r := L.r + 0.5 } else L
    let L := if n % 4 >= 2 then { L with g := L.g + 0.5 } else L
    let L := if n % 8 >= 4 then { L with b := L.b + 0.5 } else L
    ret (if n % 16 >= 8 then { L with r := L.r + 0.2, g := L.g + 0.2 } else L)
  | 286 | 619 => ret (rgb L 0.1 0.2 0.7)                                      -- IL_1281
  | 620 =>                                                                    -- IL_1298
    let hue := frem1 (c.globalTimeWrappedHourly * 0.5)
    let col ← multiplyRGBA (Color.ofInts 230 230 230 0) (← hslToRgb hue 1 0.5 255)
    let col ← col.scale 0.4
    ret (rgb L (col.r.toFloat32 / 255) (col.g.toFloat32 / 255) (col.b.toFloat32 / 255))
  | 582 | 598 => ret (rgb L 0.7 0.2 0.1)                                      -- IL_1319
  | 270 => ret (rgb L 0.73 1 0.41)                                            -- IL_1330
  | 271 => ret (rgb L 0.45 0.95 1)                                            -- IL_1347
  | 581 => ret (rgb L 1 0.75 0.5)                                             -- IL_135e
  | 660 => let (r, g, b) := torchColor c 23; ret (rgb L r g b)                -- IL_1375
  | 572 =>                                                                    -- IL_1387
    match (fy / 36).toInt with
    | 0 => ret (rgb L 0.9 0.5 0.7)
    | 1 => ret (rgb L 0.7 0.55 0.96)
    | 2 => ret (rgb L 0.45 0.96 0.95)
    | 3 => ret (rgb L 0.5 0.96 0.62)
    | 4 => ret (rgb L 0.47 0.69 0.95)
    | 5 => ret (rgb L 0.92 0.57 0.51)
    | _ => none_
  | 262 => ret { L with r := 0.75, b := 0.75 }                                -- IL_1440
  | 263 => ret { L with r := 0.75, g := 0.75 }                                -- IL_1451
  | 264 => ret { L with b := 0.75 }                                           -- IL_1462
  | 265 => ret { L with g := 0.75 }                                           -- IL_146d
  | 266 => ret { L with r := 0.75 }                                           -- IL_1478
  | 267 => ret (rgb L 0.75 0.75 0.75)                                         -- IL_1483
  | 268 => ret { L with r := 0.75, g := 0.375 }                               -- IL_149a
  | 237 => ret { L with r := 0.1, g := 0.1 }                                  -- IL_14ab
  | 238 =>                                                                    -- IL_14bc: writes lightColor
    let lx := if light.x.toFloat < 0.5 then 0.5 else light.x
    let lz := if light.z.toFloat < 0.5 then 0.5 else light.z
    pure (L, rnd, { light with x := lx, z := lz })
  | 235 =>                                                                    -- IL_1502: writes lightColor
    let lx := if light.x.toFloat < 0.6 then 0.6 else light.x
    let ly := if light.y.toFloat < 0.6 then 0.6 else light.y
    pure (L, rnd, { light with x := lx, y := ly })
  | 405 =>                                                                    -- IL_1548
    if fx >= 54 then none_ else
    let (n, rnd) := rnd.nextRange 28 42
    let v : Float32 := n.toFloat32 * 0.005
    let v := v + mw / 700
    let d := c.demonTorch
    let L := match (fx / 54 - 1).toInt with
      | 0 => rgb L 0.7 1 0.5
      | 1 => rgb L (0.5 * d + 1 * (1 - d)) 0.3 (1 * d + 0.5 * (1 - d))
      | 2 => rgb L 0.45 0.75 1
      | 3 => rgb L 1.15 1.15 0.5
      | 4 => let (r, g, b) := disco c; rgb L r g b
      | _ => rgb L 0.9 0.3 0.1
    pure (rgb L (L.r + v) (L.g + v) (L.b + v), rnd, light)
  | 215 =>                                                                    -- IL_167f campfires
    if fy >= 36 then none_ else
    let (n, rnd) := rnd.nextRange 28 42
    let v : Float32 := n.toFloat32 * 0.005
    let v := v + mw / 700
    let d := c.demonTorch
    let L := match (fx / 54 - 1).toInt with
      | 0 => rgb L 0.7 1 0.5
      | 1 => rgb L (0.5 * d + 1 * (1 - d)) 0.3 (1 * d + 0.5 * (1 - d))
      | 2 => rgb L 0.45 0.75 1
      | 3 => rgb L 1.15 1.15 0.5
      | 4 => let (r, g, b) := disco c; rgb L r g b
      | 5 => rgb L 0.75 1.2824999 1.2
      | 6 => rgb L 0.95 0.65 1.3
      | 7 => rgb L 1.4 0.85 0.55
      | 8 => rgb L 0.25 1.3 0.8
      | 9 => rgb L 0.95 0.4 1.4
      | 10 => rgb L 1.4 0.7 0.5
      | 11 => rgb L 1.25 0.6 1.2
      | 12 => rgb L 0.75 1.45 0.9
      | 13 => rgb L 0.25 0.65 1
      | 14 => let (r, g, b) := torchColor c 23; rgb L r g b
      | _ => rgb L 0.9 0.3 0.1
    pure (rgb L (L.r + v) (L.g + v) (L.b + v), rnd, light)
  | 92 => if fy > 18 || fx != 0 then none_ else ret (rgb L 1 1 1)             -- IL_18b9
  | 592 =>                                                                    -- IL_18e8
    if fy <= 0 then none_ else
    let (n, rnd) := rnd.nextRange 28 42
    let v : Float32 := n.toFloat32 * 0.005
    let v := v + mw / 700
    let L := rgb L 1.35 0.45 0.15
    pure (rgb L (L.r + v) (L.g + v) (L.b + v), rnd, light)
  | 593 => if fx < 18 then ret (rgb L 0.8 0.3 0.1) else none_                 -- IL_1945
  | 594 => if fx < 36 then ret (rgb L 0.8 0.3 0.1) else none_                 -- IL_1969
  | 548 => if fx / 54 >= 7 then ret (rgb L 0.7 0.3 0.2) else none_            -- IL_198d
  | 613 | 614 => ret (rgb L 0.7 0.3 0.2)                                      -- IL_19b3
  | 93 =>                                                                     -- IL_19ca lamps
    if fx != 0 then none_ else
    ret (← switchArm c tile x y L Arms.lampArms Arms.lampArmsDefault (fy / 54 - 1))
  | 96 => if fx >= 36 then ret (rgb L 0.5 0.35 0.1) else none_               -- IL_20b3
  | 98 => if fy == 0 then ret (rgb L 1 0.97 0.85) else none_                  -- IL_20d7
  | 4 =>                                                                      -- IL_20f9 torches
    if fx >= 66 then none_ else
    let (r, g, b) := torchColor c (fy / 22); ret (rgb L r g b)
  | 372 => if fx == 0 then ret (rgb L 0.9 0.1 0.75) else none_               -- IL_211f
  | 646 => if fx == 0 then ret (rgb L 0.2 0.3 0.32) else none_               -- IL_2141
  | 33 =>                                                                     -- IL_2163 candles
    if fx != 0 then none_ else
    ret (← switchArm c tile x y L Arms.candleArms Arms.candleArmsDefault (fy / 22))
  | 174 => if fx == 0 then ret (rgb L 1 0.95 0.65) else none_                -- IL_281c
  | 100 | 173 =>                                                              -- IL_283e candelabras
    if fx >= 36 then none_ else
    ret (← switchArm c tile x y L Arms.candelabraArms Arms.candelabraArmsDefault (fy / 36 - 1))
  | 34 =>                                                                     -- IL_2f29 chandeliers
    if fx % 108 >= 54 then none_ else
    ret (← switchArm c tile x y L Arms.chandelierArms Arms.chandelierArmsDefault (fy / 54 + 37 * (fx / 108) - 7))
  | 35 => if fx < 36 then ret (rgb L 0.75 0.6 0.3) else none_                -- IL_3628
  | 95 => if fx < 36 then ret (rgb L 1 0.95 0.8) else none_                  -- IL_364c
  | 17 | 133 | 302 => ret (rgb L 0.83 0.6 0.5)                                -- IL_3670
  | 77 => ret (rgb L 0.75 0.45 0.25)                                          -- IL_3687
  | 37 =>                                                                     -- IL_369e
    let (n, rnd) := rnd.nextRange 95 106
    let v : Float32 := n.toFloat32 * 0.01
    pure (rgb L (0.56 * v) (0.43 * v) (0.15 * v), rnd, light)
  | 22 | 140 =>                                                               -- IL_36d2
    let L := if tile.color != 27 && tile.color != 26 then { L with r := 0.12 } else L
    ret { L with g := 0.07, b := 0.32 }
  | 171 =>                                                                    -- IL_36fd: reads the tree's origin tile
    let (ox, oy) := if fx < 10 then (x - fx, y - fy) else (x, y)
    let origin ← grid.get! ox oy
    let n := (origin.frameY.toInt32 &&& 15360) >>> 10
    let L := match (n - 1).toInt with
      | 0 => rgb L 0.1 0.1 0.1
      | 1 => { L with r := 0.2 }
      | 2 => { L with g := 0.2 }
      | 3 => { L with b := 0.2 }
      | 4 => { L with r := 0.125, g := 0.125 }
      | 5 => { L with r := 0.2, g := 0.1 }
      | 6 => { L with r := 0.125, g := 0.125 }
      | 7 => { L with r := 0.08, g := 0.175 }
      | 8 => { L with g := 0.125, b := 0.125 }
      | 9 => { L with r := 0.125, b := 0.125 }
      | 10 => rgb L 0.1 0.1 0.2
      | _ => rgb L 0 0 0
    ret (rgb L (L.r * 0.5) (L.g * 0.5) (L.b * 0.5))
  | 204 | 347 =>                                                              -- IL_3832
    if tile.color == 27 || tile.color == 26 then none_ else ret { L with r := 0.35 }
  | 42 =>                                                                     -- IL_3857 lanterns
    if fx != 0 then none_ else
    ret (← switchArm c tile x y L Arms.lanternArms Arms.lanternArmsDefault (fy / 36))
  | 49 => if fx == 0 then ret (rgb L 0 0.35 0.8) else none_                  -- IL_3ff6
  | 519 =>                                                                    -- IL_4018
    if fy != 90 then none_ else
    if tile.color != 0 then ret { L with paint := true } else
    let (v, rnd) := flicker c rnd 0.005 1000
    pure (rgb L 0.1 (0.2 + v / 2) (0.7 + v), rnd, light)
  | 70 | 71 | 72 | 190 | 348 | 349 | 528 | 578 =>                             -- IL_4082 mushrooms
    if tile.type == 349 && fx < 36 then none_ else
    let (v, rnd) := flicker c rnd 0.005 1000
    if tile.color == 0 then pure (rgb L 0 (0.2 + v / 2) 1, rnd, light)
    else pure ({ L with paint := true }, rnd, light)
  | 739 => ret { rgb L 0.35 0.63 0.7 with paint := true }                     -- IL_40f6
  | 350 =>                                                                    -- IL_410f
    let v := c.timeForVisualEffects * 0.08
    let k ← convI4F64 (v / 6.283)
    let arg : Float := if k % 3 == 1 then v else 0
    let s : Float32 := (-(Float.cos arg) * 0.1 + 0.1).toFloat32
    ret (rgb L s s s)
  | 61 | 703 =>                                                               -- IL_4164
    if fx != 144 then none_ else
    let a : Float32 := 1 + mw / 400
    let b : Float32 := 0.8 - mw / 400
    ret (rgb L (0.42 * b) (0.81 * a) (0.52 * b))
  | 26 | 31 | 695 | 696 =>                                                    -- IL_41c8
    let crimson := ((tile.type == 31 || tile.type == 696) && fx >= 36)
      || ((tile.type == 26 || tile.type == 695) && fx >= 54)
    let (n, rnd) := rnd.nextRange (-5) 6
    let v : Float32 := n.toFloat32 * 0.0025
    if crimson then pure (rgb L (0.5 + v * 2) (0.2 + v) 0.1, rnd, light)        -- IL_420a
    else pure (rgb L (0.31 + v) 0.1 (0.44 + v * 2), rnd, light)                -- IL_4240
  | 699 => ret (rgb L 0.4 0.2 0.15)                                           -- IL_4276
  | 84 =>                                                                     -- IL_428d herbs
    let n := fx / 18
    if n == 2 then
      let v : Float32 := mw / 400
      let v := if v > 1 then 1 else if v < 0 then 0 else v
      ret (rgb L (v * 1.4) (v * 1.2) (v / 2))
    else if n == 5 then
      let v : Float32 := 0.9
      ret (rgb L v (v * 0.8) (v * 0.2))
    else if n == 6 then
      let v : Float32 := 0.08
      ret { L with g := v * 0.8, b := v }
    else none_
  | 83 =>                                                                     -- IL_4340
    let L := if fx == 18 && !c.dayTime then rgb L 0.1 0.4 0.6 else L
    if fx == 90 && !c.raining && c.time > 40500 then ret (rgb L 0.9 0.72 0.18) else ret L
  | 126 =>                                                                    -- IL_43a4
    if fx >= 36 then none_ else let (r, g, b) := disco c; ret (rgb L r g b)
  | 125 =>                                                                    -- IL_43dd: writes lightColor.Y/Z
    let (v, rnd) := flicker c rnd 0.01 800
    let g := 0.3 * v
    let b := 0.6 * v
    pure ({ L with g, b }, rnd, { light with y := g, z := b })
  | 129 =>                                                                    -- IL_4437 crystal shards
    match (fx / 18 % 3).toInt with
    | 0 => ret (rgb L 0 0.05 0.25)
    | 1 => ret (rgb L 0.2 0 0.15)
    | 2 => ret (rgb L 0.1 0 0.2)
    | _ => none_
  | 149 =>                                                                    -- IL_44a1
    if fx > 36 then none_ else
    let L := match (fx / 18).toInt with
      | 0 => rgb L 0.1 0.2 0.5
      | 1 => rgb L 0.5 0.1 0.1
      | 2 => rgb L 0.2 0.5 0.1
      | _ => L
    let (n1, rnd) := rnd.nextRange 970 1031
    let r := L.r * (n1.toFloat32 * 0.001)
    let (n2, rnd) := rnd.nextRange 970 1031
    let g := L.g * (n2.toFloat32 * 0.001)
    let (n3, rnd) := rnd.nextRange 970 1031
    let b := L.b * (n3.toFloat32 * 0.001)
    pure (rgb L r g b, rnd, light)
  | 160 =>                                                                    -- IL_455b
    let (r, g, b) := disco c; ret (rgb L (r * 0.25) (g * 0.25) (b * 0.25))
  | 354 => ret (rgb L 0.65 0.35 0.15)                                         -- IL_4596
  | _ => none_

/-- `ApplyTileLight(Tile tile, int x, int y, ref FastRandom localRandom, ref Vector3 lightColor)`. -/
def applyTileLight (c : Context) (grid : Grid) (tile : Tile) (x y : Int32) (rnd : FastRandom)
    (light : Vector3) : M (FastRandom × Vector3) := do
  -- IL_0014: `if (Main.tileLighted[tile.type])`
  let (L, rnd, light) ← if ← lookupBool c.tileLighted tile.type.toNat then tileSwitch c grid tile x y rnd light
    else pure ({}, rnd, light)
  -- IL_45a8: paint override `WorldGen.paintColor(tile.color()) / 255f`.
  let L := if L.paint && tile.color != 0 then
      let col := paintColor tile.color.toNat.toInt32
      { L with r := col.r.toFloat32 / 255, g := col.g.toFloat32 / 255, b := col.b.toFloat32 / 255 }
    else L
  return (rnd, raiseInto light L.r L.g L.b)

/-! ## `GetTileLight` (100672130) and the scanner state -/

/-- The scanner's own state: `_random` (`_drawInvisibleWalls` is `Context.mask.drawInvisibleWalls`). -/
structure Scanner where
  random : FastRandom
  deriving Repr, BEq, Inhabited

/-- `TileLightScanner.Update()`: `_random.NextSeed()`. -/
def Scanner.update (s : Scanner) : Scanner := { s with random := s.random.nextSeed }

/-- Result of `GetTileLight`: the colour and the cell's final `localRandom` (natively a discarded
local), or the native exception; and `Main.rand` after the call in either case — a surface draw made
before a later fault (e.g. a cage lookup in `ApplyTileLight`) persists natively. -/
structure CellLight (σ : Type) where
  result : Except Fault (Vector3 × FastRandom)
  rand : MainRand σ

/-- Lift a pure-fault computation into the `Main.rand`-carrying monad. -/
@[inline] def liftFault {σ α : Type} (e : M α) : ExceptT Fault (StateM (MainRand σ)) α := ExceptT.mk (pure e)

/-- `GetTileLight(int x, int y, out Vector3 outputColor)`.  `_random` itself is not advanced. -/
def getTileLight {σ : Type} (c : Context) (grid : Grid) (scanner : Scanner) (rand : MainRand σ)
    (x y : Int32) : CellLight σ :=
  let body : ExceptT Fault (StateM (MainRand σ)) (Vector3 × FastRandom) := do
    let light := Vector3.zero
    let tile? ← liftFault (grid x y)
    let rnd := scanner.random.withModifierXY x y
    let surface ← liftFault (convI4F64 c.worldSurface)
    let tile ← match tile? with
      | some t => pure t
      | none => throw .nullReference   -- every path dereferences `tile` before any other fault/effect
    let light ← if y <= surface then do
        let (l, r') ← liftFault (applySurfaceLight c grid (← get) tile x y light)
        set r'
        pure l
      else if y > c.underworldLayer then liftFault (applyHellLight c grid tile x y light)
      else pure light
    let (rnd, light) ← liftFault (applyWallLight c tile rnd light)
    let (rnd, light) ← if tile.active then liftFault (applyTileLight c grid tile x y rnd light) else pure (rnd, light)
    return (applyLiquidLight c tile light, rnd)
  let (result, rand) := body.run.run rand
  { result, rand }

end Lean4Terria.OriginalTileLightSources
