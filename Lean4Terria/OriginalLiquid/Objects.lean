import Lean4Terria.OriginalLiquid.Tree
import Lean4Terria.OriginalLiquid.Piles
import Lean4Terria.OriginalLiquid.Plants

/-!
# Tiles destroyed by liquid (`TileObjectData.CheckWaterDeath` / `CheckLavaDeath`)

`Liquid.AddWater` (outside loading) and `WorldGen.WaterCheck` kill an active tile when
`CheckLavaDeath` (lava) or `CheckWaterDeath` (water, honey, shimmer) holds: the tile's
`TileObjectData` for its style, or `Main.tileWaterDeath` / `tileLavaDeath` when it has none or uses the
global checks. The tables below were read from the initialized 1.4.5.8 runtime for every type and
styles 0–31 (`.lake/original-liquid/integration/out/death_tables.json`). Types whose answer depends on
the style are resolved for platforms (19, style = frameY / 18), torches (4, frameY / 22) and herbs
(82, frameX / 18);
the rest are reported.

`killTile` runs `Kill.killSingleCell` (vanilla `KillTile` random calls, drops, clearing, framing) for
the single-cell tiles and `Tree.killTree` for trees (with `Tree.frameTree` as the `TileFrame` dispatch);
objects spanning several cells, vines and cacti are reported.
-/

namespace Lean4Terria.OriginalLiquid.Objects

open Lean4Terria.OriginalLiquid

private def flags (ids : Array Nat) : Array Bool :=
  ids.foldl (fun t i => t.set! i true) (Array.replicate 754 false)

/-- types every style of which dies in water / honey / shimmer -/
def waterDeathAll : Array Bool := flags
  #[51, 93, 98, 552, 697]

/-- types every style of which dies in lava -/
def lavaDeathAll : Array Bool := flags
  #[3, 5, 12, 13, 16, 17, 20, 24, 27, 28, 29, 32, 34, 35, 36, 42, 49, 50, 51, 52, 55, 61, 62, 69,
    71, 72, 73, 74, 80, 81, 83, 84, 86, 91, 92, 94, 95, 96, 97, 98, 102, 103, 106, 110, 113, 115, 125, 126,
    128, 173, 174, 184, 201, 205, 209, 210, 212, 213, 215, 216, 217, 218, 219, 220, 227, 228, 233, 236, 238, 240, 241, 242,
    243, 244, 245, 246, 247, 254, 269, 270, 271, 275, 276, 277, 278, 279, 280, 281, 282, 283, 285, 286, 287, 288, 289, 290,
    291, 292, 293, 294, 295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 316, 317, 318, 319,
    323, 324, 335, 338, 339, 352, 353, 358, 359, 360, 361, 362, 363, 364, 372, 376, 377, 382, 386, 387, 388, 389, 390, 391,
    392, 393, 394, 395, 405, 406, 412, 413, 414, 425, 427, 435, 436, 437, 438, 439, 440, 444, 452, 453, 454, 455, 457, 462,
    463, 464, 465, 466, 484, 485, 490, 493, 494, 499, 505, 506, 510, 511, 520, 521, 522, 523, 524, 525, 526, 527, 528, 529,
    530, 531, 532, 533, 538, 542, 543, 544, 547, 550, 551, 552, 553, 554, 555, 556, 558, 559, 564, 565, 567, 568, 569, 570,
    571, 572, 573, 579, 580, 581, 582, 591, 592, 593, 594, 598, 599, 600, 601, 602, 603, 604, 605, 606, 607, 608, 609, 610,
    611, 612, 619, 620, 621, 622, 623, 624, 629, 630, 631, 632, 636, 639, 640, 642, 643, 644, 645, 646, 647, 648, 649, 650,
    651, 652, 653, 654, 655, 656, 660, 665, 668, 693, 694, 696, 697, 698, 699, 700, 701, 702, 703, 706, 707, 710, 713, 714,
    715, 716, 733, 751, 752, 753]

/-- `Main.tileFrameImportant` -/
def frameImportant : Array Bool := flags
  #[3, 4, 5, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 24, 26, 27, 28, 29, 31, 33, 34, 35,
    36, 42, 49, 50, 55, 61, 71, 72, 73, 74, 77, 78, 79, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
    92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 110, 113, 114, 125, 126, 128, 129, 132, 133,
    134, 135, 136, 137, 138, 139, 141, 142, 143, 144, 149, 165, 171, 172, 173, 174, 178, 184, 185, 186, 187, 201, 207, 209,
    210, 212, 215, 216, 217, 218, 219, 220, 227, 228, 231, 233, 235, 236, 237, 238, 239, 240, 241, 242, 243, 244, 245, 246,
    247, 254, 269, 270, 271, 275, 276, 277, 278, 279, 280, 281, 282, 283, 285, 286, 287, 288, 289, 290, 291, 292, 293, 294,
    295, 296, 297, 298, 299, 300, 301, 302, 303, 304, 305, 306, 307, 308, 309, 310, 314, 316, 317, 318, 319, 320, 323, 324,
    334, 335, 337, 338, 339, 349, 354, 355, 356, 358, 359, 360, 361, 362, 363, 364, 372, 373, 374, 375, 376, 377, 378, 380,
    386, 387, 388, 389, 390, 391, 392, 393, 394, 395, 405, 406, 410, 411, 412, 413, 414, 419, 420, 423, 424, 425, 427, 428,
    429, 435, 436, 437, 438, 439, 440, 441, 442, 443, 444, 445, 452, 453, 454, 455, 456, 457, 461, 462, 463, 464, 465, 466,
    467, 468, 469, 470, 471, 475, 476, 480, 484, 485, 486, 487, 488, 489, 490, 491, 493, 494, 497, 499, 505, 506, 509, 510,
    511, 518, 519, 520, 521, 522, 523, 524, 525, 526, 527, 529, 530, 531, 532, 533, 538, 542, 543, 544, 545, 547, 548, 549,
    550, 551, 552, 553, 554, 555, 556, 558, 559, 560, 564, 565, 567, 568, 569, 570, 571, 572, 573, 579, 580, 581, 582, 583,
    584, 585, 586, 587, 588, 589, 590, 591, 592, 593, 594, 595, 596, 597, 598, 599, 600, 601, 602, 603, 604, 605, 606, 607,
    608, 609, 610, 611, 612, 613, 614, 615, 616, 617, 619, 620, 621, 622, 623, 624, 629, 630, 631, 632, 634, 637, 639, 640,
    642, 643, 644, 645, 646, 647, 648, 649, 650, 651, 652, 653, 654, 656, 657, 658, 660, 663, 664, 665, 693, 694, 695, 696,
    698, 699, 700, 701, 702, 703, 704, 705, 706, 707, 709, 710, 711, 712, 713, 714, 715, 716, 720, 721, 723, 724, 725, 726,
    733, 751, 752, 753]

/-- style-dependent types: styles (0–31) that die -/
def waterDeathStyles : List (Nat × List Nat) := [(4, [0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (215, [0, 2, 3, 5, 6, 7, 8, 10, 11, 12, 13, 14, 15, 16, 18, 19, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31])]
def lavaDeathStyles : List (Nat × List Nat) := [(4, [0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (10, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (11, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (14, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (15, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (18, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (19, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (33, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31]), (79, [0, 1, 2, 3, 4, 5, 6, 7, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (82, [0, 1, 2, 3, 4, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (87, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (89, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (90, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31]), (93, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 24, 25, 26, 27, 28, 29, 30, 31]), (100, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 26, 27, 28, 29, 30, 31]), (101, [0, 1, 2, 3, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (104, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (172, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (469, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (497, [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31]), (548, [0, 1, 2, 3, 4, 5, 6, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31])]

def styleOf (c : Cell) : Option Nat :=
  match c.type, c.frame with
  | 19, some (_, fy) => some (fy / 18).toNat
  | 4, some (_, fy) => some (fy / 22).toNat
  | 82, some (fx, _) => some (fx / 18).toNat
  | _, _ => none

/-- `CheckLavaDeath` for lava, `CheckWaterDeath` otherwise; `none` when the style rule is not ported. -/
def liquidKills (c : Cell) : Option Bool :=
  if !c.active then some false else
  let lava := c.kind == 1
  let all := if lava then lavaDeathAll else waterDeathAll
  let styles := if lava then lavaDeathStyles else waterDeathStyles
  match styles.lookup c.type with
  | none => some (all[c.type]?.getD false)
  | some dying => (styleOf c).map (dying.contains ·)

def killTile (hk : Hooks) (env : Kill.Env) (x y : Int) : M Unit := do
  let c ← getCell x y
  if Kill.singleCell.contains c.type then Kill.killSingleCell hk env x y
  else if c.type == 5 then Tree.killTree hk env x y
  else
    note s!"KillTile({x},{y}) type {c.type} destroyed by liquid: multi-cell object / cascade not ported"

end Lean4Terria.OriginalLiquid.Objects

namespace Lean4Terria.OriginalLiquid

/-- The ported part of `TileFrame` after `AddWater`. Frame-important tiles go to `TileFrameImportant`
(here: `CheckTree` 5, `Check3x2` 186 / 187, `PlantCheck`) and stop; other tiles pass `CheckVines`
(vines) and reach `TileFrameCosmetic` — the return value. -/
def frameDispatch (hk : Hooks) (env : Kill.Env) (x y : Int) : M Bool := do
  let c ← getCell x y
  let t := c.type
  if Objects.frameImportant[t]?.getD false then
    if t == 5 then Tree.frameTree hk env x y
    else if t == 186 || t == 187 then Piles.checkPile3x2 x y
    else if Plants.plantTypes.contains t then Plants.plantCheck hk env x y
    return false
  if Plants.vineTypes.contains t then return (← Plants.checkVines hk env x y)
  if t == 80 then note s!"TileFrame({x},{y}): CheckCactus not ported"
  return true

/-- Hooks with the liquid-death tables and single-cell `KillTile`; a kill nested deeper than `depth`
(a kill whose framing kills again) is reported instead of run. -/
def objectHooksUpTo (env : Kill.Env) : Nat → Hooks
  | 0 => { liquidKills := Objects.liquidKills }
  | n + 1 => { liquidKills := Objects.liquidKills, killTile := fun x y => Objects.killTile (objectHooksUpTo env n) env x y,
               frameTile := fun x y => frameDispatch (objectHooksUpTo env n) env x y }

/-- `env` describes the nearest player and live critters (`Kill.Env`, default: none). -/
def objectHooks (env : Kill.Env := {}) : Hooks := objectHooksUpTo env 8

end Lean4Terria.OriginalLiquid
