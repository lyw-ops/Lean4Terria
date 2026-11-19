import Lean4Terria.OriginalLiquid.Core
import Lean4Terria.OriginalLiquid.Load
import Lean4Terria.OriginalLiquid.Frame
import Lean4Terria.OriginalLiquid.Kill
import Lean4Terria.OriginalLiquid.Tree
import Lean4Terria.OriginalLiquid.Piles
import Lean4Terria.OriginalLiquid.Plants
import Lean4Terria.OriginalLiquid.Objects

/-!
# Original liquids (Terraria 1.4.5.8)

Vanilla liquid update on the real `.wld` map, hand-ported from `TerrariaServer.exe` 1.4.5.8:

* `OriginalLiquid.Core` — queue, buffer, `Update`, `DelWater`, `UpdateLiquid`, contact, lily pads,
  over a sparse overlay of the run-length `OriginalWorld.Terrain`;
* `OriginalLiquid.Load` — `WorldFile.LoadWorld`'s liquid pass (`QuickWater`, `WaterCheck`, settling);
* `OriginalLiquid.Kill` — vanilla `KillTile` random calls and drops for liquid-killed single-cell tiles;
* `OriginalLiquid.Tree` — trees killed by lava: drops and the `CheckTree` cascade;
* `OriginalLiquid.Piles` — `Check3x2` for large piles reached by liquid-driven framing;
* `OriginalLiquid.Plants` — `PlantCheck` and `CheckVines` after liquid changes a plant's ground;
* `OriginalLiquid.Objects` — tiles destroyed by liquid (runtime death tables, single-cell `KillTile`);
* `OriginalLiquid.Frame` — per-frame schedule (`skipCount`, `Main.DoUpdate` statics, `UpdateLiquid`
  stream) and the liquid side of mining, placing and buckets.

Verification: `.lake/original-liquid/integration/REPORT.md`.
-/
