/-! The ordinary color-lighting engine's scene-metrics clock, Terraria 1.4.5.8.

This models the control state of real `LightingEngine.ProcessArea`, not light
propagation. Each actual `Lighting.LightTiles` call reaches ProcessArea once.
There may be zero, one, or several calls between player updates. See the native
continuous evidence in `.lake/original-scene-metrics-clock/REPORT.md`. -/
namespace Lean4Terria.OriginalSceneMetricsClock

inductive Phase where
  | minimap
  | metrics
  | scan
  | blurPresent
  deriving BEq, Repr, Inhabited

def Phase.next : Phase → Phase
  | .minimap => .metrics
  | .metrics => .scan
  | .scan => .blurPresent
  | .blurPresent => .minimap

def Phase.index : Phase → Nat
  | .minimap => 0
  | .metrics => 1
  | .scan => 2
  | .blurPresent => 3

structure State where
  phase : Phase := .minimap
  /-- Main.mapDelay, carried here only because ProcessArea changes it. -/
  mapDelay : Int := 0
  /-- Main.renderCount, not the engine's internal phase. -/
  renderCount : Int := 0
  deriving BEq, Repr, Inhabited

structure Step where
  state : State
  exportToMinimap : Bool := false
  updateSceneMetrics : Bool := false
  processScan : Bool := false
  processBlurAndPresent : Bool := false
  deriving BEq, Repr, Inhabited

/-- One successful ordinary ProcessArea call (IL_0006..008f).
`updateSceneMetrics` requests the real Main method: its capture early return
and SceneMetrics.Scan's same-tick/center cache remain the caller's responsibility.
All four cases advance the phase, including when a metric scan is cached. -/
def State.processArea (s : State) : Step :=
  let advanced := { s with phase := s.phase.next }
  match s.phase with
  | .minimap =>
    { state := { advanced with
        mapDelay := if s.mapDelay > 0 then s.mapDelay - 1 else s.mapDelay
        renderCount := 3 }
      exportToMinimap := s.mapDelay ≤ 0 }
  | .metrics => { state := { advanced with renderCount := 0 }, updateSceneMetrics := true }
  | .scan => { state := { advanced with renderCount := 1 }, processScan := true }
  | .blurPresent => { state := { advanced with renderCount := 2 }, processBlurAndPresent := true }

/-- Clear erases light buffers and per-frame lights, but preserves `_state`.
Do not reset the clock for Main.BlackFadeCameraTeleport's Lighting.Clear. -/
def State.clear (s : State) : State := s

/-- Rebuild/Lighting.Initialize resets the internal phase to MinimapUpdate.
It does not write Main.mapDelay or Main.renderCount. -/
def State.rebuild (s : State) : State := { s with phase := .minimap }

end Lean4Terria.OriginalSceneMetricsClock
