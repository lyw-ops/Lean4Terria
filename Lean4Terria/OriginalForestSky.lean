import Lean4Terria.OriginalCollision
import Lean4Terria.OriginalForestAmbience

/-!
Terraria 1.4.5.8 ambient BirdsV, AirBalloon and Butterflies. The client constructs
each entity from the seed in its sky request. The renderer consumes the
parameters of `SkyEntity.CommonDraw`.
-/

namespace Lean4Terria.OriginalForestSky

open OriginalCollision (Vector single)

inductive Kind where
  | airBalloon
  | butterflies
  | birdsV
  deriving BEq, Repr

def Kind.ofTypeId : UInt8 → Option Kind
  | 0 => some .birdsV
  | 3 => some .airBalloon
  | 8 => some .butterflies
  | _ => none

inductive Atlas where
  | airBalloonsLarge
  | airBalloonsSmall
  | butterflySwarm1
  | butterflySwarm2
  | birdsVShape
  deriving BEq, Repr

def Atlas.name : Atlas → String
  | .airBalloonsLarge => "AirBalloons_Large"
  | .airBalloonsSmall => "AirBalloons_Small"
  | .butterflySwarm1 => "ButterflySwarm1"
  | .butterflySwarm2 => "ButterflySwarm2"
  | .birdsVShape => "BirdsVShape"

def Atlas.path (atlas : Atlas) : String := "Images/Backgrounds/Ambience/" ++ atlas.name

def Atlas.size : Atlas → Nat × Nat
  | .airBalloonsLarge => (76, 460)
  | .airBalloonsSmall => (16, 110)
  | .butterflySwarm1 => (40, 646)
  | .butterflySwarm2 => (64, 988)
  | .birdsVShape => (108, 280)

def Atlas.rows : Atlas → Nat
  | .airBalloonsLarge | .airBalloonsSmall => 5
  | .butterflySwarm1 => 17
  | .butterflySwarm2 => 19
  | .birdsVShape => 4

structure Rect where
  x : Nat
  y : Nat
  width : Nat
  height : Nat
  deriving BEq, Repr

/-- `SpriteFrame` defaults to two pixels of padding on axes with several cells. -/
def Atlas.source (atlas : Atlas) (row : Nat) : Rect :=
  let (width, height) := atlas.size
  let stride := height / atlas.rows
  ⟨0, row * stride, width, stride - 2⟩

private def nextBits (seed : UInt64) (bits : UInt64) : Nat × UInt64 :=
  let seed := (seed * 25214903917 + 11) &&& 281474976710655
  ((seed >>> (48 - bits)).toNat, seed)

/-- The power-of-two and signed overflow rejection branches in FastRandom.Next. -/
private def nextInt (seed : UInt64) (bound : Nat) : Nat × UInt64 := Id.run do
  let mut seed := seed
  repeat
    let (bits, next) := nextBits seed 31
    seed := next
    if (bound &&& (bound - 1)) == 0 then return (bound * bits / 2147483648, seed)
    let value := bits % bound
    if bits - value + (bound - 1) < 2147483648 then return (value, seed)
  return (0, seed)

private def nextFloat (seed : UInt64) : Float32 × UInt64 :=
  let (bits, next) := nextBits seed 24
  (single bits * 5.960464477539063e-8, next)

private def scaleVec (v : Vector) (scale : Float32) : Vector := ⟨v.x * scale, v.y * scale⟩
private def subtract (a b : Vector) : Vector := ⟨a.x - b.x, a.y - b.y⟩

structure Camera where
  unscaledPosition : Vector
  size : Vector
  deriving BEq, Repr

def Camera.center (camera : Camera) : Vector :=
  camera.unscaledPosition + scaleVec camera.size 0.5

structure SpawnView where
  playerCenter : Vector
  camera : Camera
  worldSurface : Float
  visualWind : Float32
  /-- `VirtualCamera` uses Main.maxScreenW/maxScreenH, independent of the actual camera size. -/
  virtualSize : Vector := ⟨1920, 1200⟩

structure Entity where
  kind : Kind
  position : Vector
  depth : Float32
  atlas : Atlas
  flipX : Bool
  row : Nat := 0
  life : Nat
  velocity : Vector := ⟨0, 0⟩
  opacity : Float32 := 0
  startFrame : Int := -1
  deriving BEq, Repr

/-- The original constructor's state and the final independent FastRandom seed.
The entity itself performs no further random draws during update or draw. -/
def construct (kind : Kind) (seed : Int) (view : SpawnView) : Entity × UInt64 :=
  let rng := (Int64.ofInt (Int32.ofInt seed).toInt).toUInt64
  match kind with
  | .airBalloon =>
    let (flip, rng) := nextInt rng 2
    let (offset, rng) := nextFloat rng
    let (height, rng) := nextInt rng 31
    let (depth, rng) := nextFloat rng
    let (asset, rng) := nextInt rng 2
    let (row, rng) := nextInt rng 5
    let (life, rng) := nextInt rng 31
    let tileX := view.playerCenter.x.toInt32.toInt / 16
    let x := (single tileX + 100 * (offset * 2 - 1)) * 16
    let y := view.worldSurface.toFloat32 * 16 - single (height + 50 : Nat) * 16
    ({ kind, position := ⟨x, y⟩, depth := depth * 3 + 3, flipX := flip != 0,
       atlas := if asset == 0 then .airBalloonsLarge else .airBalloonsSmall,
       row, life := (life + 20) * 60 }, rng)
  | .butterflies | .birdsV =>
    let (height, rng) := nextFloat rng
    let (depth, rng) := nextFloat rng
    let (atlas, rng) :=
      if kind == .birdsV then (.birdsVShape, rng) else
        let (asset, rng) := nextInt rng 2
        (if asset == 0 then .butterflySwarm1 else .butterflySwarm2, rng)
    let (life, rng) := nextInt rng 61
    let flipX := view.visualWind ≤ 0
    let left := view.playerCenter.x - view.virtualSize.x * 0.5
    let x := if flipX then left + view.virtualSize.x + 100 else left - 100
    let surface := view.worldSurface.toFloat32 * 16
    let y := if kind == .birdsV then height * (surface - 1600 - 2400) + 2400
      else height * (surface - 4000) + 4000
    let depth := depth * 3 + 3
    let center := view.camera.center
    let position := center + scaleVec (subtract ⟨x, y⟩ center) (depth / 3)
    ({ kind, position, depth, flipX, atlas, life := (life + 60) * 60 }, rng)

structure Motion where
  worldSurface : Float
  visualWind : Float32
  dayTime : Bool := true
  raining : Bool := false
  eclipse : Bool := false

private def fading (entity : Entity) : Float32 × Float32 :=
  match entity.kind with
  | .airBalloon => (0.05, 0.95)
  | .butterflies | .birdsV => (0.15, 0.85)

private def lerpValue (a b value : Float32) : Float32 :=
  if a < b then
    if value < a then 0 else if value > b then 1 else (value - a) / (b - a)
  else
    if value < b then 1 else if value > a then 0 else (value - a) / (b - a)

private def updateVelocity (entity : Entity) (motion : Motion) : Vector := Id.run do
  let wind := motion.visualWind
  if entity.kind == .airBalloon then
    let mut climb := 3 + wind.abs
    let y := entity.position.y.toFloat
    if y < motion.worldSurface * 12 then climb := climb * 0.5
    if y < motion.worldSurface * 8 then climb := climb * 0.5
    if y < motion.worldSurface * 4 then climb := climb * 0.5
    return ⟨wind * 4, -climb⟩
  let speed := if entity.kind == .birdsV then 3 + wind.abs * 0.8 else 0.1 + wind.abs * 0.05
  return ⟨if entity.flipX then -speed else speed, 0⟩

/-- One ambient frame. Weather starts an accelerated fade after this frame's
normal opacity and movement. The parent removes the entity at its lifetime. -/
def Entity.update (entity : Entity) (motion : Motion) (frame : Int) : Option Entity :=
  let start := if entity.startFrame == -1 then frame else entity.startFrame
  let age := frame - start
  if age ≥ entity.life then none else
    let life := single entity.life
    let (fadeIn, fadeOut) := fading entity
    let cutoff := life * fadeOut
    let elapsed := single age
    let opacity := if elapsed ≥ cutoff then lerpValue life cutoff elapsed
      else lerpValue 0 (life * fadeIn) elapsed
    let frameRate := if entity.kind == .airBalloon then 2147483647 else 5
    let row := if frame % frameRate == 0 then (entity.row + 1) % entity.atlas.rows else entity.row
    let velocity := updateVelocity entity motion
    let startFrame := if motion.raining || !motion.dayTime || motion.eclipse then
        min start (frame - cutoff.toInt32.toInt)
      else start
    some { entity with startFrame, row, opacity, velocity, position := entity.position + velocity }

structure Sky where
  frameCounter : Int := 0
  private slots : Array (Option Entity) := #[]
  private freeSlots : List Nat := []
  active : Bool := false
  deriving BEq, Repr, Inhabited

def Sky.entities (sky : Sky) : Array Entity := sky.slots.filterMap id

/-- Supported packet kinds are activated unless a Moon Lord sky
currently conflicts. In an inactive sky, retained entities are suspended. -/
def Sky.spawn (sky : Sky) (request : OriginalForestAmbience.SkySpawn) (view : SpawnView)
    (conflict : Bool := false) : Option Sky := do
  let kind ← Kind.ofTypeId request.kind
  let (entity, _) := construct kind request.seed view
  let sky := { sky with active := sky.active || !conflict }
  -- AmbientSky keeps a 500-entry SlotVector. Its free list is last removed first.
  return match sky.freeSlots with
  | slot :: rest => { sky with slots := sky.slots.set! slot (some entity), freeSlots := rest }
  | [] => if sky.slots.size < 500 then { sky with slots := sky.slots.push (some entity) } else sky

structure Tick where
  motion : Motion
  dayRate : Nat := 1
  paused : Bool := false
  conflict : Bool := false

/-- `SkyManager.Update` runs the active sky at least once per tick. The first
conflicting update still advances its entities and removes the sky from the
manager's active list. A compatible spawn can activate that sky again. -/
def Sky.update (sky : Sky) (tick : Tick) : Sky := Id.run do
  if !sky.active || tick.paused then return sky
  let mut sky := sky
  for _ in [:max 1 tick.dayRate] do
    if !sky.active then break
    let frame := sky.frameCounter + 1
    let mut slots := sky.slots
    let mut freeSlots := sky.freeSlots
    for index in [:slots.size] do
      if let some entity := slots[index]! then
        let next := entity.update tick.motion frame
        slots := slots.set! index next
        if next.isNone then freeSlots := index :: freeSlots
    sky := { frameCounter := frame, slots, freeSlots, active := !tick.conflict && slots.any Option.isSome }
  return sky

structure Rgba where
  r : Nat
  g : Nat
  b : Nat
  a : Nat
  deriving BEq, Repr

private def Rgba.map (color : Rgba) (channel : Nat → Nat) : Rgba :=
  ⟨channel color.r, channel color.g, channel color.b, channel color.a⟩

private def byte (value : Float32) : Nat :=
  min 255 value.toInt32.toInt.toNat

structure DrawView where
  camera : Camera
  color : Rgba
  atmosphere : Float32 := 1
  oceanScreenY : Float32 := 0
  oceanOpacity : Float32 := 0
  minDepth : Float32
  maxDepth : Float32
  gameMenu : Bool := false

structure Sprite where
  atlas : Atlas
  position : Vector
  source : Rect
  color : Rgba
  origin : Vector
  scale : Float32
  flipX : Bool
  rotation : Float32 := 0
  layerDepth : Float32 := 0
  deriving BEq, Repr

/-- The actual SpriteBatch parameters, before the screen-space draw transform. -/
def Entity.draw (entity : Entity) (view : DrawView) : Option Sprite := do
  if entity.depth ≤ view.minDepth || entity.depth > view.maxDepth then none else
    let center := view.camera.center
    let delta := subtract entity.position center
    let parallax := ⟨delta.x * (1 / entity.depth), delta.y * (0.9 / entity.depth)⟩
    let position := subtract (parallax + center) view.camera.unscaledPosition
    let source := entity.atlas.source entity.row
    let origin := ⟨single source.width / 2, single source.height / 2⟩
    let tinted := view.color.map fun c => byte (single c + (255 - single c) * 0.2)
    let oceanFade := 1 - lerpValue (view.oceanScreenY - 10) (view.oceanScreenY - 2)
      (position.y + single (source.height / 2 : Nat)) * view.oceanOpacity
    let color := (tinted.map fun c => byte (single c * entity.opacity))
    let color := (color.map fun c => byte (single c * oceanFade)).map fun c => byte (single c * view.atmosphere)
    return { atlas := entity.atlas, position, source, color, origin, scale := 3 / entity.depth, flipX := entity.flipX }

/-- In single-player, the ambient draw pass clears the sky when returning to
the game menu. Drawing a hidden sky emits no sprites. -/
def Sky.draw (sky : Sky) (view : DrawView) : Sky × Array Sprite :=
  if !sky.active then (sky, #[])
  else if view.gameMenu then ({ frameCounter := sky.frameCounter }, #[])
  else (sky, sky.entities.filterMap (·.draw view))

end Lean4Terria.OriginalForestSky
