import Lean4Terria.OriginalOuterWeather
import Lean4Terria.OriginalCollision

/-! Desktop1.4.5.8 rain/atmosphere stages upstream of OuterWeather, and the
separate draw-clock rain lifetime. Native evidence is private under
`.lake/original-rain-atmosphere/`. Concrete NewDust and Lighting inputs remain
explicit boundaries; this module does not certify whole-game completion. -/
namespace Lean4Terria.OriginalRainAtmosphere
open OriginalCollision (Vector Grid)

private def i32 (v : Int) : Int := (Int32.ofInt v).toInt
private def f32 (v : Int) : Float32 := (Int32.ofInt v).toFloat32

structure MusicState where
  windy : Bool := false
  storm : Bool := false
  deriving BEq, Repr

structure MusicContext where
  cloudAlpha : Float32 := 0
  windTarget : Float32 := 0
  time : Float := 13500
  dayTime : Bool := true
  remixWorld : Bool := false
  minWind : Float32 := 0.34
  maxWind : Float32 := 0.4
  minRain : Float32 := 0.4
  maxRain : Float32 := 0.5
  deriving Repr

/-- Complete UpdateWindyDayState. Both music flags have hysteresis; the middle
band preserves the previous value. The remix wet return preserves windy. -/
def updateWindyDayState (state : MusicState) (c : MusicContext) : MusicState := Id.run do
  let mut state := state
  if c.cloudAlpha == 0 then
    state := {state with storm := false}
    if c.time < 10800 || c.time > 43200 || !c.dayTime then return {state with windy := false}
    if c.windTarget.abs < c.minWind then state := {state with windy := false}
    if c.windTarget.abs ≥ c.maxWind then state := {state with windy := true}
    return state
  if c.cloudAlpha < c.minRain || c.windTarget.abs < c.minWind then
    state := {state with storm := false}
  else if c.cloudAlpha ≥ c.maxRain && c.windTarget.abs ≥ c.maxWind then
    state := {state with storm := true}
  if c.remixWorld then return {state with storm := false}
  return {state with windy := false}

/-- FocusHelper.AllowRain forwards GameplayActive. PlayWhenUnfocused is NOT a
rain permission; it is used by other helpers such as background thunder. -/
def allowRain (selectedApplication gamePaused : Bool) : Bool := selectedApplication && !gamePaused

structure Drop where
  position : Vector := ⟨0,0⟩
  velocity : Vector := ⟨0,0⟩
  scale : Float32 := 0
  rotation : Float32 := 0
  alpha : Int := 0
  active : Bool := false
  type : UInt8 := 0
  deriving Repr

structure Pool where
  maxRain : Nat := 750
  drops : Array Drop := Array.replicate 751 {}
  deriving Repr

structure RainContext where
  netMode : Int := 0
  gamePaused : Bool := false
  gameMenu : Bool := false
  zoneRain : Bool := false
  shimmerAlpha : Float32 := 0
  cloudAlpha : Float32 := 0
  gfxQuality : Float32 := 1
  atmosphere : Float32 := 1
  snowTiles : Int := 0
  snowThreshold : Int := 1500
  waterStyle : Int := 0
  windCurrent : Float32 := 0
  screenPosition : Vector := ⟨0,0⟩
  screenWidth : Int := 1920
  screenHeight : Int := 1080
  maxWorldViewX : Int := 1920
  playerPosition : Vector := ⟨0,0⟩
  playerVelocity : Vector := ⟨0,0⟩
  remixWorld : Bool := false
  anyDanger : Bool := false
  deriving Repr

inductive CallKind where
  | newRain (position velocity : Vector)
  deriving Repr

structure Call where
  kind : CallKind
  random : UnifiedRandom

structure CreateResult where
  pool : Pool
  slot : Nat
  random : UnifiedRandom

structure MakeResult where
  pool : Pool
  random : UnifiedRandom
  calls : Array Call := #[]

private def checkPool (pool : Pool) : Except String Unit :=
  if pool.drops.size < pool.maxRain then throw "Rain pool is shorter than Main.maxRain" else pure ()

/-- NewRain's effective allocation limit. Its dead player-height computation is
not used by the original. Snow attenuation and Math.Pow(atmo,9) are live. -/
def allocationLimit (pool : Pool) (c : RainContext) (forced : Bool) : Int := Id.run do
  let mut limit := if forced then (pool.maxRain : Int) else
    min (pool.maxRain : Int) (f32 pool.maxRain * c.cloudAlpha).toInt32.toInt
  let quality := (1 + c.gfxQuality) / 2
  if (if forced then quality < 0.9 else quality.toFloat < 0.9) then
    limit := (f32 limit * quality).toInt32.toInt
  if forced then return limit
  let snow := min (1 : Float32) (max 0 (f32 c.snowTiles / f32 c.snowThreshold))
  let snow := snow * snow
  limit := (f32 limit * (1 - snow)).toInt32.toInt
  limit := (limit.toFloat * Float.pow c.atmosphere.toFloat 9).toInt32.toInt
  if c.atmosphere.toFloat < 0.4 then limit := 0
  return limit

/-- Complete NewRain/NewRainForced. A full/disabled pool returns maxRain without
any draw. Reused slots retain alpha; the original never resets that field. -/
def newRain (pool : Pool) (c : RainContext) (position velocity : Vector) (forced : Bool)
    (random : UnifiedRandom) : Except String CreateResult := do
  checkPool pool
  let limit := allocationLimit pool c forced
  let some slot := (pool.drops.extract 0 limit.toNat).findIdx? (!·.active) |
    return {pool,slot := pool.maxRain,random}
  let old := pool.drops[slot]?.getD {}
  let (scale,random) := random.next (-20) 21
  let scale : Float32 := 1 + f32 scale * 0.01
  let velocity : Vector := ⟨velocity.x * scale,velocity.y * scale⟩
  let rotation := (Float.atan2 velocity.x.toFloat (-velocity.y).toFloat).toFloat32
  let (kind,random) := random.next 0 3
  let drop := {old with active := true,position,velocity,scale,rotation,type := (Int32.ofInt (i32 (i32 (c.waterStyle * 3) + kind))).toUInt32.toUInt8}
  return {pool := {pool with drops := pool.drops.set! slot drop},slot,random}

/-- Complete ClearRain; only active is cleared, preserving inactive slot fields. -/
def clearRain (pool : Pool) : Except String Pool := do
  checkPool pool
  return {pool with drops := pool.drops.mapIdx fun i d => if i < pool.maxRain then {d with active := false} else d}

def fallVelocity (windCurrent : Float32) : Vector := ⟨windCurrent * 18,14⟩

private def solidTile (grid : Grid) (x y : Int) : Bool :=
  if x < 0 || y < 0 || x ≥ grid.width || y ≥ grid.height then false else
  let tile := grid.tileAt x.toNat y.toNat
  let rules := (tile.id.map grid.rules).getD {}
  let slope := match tile.shape with | .slope id => id != 0 | _ => false
  tile.id.isSome && !tile.actuated && rules.solid && !rules.solidTop && tile.shape != .halfBlock && !slope

/-- Complete MakeRain, including the full-pool and blocked-roof random schedule.
It is the caller's responsibility to apply the separate AllowRain/alpha gate. -/
def makeRain (pool : Pool) (c : RainContext) (grid : Grid) (random : UnifiedRandom) : Except String MakeResult := do
  checkPool pool
  if c.netMode == 2 || c.gamePaused || c.gameMenu || !c.zoneRain || c.shimmerAlpha > 0 then return {pool,random}
  let mut budget := f32 c.screenWidth / f32 c.maxWorldViewX * 25
  budget := budget * (0.25 + 1 * c.cloudAlpha)
  if c.anyDanger then
    let original := budget
    budget := budget * 0.05
    if original > 1 && budget < 1 then budget := 1
  let mut pool := pool
  let mut random := random
  let mut calls := #[]
  let mut index : Int := 0
  while f32 index < budget do
    let margin := if c.playerVelocity.y < 0 then
      i32 (600 + (c.playerVelocity.y.abs * 30).toInt32.toInt) else 600
    let (x,next) := random.next (i32 (c.screenPosition.x.toInt32.toInt - margin))
      (i32 (i32 (c.screenPosition.x.toInt32.toInt + c.screenWidth) + margin))
    let (y,next) := next.next 20 100
    random := next
    let x := f32 x - c.windCurrent * 15 * 40
    let x := x + c.playerVelocity.x * 40
    let x := if x < 0 then 0 else x
    let edge := f32 (i32 (((grid.width : Int) - 1) * 16))
    let x := if x > edge then edge else x
    let position : Vector := ⟨x,c.screenPosition.y - f32 y⟩
    let tx := max 0 (min ((grid.width : Int) - 1) (position.x.toInt32.toInt.tdiv 16))
    let ty := max 0 (min ((grid.height : Int) - 1) (position.y.toInt32.toInt.tdiv 16))
    let blocked := !c.remixWorld && !c.gameMenu &&
      (solidTile grid tx ty || (grid.tileAt tx.toNat ty.toNat).wall > 0)
    if !blocked then
      let velocity := fallVelocity c.windCurrent
      calls := calls.push {kind := .newRain position velocity,random}
      let next ← newRain pool c position velocity false random
      pool := next.pool
      random := next.random
    index := i32 (index + 1)
  return {pool,random,calls}

structure WetFlags where
  honey : Bool := false
  shimmer : Bool := false
  deriving BEq, Repr

private def clampInt (v lo hi : Int) : Int := max lo (min hi v)

/-- SolidCollision's active, unactuated, solid/non-platform tile rectangles. -/
private def solidCollision (grid : Grid) (pos : Vector) : Bool := Id.run do
  let x0 := clampInt ((pos.x / 16).toInt32.toInt - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (((pos.x + 2) / 16).toInt32.toInt + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt ((pos.y / 16).toInt32.toInt - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (((pos.y + 2) / 16).toInt32.toInt + 2) 0 ((grid.height : Int) - 40)
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      let rules := (tile.id.map grid.rules).getD {}
      if tile.actuated || tile.id.isNone || !rules.solid || rules.solidTop then continue
      let left := f32 (x*16)
      let half := tile.shape == .halfBlock
      let top := f32 (y*16) + (if half then 8 else 0)
      let height : Float32 := if half then 8 else 16
      if pos.x + 2 > left && pos.x < left + 16 && pos.y + 2 > top && pos.y < top + height then return true
  return false

private def liquidAmount (tile : OriginalWorld.Tile) : Nat := (tile.liquid.map (·.amount.toNat)).getD 0

/-- WetCollision(position,2,2) has a centred 2x1 probe. Its honey/shimmer
statics are cleared even when no liquid hits; caller persists those effects. -/
private def wetCollision (grid : Grid) (pos : Vector) : Bool × WetFlags := Id.run do
  let box : Vector := ⟨pos.x,pos.y + 1⟩
  let x0 := clampInt ((pos.x / 16).toInt32.toInt - 1) 0 ((grid.width : Int) - 1)
  let x1 := clampInt (((pos.x + 2) / 16).toInt32.toInt + 2) 0 ((grid.width : Int) - 1)
  let y0 := clampInt ((pos.y / 16).toInt32.toInt - 1) 0 ((grid.height : Int) - 40)
  let y1 := clampInt (((pos.y + 2) / 16).toInt32.toInt + 2) 0 ((grid.height : Int) - 40)
  let overlaps (left top : Float32) (height : Int) :=
    box.x + 2 > left && box.x < left + 16 && box.y + 1 > top && box.y < top + f32 height
  for x in [x0.toNat:x1.toNat] do
    for y in [y0.toNat:y1.toNat] do
      let tile := grid.tileAt x y
      let amount := liquidAmount tile
      if amount > 0 then
        let depth := f32 (256 - amount : Nat) / 32
        let top := f32 (y*16) + depth*2
        let height := 16 - (depth*2).toInt32.toInt
        if overlaps (f32 (x*16)) top height then
          let kind := tile.liquid.map (·.kind)
          return (true,{honey := kind == some .honey,shimmer := kind == some .shimmer})
      else
        let slope := match tile.shape with | .slope id => id != 0 | _ => false
        if tile.id.isSome && slope && y > 0 then
          let above := grid.tileAt x (y-1)
          if liquidAmount above > 0 && overlaps (f32 (x*16)) (f32 (y*16)) 16 then
            let kind := above.liquid.map (·.kind)
            return (true,{honey := kind == some .honey,shimmer := kind == some .shimmer})
  return (false,{})

def waterDustType (waterStyle : Int) : Nat :=
  match waterStyle with
  | 2 => 98 | 3 => 99 | 4 => 100 | 5 => 101 | 6 => 102 | 7 => 103
  | 8 => 104 | 9 => 105 | 10 => 123 | 12 => 288 | _ => 33

structure SplashRequest where
  position : Vector
  width : Nat := 2
  height : Nat := 2
  typeId : Nat
  speed : Vector := ⟨0,0⟩
  alpha : Int := 0
  color : UInt32 := 0
  scale : Float32 := 1
  rainVelocity : Vector
  deriving Repr

structure SplashPost where
  position : Vector
  velocity : Vector
  alpha : Int := 38
  scale : Float32 := 0.6
  noGravity : Bool := true
  deriving Repr

/-- Apply after the REAL Dust.NewDust return, including its maxDust sentinel.
Other dust fields and active are preserved by the caller. -/
def finishSplashDust (request : SplashRequest) (position velocity : Vector) : SplashPost :=
  let vx := velocity.x * 0.1 + (-request.rainVelocity.x) * 0.025
  let vy := velocity.y * 0.1 + (-request.rainVelocity.y) * 0.025
  {position := ⟨position.x - 2,position.y + 2⟩,velocity := ⟨vx,vy-2⟩}

structure UpdateResult where
  drop : Drop
  random : UnifiedRandom
  wetFlags : WetFlags
  pendingSplash : Option SplashRequest := none

/-- Complete rain-owned Update instructions up to a real NewDust request.
Paused/menu/remix paths have their source gates; ordinary death still consumes
Next100 before deciding splash. Pending means the native callee is unfinished. -/
def updateDrop (drop : Drop) (c : RainContext) (grid : Grid) (wetFlags : WetFlags)
    (random : UnifiedRandom) : UpdateResult := Id.run do
  if c.gamePaused then return {drop,random,wetFlags}
  let drop := {drop with position := drop.position + drop.velocity}
  if c.gameMenu then
    return {drop := if drop.position.y > c.screenPosition.y + f32 c.screenHeight + 2000 then
      {drop with active := false} else drop,random,wetFlags}
  if c.remixWorld then
    return {drop := if drop.position.y > c.screenPosition.y + f32 c.screenHeight + 100 then
      {drop with active := false} else drop,random,wetFlags}
  let mut dead := solidCollision grid drop.position
  let mut wetFlags := wetFlags
  if !dead && drop.position.y > c.screenPosition.y + f32 c.screenHeight + 100 then dead := true
  if !dead then
    let (wet,flags) := wetCollision grid drop.position
    dead := wet
    wetFlags := flags
  if !dead then return {drop,random,wetFlags}
  let drop := {drop with active := false}
  let (chance,random) := random.next 0 100
  let pendingSplash := if f32 chance < c.gfxQuality * 100 then some {
    position := ⟨drop.position.x - drop.velocity.x,drop.position.y - drop.velocity.y⟩
    typeId := waterDustType c.waterStyle,rainVelocity := drop.velocity} else none
  return {drop,random,wetFlags,pendingSplash}

structure Sprite where
  position : Vector
  frameX : Int
  frameY : Int := 0
  width : Int := 2
  height : Int := 40
  /-- Native FNA Color.PackedValue: R | G<<8 | B<<16 | A<<24. -/
  color : UInt32
  rotation : Float32
  origin : Vector := ⟨0,0⟩
  scale : Float32
  effects : Int := 0
  depth : Float32 := 0
  deriving Repr

private def multiplyColor (color : UInt32) (factor : Float32) : UInt32 := Id.run do
  let mut out : UInt32 := 0
  for shift in #[0,8,16,24] do
    let channel := (color >>> shift) &&& 255
    let value := (channel.toFloat32 * factor).toInt32.toInt
    let value := (max 0 (min 255 value)).toNat.toUInt32
    out := out ||| (value <<< shift)
  return out

inductive DrawContinuation where
  | afterSplash (nextSlot : Nat)
  deriving BEq, Repr

structure DrawPending where
  splash : SplashRequest
  continuation : DrawContinuation

structure SpriteCall where
  sprite : Sprite
  random : UnifiedRandom

structure DrawResult where
  pool : Pool
  random : UnifiedRandom
  wetFlags : WetFlags
  sprites : Array Sprite := #[]
  calls : Array SpriteCall := #[]
  pending : Option DrawPending := none

/-- Main.DrawRain in source slot order: sprite first, then rain Update when
AllowRain. The colour query is the caller's real Lighting.GetColor. -/
def drawFrom (pool : Pool) (c : RainContext) (grid : Grid) (selected : Bool)
    (lightAt : Int → Int → UInt32) (wetFlags : WetFlags) (random : UnifiedRandom)
    (startSlot : Nat := 0) : Except String DrawResult := do
  checkPool pool
  let mut pool := pool
  let mut random := random
  let mut wetFlags := wetFlags
  let mut sprites := #[]
  let mut calls := #[]
  for slot in [startSlot:pool.maxRain] do
    let drop := pool.drops[slot]?.getD {}
    if !drop.active then continue
    let tx := (drop.position.x + 4).toInt32.toInt / 16
    let ty := (drop.position.y + 4).toInt32.toInt / 16
    let color := multiplyColor (lightAt tx ty) 0.85
    let color := if c.shimmerAlpha > 0 then multiplyColor color (1-c.shimmerAlpha) else color
    let sprite : Sprite := {
      position := ⟨drop.position.x - c.screenPosition.x,drop.position.y - c.screenPosition.y⟩
      frameX := drop.type.toNat * 4,color,rotation := drop.rotation,scale := drop.scale}
    sprites := sprites.push sprite
    calls := calls.push {sprite,random}
    if allowRain selected c.gamePaused then
      let updated := updateDrop drop c grid wetFlags random
      pool := {pool with drops := pool.drops.set! slot updated.drop}
      random := updated.random
      wetFlags := updated.wetFlags
      if let some splash := updated.pendingSplash then
        return {pool,random,wetFlags,sprites,calls,pending := some {splash,continuation := .afterSplash (slot+1)}}
  return {pool,random,wetFlags,sprites,calls}

/-- Resume only after NewDust and finishSplashDust were actually applied by
its owner; use the resulting live random and collision/world context. -/
def resumeDraw (pool : Pool) (c : RainContext) (grid : Grid) (selected : Bool)
    (lightAt : Int → Int → UInt32) (wetFlags : WetFlags) (random : UnifiedRandom)
    (continuation : DrawContinuation) : Except String DrawResult :=
  match continuation with
  | .afterSplash next => drawFrom pool c grid selected lightAt wetFlags random next

/-- Scalar state slice of DoDraw IL0516..05c9, not a claim that the complete GPU
DoDraw method has run. This advances on draw calls, even when dayRate is zero. -/
def drawCloudAlpha (alpha target : Float32) (raining menu : Bool) (dayRate : Int) : Float32 :=
  let step := 0.0005 * f32 (max 1 dayRate)
  let step := if menu then step * 20 else step
  if raining then
    if alpha > target then
      let next := alpha - step
      if next < target then target else next
    else if alpha < target then
      let next := alpha + step
      if next > target then target else next
    else alpha
  else
    let next := alpha - step
    if next < 0 then 0 else next

/-- Scalar slice of DrawSurfaceBG IL01ae..0255. Caller provides the result of
ShouldDrawSurfaceBackground and actual background/surface eligibility. -/
def drawCloudBackgroundAlpha (alpha active : Float32) (dayRate : Int)
    (drunkWorldGen drawSurface backgroundsEnabled : Bool) (screenY : Float32) (worldSurface : Float) : Float32 :=
  if drunkWorldGen || !drawSurface || !((screenY / 16).toFloat ≤ worldSurface + 10) || !backgroundsEnabled then alpha else
  if active > 0 then
    let next := alpha + 0.0005 * f32 dayRate
    if next > 1 then 1 else next
  else
    let next := alpha - 0.0005 * f32 dayRate
    if next < 0 then 0 else next

end Lean4Terria.OriginalRainAtmosphere
