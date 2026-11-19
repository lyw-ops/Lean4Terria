import Lean4Terria.OriginalForestClock

/-!
Terraria 1.4.5.8 single-player ambience. The loader and the timed server
update use the outer Main.rand. The spawn request is the original
NetAmbienceModule payload, before client-side sky entity construction.
-/

namespace Lean4Terria.OriginalForestAmbience

structure RabbitOrderRolls where
  order : Int
  delay : Int
  deriving BEq, Repr

/-- `Player.Spawn → RabbitOrderFrameHelper.Reset → Update` in animation state zero. -/
def rollRabbitOrder (random : UnifiedRandom) : RabbitOrderRolls × UnifiedRandom :=
  let (order, random) := random.next 1 4
  let (delay, random) := random.next 180 3600
  (⟨order, delay⟩, random)

structure State where
  updatesUntilNextAttempt : Int
  deriving BEq, Repr

/-- The ambience constructor called by the local player's world-loaded hook. -/
def worldLoaded (random : UnifiedRandom) (tenthAnniversary : Bool := false) : State × UnifiedRandom :=
  let (delay, random) := OriginalForestClock.rollAmbienceDelay random tenthAnniversary
  (⟨delay⟩, random)

structure Startup where
  rabbit : RabbitOrderRolls
  ambience : State
  random : UnifiedRandom

/-- Desktop order, starting at the rabbit frame reset and continuing through the
world-loaded ambience constructor with the same generator. -/
def desktopStart (random : UnifiedRandom) (tenthAnniversary : Bool := false) : Startup :=
  let (rabbit, random) := rollRabbitOrder random
  let (ambience, random) := worldLoaded random tenthAnniversary
  ⟨rabbit, ambience, random⟩

structure World where
  worldSurface : Float
  underworldLayer : Int
  time : Float
  dayTime : Bool
  dayRate : Nat := 1
  cloudAlpha : Float32 := 0
  windTarget : Float32 := 0
  windyDayMusic : Bool := false
  butterflyChance : Nat := 999999
  tenthAnniversary : Bool := false
  eclipse : Bool := false
  hardMode : Bool := false
  bloodMoon : Bool := false
  pumpkinMoon : Bool := false
  snowMoon : Bool := false

structure Player where
  slot : Nat
  positionY : Float32
  active : Bool := true
  desert : Bool := false
  hallow : Bool := false
  beach : Bool := false
  jungle : Bool := false
  corrupt : Bool := false
  crimson : Bool := false
  deriving Inhabited, Repr

/-- Regular `UpdateWindyDayState` in a Classic world, before ambience and weather.
Between 0.34 and 0.4 the prior music flag remains in effect. -/
def windyDayMusic (clock : OriginalForestClock.Clock) (cloudAlpha windTarget : Float32)
    (previous : Bool) : Bool :=
  if cloudAlpha != 0 || !clock.dayTime || clock.time < 10800 || clock.time > 43200 then false
  else windTarget.abs ≥ 0.4 || (windTarget.abs ≥ 0.34 && previous)

private def canSeeSky (world : World) (player : Player) : Bool :=
  player.positionY.toFloat ≤ world.worldSurface * 16 + 1600

private def canSeeHell (world : World) (player : Player) : Bool :=
  player.positionY ≥ (Int32.ofInt ((world.underworldLayer - 100) * 16)).toFloat32

/-- Spawn conditions in the constructor's original dictionary insertion order.
`none` marks a general entity. A biome-specific entry also appears in the
fallback selection when its biome matches. -/
private def candidates (world : World) (player : Player) : Array (UInt8 × Bool × Option Bool) :=
  let rain := world.cloudAlpha > 0
  let sunny := !rain && world.dayTime && !world.eclipse
  let sunset := world.dayTime && world.time > 40500
  let calm := !rain && !world.dayTime && !world.bloodMoon && !world.pumpkinMoon && !world.snowMoon
  let windy := world.windyDayMusic
  #[
    (0, sunny, none),                     -- BirdsV
    (1, sunny && world.hardMode, none),   -- Wyvern
    (2, sunny && windy, none),            -- Airship
    (3, sunny && !windy, none),           -- AirBalloon
    (4, !world.dayTime, none),            -- Eyeball
    (8, sunny && !windy && world.windTarget.abs < 0.4 && world.butterflyChance < 6, none),
    (9, world.dayTime && !world.eclipse && windy, none),
    (10, sunny, some player.desert),      -- Vulture
    (7, (sunset && sunny) || calm, some player.jungle),
    (11, sunny || calm, some player.hallow),
    (12, sunny, some player.beach),
    (13, sunny && windy, none),
    (14, calm, some player.hallow),
    (15, sunny, some player.hallow),
    (16, sunny || calm, some player.corrupt),
    (17, sunny || calm, some player.crimson),
    (18, true, none)                      -- Hellbats
  ]

/-- Payload of a timed `NetAmbienceModule.SerializeSkyEntitySpawn`.
The client uses `seed` when constructing the selected sky entity. -/
structure SkySpawn where
  playerSlot : Nat
  kind : UInt8
  seed : Int
  deriving BEq, Repr

structure Step where
  state : State
  random : UnifiedRandom
  spawn : Option SkySpawn := none

/-- Run the timed part of `AmbienceServer.Update` before cloud and weather
updates. Reaching zero by subtraction schedules the attempt for the next call.
The debug-only forced queue is handled separately by the host. -/
def step (state : State) (world : World) (players : Array Player) (random : UnifiedRandom) : Step := Id.run do
  if state.updatesUntilNextAttempt > 0 then
    return { state := ⟨state.updatesUntilNextAttempt - world.dayRate⟩, random }
  let (state, random) := worldLoaded random world.tenthAnniversary
  let viewers := (players.filter fun p => p.active && (canSeeSky world p || canSeeHell world p)).qsort
    (·.slot < ·.slot)
  if viewers.isEmpty then return { state, random }
  let (playerIndex, random) := random.next 0 viewers.size
  let player := viewers[playerIndex.toNat]!
  let available := (candidates world player).filter fun (kind, enabled, biome) =>
    enabled && (if kind == 18 then canSeeHell world player else canSeeSky world player) && biome.getD true
  let preferred := (available.filter fun (_, _, biome) => biome.isSome).map (·.1)
  let (prefer, random) := if preferred.isEmpty then (false, random)
    else
      let (chance, random) := random.next 0 5
      (chance < 3, random)
  let pool := if prefer then preferred else available.map (·.1)
  if pool.isEmpty then return { state, random }
  let (kindIndex, random) := random.next 0 pool.size
  let (seed, random) := random.nextInt
  return { state, random, spawn := some ⟨player.slot, pool[kindIndex.toNat]!, seed⟩ }

end Lean4Terria.OriginalForestAmbience
