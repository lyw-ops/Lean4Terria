import Lean4Terria.OriginalWorld
import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalWorldWeather

/-!
# Original forest clock and critter denominators

Terraria 1.4.5.8 restores the world clock from the version 326 header.
In each tick the SpawnNPC and NPC stages read the clock before UpdateTime.
The ordinary clock advance and the daily critter table are independent
operations. At dusk the clock stage also runs Star.NightSetup and other
event code, which can advance its RNG before it rerolls the critter table.
-/

namespace Lean4Terria.OriginalForestClock

structure Clock where
  time : Float
  dayTime : Bool
  moonPhase : Nat
  deriving BEq, Repr

structure SavedClock where
  clock : Clock
  tenthAnniversary : Bool
  deriving BEq, Repr

private def afterString (source : ByteArray) (start limit : Nat) : Except String Nat := do
  let mut size := 0
  for i in [:5] do
    let offset := start + i
    if offset ≥ limit then throw "Truncated Terraria world header string"
    let byte := source[offset]!.toNat
    size := size + (byte % 128) * (128 ^ i)
    if byte < 128 then
      let stop := offset + 1 + size
      if size > 2147483647 || stop > limit then throw "Invalid Terraria world header string length"
      return stop
  throw "Invalid Terraria world header string length"

private def readLE (source : ByteArray) (offset count : Nat) : UInt64 := Id.run do
  let mut bits := 0
  for i in [:count] do
    bits := bits ||| (source[offset + i]!.toUInt64 <<< (i * 8).toUInt64)
  return bits

private def clockOffset (source : ByteArray) (header : OriginalWorld.Header) :
    Except String (Nat × Nat × Nat) := do
  let offsets := header.sectionOffsets
  let some start := offsets[0]? | throw "Missing Terraria world header pointer"
  let some limit := offsets[1]? | throw "Missing Terraria tile section pointer"
  if start ≥ limit || limit > source.size then throw "Invalid Terraria world header bounds"
  let afterName ← afterString source start limit
  let fixed ← afterString source afterName limit
  -- Generator version, GUID, world ID, bounds and dimensions precede the world flags.
  let flags := fixed + 8 + 16 + 4 + 16 + 8 + 4
  -- Flags, timestamps, moon type, tree/backgrounds, spawn, surface and rock layer.
  let offset := flags + 9 + 16 + 1 + 17 * 4 + 8 + 8 + 8
  if offset + 13 > limit then throw "Truncated Terraria world clock"
  return (offset, flags, limit)

/-- Read the version-326 clock fields that are omitted from the general terrain header. -/
def readSaved (source : ByteArray) (header : OriginalWorld.Header) : Except String SavedClock := do
  let (offset, flags, _) ← clockOffset source header
  let clock := {
    time := Float.ofBits (readLE source offset 8)
    dayTime := source[offset + 8]! != 0
    moonPhase := (readLE source (offset + 9) 4).toNat }
  return { clock, tenthAnniversary := source[flags + 2]! != 0 }

structure SavedWeather where
  cloudBackgroundActive : Int
  clouds : Int
  windTarget : Float32
  deriving BEq, Repr

/-- The version-326 weather fields restored by `WorldFile.LoadWorldFlags`.
The saved wind supplies both target and current speed. The signed cloud count
is the saved population, before any client cloud sprites are created. -/
def readSavedWeather (source : ByteArray) (header : OriginalWorld.Header) : Except String SavedWeather := do
  let (clock, _, limit) ← clockOffset source header
  -- The clock, moon events, dungeon, bosses, invasion, rain, ore and background fields.
  let offset := clock + 109
  if offset + 10 > limit then throw "Truncated Terraria world weather"
  return {
    cloudBackgroundActive := (Int32.ofUInt32 (readLE source offset 4).toUInt32).toInt
    clouds := (Int16.ofUInt16 (readLE source (offset + 4) 2).toUInt16).toInt
    windTarget := Float32.ofBits (readLE source (offset + 6) 4).toUInt32 }


/-- Raw version326 saved weather flags. This is parsing, not a claim to execute
LoadWorld's temp restoration, FixEndlessRainWorlds or local sky activation. -/
structure SavedWorldWeather where
  raining : Bool
  rainTime : Int
  maxRain : Float32
  slimeRainTime : Float
  cloudBackgroundActive : Int
  clouds : Int
  windTarget : Float32
  sandstorm : OriginalWorldWeather.Sandstorm
  deriving Repr

private def requireSavedBytes (offset count limit : Nat) : Except String Unit :=
  if offset > limit || count > limit - offset then throw "Truncated Terraria saved weather" else pure ()

private def afterSavedArray (source : ByteArray) (offset width countBytes limit : Nat) : Except String Nat := do
  requireSavedBytes offset countBytes limit
  let count := if countBytes == 2 then
    (Int16.ofUInt16 (readLE source offset 2).toUInt16).toInt
    else (Int32.ofUInt32 (readLE source offset 4).toUInt32).toInt
  if count < 0 then throw "Invalid Terraria saved weather array count"
  let start := offset + countBytes
  requireSavedBytes start (count.toNat * width) limit
  return start + count.toNat * width

/-- Read rain intensity/duration and sandstorm fields omitted by the general
terrain header. Variable angler, banner and party lists are traversed with
bounds checks; no fixed-offset assumption is made for the sandstorm block. -/
def readSavedWorldWeather (source : ByteArray) (header : OriginalWorld.Header) : Except String SavedWorldWeather := do
  let (clock, _, limit) ← clockOffset source header
  requireSavedBytes clock 123 limit
  let mut offset := clock + 119
  let names := (Int32.ofUInt32 (readLE source offset 4).toUInt32).toInt
  if names < 0 then throw "Invalid Terraria saved weather angler count"
  offset := offset + 4
  if names.toNat > limit - offset then throw "Invalid Terraria saved weather angler count"
  for _ in [:names.toNat] do offset ← afterString source offset limit
  requireSavedBytes offset 16 limit
  offset := offset + 16
  offset ← afterSavedArray source offset 4 2 limit
  offset ← afterSavedArray source offset 2 2 limit
  requireSavedBytes offset 25 limit
  offset := offset + 25
  offset ← afterSavedArray source offset 4 4 limit
  requireSavedBytes offset 13 limit
  return {
    raining := source[clock + 80]! != 0
    rainTime := (Int32.ofUInt32 (readLE source (clock + 81) 4).toUInt32).toInt
    maxRain := Float32.ofBits (readLE source (clock + 85) 4).toUInt32
    slimeRainTime := Float.ofBits (readLE source (clock + 71) 8)
    cloudBackgroundActive := (Int32.ofUInt32 (readLE source (clock + 109) 4).toUInt32).toInt
    clouds := (Int16.ofUInt16 (readLE source (clock + 113) 2).toUInt16).toInt
    windTarget := Float32.ofBits (readLE source (clock + 115) 4).toUInt32
    sandstorm := {
      active := source[offset]! != 0
      timeLeft := (Int32.ofUInt32 (readLE source (offset + 1) 4).toUInt32).toInt
      severity := Float32.ofBits (readLE source (offset + 5) 4).toUInt32
      intendedSeverity := Float32.ofBits (readLE source (offset + 9) 4).toUInt32 } }

inductive Transition where
  | none
  | dusk
  | dawn
  deriving BEq, Repr

/-- The clock part of UpdateTime, with the effective day rate supplied by the
caller. Night begins above 54000 and day begins above 32400. Equality stays
in the same phase. This function leaves event and RNG updates to the caller. -/
def advance (clock : Clock) (dayRate : Nat := 1) : Clock × Transition :=
  let time := clock.time + dayRate.toFloat
  if clock.dayTime && time > 54000 then
    ({ clock with time := 0, dayTime := false }, .dusk)
  else if !clock.dayTime && time > 32400 then
    ({ time := 0, dayTime := true, moonPhase := if clock.moonPhase + 1 ≥ 8 then 0 else clock.moonPhase + 1 }, .dawn)
  else ({ clock with time }, .none)

/-- Denominators used when the spawning code tries a critter branch. 999999
is the original suppression denominator. It remains a valid Next bound. -/
structure CritterChances where
  fireFlyChance : Nat
  fireFlyFriendly : Nat
  fireFlyMultiple : Nat
  stinkBugChance : Nat
  butterflyChance : Nat
  deriving BEq, Repr

/-- Set once by NPC's static constructor. Loading, dusk and dawn do not reroll it. -/
def goldCritterChance : Nat := 400

private def suppressed : Int := 999999

private def rollFireflies (random : UnifiedRandom) (chance friendly multiple : Int × Int) :
    (Int × Int × Int) × UnifiedRandom :=
  let (a, random) := random.next chance.1 chance.2
  let (b, random) := random.next friendly.1 friendly.2
  let (c, random) := random.next multiple.1 multiple.2
  ((a, b, c), random)

/-- NPC.setFireFlyChance. At both loading and dusk, its WorldGen.genRand
getter returns the active Main.rand. Pass that single stream at this method's
entry. Its calls are separate from the persistent SpawnNPC stream. -/
def rollCritters (random : UnifiedRandom) (tenthAnniversary : Bool := false) :
    CritterChances × UnifiedRandom :=
  let (frequent, random) := random.next 0 9
  let ((fireFlyChance, fireFlyFriendly, fireFlyMultiple), random) :=
    if frequent == 0 then rollFireflies random (5, 10) (1, 4) (3, 8)
    else
      let (quiet, random) := random.next 0 3
      if quiet == 0 then ((suppressed, suppressed, suppressed), random)
      else rollFireflies random (10, 60) (2, 15) (6, 30)
  let (kind, random) := random.next 0 3
  let (chance, random) := random.next 1 (if kind == 0 then 14 else 21)
  let (quiet, random) := random.next 0 5
  let chance := if quiet == 0 then suppressed else chance
  let stinkBugChance := if kind == 0 then chance else suppressed
  let butterflyChance := if kind == 0 then suppressed else chance
  let anniversary (value : Int) := if tenthAnniversary then max 1 (value / 2) else value
  ({
    fireFlyChance := (anniversary fireFlyChance).toNat
    fireFlyFriendly := fireFlyFriendly.toNat
    fireFlyMultiple := fireFlyMultiple.toNat
    stinkBugChance := stinkBugChance.toNat
    butterflyChance := (anniversary butterflyChance).toNat }, random)

structure CloudBackground where
  active : Float32
  alpha : Float32
  deriving BEq, Repr

/-- LoadWorldFlags, after reading the saved cloud background integer.
The saved value determines alpha. The new negative timer uses Main.rand. -/
def rollCloudBackground (savedActive : Int) (random : UnifiedRandom) :
    CloudBackground × UnifiedRandom :=
  let (delay, random) := random.next 8640 86400
  (⟨-(Int32.ofInt delay).toFloat32, if savedActive ≥ 1 then 1 else 0⟩, random)

structure LoadedWeather where
  weatherCounter : Nat
  critters : CritterChances
  deriving BEq, Repr

/-- The Main.rand draws in the ordinary LoadWorld tail beginning at
quickSettle = false, with no saved slime rain. Desktop cloud creation
uses Cloud.rand and leaves this stream unchanged. -/
def finishOrdinaryLoad (randomAfterLiquids : UnifiedRandom) (tenthAnniversary : Bool := false) :
    LoadedWeather × UnifiedRandom :=
  let (counter, random) := randomAfterLiquids.next 3600 18000
  let (critters, random) := rollCritters random tenthAnniversary
  (⟨counter.toNat, critters⟩, random)

/-- AmbienceServer.ResetSpawnTime, including its call from the WorldLoaded hook.
Use the active Main.rand at this hook. Local desktop Player.Spawn can make
its own draws before the hook, while the server callback invokes it earlier. -/
def rollAmbienceDelay (random : UnifiedRandom) (tenthAnniversary : Bool := false) :
    Nat × UnifiedRandom :=
  let (delay, random) := random.next 600 7200
  (if tenthAnniversary then delay.toNat / 2 else delay.toNat, random)

/-- Weather statics for the dry forest. The extreme counter decreases only
when the ordinary wind counter expires. -/
structure DryWeather where
  windTarget : Float32
  windCurrent : Float32
  windCounter : Int
  extremeWindCounter : Int := 0
  weatherCounter : Int
  cloudBackground : CloudBackground
  clouds : Int
  pendingClouds : Int
  deriving BEq, Repr

/-- The two ranges used by `ResetWindCounter` both take FastRandom's
non-power-of-two path. The acceptance test is the native signed Int32
overflow check on `bits - remainder + (bound - 1)`. -/
private def nextFastRange (seed : UInt64) (lower upper : Nat) : Int × UInt64 := Id.run do
  let bound := upper - lower
  let mut seed := seed
  repeat
    seed := (seed * 25214903917 + 11) &&& 281474976710655
    let bits := (seed >>> 17).toNat
    let remainder := bits % bound
    if bits - remainder + (bound - 1) < 2147483648 then
      return ((lower + remainder : Nat), seed)
  return (lower, seed)

/-- `ResetWindCounter` creates a fresh FastRandom seeded from the sign-extended
`Guid.NewGuid().GetHashCode()` of this call. No `Main.rand` state enters or leaves it. -/
def resetWindCounters (guidHash : Int32) (resetExtreme : Bool) (extreme : Int := 0) : Int × Int :=
  let (wind, seed) := nextFastRange (Int64.ofInt guidHash.toInt).toUInt64 900 2701
  let extreme := if resetExtreme then (nextFastRange seed 10 31).1 else extreme
  (wind, extreme)

/-- Combine the three load stages. `pendingClouds` is initialized to 200 by
`Main` at process start. A later world load receives the live pending count. -/
def dryWeatherFromLoad (saved : SavedWeather) (background : CloudBackground) (loaded : LoadedWeather)
    (windGuidHash : Int32) (pendingClouds : Int := 200) : DryWeather :=
  let (windCounter, extremeWindCounter) := resetWindCounters windGuidHash true
  { windTarget := saved.windTarget, windCurrent := saved.windTarget
    windCounter, extremeWindCounter, weatherCounter := loaded.weatherCounter
    cloudBackground := background, clouds := saved.clouds, pendingClouds }

structure DryWeatherInputs where
  /-- Whether some active player has `statLifeMax ≥ 120`. -/
  hasLifeCrystal : Bool := false
  /-- One fresh GUID hash, supplied only when `windCounter ≤ 1`. -/
  windGuidHash : Option Int32 := none

inductive OpeningWeatherBoundary where
  | cloudBackground
  | windReroll
  | weatherReroll
  deriving BEq, Repr

private def advanceCloudLayer (background : CloudBackground) (random : UnifiedRandom) :
    CloudBackground × UnifiedRandom :=
  let timer := background.active
  if timer > 0 then
    let timer := max 1 (timer - 1)
    if timer == 1 then
      let (chance, random) := random.next 0 7200
      if chance == 0 then
        let (wait, random) := random.next 14400 345600
        ({ background with active := -wait.toFloat.toFloat32 }, random)
      else ({ background with active := timer }, random)
    else ({ background with active := timer }, random)
  else
    let timer := min 0 (timer + 1)
    if timer == 0 then
      let (chance, random) := random.next 0 43200
      if chance == 0 then
        let (wait, random) := random.next 10800 172800
        ({ background with active := wait.toFloat.toFloat32 }, random)
      else ({ background with active := timer }, random)
    else ({ background with active := timer }, random)

private def boundWind (target : Float32) (hasLifeCrystal : Bool) : Float32 :=
  if hasLifeCrystal then target else min 0.35 (max (-0.35) target)

private def nextWind (weather : DryWeather) (inputs : DryWeatherInputs) (guidHash : Int32)
    (random : UnifiedRandom) : Float32 × Int × Int × UnifiedRandom := Id.run do
  let (small, random) := random.next 0 4
  let (change, random) :=
    if small == 0 then random.next (-25) 26
    else
      let (medium, random) := random.next 0 2
      if medium == 0 then random.next (-50) 51 else random.next (-100) 101
  let mut target := boundWind (weather.windTarget + change.toFloat.toFloat32 * 0.001) inputs.hasLifeCrystal
  let extreme := weather.extremeWindCounter - 1
  let (wind, extreme) := resetWindCounters guidHash (extreme ≤ 0) extreme
  let mut wind := wind
  let mut extreme := extreme
  let mut random := random
  if weather.extremeWindCounter ≤ 1 then
    let (calm, next) := random.next 0 30
    random := next
    if calm < 13 then
      let (still, next) := random.next 0 2
      random := next
      if still == 0 then
        target := 0
        let (interval, next) := random.next 7200 28801
        wind := interval
        random := next
      else
        let (speed, next) := random.next (-200) 201
        target := speed.toFloat.toFloat32 * 0.001
        random := next
    else
      let (medium, next) := random.next 0 20
      let (speed, next) := if medium < 13 then next.next (-400) 401 else next.next (-850) 851
      target := speed.toFloat.toFloat32 * 0.001
      random := next
    target := boundWind target inputs.hasLifeCrystal
    for (threshold, lower, upper) in #[(0.3, 5, 11), (0.5, 10, 21), (0.7, 15, 31)] do
      if target.abs.toFloat > threshold then
        let (extra, next) := random.next lower upper
        extreme := extreme + extra
        random := next
  let (flip, next) := random.next 0 3
  if flip != 0 && ((weather.windTarget < 0 && target > 0) || (weather.windTarget ≥ 0 && target < 0)) then
    target := -target
  return (target, wind, extreme, next)

/-- The dry, rate-one outer weather stage, using the unscoped `Main.rand`
at entry. The live sound, visual cloud and ambience systems own their
separate updates. `windGuidHash` supplies independent entropy at a reset. -/
def advanceDryWeather (weather : DryWeather) (random : UnifiedRandom) (inputs : DryWeatherInputs := {}) :
    Except OpeningWeatherBoundary (DryWeather × UnifiedRandom) := do
  let (cloudBackground, random) := advanceCloudLayer weather.cloudBackground random
  let target := weather.windTarget
  let current := weather.windCurrent
  let windStep := 0.0003 + (target - current).abs * 0.0015
  let windCurrent := if current < target then min target (current + windStep)
    else if current > target then max target (current - windStep) else current
  let (target, windCounter, extremeWindCounter, random) ←
    if weather.windCounter > 1 then pure (target, weather.windCounter - 1, weather.extremeWindCounter, random)
    else match inputs.windGuidHash with
      | some guidHash => pure (nextWind weather inputs guidHash random)
      | none => throw .windReroll
  let windTarget := min 0.8 (max (-0.8) target)
  let mut pending := weather.pendingClouds
  let (drift, random) := random.next 0 60
  let (delta, random) := if drift == 0 then random.next (-1) 2 else (0, random)
  pending := pending + delta
  let (increase, random) := random.next 0 1000
  let (decrease, random) := random.next 0 1300
  let alpha := cloudBackground.alpha
  if (Int32.ofInt increase).toFloat32 < 50 * alpha then pending := pending + 1
  if (Int32.ofInt decrease).toFloat32 < 25 * (1 - alpha) then pending := pending - 1
  -- The two rain-related rolls execute even at zero cloudAlpha.
  let random := ((random.next 0 1000).2.next 0 1000).2
  let mut random := random
  if pending > 66 then
    let (chance, next) := random.next 0 100
    random := next
    if chance == 0 then
      let (count, next) := random.next 1 3
      pending := pending - count
      random := next
  if pending < 50 then
    let (chance, next) := random.next 0 100
    random := next
    if chance == 0 then
      let (count, next) := random.next 1 3
      pending := pending + count
      random := next
  if cloudBackground.active ≤ 0 then pending := min pending 100
  pending := max (-20) pending
  let mut clouds := weather.clouds
  let mut weatherCounter := weather.weatherCounter - 1
  if weatherCounter ≤ 0 then
    let (burst, next) := random.next 0 2
    random := next
    if burst == 0 then
      let (large, next) := random.next 0 2
      let (extra, next) := next.next 0 (if large == 0 then 250 else 100)
      pending := pending + extra
      random := next
    pending := max 0 (min 200 pending)
    clouds := pending
    let (interval, next) := random.next 3600 10800
    weatherCounter := interval
    random := next
  return ({
    windCurrent, windTarget, windCounter, extremeWindCounter, weatherCounter
    cloudBackground, clouds, pendingClouds := pending }, random)

inductive OpeningTimeBoundary where
  | season
  | phaseTransition
  | weather
  | sandstorm
  | travellingMerchant
  | homelessNPC
  deriving BEq, Repr

structure DryTimeInputs where
  windCurrent : Float32
  slimeRainTime : Float
  lifeMax : Nat
  travelTownCount : Nat
  halloween : Bool := false
  christmas : Bool := false
  deriving BEq, Repr

structure DryTimeStep where
  clock : Clock
  slimeRainTime : Float
  random : UnifiedRandom
  duskCritters : Option CritterChances := none
  starfallBoost : Option Float32 := none
  anglerQuest : Option Nat := none

/-- `Star.NightSetup` rolls the drop-rate multiplier before the critter reset. -/
private def rollStarfall (anniversary : Bool) (random : UnifiedRandom) : Float32 × UnifiedRandom :=
  let (high, random) := random.next 0 (if anniversary then 5 else 10)
  if high == 0 then
    let (percent, random) := random.next 300 501
    ((Int32.ofInt percent).toFloat32 * 0.01, random)
  else
    let (low, random) := random.next 0 (if anniversary then 2 else 3)
    if low == 0 then
      let (percent, random) := random.next 100 151
      ((Int32.ofInt percent).toFloat32 * 0.01, random)
    else (1, random)

/-- `AnglerQuestSwap` chooses an index from Main.anglerQuestItemNetIDs.
The two extra IDs follow the consecutive 2450–2488 range. -/
private def rollAnglerQuest (header : OriginalWorld.Header) (random : UnifiedRandom) : Nat × UnifiedRandom := Id.run do
  let progression := header.downedEye || header.downedEvilBoss || header.downedSkeletron ||
    header.hardMode || header.downedSlimeKing || header.downedQueenBee
  let mut random := random
  let mut rejected := true
  let mut index := 0
  while rejected do
    let (candidate, next) := random.next 0 41
    random := next
    index := candidate
    let item := if index < 39 then 2450 + index else 4393 + (index - 39)
    let needsHardMode := #[2454, 2462, 2463, 2465, 2468, 2471, 2473, 2480, 2483, 2484].contains item
    let needsCorruption := #[2454, 2457, 2485].contains item
    let needsCrimson := #[2463, 2477].contains item
    rejected := (needsHardMode && !header.hardMode) || (needsCorruption && header.crimson) ||
      (needsCrimson && !header.crimson) || (#[2453, 2473, 2476].contains item && !progression)
  return (index.toNat, random)

private def advanceDryTimeCore (header : Option OriginalWorld.Header) (clock : Clock) (inputs : DryTimeInputs)
    (random : UnifiedRandom) : Except OpeningTimeBoundary DryTimeStep := do
  if inputs.halloween || inputs.christmas then throw .season
  let (advanced, transition) := advance clock
  if header.isNone && (!clock.dayTime || transition != .none) then throw .phaseTransition
  if inputs.lifeMax ≥ 120 || inputs.slimeRainTime > 0 then throw .weather
  if inputs.windCurrent.abs ≥ 0.6 then throw .sandstorm
  let slimeRainTime := if inputs.slimeRainTime < 0 then min 0 (inputs.slimeRainTime + 1) else 0
  let clock := advanced
  let (severity, random) := random.next 0 18000
  if severity == 0 then throw .sandstorm
  if transition != .none && header.any (fun h => h.hardMode || h.skyblock || h.invasionType != 0 ||
      h.bloodMoon || h.eclipse) then throw .phaseTransition
  if transition == .dusk then
    let some header := header | throw .phaseTransition
    let (starfallBoost, random) := rollStarfall header.tenthAnniversary random
    let (critters, random) := rollCritters random header.tenthAnniversary
    let (meteor, random) := random.next 0 50
    if meteor == 0 && header.downedEvilBoss then throw .phaseTransition
    -- The blood-moon roll precedes the life requirement, even on a new character.
    let random := if clock.moonPhase == 4 then random
      else (random.next 0 (if header.tenthAnniversary then 6 else 9)).2
    return { clock, slimeRainTime, random, duskCritters := some critters, starfallBoost := some starfallBoost }
  if transition == .dawn then
    let some header := header | throw .phaseTransition
    if header.shadowOrbSmashed then throw .phaseTransition
    let (anglerQuest, random) := rollAnglerQuest header random
    return { clock, slimeRainTime, random, anglerQuest := some anglerQuest }
  if !clock.dayTime || clock.time ≥ 27000 then return { clock, slimeRainTime, random }
  let (visit, random) := random.next 0 (27000 * 4)
  if visit == 0 && inputs.travelTownCount ≥ 2 then throw .travellingMerchant
  let (resident, random) := random.next 0 27000
  if resident == 0 then throw .homelessNPC
  return { clock, slimeRainTime, random }

/-- Advance the daytime dry opening without applying world phase changes. -/
def advanceDryTime (clock : Clock) (inputs : DryTimeInputs) (random : UnifiedRandom) :
    Except OpeningTimeBoundary (Clock × Float × UnifiedRandom) := do
  let next ← advanceDryTimeCore none clock inputs random
  return (next.clock, next.slimeRainTime, next.random)

/-- The dry Classic time stage, including the ordinary dusk and dawn hooks.
Unsupported invasion and boss paths return a boundary at their entry. -/
def advanceForestTime (header : OriginalWorld.Header) (clock : Clock) (inputs : DryTimeInputs)
    (random : UnifiedRandom) : Except OpeningTimeBoundary DryTimeStep :=
  advanceDryTimeCore (some header) clock inputs random


/-! The general weather path is deliberately staged at the actual method-call
boundaries in Main.UpdateTime. The old dry opening API above remains for its
existing checked caller. New callers persist this state and execute the named
external hooks between stages; a pending call never discards an advanced RNG. -/

structure WorldTimeState where
  clock : Clock
  rain : OriginalWorldWeather.Rain
  slime : OriginalSlimeRain.State
  sandstorm : OriginalWorldWeather.Sandstorm
  random : UnifiedRandom

inductive WorldTimeStage where
  /-- CultistRitual, BirthdayParty, LanternNight, then this module's sandstorm. -/
  | worldEventHooks
  /-- DD2, credits, mystic logs, pylons, Moon Lord and taxes, then slime warning. -/
  | postSandstormHooks
  /-- Merchant and homeless NPC processing, then phase and town census hooks. -/
  | townChecks
  | phaseEvents
  deriving BEq, Repr

structure WorldTimeStep where
  state : WorldTimeState
  events : Array OriginalWorldWeather.Event := #[]
  next : WorldTimeStage

/-- Exact UpdateTime prefix through clock addition, ending BEFORE CultistRitual.
The rain/slime stages use entry.dayRate; effectiveDayRate is the result of the
caller's intervening UpdateTimeRate. Clock phase remains unchanged here because
StartDay/StartNight run after town checks, not immediately at the threshold. -/
def beginWorldTime (state : WorldTimeState) (entry : OriginalWorldWeather.Context)
    (effectiveDayRate : Int) : WorldTimeStep :=
  let weatherStep := OriginalWorldWeather.advancePrefix state.rain state.slime entry state.random
  let clock := {state.clock with time := state.clock.time + effectiveDayRate.toFloat}
  let state := {state with clock,rain := weatherStep.rain,slime := weatherStep.slime,random := weatherStep.random}
  {state,events := weatherStep.events,next := .worldEventHooks}


/-- Invoke after Cultist/Birthday/Lantern hooks. Context must contain their live
results and the effective rate. This does not silently run or omit those hooks. -/
def advanceWorldTimeSandstorm (state : WorldTimeState) (live : OriginalWorldWeather.Context) : WorldTimeStep :=
  let sand := OriginalWorldWeather.advanceSandstorm state.sandstorm live state.random
  {state := {state with sandstorm := sand.state,random := sand.random}
   events := sand.events,next := .postSandstormHooks}

/-- Invoke after the remaining event/pylon/Moon Lord/tax hooks. The warning
clock is one invocation, independent of dayRate, and skipped on netMode1. -/
def advanceWorldTimeWarning (state : WorldTimeState) (netMode : Int) : WorldTimeStep :=
  let (slime,events) := if netMode == 1 then (state.slime,#[])
    else OriginalSlimeRain.updateWarning state.slime
  {state := {state with slime},events := events.map OriginalWorldWeather.Event.slime,next := .townChecks}

structure TownTimeInputs where
  netMode : Int := 0
  dayRate : Int := 1
  fastForwarding : Bool := false
  worldUpdateRate : Int := 1
  travelNPC : Bool := false
  /-- Active townNPCs excluding types37 and453, as scanned by the original. -/
  eligibleTownCount : Nat := 0
  /-- Actual live homeless actors, used only to avoid an empty unspawn scan. -/
  homelessActorPresent : Bool := false
  deriving Repr

inductive TownTimeCall where
  | spawnTravelNPC
  | unspawnTravelNPC
  | spawnHomelessNPC
  | unspawnHomelessNPC
  deriving BEq, Repr

inductive TownTimeContinuation where
  | afterTravel
  | afterHomeless
  deriving BEq, Repr

structure PendingTownTime where
  call : TownTimeCall
  continuation : TownTimeContinuation
  deriving BEq, Repr

structure TownTimeStep where
  state : WorldTimeState
  travelNPC : Bool
  pending : Option PendingTownTime := none
  next : WorldTimeStage := .phaseEvents

private def advanceHomelessTime (state : WorldTimeState) (inputs : TownTimeInputs) : TownTimeStep :=
  if !state.clock.dayTime || !(state.clock.time ≤ 48600) then
    {state,travelNPC := false,pending := if inputs.homelessActorPresent then
      some {call := .unspawnHomelessNPC,continuation := .afterHomeless} else none
     next := if inputs.homelessActorPresent then .townChecks else .phaseEvents}
  else if state.clock.dayTime && state.clock.time < 27000 && !inputs.fastForwarding && inputs.worldUpdateRate > 0 then
    let (spawn,random) := state.random.next 0 (27000.0 / (max 1 inputs.dayRate).toFloat).toInt32.toInt
    {state := {state with random},travelNPC := false,pending := if spawn == 0 then
      some {call := .spawnHomelessNPC,continuation := .afterHomeless} else none
     next := if spawn == 0 then .townChecks else .phaseEvents}
  else {state,travelNPC := false}

/-- Resume only AFTER the caller executed the requested original-equivalent NPC
callee, passing its resulting state/RNG. No callee is represented by false or a
skipped random draw. NPC.travelNPC is cleared after the travel branch in CIL. -/
def resumeTownTime (stateAfterCallee : WorldTimeState) (inputs : TownTimeInputs)
    (continuation : TownTimeContinuation) : TownTimeStep :=
  match continuation with
  | .afterTravel => advanceHomelessTime stateAfterCallee inputs
  | .afterHomeless => {state := stateAfterCallee,travelNPC := false}

/-- Original travel/homeless scheduling. Rare successful samples return a
concrete pending callee with all preceding state and randomness retained.
Phase events and town census are still the next caller-owned stage. -/
def advanceTownTime (state : WorldTimeState) (inputs : TownTimeInputs) : TownTimeStep :=
  if inputs.netMode == 1 then {state,travelNPC := inputs.travelNPC} else
  if inputs.travelNPC then
    if !state.clock.dayTime || !(state.clock.time ≤ 48600) then
      {state,travelNPC := true,pending := some {call := .unspawnTravelNPC,continuation := .afterTravel},next := .townChecks}
    else advanceHomelessTime state inputs
  else if !inputs.fastForwarding && state.clock.dayTime && state.clock.time < 27000 then
    let denominator := (27000.0 / (max 1 inputs.dayRate).toFloat).toInt32.toInt * 4
    let (visit,random) := state.random.next 0 denominator
    let state := {state with random}
    if visit == 0 && inputs.eligibleTownCount ≥ 2 then
      {state,travelNPC := false,pending := some {call := .spawnTravelNPC,continuation := .afterTravel},next := .townChecks}
    else advanceHomelessTime state inputs
  else advanceHomelessTime state inputs

end Lean4Terria.OriginalForestClock
