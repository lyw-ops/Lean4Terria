namespace Lean4Terria

/-- Classic single-player weather at the normal day rate. Cloud activity is
the signed timer used by Main.updateCloudLayer. -/
structure Weather where
  day : Nat := 0
  rainTicks : Nat := 0
  rainStrength : Nat := 0
  cloudActivity : Float32 := 0
  cloudOpacity : Float32 := 0
  deriving Repr, Inhabited

instance : BEq Weather where
  beq a b := a.day == b.day && a.rainTicks == b.rainTicks && a.rainStrength == b.rainStrength &&
    a.cloudActivity.toBits == b.cloudActivity.toBits && a.cloudOpacity.toBits == b.cloudOpacity.toBits

def Weather.moonPhase (weather : Weather) : Nat := weather.day % 8
def Weather.raining (weather : Weather) : Bool := weather.rainTicks > 0
def Weather.overcast (weather : Weather) : Bool := weather.cloudOpacity > 0

def initialWeather (seed : UInt64) : Weather :=
  let clearTicks := 8640 + (hash (seed, (3868 : Nat))).toNat % 77760
  { cloudActivity := -(Int32.ofNat clearTicks).toFloat32 }

private def single (n : Nat) : Float32 := (Int32.ofNat n).toFloat32
private def natural (n : Float32) : Nat := n.toUInt32.toNat

/-- The duration rolls in Main.StartRain, expressed with bounded samples. -/
def rainDuration (roll : Nat → Nat) : Nat :=
  let increments := #[3600, 7200, 7200, 10800, 14400, 18000]
  let duration := (List.range increments.size).foldl (fun ticks index =>
    if roll (1 + index * 2) % (index + 3) == 0 then
      ticks + roll (2 + index * 2) % increments[index]!
    else ticks) (28800 + roll 0 % 57600)
  let scale : Float32 := (List.range 4).foldl (fun factor index =>
    if roll (13 + index) % (index + 2) == 0 then factor + (#[0.05, 0.1, 0.15, 0.2] : Array Float32)[index]!
    else factor) 1
  natural (single duration * scale)

private def sampleRainStrength (overcast : Bool) (roll : Nat → Nat) : Nat :=
  let lighter := roll 17 % 3 == 0
  let minimum := if overcast then if lighter then 20 else 40 else 5
  let maximum := if overcast then 90 else if lighter then 40 else 30
  minimum + roll 18 % (maximum - minimum + 1)

/-- Advance the weather once per world tick. The world supplies independent
samples and the compact surface-background visibility check. -/
def Weather.advance (weather : Weather) (dawn canStartRain surfaceVisible : Bool)
    (roll : Nat → Nat) : Weather := Id.run do
  let mut weather := if dawn then { weather with day := weather.day + 1 } else weather
  let factor := max 1 (1 + 4 * (single weather.rainStrength * 0.01))
  let activity := weather.cloudActivity
  let cloudActivity :=
    if activity > 0 then
      let next := max 1 (activity - 1 / factor)
      if next == 1 && roll 19 % natural (7200 * factor) == 0 then
        -(single (14400 + roll 20 % 331200))
      else next
    else
      let next := if activity < 0 then
        min 0 (activity + factor + if weather.raining then 2 * factor else 0)
        else 0
      if next == 0 && roll 21 % natural (43200 / factor) == 0 then
        single (10800 + roll 22 % 162000)
      else next
  let opacity := if !surfaceVisible then weather.cloudOpacity
    else if cloudActivity > 0 then min 1 (weather.cloudOpacity + 0.0005)
    else max 0 (weather.cloudOpacity - 0.0005)
  weather := { weather with cloudActivity, cloudOpacity := opacity }
  if weather.raining then
    let left := weather.rainTicks - 1
    if left == 0 then return { weather with rainTicks := 0, rainStrength := 0 }
    let strength := if roll 23 % 7200 == 0 then sampleRainStrength (cloudActivity ≥ 1) roll else weather.rainStrength
    return { weather with rainTicks := left, rainStrength := strength }
  if canStartRain && (roll 24 % 496800 == 0 || (cloudActivity ≥ 1 && roll 25 % 367200 == 0)) then
    return { weather with rainTicks := rainDuration roll, rainStrength := sampleRainStrength (cloudActivity ≥ 1) roll }
  return weather

end Lean4Terria
