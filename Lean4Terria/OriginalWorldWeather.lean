import Lean4Terria.OriginalSlimeRain

/-! Desktop1.4.5.8 rainfall, coin-rain and sandstorm state transitions. These
are the actual UpdateTime weather stages, not the outer cloud/wind/ambience
update. Inputs describe the live world/players and creative/lantern flags;
outputs preserve random ownership and request external UI/sky/network effects.
Native method and continuous replay receipts: `.lake/original-world-time-weather/`. -/
namespace Lean4Terria.OriginalWorldWeather

structure Player where
  active : Bool := true
  lifeMax : Int := 100
  defense : Int := 0
  luck : Float32 := 0
  deriving Repr

structure Rain where
  active : Bool := false
  time : Int := 0
  strength : Float32 := 0
  oldStrength : Float32 := 0
  coinRain : Int := 0
  cloudBackgroundActive : Float32 := 0
  cloudAlpha : Float32 := 0
  clouds : Int := 0
  ladyBugBoost : Int := 0
  deriving Repr

structure Sandstorm where
  active : Bool := false
  timeLeft : Int := 0
  severity : Float32 := 0
  intendedSeverity : Float32 := 0
  deriving Repr

structure Context where
  netMode : Int := 0
  gameMenu : Bool := false
  dayRate : Int := 1
  dayTime : Bool := true
  time : Float := 13500
  windCurrent : Float32 := 0
  hardMode : Bool := false
  expertMode : Bool := false
  remixWorld : Bool := false
  hasWorldSurface : Bool := true
  tenthAnniversary : Bool := false
  goodWorld : Bool := false
  lowTiles : Bool := false
  downedSlimeKing : Bool := false
  /-- Blood/eclipse/moon/invasion/DD2 flags. Live slime time is checked below,
  after this tick's timer update, just as NPC.BusyWithAnyInvasionOfSorts does. -/
  busyInvasion : Bool := false
  freezeRain : Bool := false
  lanternsUp : Bool := false
  nextNightIsLanternNight : Bool := false
  worldWidth : Int := 4200
  myPlayer : Int := 0
  players : Array Player := #[]
  deriving Repr

inductive Event where
  | rainStarted
  | rainStopped
  | rainChanged
  | coinRainStarted
  | syncWorld
  | sandstormStarted
  | sandstormStopped
  | severityChanged
  | slime (event : OriginalSlimeRain.Event)
  deriving BEq, Repr

structure RainResult where
  state : Rain
  random : UnifiedRandom
  events : Array Event := #[]

structure SandstormResult where
  state : Sandstorm
  random : UnifiedRandom
  events : Array Event := #[]

private def i32 (value : Int) : Int := (Int32.ofInt value).toInt
private def single (value : Int) : Float32 := (Int32.ofInt value).toFloat32
private def nextFloat (random : UnifiedRandom) : Float32 × UnifiedRandom :=
  let (sample,random) := random.sample
  (sample.toFloat32,random)

/-- GetPlayerWithHighestLuck scans active slots0..<255 and keeps the first tie.
No active player means the original fresh Player's default luck0. -/
def highestLuck (players : Array Player) : Float32 := Id.run do
  let mut best : Option Float32 := none
  for player in players.extract 0 255 do
    if player.active then
      best := match best with
        | none => some player.luck
        | some luck => some (if player.luck > luck then player.luck else luck)
  return best.getD 0

/-- GameContent.Luck.RollLuck, including its nested range choice. -/
def rollLuck (luck : Float32) (range : Int) (random : UnifiedRandom) : Int × UnifiedRandom :=
  let (biased,random) := if luck > 0 then
      let (chance,random) := nextFloat random
      (chance < luck,random)
    else if luck < 0 then
      let (chance,random) := nextFloat random
      (chance < -luck,random)
    else (false,random)
  if biased then
    let (bound,random) := if luck > 0 then random.next (range.tdiv 2) range
      else random.next range (i32 (range * 2))
    random.next 0 bound
  else random.next 0 range

def rainingForever (state : Rain) : Bool := state.time ≥ 5184000

def changeRain (state : Rain) (maxOut : Bool) (strengthOverride : Option Float32)
    (random : UnifiedRandom) : RainResult :=
  let (strength,random) := match strengthOverride with
    | some strength => (strength,random)
    | none =>
      let (choice,random) := random.next 0 3
      let (lower,upper) := if state.cloudBackgroundActive ≥ 1 || state.clouds > 150 then
          (if choice == 0 then (20,91) else (40,91))
        else if state.clouds > 100 then
          (if choice == 0 then (10,71) else (20,61))
        else (if choice == 0 then (5,41) else (5,31))
      let (strength,random) := random.next lower upper
      (single strength * 0.01,random)
  let strength := if rainingForever state then max strength 0.1 else strength
  {state := {state with strength,cloudAlpha := if maxOut then strength else state.cloudAlpha}
   random,events := #[.rainChanged]}

/-- StartRain still consumes every duration draw when an existing forever timer
prevents the final time assignment. Coin luck is evaluated before forceCoin OR. -/
def startRain (state : Rain) (context : Context) (maxOut : Bool)
    (strengthOverride : Option Float32) (forceCoin : Bool) (random : UnifiedRandom) : RainResult := Id.run do
  let mut state := state
  let mut random := random
  let mut events := #[]
  if !context.remixWorld && context.hasWorldSurface && !state.active then
    let bound := if context.tenthAnniversary && !context.goodWorld then 5 else 25
    let (roll,next) := rollLuck (highestLuck context.players) bound random
    random := next
    if roll == 0 || forceCoin then
      let (amount,next) := random.next 75 151
      random := next
      let coins := i32 (i32 (amount * 100) * 100)
      state := {state with coinRain := (single coins * (single context.worldWidth / 4200)).toInt32.toInt}
      events := events.push .coinRainStarted
  let (duration,next) := random.next 28800 86400
  let mut duration := duration
  random := next
  for (bound,extra) in #[(3,3600),(4,7200),(5,7200),(6,10800),(7,14400),(8,18000)] do
    let (choice,next) := random.next 0 bound
    random := next
    if choice == 0 then
      let (extra,next) := random.next 0 extra
      duration := i32 (duration + extra)
      random := next
  let mut multiplier : Float32 := 1
  for (bound,extra) in #[(2,0.05),(3,0.1),(4,0.15),(5,0.2)] do
    let (choice,next) := random.next 0 bound
    random := next
    if choice == 0 then multiplier := multiplier + extra
  if !rainingForever state then
    state := {state with time := (single duration * multiplier).toInt32.toInt}
  let changed := changeRain state maxOut strengthOverride random
  return {
    state := {changed.state with active := true}
    random := changed.random
    events := events ++ changed.events ++ #[.rainStarted]}

def stopRain (state : Rain) (maxOut : Bool) (random : UnifiedRandom) : RainResult :=
  if rainingForever state then {state,random} else
  {
    state := {state with
      active := false,time := 0,strength := 0,coinRain := 0
      cloudAlpha := if maxOut then 0 else state.cloudAlpha}
    random,events := #[.rainStopped]}

def readyForKingSlime (players : Array Player) : Bool :=
  (players.extract 0 255).any fun player => player.active && player.lifeMax > 140 && player.defense > 8

structure PrefixResult where
  rain : Rain
  slime : OriginalSlimeRain.State
  random : UnifiedRandom
  events : Array Event := #[]

private def finishPrefix (rain : Rain) (slime : OriginalSlimeRain.State) (context : Context)
    (random : UnifiedRandom) (events : Array Event := #[]) : PrefixResult :=
  let changed := rain.strength != rain.oldStrength
  {rain := if changed then {rain with oldStrength := rain.strength} else rain
   slime,random,events := if changed && context.netMode == 2 then events.push .syncWorld else events}

def slimeContext (context : Context) : OriginalSlimeRain.Context :=
  {netMode := context.netMode,remixWorld := context.remixWorld
   hasWorldSurface := context.hasWorldSurface,myPlayer := context.myPlayer}

/-- The rainfall/slime prefix of complete Main.UpdateTime, before time-rate
recalculation and clock advancement. It uses the entry rate/time/player flags. -/
def advancePrefix (rain : Rain) (slime : OriginalSlimeRain.State) (context : Context)
    (random : UnifiedRandom) : PrefixResult := Id.run do
  let rain := if context.lanternsUp then
    {rain with cloudBackgroundActive := 0,clouds := min rain.clouds 30} else rain
  let rain := if rain.ladyBugBoost > 0 then
    {rain with ladyBugBoost := i32 (rain.ladyBugBoost - context.dayRate)} else rain
  if (context.netMode == 1 || context.gameMenu) && context.netMode != 2 then
    return finishPrefix rain slime context random
  if !context.hasWorldSurface && !context.remixWorld then return finishPrefix rain slime context random
  let timer := OriginalSlimeRain.advanceTimer slime (slimeContext context) context.dayRate random
  let mut slime := timer.state
  let mut rain := rain
  let mut random := timer.random
  let mut events := timer.events.map Event.slime
  if rain.active then
    if !context.freezeRain then
      if context.lanternsUp then
        let stopped := stopRain rain false random
        rain := stopped.state
        events := events ++ stopped.events
      else
        rain := {rain with time := i32 (rain.time - context.dayRate)}
        if context.dayRate > 0 then
          if rain.time ≤ 0 then
            let stopped := stopRain rain false random
            rain := stopped.state
            events := events ++ stopped.events
          else
            let (change,next) := random.next 0 (((86400 : Int).tdiv context.dayRate).tdiv 24 * 2)
            random := next
            if change == 0 then
              let changed := changeRain rain false none random
              rain := changed.state
              random := changed.random
              events := events ++ changed.events
  else if !slime.active && !context.lanternsUp && !context.nextNightIsLanternNight then
    let dayLength := (86400 : Int).tdiv (if context.dayRate == 0 then 1 else context.dayRate)
    let canRain := !context.freezeRain && context.dayRate > 0 &&
      (context.players.extract 0 255).any (fun player => player.active && player.lifeMax ≥ 120)
    if canRain then
      let (chance,next) := random.next 0 (dayLength.toFloat * 5.75).toInt64.toInt
      random := next
      let mut start := chance == 0
      if !start && rain.cloudBackgroundActive ≥ 1 then
        let (chance,next) := random.next 0 (dayLength.toFloat * 4.25).toInt64.toInt
        random := next
        start := chance == 0
      if !start && rain.ladyBugBoost > 0 then
        let (chance,next) := random.next 0 dayLength
        random := next
        start := chance == 0
      if start then
        let started := startRain rain context false none false random
        rain := started.state
        random := started.random
        events := events ++ started.events
    if !rain.active && slime.time == 0 && !context.busyInvasion && context.dayTime &&
        context.time < 27000 && context.dayRate > 0 then
      let mut chance := (450000.00000000006 / context.dayRate.toFloat).toInt64.toInt
      if !context.downedSlimeKing then
        chance := chance.tdiv 2
        if context.lowTiles then chance := chance.tdiv 5
      else if context.hardMode then chance := (chance.toFloat * 1.5).toInt64.toInt
      let ready := readyForKingSlime context.players
      if !ready then chance := i32 (chance * 5)
      if chance > 0 && (ready || context.expertMode) then
        let (start,next) := random.next 0 chance
        random := next
        if start == 0 then
          let started := OriginalSlimeRain.start slime (slimeContext context) rain.active true random
          slime := started.state
          random := started.random
          events := events ++ started.events.map Event.slime
  return finishPrefix rain slime context random events

def hasSufficientWind (wind : Float32) : Bool := !(wind.abs < 0.6)

def changeSeverity (state : Sandstorm) (context : Context) (random : UnifiedRandom) : SandstormResult :=
  let (intendedSeverity,random) := if state.active then
      let (severity,random) := nextFloat random
      (0.4 + severity,random)
    else
      let (quiet,random) := random.next 0 3
      if quiet == 0 then (0,random) else
      let (severity,random) := nextFloat random
      (severity * 0.3,random)
  {state := {state with intendedSeverity},random
   events := #[.severityChanged] ++ if context.netMode != 1 then #[.syncWorld] else #[]}

def startSandstorm (state : Sandstorm) (context : Context) (random : UnifiedRandom) : SandstormResult :=
  let (timeLeft,random) := random.next 28800 86401
  let next := changeSeverity {state with active := true,timeLeft} context random
  {next with events := #[.sandstormStarted] ++ next.events}

def stopSandstorm (state : Sandstorm) (context : Context) (random : UnifiedRandom) : SandstormResult :=
  let next := changeSeverity {state with active := false,timeLeft := 0} context random
  {next with events := #[.sandstormStopped] ++ next.events}

private def sign (value : Float32) : Int := if value > 0 then 1 else if value < 0 then -1 else 0

def updateSeverity (state : Sandstorm) : Sandstorm :=
  let severity := if state.severity.isNaN then 0 else state.severity
  let intendedSeverity := if state.intendedSeverity.isNaN then 0 else state.intendedSeverity
  let before := sign (intendedSeverity - severity)
  let severity := min 1 (max 0 (severity + 0.003 * single before))
  let after := sign (intendedSeverity - severity)
  {state with severity := if before != after then intendedSeverity else severity,intendedSeverity}

/-- Complete Sandstorm.UpdateTime; start/stop/intent helpers retain their real
RNG. The per-dayRate start loop does not break after a successful start. -/
def advanceSandstorm (state : Sandstorm) (context : Context) (random : UnifiedRandom) : SandstormResult := Id.run do
  let mut state := state
  let mut random := random
  let mut events := #[]
  if context.netMode != 1 then
    if state.active then
      if state.timeLeft > 86400 then state := {state with timeLeft := 0}
      state := {state with timeLeft := i32 (state.timeLeft - context.dayRate)}
      if !hasSufficientWind context.windCurrent then
        state := {state with timeLeft := i32 (state.timeLeft - 15 * context.dayRate)}
      if context.windCurrent == 0 then state := {state with timeLeft := 0}
      if state.timeLeft ≤ 0 then
        let stopped := stopSandstorm state context random
        state := stopped.state
        random := stopped.random
        events := events ++ stopped.events
    else if hasSufficientWind context.windCurrent then
      let denominator := if context.hardMode then 43200 else 64800
      for _ in [:context.dayRate.toNat] do
        let (start,next) := random.next 0 denominator
        random := next
        if start == 0 then
          let started := startSandstorm state context random
          state := started.state
          random := started.random
          events := events ++ started.events
    let (change,next) := random.next 0 18000
    random := next
    if change == 0 then
      let changed := changeSeverity state context random
      state := changed.state
      random := changed.random
      events := events ++ changed.events
  return {state := updateSeverity state,random,events}

end Lean4Terria.OriginalWorldWeather
