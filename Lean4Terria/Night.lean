import Lean4Terria.Combat
import Lean4Terria.Equipment

namespace Lean4Terria

def dayTicks : Nat := 7200
def nightTicks : Nat := 3600
def cycleTicks : Nat := dayTicks + nightTicks

def classicDayTicks : Nat := 54000
def classicNightTicks : Nat := 32400
def classicCycleTicks : Nat := classicDayTicks + classicNightTicks
def classicStartClock : Nat := 13500
def classicDawnFleeTicks : Nat := 600

def phaseDayTicks (legacyEarly : Bool) : Nat :=
  if legacyEarly then dayTicks else classicDayTicks

def phaseCycleTicks (legacyEarly : Bool) : Nat :=
  if legacyEarly then cycleTicks else classicCycleTicks

private def twilightTicks : Nat := 600
private def sunLevel : Nat := 15
private def moonLevel : Nat := 4

def isNight (clock : Nat) (legacyEarly := true) : Bool :=
  clock % phaseCycleTicks legacyEarly ≥ phaseDayTicks legacyEarly

def daylight (clock : Nat) (legacyEarly := true) : Nat :=
  let cycle := phaseCycleTicks legacyEarly
  let day := phaseDayTicks legacyEarly
  let twilight := if legacyEarly then twilightTicks else 4500
  let time := clock % cycle
  let change := sunLevel - moonLevel
  if time < day then
    sunLevel - change * (time - (day - twilight)) / twilight
  else
    moonLevel + change * (time - (cycle - twilight)) / twilight

def nightCreatureMaxHealth : Nat := 2
def nightCreatureHopDelay : Nat := 58
def nightCreatureMaxSpeed : Int := 24
def demonEyeMaxVerticalSpeed : Int := 32
private def spawnInterval : Nat := 300
private def hareSpawnInterval : Nat := 240
private def eyeSpawnInterval : Nat := 420
private def eyeSpawnDelay : Nat := 120

inductive SurfaceKind where
  | slime | cedarHare | brambleCrawler | greenSlime | blueSlime | zombie | demonEye
  deriving BEq, Repr, Inhabited

def SurfaceKind.name : SurfaceKind → String
  | .slime => "night slime"
  | .cedarHare => "Cedar Hare"
  | .brambleCrawler => "Bramble Crawler"
  | .greenSlime => "Green Slime"
  | .blueSlime => "Blue Slime"
  | .zombie => "Zombie"
  | .demonEye => "Demon Eye"

def SurfaceKind.maxHealth : SurfaceKind → Nat
  | .slime => nightCreatureMaxHealth
  | .cedarHare => 1
  | .brambleCrawler => 4
  | .greenSlime => 14
  | .blueSlime => 25
  | .zombie => 45
  | .demonEye => 60

def SurfaceKind.maxSpeed : SurfaceKind → Int
  | .slime => nightCreatureMaxSpeed
  | .cedarHare => 32
  | .brambleCrawler => 34
  | .greenSlime | .blueSlime => 22
  | .zombie => 20
  | .demonEye => 40

def SurfaceKind.hopDelay : SurfaceKind → Nat
  | .slime => nightCreatureHopDelay
  | .cedarHare => 32
  | .brambleCrawler => 42
  | .greenSlime | .blueSlime => 60
  | .zombie => 48
  | .demonEye => 0

def SurfaceKind.contactDamage : SurfaceKind → Nat
  | .slime => 1
  | .cedarHare => 0
  | .brambleCrawler => 2
  | .greenSlime => 6
  | .blueSlime => 7
  | .zombie => 14
  | .demonEye => 18

def SurfaceKind.defense : SurfaceKind → Nat
  | .blueSlime | .demonEye => 2
  | .zombie => 6
  | _ => 0

def SurfaceKind.damageTaken (kind : SurfaceKind) (raw : Nat) : Nat :=
  damageAfterDefense raw (kind.defense / 2)

def SurfaceKind.rewardCoins : SurfaceKind → Nat
  | .slime => 2
  | .cedarHare => 0
  | .brambleCrawler => 3
  | .greenSlime => 3
  | .blueSlime => 25
  | .zombie => 60
  | .demonEye => 75

def SurfaceKind.isDaySlime (kind : SurfaceKind) : Bool :=
  kind == .greenSlime || kind == .blueSlime

/-- Night enemies already present at sunrise get a short window to flee. -/
def SurfaceKind.activeAt (kind : SurfaceKind) (clock : Nat) (legacyEarly := true) : Bool :=
  if kind.isDaySlime then true
  else if kind == .cedarHare then !isNight clock legacyEarly
  else if kind == .zombie || kind == .demonEye then
    !legacyEarly && (isNight clock false || clock % classicCycleTicks < classicDawnFleeTicks)
  else isNight clock legacyEarly

def SurfaceKind.inHabitat (kind : SurfaceKind) (position : TilePos) : Bool :=
  if kind.isDaySlime then position.x < frostCavernStart
  else if kind == .slime || kind == .zombie then position.x < highlandStart
  else if kind == .demonEye then true
  else highlandStart ≤ position.x

structure NightCreature where
  body : Player
  health : Nat
  hopIn : Nat
  kind : SurfaceKind := .slime
  deriving BEq, Repr, Inhabited

structure SurfaceState where
  clock : Nat := 0
  creature : Option NightCreature := none
  flying : Option NightCreature := none
  deriving BEq, Repr, Inhabited

inductive SurfaceEvent where
  | spawned (kind : SurfaceKind)
  | hit (kind : SurfaceKind)
  | defeated (kind : SurfaceKind)
  | hurt (kind : SurfaceKind)
  | fell
  deriving BEq, Repr

private def nearSurface (seed : UInt64) (pos : TilePos) : Bool :=
  let ground : Int := groundHeight seed pos.x.toNat
  ground - 8 ≤ pos.y && pos.y < ground

private def hasOpenSky (world : World) (pos : TilePos) : Bool := Id.run do
  for y in [:pos.y.toNat] do
    if (world.tile ⟨pos.x, y⟩).solid then return false
  return true

private def spawnCreature (world : World) (seed : UInt64) (player : Player) (clock : Nat) (kind : SurfaceKind) :
    Option NightCreature := Id.run do
  let center := player.centerTile
  if !nearSurface seed center then return none
  let fromRight := ((seed ^^^ (clock / spawnInterval).toUInt64) &&& 1) == 0
  let directions : List Int := if fromRight then [1, -1] else [-1, 1]
  for offset in [9:18] do
    for direction in directions do
      let x := center.x + direction * (offset : Int)
      if x ≤ 0 || x ≥ (world.width : Int) - 1 || !kind.inHabitat ⟨x, center.y⟩ then continue
      let ground := groundHeight seed x.toNat
      if let some body := findStandingNear world x ground 0 5 fun candidate =>
          let pos := candidate.centerTile
          nearSurface seed pos && (pos.y - center.y).natAbs ≤ 6 && hasOpenSky world pos then
        let body := if kind.isDaySlime then { body with vx := -direction } else body
        return some { body, health := kind.maxHealth, hopIn := kind.hopDelay, kind }
  return none

private def NightCreature.hopSlime (creature : NightCreature) (world : World) (seed : UInt64)
    (player : Player) (clock : Nat) (legacyEarly := true) : NightCreature :=
  let dx := player.x - creature.body.x
  let pursuing := isNight clock legacyEarly || creature.health < creature.kind.maxHealth ||
    !nearSurface seed creature.body.centerTile
  let direction := if pursuing && dx != 0 then (if dx < 0 then -1 else 1)
    else if creature.body.vx < 0 then -1 else 1
  let jump := creature.hopIn == 0 && creature.body.grounded
  let wanted := direction * (if jump || !creature.body.grounded then creature.kind.maxSpeed else 1)
  let body := moveBody world creature.body wanted jump
  let body := if body.vx == 0 then { body with vx := -direction } else body
  { creature with body, hopIn := if jump then creature.kind.hopDelay else creature.hopIn - 1 }

private def NightCreature.chase (creature : NightCreature) (world : World) (seed : UInt64)
    (player : Player) (clock : Nat) :
    NightCreature :=
  if creature.kind == .greenSlime then creature.hopSlime world seed player clock
  else
    let dx := player.x - creature.body.x
    let speed := creature.kind.maxSpeed
    let wanted := if creature.kind == .cedarHare then
        if dx.natAbs ≤ (7 * tileUnits).toNat then (if dx < 0 then speed else -speed)
        else if creature.body.vx < 0 then -10 else 10
      else max (-speed) (min dx speed)
    let jump := creature.hopIn == 0 && creature.body.grounded && dx.natAbs > tileUnits.toNat
    let body := moveBody world creature.body wanted jump
    { creature with body, hopIn := if jump then creature.kind.hopDelay else creature.hopIn - 1 }

def NightCreature.staysNear (creature : NightCreature) (seed : UInt64) (player : Player) :
    Bool :=
  let pos := creature.body.centerTile
  creature.kind.inHabitat pos && creature.kind.inHabitat player.centerTile &&
    pos.y ≤ (groundHeight seed pos.x.toNat : Int) + 4 &&
    (pos.x - player.centerTile.x).natAbs ≤ 35 && (pos.y - player.centerTile.y).natAbs ≤ 16

private def advanceLegacySurface (world : World) (seed : UInt64) (player : Player)
    (combat : CombatState) (surface : SurfaceState) (attack : Option Nat) (defense : Nat)
    (thrust : Option TilePos) (canStrike : SurfaceKind → Bool) (onHit : SurfaceKind → Nat → Nat) :
    Player × CombatState × SurfaceState × Option SurfaceEvent := Id.run do
  let clock := (surface.clock + 1) % cycleTicks
  let quiet : SurfaceState := { clock }
  let kind := if highlandStart ≤ player.centerTile.x then
      if isNight clock then SurfaceKind.brambleCrawler else .cedarHare
    else .slime
  if !kind.activeAt clock then return (player, combat, quiet, none)
  let some initialCreature := surface.creature.filter (·.kind == kind)
    | let spawn := if kind == .cedarHare then clock % hareSpawnInterval == 0
          else (clock - dayTicks) % spawnInterval == 0
      if spawn then
        if let some creature := spawnCreature world seed player clock kind then
          return (player, combat, { clock, creature := some creature }, some (.spawned kind))
      return (player, combat, quiet, none)
  let mut creature := initialCreature
  let mut event := none
  if let some damage := attack.filter (fun _ => canStrike kind) then
    let damage := onHit kind damage
    let reaches := match thrust with
      | some target => shortswordReaches world player creature.body target combat.facing
      | none => swordReaches world player creature.body combat.facing true
    if damage > 0 && reaches then
      if creature.health ≤ damage then
        return (player, combat, quiet, some (.defeated kind))
      creature := { creature with health := creature.health - damage }
      event := some (.hit kind)
  creature := creature.chase world seed player clock
  if !creature.staysNear seed player then
    return (player, combat, quiet, event)
  let surface := { clock, creature := some creature }
  if kind.contactDamage > 0 && combat.health > 0 && combat.invulnerable == 0 &&
      bodiesOverlap player creature.body then
    let damage := damageAfterDefense kind.contactDamage defense * healthPerHeart
    if combat.health ≤ damage then
      return (player, combat.afterDefeat, quiet, some .fell)
    let combat := combat.hurt damage
    return (player, combat, surface, some (.hurt kind))
  return (player, combat, surface, event)

private def spawnDemonEye (world : World) (seed : UInt64) (player : Player)
    (clock : Nat) : Option NightCreature := Id.run do
  let center := player.centerTile
  if !nearSurface seed center then return none
  let first : Int := if (seed + (clock / eyeSpawnInterval).toUInt64) % 2 == 0 then 1 else -1
  for offset in [9:18] do
    for direction in [first, -first] do
      for height in [6:12] do
        let position : TilePos := ⟨center.x + direction * offset, center.y - height⟩
        if !world.contains position || !hasOpenSky world position then continue
        let body : Player :=
          { x := position.x * tileUnits + (tileUnits - playerWidth) / 2
            y := position.y * tileUnits + (tileUnits - playerHeight) / 2
            vx := -direction * 8, vy := 0, grounded := false }
        if !body.collides world then
          return some ⟨body, SurfaceKind.demonEye.maxHealth, 0, .demonEye⟩
  return none

private def accelerate (velocity target acceleration : Int) : Int :=
  velocity + max (-acceleration) (min acceleration (target - velocity))

private def toward (difference speed : Int) : Int :=
  max (-speed) (min speed difference)

private def NightCreature.walkZombie (creature : NightCreature) (world : World)
    (player : Player) (fleeing : Bool) : NightCreature :=
  let dx := player.x - creature.body.x
  let speed := creature.kind.maxSpeed
  let wanted := if fleeing then
      if dx < 0 || (dx == 0 && creature.body.vx > 0) then speed else -speed
    else toward dx speed
  let vx := accelerate creature.body.vx wanted 2
  let walked := moveBody world creature.body vx false
  let blocked := vx != 0 && walked.vx == 0
  let jump := creature.body.grounded && creature.hopIn == 0 && blocked
  { creature with
    body := if jump then moveBody world creature.body vx true else walked
    hopIn := if jump then creature.kind.hopDelay else creature.hopIn - 1 }

private def NightCreature.fly (creature : NightCreature) (world : World)
    (player : Player) (fleeing : Bool) : NightCreature :=
  let dx := player.x - creature.body.x
  let dy := player.y - creature.body.y
  let speed := creature.kind.maxSpeed
  let wantedX := if fleeing then (if dx < 0 then speed / 2 else -speed / 2)
    else toward dx speed
  let wantedY := if fleeing then -demonEyeMaxVerticalSpeed
    else toward dy demonEyeMaxVerticalSpeed
  let vx := accelerate creature.body.vx wantedX 2
  let vy := accelerate creature.body.vy wantedY 2
  let body := moveBodyDelta world creature.body vx vy (dropThrough := true)
  -- The rebound changes velocity only. The collision sweep has already clamped position.
  let bounce (moved intended : Int) := if moved == 0 then - (intended / 2) else moved
  { creature with body := { body with
      vx := bounce body.vx vx, vy := bounce body.vy vy, grounded := false } }

private def NightCreature.moveClassic (creature : NightCreature) (world : World)
    (seed : UInt64) (player : Player) (clock : Nat) : NightCreature :=
  match creature.kind with
  | .greenSlime | .blueSlime => creature.hopSlime world seed player clock false
  | .zombie => creature.walkZombie world player (!isNight clock false)
  | .demonEye => creature.fly world player (!isNight clock false)
  | _ => creature.chase world seed player clock

private def spawnsOn (elapsed interval : Nat) (townProtected : Bool) : Bool :=
  elapsed % interval == 0 && (!townProtected || (elapsed / interval) % 4 == 3)

private def classicGround (seed : UInt64) (clock : Nat) (player : Player) : SurfaceKind :=
  let night := isNight clock false
  if highlandStart ≤ player.centerTile.x then
    if night then .brambleCrawler else .cedarHare
  else if night then .zombie
  else if (seed + (clock / spawnInterval).toUInt64) % 5 == 0 then .blueSlime
  else .greenSlime

private def strikeCreature (world : World) (player : Player) (combat : CombatState)
    (attack : Nat) (thrust : Option TilePos) (creature : NightCreature) :
    Option (Option NightCreature × SurfaceEvent) := do
  let reaches := match thrust with
    | some target => shortswordReaches world player creature.body target combat.facing
    | none => swordReaches world player creature.body combat.facing true
  if attack == 0 || !reaches then none
  else
    let damage := creature.kind.damageTaken attack
    if creature.health ≤ damage then some (none, .defeated creature.kind)
    else
      let direction := if creature.body.x < player.x ||
          (creature.body.x == player.x && combat.facing < 0) then -1 else 1
      let body := match creature.kind with
        | .zombie => { creature.body with
            vx := direction * creature.kind.maxSpeed, vy := -48, grounded := false }
        | .demonEye => { creature.body with vx := direction * creature.kind.maxSpeed }
        | _ => creature.body
      some (some { creature with body, health := creature.health - damage }, .hit creature.kind)

private def advanceClassicSurface (world : World) (seed : UInt64) (player : Player)
    (combat : CombatState) (surface : SurfaceState) (attack : Option Nat) (defense : Nat)
    (thrust : Option TilePos) (townProtected : Bool) (canStrike : SurfaceKind → Bool)
    (onHit : SurfaceKind → Nat → Nat) :
    Player × CombatState × SurfaceState × Option SurfaceEvent := Id.run do
  let clock := (surface.clock + 1) % classicCycleTicks
  let night := isNight clock false
  let kind := classicGround seed clock player
  let mut ground := surface.creature.filter fun creature =>
    creature.kind.activeAt clock false && creature.kind.inHabitat player.centerTile &&
      (creature.kind == kind || (!night && (creature.kind.isDaySlime || creature.kind == .zombie)))
  let mut flying := surface.flying.filter fun creature =>
    creature.kind == .demonEye && creature.kind.activeAt clock false
  let mut event := none
  if let some damage := attack then
    let strike := fun (creature : NightCreature) =>
      if canStrike creature.kind then strikeCreature world player combat (onHit creature.kind damage) thrust creature else none
    if let some (remaining, hit) := ground.bind strike then
      ground := remaining
      event := some hit
    else if let some (remaining, hit) := flying.bind strike then
      flying := remaining
      event := some hit
    if event.any (fun hit => match hit with | .defeated _ => true | _ => false) then
      let creature := ground.filter (·.staysNear seed player)
      let nearbyEye := flying.filter (·.staysNear seed player)
      return (player, combat, { clock, creature, flying := nearbyEye }, event)
  let advance := fun creature =>
    let moved := creature.moveClassic world seed player clock
    if moved.staysNear seed player then some moved else none
  ground := ground.bind advance
  flying := flying.bind advance
  let duskElapsed := clock - classicDayTicks
  let groundInterval := if kind == .cedarHare then hareSpawnInterval else spawnInterval
  if ground.isNone && spawnsOn (if night then duskElapsed else clock) groundInterval
      (townProtected && kind.contactDamage > 0) then
    if let some creature := spawnCreature world seed player clock kind then
      ground := some creature
      event := event.or (some (.spawned kind))
  if flying.isNone && night && eyeSpawnDelay ≤ duskElapsed &&
      spawnsOn (duskElapsed - eyeSpawnDelay) eyeSpawnInterval townProtected then
    if let some creature := spawnDemonEye world seed player clock then
      flying := some creature
      event := event.or (some (.spawned .demonEye))
  let surface : SurfaceState := { clock, creature := ground, flying }
  if combat.health == 0 || combat.invulnerable != 0 then
    return (player, combat, surface, event)
  let touches := fun (creature : NightCreature) =>
    creature.kind.contactDamage > 0 && bodiesOverlap player creature.body
  let some creature := (ground.filter touches).or (flying.filter touches)
    | return (player, combat, surface, event)
  let damage := if creature.kind == .brambleCrawler then
      damageAfterDefense creature.kind.contactDamage defense * healthPerHeart
    else damageAfterDefense creature.kind.contactDamage ((defense + 1) / 2)
  if combat.health ≤ damage then
    return (player, combat.afterDefeat, { clock }, some .fell)
  return (player, combat.hurt damage, surface, event.or (some (.hurt creature.kind)))

/-- Advance the legacy surface simulation or the Classic clock and its ground and flying encounters. -/
def advanceSurface (world : World) (seed : UInt64) (player : Player) (combat : CombatState)
    (surface : SurfaceState) (attack : Option Nat) (defense : Nat := 0)
    (classic : Bool := false) (thrust : Option TilePos := none) (townProtected := false)
    (canStrike : SurfaceKind → Bool := fun _ => true)
    (onHit : SurfaceKind → Nat → Nat := fun _ damage => damage) :
    Player × CombatState × SurfaceState × Option SurfaceEvent :=
  if classic then advanceClassicSurface world seed player combat surface attack defense thrust townProtected canStrike onHit
  else advanceLegacySurface world seed player combat surface attack defense thrust canStrike onHit

end Lean4Terria
