import Lean4Terria.Physics

namespace Lean4Terria

def healthPerHeart : Nat := 20
def maxHealth : Nat := 100
def maxLifeCrystals : Nat := 15
def crawlerMaxHealth : Nat := 3
def hurtCooldown : Nat := 48
def npcContactKnockbackX : Int := 72
def lavaHurtCooldown : Nat := 40
def lavaBurnTicks : Nat := 420
def burnDamageInterval : Nat := 15
def maxRegenTime : Nat := 3600
def regenPointThreshold : Nat := 120
def classicRespawnTicks : Nat := 600
def spawnInvulnerability : Nat := 180
def swingCooldown : Nat := 18
def maxSwordUseTime : Nat := 22
def crawlerHopDelay : Nat := 78

structure Crawler where
  body : Player
  health : Nat
  hopIn : Nat
  deriving BEq, Repr

structure CombatState where
  health : Nat
  invulnerable : Nat
  swing : Nat
  facing : Int
  crawler : Option Crawler
  cleared : Bool
  lifeCrystals : Nat := 0
  onFire : Nat := 0
  fireProgress : Nat := 0
  lavaCooldown : Nat := 0
  regenTime : Nat := 0
  regenProgress : Nat := 0
  deriving BEq, Repr

def CombatState.healthLimit (combat : CombatState) : Nat :=
  maxHealth + combat.lifeCrystals * healthPerHeart

def CombatState.afterDefeat (combat : CombatState) : CombatState :=
  { combat with
    health := 0
    invulnerable := 0
    swing := 0
    facing := 1
    onFire := 0
    fireProgress := 0
    lavaCooldown := 0
    regenTime := 0
    regenProgress := 0 }

def CombatState.afterRespawn (combat : CombatState) : CombatState :=
  { combat with health := max maxHealth (combat.healthLimit / 2), invulnerable := spawnInvulnerability }

def CombatState.hurt (combat : CombatState) (damage : Nat) : CombatState :=
  { combat with health := combat.health - damage, invulnerable := hurtCooldown, regenTime := 0 }

/-- Classic contact damage at zero luck, given the draw from the 31-value roll.
Round the varied Single to even, then keep the half-point defense result until
after selecting the General immunity duration. -/
def classicNpcContactDamage (base roll defense : Nat) (afterVariance : Nat := 1) : Nat × Nat :=
  let percent := (Int32.ofNat (roll % 31) - 15).toFloat32
  let varied := (Int32.ofNat base).toFloat32 * (1 + percent * 0.01)
  let whole := varied.toUInt32.toNat
  let fraction := varied - (Int32.ofNat whole).toFloat32
  let rounded := if fraction > 0.5 || (fraction == 0.5 && whole % 2 == 1) then whole + 1 else whole
  let doubled := max 2 (2 * rounded * afterVariance - defense)
  (doubled / 2, if doubled == 2 then 20 else 40)

private def roundRate (numerator denominator : Nat) : Nat :=
  let whole := numerator / denominator
  let rest := numerator % denominator
  if 2 * rest > denominator || (2 * rest == denominator && whole % 2 == 1) then whole + 1
  else whole

/-- Natural recovery ramps with time since a hit. Fire drains one HP every
15 ticks, including when the ordinary damage immunity timer is active. -/
def CombatState.tickLife (combat : CombatState) (standingStill wet : Bool) (honey := false) : CombatState :=
  if combat.health == 0 then combat
  else if combat.onFire > 0 && !wet then
    let ticks := combat.onFire - 1
    let progress := combat.fireProgress + if honey then combat.onFire % 2 else 1
    { combat with
      health := combat.health - progress / burnDamageInterval
      onFire := ticks
      fireProgress := if ticks == 0 then 0 else progress % burnDamageInterval
      regenTime := 0
      regenProgress := 0 }
  else
    let time := min maxRegenTime (combat.regenTime + if honey then 3 else 1)
    let effective := if time ≤ 1800 then time / 300 else 6 + (time - 1800) / 600
    let activity := if standingStill then 5 else 2
    let rate := roundRate ((17 * combat.healthLimit + 1200) * effective * activity) 32000
    let progress := combat.regenProgress + rate + if honey then 2 else 0
    { combat with
      health := min combat.healthLimit (combat.health + progress / regenPointThreshold)
      onFire := 0
      fireProgress := 0
      regenTime := time
      regenProgress := if combat.health == combat.healthLimit then 0 else progress % regenPointThreshold }

inductive CombatEvent where
  | swing | hitCrawler | defeatedCrawler | hurt | respawned
  deriving BEq, Repr

private def crawlerLeft : Int := 66 * tileUnits
private def crawlerRight : Int := 76 * tileUnits

def bodiesOverlap (a b : Player) : Bool :=
  (a.x - b.x).natAbs < playerWidth.toNat && (a.y - b.y).natAbs < playerHeight.toNat

private def standingAt (world : World) (col floor : Int) : Option Player :=
  let player : Player :=
    { x := col * tileUnits + (tileUnits - playerWidth) / 2
      y := floor * tileUnits - playerHeight
      vx := 0, vy := 0, grounded := true }
  let feet : TilePos := ⟨col, floor⟩
  if world.contains feet && (world.tile feet).solid && !player.collides world then
    some player
  else
    none

private def outwardOffset (index : Nat) : Int :=
  let distance : Int := (index + 1) / 2
  if index % 2 == 0 then -distance else distance

def findStandingNear (world : World) (col floor : Int) (columns rows : Nat)
    (acceptable : Player → Bool) : Option Player := Id.run do
  for row in [:2 * rows + 1] do
    for column in [:2 * columns + 1] do
      if let some body := standingAt world (col + outwardOffset column) (floor + outwardOffset row) then
        if acceptable body then return some body
  return none

private def inCrawlerArea (body : Player) : Bool :=
  crawlerLeft ≤ body.x && body.x + playerWidth ≤ crawlerRight

def initialCombat (world : World) : CombatState :=
  let crawler := (findStandingNear world 71 52 5 6 inCrawlerArea).map fun body =>
    { body, health := crawlerMaxHealth, hopIn := crawlerHopDelay : Crawler }
  { health := maxHealth, invulnerable := 0, swing := 0, facing := 1, crawler, cleared := false }

def respawn (world : World) (previous : Player) (crawler : Option Crawler) : Player := Id.run do
  let safe := fun body => !crawler.any (fun enemy => bodiesOverlap body enemy.body)
  if let some body := findStandingNear world 19 40 12 12 safe then
    return body
  for col in [:world.width] do
    for row in [:world.height] do
      if let some body := standingAt world col row then
        if safe body then return body
  -- In a fully excavated world, the current free space is a valid fallback.
  return { previous with vx := 0, vy := 0 }

private def moveCrawler (world : World) (player : Player) (crawler : Crawler) : Crawler :=
  let body := crawler.body
  let dx := player.x - body.x
  let dy := player.y - body.y
  let chasing := 66 ≤ player.centerTile.x && dx.natAbs ≤ 10 * tileUnits.toNat &&
    dy.natAbs ≤ 5 * tileUnits.toNat
  let direction :=
    if chasing then
      if dx < 0 then -1 else if dx > 0 then 1 else 0
    else if body.vx > 0 then 1 else -1
  let jump := chasing && crawler.hopIn == 0 && body.grounded
  let moved := movePlayer world body direction jump
  let body :=
    if !chasing && (moved.x == body.x || !inCrawlerArea moved) then
      let reflected := movePlayer world body (-direction) jump
      if inCrawlerArea reflected then reflected else movePlayer world body 0 jump
    else if inCrawlerArea moved then moved
    else movePlayer world body 0 jump
  { crawler with body, hopIn := if jump then crawlerHopDelay else crawler.hopIn - 1 }

private def clearSwordPath (world : World) (player enemy : Player) : Bool := Id.run do
  let startX := player.x + playerWidth / 2
  let startY := player.y + playerHeight / 2
  let dx := enemy.x - player.x
  let dy := enemy.y - player.y
  let steps := max 1 (max dx.natAbs dy.natAbs)
  -- A one-subpixel stride along the dominant axis detects intervening solid tiles.
  for step in [:steps + 1] do
    let x := startX + dx * (step : Int) / (steps : Int)
    let y := startY + dy * (step : Int) / (steps : Int)
    if (world.tile ⟨x / tileUnits, y / tileUnits⟩).solid then return false
  return true

def swordReaches (world : World) (player enemy : Player) (facing : Int)
    (allowOverlap : Bool) (ignoreTerrain := false) : Bool :=
  let dx := enemy.x - player.x
  let dy := enemy.y - player.y
  let aimedAt := 0 ≤ facing * dx || (allowOverlap && bodiesOverlap player enemy)
  aimedAt && dx.natAbs ≤ (tileUnits * 5 / 2).toNat && dy.natAbs ≤ playerHeight.toNat &&
    (ignoreTerrain || clearSwordPath world player enemy)

/-- A shortsword follows the cursor along a narrow, two-tile thrust. -/
def shortswordReaches (world : World) (player enemy : Player) (target : TilePos) (facing : Int)
    (ignoreTerrain := false) : Bool :=
  let centerX := player.x + playerWidth / 2
  let centerY := player.y + playerHeight / 2
  let dx := enemy.x - player.x
  let dy := enemy.y - player.y
  let cursorX := target.x * tileUnits + tileUnits / 2 - centerX
  let cursorY := target.y * tileUnits + tileUnits / 2 - centerY
  let aimX := if target == player.centerTile then facing * tileUnits else cursorX
  let aimY := if target == player.centerTile then 0 else cursorY
  let lengthSquared := aimX * aimX + aimY * aimY
  let alignment := dx * aimX + dy * aimY
  let offset := dx * aimY - dy * aimX
  alignment ≥ 0 && dx * dx + dy * dy ≤ (2 * tileUnits) * (2 * tileUnits) &&
    offset * offset ≤ lengthSquared * (tileUnits / 2) * (tileUnits / 2) &&
    (ignoreTerrain || clearSwordPath world player enemy)

private def trySwing (world : World) (player : Player) (combat : CombatState)
    (target : TilePos) (damage delay : Nat) (shortsword canStrikeCrawler : Bool) : CombatState × CombatEvent := Id.run do
  let aim := target.x - player.centerTile.x
  let facing := if aim < 0 then -1 else if aim > 0 then 1 else combat.facing
  let combat := { combat with facing, swing := delay }
  if let some enemy := combat.crawler then
    let reaches := if shortsword then shortswordReaches world player enemy.body target facing
      else swordReaches world player enemy.body facing (aim == 0)
    if canStrikeCrawler && reaches then
      if enemy.health ≤ damage then
        return ({ combat with crawler := none, cleared := true }, .defeatedCrawler)
      return ({ combat with crawler := some { enemy with health := enemy.health - damage } }, .hitCrawler)
  return (combat, .swing)

/-- Advance combat after player movement. A sword strike resolves before enemy movement
and contact damage, so defeating the crawler on that tick also prevents its contact hit. -/
def advanceCombat (world : World) (player : Player) (combat : CombatState) (horizontal : Int)
    (attack : Option TilePos) (damage : Nat := 1) (delay := swingCooldown) (shortsword := false)
    (immunityTicked := false) (canStrikeCrawler := true) :
    Player × CombatState × Option CombatEvent := Id.run do
  let facing := if horizontal < 0 then -1 else if horizontal > 0 then 1 else combat.facing
  let mut combat := { combat with
    facing
    invulnerable := if immunityTicked then combat.invulnerable else combat.invulnerable - 1
    lavaCooldown := combat.lavaCooldown - 1
    swing := combat.swing - 1 }
  let mut event := none
  if let some target := attack then
    if combat.swing == 0 then
      let (next, strike) := trySwing world player combat target damage delay shortsword canStrikeCrawler
      combat := next
      event := some strike
  if let some crawler := combat.crawler then
    let crawler := moveCrawler world player crawler
    combat := { combat with crawler := some crawler }
    if combat.invulnerable == 0 && bodiesOverlap player crawler.body then
      if combat.health ≤ healthPerHeart then
        return (player, combat.afterDefeat, some .respawned)
      return (player, combat.hurt healthPerHeart, some .hurt)
  return (player, combat, event)

end Lean4Terria
