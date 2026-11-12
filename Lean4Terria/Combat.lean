import Lean4Terria.Physics

namespace Lean4Terria

def maxHealth : Nat := 5
def crawlerMaxHealth : Nat := 3
def hurtCooldown : Nat := 48
def swingCooldown : Nat := 18
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
  deriving BEq, Repr

inductive CombatEvent where
  | swing | hitCrawler | defeatedCrawler | hurt | respawned
  deriving BEq, Repr

private def crawlerLeft : Int := 66 * tileUnits
private def crawlerRight : Int := 76 * tileUnits

private def bodiesOverlap (a b : Player) : Bool :=
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

private def findStandingNear (world : World) (col floor : Int) (columns rows : Nat)
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

private def respawn (world : World) (previous : Player) (crawler : Option Crawler) : Player := Id.run do
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

private def trySwing (world : World) (player : Player) (combat : CombatState)
    (target : TilePos) : CombatState × CombatEvent := Id.run do
  let aim := target.x - player.centerTile.x
  let facing := if aim < 0 then -1 else if aim > 0 then 1 else combat.facing
  let combat := { combat with facing, swing := swingCooldown }
  if let some enemy := combat.crawler then
    let dx := enemy.body.x - player.x
    let dy := enemy.body.y - player.y
    let aimedAt := 0 ≤ facing * dx || (aim == 0 && bodiesOverlap player enemy.body)
    let inReach := aimedAt && dx.natAbs ≤ (tileUnits * 5 / 2).toNat &&
      dy.natAbs ≤ playerHeight.toNat
    if inReach && clearSwordPath world player enemy.body then
      if enemy.health ≤ 1 then
        return ({ combat with crawler := none, cleared := true }, .defeatedCrawler)
      return ({ combat with crawler := some { enemy with health := enemy.health - 1 } }, .hitCrawler)
  return (combat, .swing)

/-- Advance combat after player movement. A sword strike resolves before enemy movement
and contact damage, so defeating the crawler on that tick also prevents its contact hit. -/
def advanceCombat (world : World) (player : Player) (combat : CombatState) (horizontal : Int)
    (attack : Option TilePos) : Player × CombatState × Option CombatEvent := Id.run do
  let facing := if horizontal < 0 then -1 else if horizontal > 0 then 1 else combat.facing
  let mut combat := { combat with facing, invulnerable := combat.invulnerable - 1, swing := combat.swing - 1 }
  let mut event := none
  if let some target := attack then
    if combat.swing == 0 then
      let (next, strike) := trySwing world player combat target
      combat := next
      event := some strike
  if let some crawler := combat.crawler then
    let crawler := moveCrawler world player crawler
    combat := { combat with crawler := some crawler }
    if combat.invulnerable == 0 && bodiesOverlap player crawler.body then
      if combat.health ≤ 1 then
        let player := respawn world player combat.crawler
        return (player, { combat with health := maxHealth, invulnerable := hurtCooldown, swing := 0, facing := 1 },
          some .respawned)
      return (player, { combat with health := combat.health - 1, invulnerable := hurtCooldown }, some .hurt)
  return (player, combat, event)

end Lean4Terria
