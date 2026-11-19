import Lean4Terria.OriginalCollision

/-!
# Green Slime (Terraria 1.4.5.8, Classic)

A hand transcription of what `NPC.UpdateNPC` does for one Green Slime (`type = 1`, `netID = -3`,
`aiStyle = 1`) in a single-player world (`Main.netMode = 0`). The IL read for this file is
`NPC.UpdateNPC`, `NPC.UpdateNPC_UpdateGravity`, `NPC.AI_001_Slimes`,
`NPC.AI_001_Slimes_GenerateItemInsideBody`, `NPC.TargetClosest` (with `TryTrackingTarget` and
`SetTargetTrackingValues`), `NPC.UpdateCollision` (with the dry, slope and stair-fall helpers),
`Collision.SolidCollision`, the slime branch of `NPC.FindFrame`, `NPC.CheckActive`,
`NPC.NewNPC` and the `-3` case of `NPC.SetDefaultsFromNetId`.

Tile collision is the shared `OriginalCollision` port (`TileCollision`, `SlopeCollision`,
`WalkDownSlope`); this file adds only `Collision.SolidCollision`.

Inputs of one frame: the slime state, the world statics in `World`, the players in `Player`
(position, size, `active`, `dead`, `ghost`, `aggro`, `itemAnimation`, `npcTypeNoAggro[1]`) and the
`Main.rand` stream (`Rng`). Outputs: the next slime state, the next `Main.rand`, the value of
the static `NPC.gravity`, and the `Player.nearbyActiveNPCs` increments written by `CheckActive`.

Covered on purpose: spawning (`NewNPC` + `SetDefaults(-3)` constants), the first-frame item roll,
seeing a player (`TargetClosest` and its facing rules), the ground pause and the three-step jump
cycle, the stuck-after-a-long-jump turn (`ai[2] = 200`), air steering, wall and platform
collision through the shared collision port, hit wake-up (`life < lifeMax` changes the timers and
retargeting), frame selection (`frame.Y`, `frameCounter`) and despawn (`timeLeft`, `active`).

Not transcribed (the step reports `unsupported` instead of guessing):
liquids (`wet`, lava, honey, shimmer), painted tiles under the slime, the `ai[0] = -999` spawn
variant, items inside the slime whose behaviour needs projectiles, tile placement or other systems
(`539`, `150`, `3609`, the hardmode ores and the event items), special seeds
(`remixWorld`, `getGoodWorld`, `noTrapsWorld`, `notTheBeesWorld`, `vampireSeed`, skyblock),
buffs and debuffs, conveyor belts, pressure plates, multiplayer smoothing (`netOffset`) and
`StrikeNPC` itself (damage roll, knockback, `HitEffect` dust). Dust and light calls that use no
`Main.rand` on these paths are omitted.
-/

namespace Lean4Terria.OriginalGreenSlime

open OriginalCollision (Grid Vector single tileIndex)
open OriginalWorld (Tile TileShape)

/-! ## `Main.rand` (`Terraria.Utilities.UnifiedRandom`) -/

/-- The subtractive generator with its whole table visible, so a replay can compare the table
after every frame with the original. `inext` is the last used slot (0 right after seeding). -/
structure Rng where
  table : Array Int32
  inext : Nat
  deriving BEq, Repr, Inhabited

namespace Rng

private def limit : Int32 := 2147483647

/-- `UnifiedRandom.SetSeed`. -/
def seed (value : Int) : Rng := Id.run do
  let value := Int32.ofInt value
  let magnitude := if value == -2147483648 then limit else if value < 0 then -value else value
  let mut previous := 161803398 - magnitude
  let mut next : Int32 := 1
  let mut table := (Array.replicate 56 (0 : Int32)).set! 55 previous
  for i in [1:55] do
    let index := (21 * i) % 55
    table := table.set! index next
    next := previous - next
    if next < 0 then next := next + limit
    previous := table[index]!
  for _ in [:4] do
    for i in [1:56] do
      let value := table[i]! - table[1 + (i + 30) % 55]!
      table := table.set! i (if value < 0 then value + limit else value)
  return ⟨table, 0⟩

/-- `UnifiedRandom.InternalSample`. -/
def internalSample (rng : Rng) : Int32 × Rng :=
  let x := if rng.inext + 1 > 55 then 1 else rng.inext + 1
  let y := if x + 21 > 55 then x + 21 - 55 else x + 21
  let value := rng.table[x]! - rng.table[y]!
  let value := if value == limit then value - 1 else value
  let value := if value < 0 then value + limit else value
  (value, ⟨rng.table.set! x value, x⟩)

/-- `UnifiedRandom.Sample`, a double in `[0, 1)`. -/
def sample (rng : Rng) : Float × Rng :=
  let (value, rng) := rng.internalSample
  (Float.ofInt value.toInt * 4.656612875245797e-10, rng)

/-- `UnifiedRandom.Next(maxValue)` for `maxValue ≥ 0`. -/
def next (rng : Rng) (maxValue : Nat) : Nat × Rng :=
  let (value, rng) := rng.sample
  ((value * maxValue.toFloat).toUInt64.toNat, rng)

/-- `UnifiedRandom.Next(minValue, maxValue)` for a range below `int.MaxValue`. -/
def nextRange (rng : Rng) (minValue maxValue : Int) : Int × Rng :=
  let (value, rng) := rng.sample
  ((value * Float.ofInt (maxValue - minValue)).toUInt64.toNat + minValue, rng)

end Rng

/-! ## World, players and slime state -/

/-- World statics read during the update. `frameHeight` is the texture frame height used by
`FindFrame`: the dedicated server has no texture and uses `1`; the desktop client uses the
`NPC_1` texture height divided by `Main.npcFrameCount[1]`. -/
structure World where
  grid : Grid
  dayTime : Bool := true
  slimeRain : Bool := false
  worldSurface : Float
  rockLayer : Float
  activeRangeX : Int := 4032
  activeRangeY : Int := 2520
  activeTime : Int := 750
  screenWidth : Int := 1920
  screenHeight : Int := 1200
  frameHeight : Int := 1
  frameCount : Int := 2

structure Player where
  index : Nat
  active : Bool := true
  dead : Bool := false
  ghost : Bool := false
  position : Vector
  width : Nat := 20
  height : Nat := 42
  aggro : Int := 0
  itemAnimation : Int := 0
  /-- `npcTypeNoAggro[1]`. -/
  noAggro : Bool := false
  deriving Repr

def Player.center (p : Player) : Vector :=
  ⟨p.position.x + single p.width / 2, p.position.y + single p.height / 2⟩

/-- `Main.player[i]` for a slot no `Player` value describes: a fresh inactive `Player`. -/
def emptyPlayer (index : Nat) : Player := { index, active := false, position := ⟨0, 0⟩ }

instance : Inhabited Player := ⟨emptyPlayer 0⟩

def playerAt (players : Array Player) (index : Int) : Player :=
  (players.find? (fun p => (p.index : Int) == index)).getD (emptyPlayer index.toNat)

structure Color where
  r : Nat
  g : Nat
  b : Nat
  a : Nat
  deriving BEq, Repr, Inhabited

def Color.rgba (color : Color) : UInt32 :=
  (color.r.toUInt32 <<< 24) ||| (color.g.toUInt32 <<< 16) ||| (color.b.toUInt32 <<< 8) ||| color.a.toUInt32

/-- Fields of one Green Slime that `UpdateNPC` reads or writes on the covered paths. -/
structure Slime where
  active : Bool := true
  position : Vector
  velocity : Vector := ⟨0, 0⟩
  oldPosition : Vector := ⟨0, 0⟩
  oldVelocity : Vector := ⟨0, 0⟩
  width : Nat := 21
  height : Nat := 17
  direction : Int := 0
  directionY : Int := 1
  oldDirection : Int := 0
  oldDirectionY : Int := 0
  spriteDirection : Int := -1
  target : Int := 255
  oldTarget : Int := 255
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  ai2 : Float32 := 0
  ai3 : Float32 := 0
  aiAction : Int := 0
  collideX : Bool := false
  collideY : Bool := false
  wet : Bool := false
  life : Int := 14
  lifeMax : Int := 14
  damage : Int := 6
  defense : Int := 0
  defDamage : Int := 6
  defDefense : Int := 0
  knockBackResist : Float32 := 1.2
  value : Float32 := 3
  scale : Float32 := 0.9
  alpha : Nat := 175
  npcSlots : Float32 := 1
  timeLeft : Int := 0
  justHit : Bool := false
  playerInteraction : Bool := false
  netUpdate : Bool := true
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  stairFall : Bool := false
  rotation : Float32 := 0
  soundDelay : Int := 0
  /-- `immune[0..255]`. -/
  immune : Array Int := Array.replicate 256 0
  frameY : Int := 0
  frameCounter : Float := 0
  color : Color := ⟨0, 220, 40, 100⟩
  /-- `targetRect` as `(x, y, width, height)`. -/
  targetRect : Int × Int × Int × Int := (0, 0, 0, 0)
  despawnEncouraged : Bool := false
  /-- The localization key written to `_givenName`, if any. -/
  givenName : Option String := none
  /-- `NPC.netID`: -3 Green; also 1 Blue, -7 Purple, -8 Red, -9 Yellow (`Variant`). -/
  netId : Int := -3
  deriving Repr

instance : Inhabited Slime := ⟨{ position := ⟨0, 0⟩ }⟩

def Slime.center (n : Slime) : Vector :=
  ⟨n.position.x + single n.width / 2, n.position.y + single n.height / 2⟩

def Slime.left (n : Slime) : Vector := ⟨n.position.x, n.position.y + single n.height / 2⟩
def Slime.right (n : Slime) : Vector := ⟨n.position.x + single n.width, n.position.y + single n.height / 2⟩

/-- The `SetDefaults(netID)` fields that differ between the ordinary slimes of `type = 1` whose
`AI_001_Slimes` path is the Green Slime's (no netID branch: not -4 Pinky, -5 Baby, -6 Black,
-10 Jungle). Values from the native survey `.lake/forest-desktop-integration/slime-variants/
npc-defaults.json` (the game's own `SetDefaults` for every id). -/
structure Variant where
  netId : Int
  lifeMax : Int
  damage : Int
  defense : Int
  knockBackResist : Float32
  value : Float32
  scale : Float32
  alpha : Nat
  color : Color
  width : Nat
  height : Nat
  /-- `TypeName` key, for death messages. -/
  name : String

def variant? : Int → Option Variant
  | 1 => some { netId := 1, lifeMax := 25, damage := 7, defense := 2, knockBackResist := Float32.ofBits 1065353216, value := Float32.ofBits 1103626240, scale := Float32.ofBits 1065353216, alpha := 175, color := ⟨0, 80, 255, 100⟩, width := 24, height := 18, name := "NPCName.BlueSlime" }
  | -3 => some { netId := -3, lifeMax := 14, damage := 6, defense := 0, knockBackResist := Float32.ofBits 1067030938, value := Float32.ofBits 1077936128, scale := Float32.ofBits 1063675494, alpha := 175, color := ⟨0, 220, 40, 100⟩, width := 21, height := 17, name := "NPCName.GreenSlime" }
  | -7 => some { netId := -7, lifeMax := 40, damage := 12, defense := 6, knockBackResist := Float32.ofBits 1063675494, value := Float32.ofBits 1092616192, scale := Float32.ofBits 1067030938, alpha := 175, color := ⟨200, 0, 255, 150⟩, width := 28, height := 21, name := "NPCName.PurpleSlime" }
  | -8 => some { netId := -8, lifeMax := 35, damage := 12, defense := 4, knockBackResist := Float32.ofBits 1065353216, value := Float32.ofBits 1090519040, scale := Float32.ofBits 1065562931, alpha := 175, color := ⟨255, 30, 0, 100⟩, width := 24, height := 18, name := "NPCName.RedSlime" }
  | -9 => some { netId := -9, lifeMax := 45, damage := 15, defense := 7, knockBackResist := Float32.ofBits 1065353216, value := Float32.ofBits 1092616192, scale := Float32.ofBits 1067030938, alpha := 175, color := ⟨255, 255, 0, 100⟩, width := 28, height := 21, name := "NPCName.YellowSlime" }
  | _ => none

/-- `NPC.NewNPC(source, x, y, -3, 0, ai0, ai1, ai2, ai3, target)`: `SetDefaults(-3)` (the Green
Slime constants after `SetDefaults(1)` with scale `0.9`), `Bottom = (x, y)`,
`timeLeft = (int)(activeTime * 1.25)`. `NewNPC` does not draw from `Main.rand` here. -/
def spawn (world : World) (x y : Int) (ai0 ai1 ai2 ai3 : Float32 := 0) (target : Int := 255)
    (netId : Int := -3) : Slime :=
  let base : Slime := { position := ⟨0, 0⟩ }
  -- Another ordinary slime: its own SetDefaults fields (the Green Slime's are the defaults above).
  let base := match variant? netId with
    | some v => { base with
        netId, life := v.lifeMax, lifeMax := v.lifeMax, damage := v.damage, defDamage := v.damage
        defense := v.defense, defDefense := v.defense, knockBackResist := v.knockBackResist, value := v.value
        scale := v.scale, alpha := v.alpha, color := v.color, width := v.width, height := v.height }
    | none => base
  { base with
    position := ⟨single x - single base.width / 2, single y - single base.height⟩
    timeLeft := (Float.ofInt world.activeTime * 1.25).toInt64.toInt
    ai0, ai1, ai2, ai3, target }

/-! ## Tile queries -/

private def tileAt (grid : Grid) (x y : Int) : Tile :=
  if x < 0 || y < 0 then default else grid.tileAt x.toNat y.toNat

private def clampInt (value low high : Int) : Int := max low (min high value)

/-- `Collision.SolidCollision(Vector2, int, int)`: solid, not solid-top, not actuated. -/
def solidCollision (grid : Grid) (pos : Vector) (width height : Nat) : Bool := Id.run do
  let w := single width
  let h := single height
  let x0 := clampInt (tileIndex pos.x - 1) 0 (grid.width - 1)
  let x1 := clampInt (tileIndex (pos.x + w) + 2) 0 (grid.width - 1)
  let y0 := clampInt (tileIndex pos.y - 1) 0 (grid.height - 40)
  let y1 := clampInt (tileIndex (pos.y + h) + 2) 0 (grid.height - 40)
  let mut x := x0
  while x < x1 do
    let mut y := y0
    while y < y1 do
      let tile := tileAt grid x y
      if let some id := tile.id then
        let rules := grid.rules id
        if !tile.actuated && rules.solid && !rules.solidTop then
          let left := single (x * 16)
          let half := tile.shape == .halfBlock
          let top := single (y * 16) + (if half then 8 else 0)
          let tall : Float32 := if half then 8 else 16
          if pos.x + w > left && pos.x < left + 16 && pos.y + h > top && pos.y < top + tall then
            return true
      y := y + 1
    x := x + 1
  return false

/-- Painted solid tiles under the slime would recolour it; the port does not model paint. -/
private def paintedTileBelow (grid : Grid) (n : Slime) : Bool := Id.run do
  let y := tileIndex (n.position.y + single n.height + 1)
  let mut x := tileIndex n.left.x
  while single x ≤ n.right.x / 16 do
    if (tileAt grid x y).tilePaint != 0 then return true
    x := x + 1
  return false

/-! ## One frame -/

/-- Everything a frame returns besides the slime itself. -/
structure Step where
  slime : Slime
  rng : Rng
  /-- The static `NPC.gravity` after the frame (`none`: an inactive slime leaves it unchanged). -/
  gravity : Option Float32
  /-- `Player.nearbyActiveNPCs` increments from `CheckActive`, by player index. -/
  nearby : Array (Nat × Float32) := #[]
  /-- A path this port does not transcribe was reached. -/
  unsupported : Option String := none
  deriving Repr

/-- `UpdateNPC_UpdateGravity` for a dry Green Slime: `(gravity, maxFallSpeed)`. -/
def gravityFor (world : World) (n : Slime) : Float32 × Float32 :=
  let width := single world.grid.width / 4200
  let width := width * width
  let depth := n.position.y / 16 - (60 + 10 * width)
  let factor := (depth.toFloat / (world.worldSurface / 6)).toFloat32
  let factor := if factor.toFloat < 0.25 then 0.25 else factor
  let factor := if factor > 1 then 1 else factor
  (0.3 * factor, 10)

/-- `NPC.TargetClosest(faceTarget)` with `TryTrackingTarget` and `SetTargetTrackingValues`. -/
def targetClosest (players : Array Player) (n : Slime) (faceTarget : Bool := true) : Slime := Id.run do
  let mut n := n
  let mut found := false
  let mut best : Float32 := 0
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.dead || p.ghost || p.index ≥ 255 then continue
    let c := p.center
    let own := n.center
    let distance := (c.x - own.x).abs + (c.y - own.y).abs
    let mut score := distance - single p.aggro
    if p.noAggro && n.direction != 0 then score := score + 1000
    if !found || score < best then
      found := true
      best := score
      n := { n with target := p.index }
  if n.target < 0 || n.target ≥ 255 then n := { n with target := 0 }
  let p := playerAt players n.target
  let rect := (p.position.x.toInt32.toInt, p.position.y.toInt32.toInt, (p.width : Int), (p.height : Int))
  n := { n with targetRect := rect }
  let mut face := faceTarget
  if p.dead then face := false
  if p.noAggro && n.direction != 0 then face := false
  if face then
    let oldValid := 0 ≤ n.oldTarget && n.oldTarget ≤ 254
    let sneaking := p.itemAnimation == 0 && p.aggro < 0
    if !(oldValid && sneaking) then
      let (rx, ry, rw, rh) := rect
      n := { n with direction := if single (rx + rw / 2) < n.position.x + single ((n.width : Int) / 2) then -1 else 1 }
      n := { n with directionY := if single (ry + rh / 2) < n.position.y + single ((n.height : Int) / 2) then -1 else 1 }
  if (n.direction != n.oldDirection || n.directionY != n.oldDirectionY || n.target != n.oldTarget) &&
      !n.collideX && !n.collideY then
    n := { n with netUpdate := true }
  return n

/-- `AI_001_Slimes_GenerateItemInsideBody(false)` in a normal (not skyblock) single-player world. -/
def generateItemInsideBody (rng : Rng) : Int × Rng :=
  let (kind, rng) := rng.next 4
  if kind == 0 then
    let (k, rng) := rng.next 7
    (if k == 0 then 290 else if k == 1 then 292 else if k == 2 then 296 else if k == 3 then 2322 else 2350, rng)
  else if kind == 1 then
    let (k, rng) := rng.next 4
    (if k == 0 then 8 else if k == 1 then 965 else if k == 2 then 166 else 58, rng)
  else if kind == 2 then
    let (k, rng) := rng.next 2
    if k == 0 then rng.nextRange 11 15 else rng.nextRange 699 703
  else
    let (k, rng) := rng.next 3
    (if k == 0 then 71 else if k == 1 then 72 else 73, rng)

/-- The per-item colour pulled by `AI_001_Slimes` (`IL_09d5`–`IL_154f`): each frame the colour
moves 1/20 of the way to this target. Torch (`8`) only tints in `getGoodWorld`. -/
def itemTint (item : Int) : Option Color :=
  match item with
  | 751 => some ⟨185, 235, 255, 100⟩ | 2 => some ⟨169, 125, 93, 150⟩ | 3 => some ⟨128, 128, 128, 150⟩
  | 9 => some ⟨229, 149, 93, 150⟩ | 150 => some ⟨193, 202, 203, 100⟩ | 3086 => some ⟨59, 28, 136, 150⟩
  | 3081 => some ⟨138, 145, 170, 150⟩ | 147 => some ⟨189, 159, 139, 150⟩ | 29 => some ⟨220, 36, 117, 125⟩
  | 3609 => some ⟨232, 172, 162, 150⟩ | 3736 => some ⟨255, 66, 152, 125⟩ | 3737 => some ⟨179, 132, 255, 125⟩
  | 3738 => some ⟨0, 206, 180, 125⟩ | 1345 => some ⟨200, 200, 200, 50⟩ | 5395 => some ⟨198, 72, 12, 150⟩
  | 1124 => some ⟨255, 158, 49, 150⟩ | 1125 => some ⟨255, 156, 12, 150⟩ | 314 => some ⟨79, 163, 241, 75⟩
  | 364 => some ⟨37, 118, 172, 100⟩ | 1104 => some ⟨227, 52, 14, 100⟩ | 365 => some ⟨22, 119, 125, 100⟩
  | 1105 => some ⟨205, 30, 199, 100⟩ | 366 => some ⟨173, 15, 13, 100⟩ | 1106 => some ⟨91, 90, 119, 100⟩
  | _ => none

private def blend (c t : Color) : Color :=
  ⟨(c.r * 19 + t.r) / 20, (c.g * 19 + t.g) / 20, (c.b * 19 + t.b) / 20, (c.a * 19 + t.a) / 20⟩

/-- Items whose in-body behaviour is transcribed (`751` cloud, `2` dirt, `3` stone, `9` wood,
`3086` granite, `3081` marble) or that have none on these paths. -/
private def itemSupported (item : Int) : Bool :=
  !([539, 150, 3609, 364, 1104, 365, 1105, 366, 1106, 147, 5395, 1124, 1125, 314, 3736, 3737, 3738,
    1345, 1103, 593, 3347, 29, 174].contains item)

/-- `AI_001_Slimes` for `type = 1`, `netID = -3`. Returns `none` in `unsupported` when a path
outside the port is reached. -/
def ai (world : World) (players : Array Player) (gravity : Float32) (rng : Rng) (n : Slime) :
    Slime × Rng × Option String := Id.run do
  let grid := world.grid
  let mut n := n
  let mut rng := rng
  if n.ai1 == 75 then return (n, rng, some "ai[1] = 75 (remix light slime)")
  -- First update: roll the item inside the body (`SlimeCanContainItems[1]`, value > 0).
  if n.ai1 == 0 && n.value > 0 then
    n := { n with ai1 := -1 }
    for _ in [:(if world.slimeRain then 3 else 1)] do
      if n.ai1 != -1 then continue
      let trapRoll : Int := if n.center.y.toFloat < world.worldSurface * 16 then -1 else 500
      let (first, r) := rng.next 20
      rng := r
      if first == 0 then
        if n.ai0 == -999 then return (n, rng, some "item roll with ai[0] = -999")
        let (item, r) := generateItemInsideBody rng
        rng := r
        n := { n with ai1 := single item }
      else
        let (second, r) := rng.next 40
        rng := r
        if second == 0 then
          let row := (n.position.y / 16).toFloat
          if row > world.worldSurface then
            let rowIndex := (n.position.y / 16).toInt32.toInt
            if !(Float.ofInt rowIndex > world.rockLayer) then
              let (k, r) := rng.next 3
              rng := r
              n := { n with ai1 := if k == 0 then 2 else if k == 1 then 3 else 9 }
            else
              let (k, r) := rng.next 10
              rng := r
              if k == 0 then n := { n with ai1 := 3609 }
              else
                let (k, r) := rng.next 4
                rng := r
                n := { n with ai1 := if k == 0 then 3 else if k == 1 then 150 else if k == 2 then 3086 else 3081 }
          else n := { n with ai1 := 751 }
        else if trapRoll > 0 then
          let (k, r) := rng.next trapRoll.toNat
          rng := r
          if k == 0 then n := { n with ai1 := 539 }
    n := { n with netUpdate := true }
  -- Colour: paint under the slime (not modelled), otherwise the item tint.
  if paintedTileBelow grid n then return (n, rng, some "painted tile under the slime")
  if n.ai1 > 0 then
    if let some tint := itemTint n.ai1.toInt32.toInt then n := { n with color := blend n.color tint }
  -- In-body item behaviour.
  if n.ai1 > 0 then
    let item := n.ai1.toInt32.toInt
    if !itemSupported item then return (n, rng, some s!"item {item} inside the slime")
    if item == 2 then
      n := { n with givenName := some "SpecialNPCName.DirtSlime" }
      if n.velocity.y == 0 then n := { n with ai0 := n.ai0 + 9 }
    else if item == 9 then
      n := { n with givenName := some "SpecialNPCName.WoodSlime", defense := n.defDefense + 16 }
    else if item == 3 then
      if n.velocity.y != 0 then
        n := { n with givenName := some "SpecialNPCName.StoneSlime" }
        if n.velocity.y > 0 then n := { n with velocity.y := n.velocity.y + gravity * 2 }
    else if item == 751 then
      if n.velocity.y != 0 then
        n := { n with givenName := some "SpecialNPCName.CloudSlime", velocity.y := n.velocity.y - gravity * 0.6 }
    else if item == 3086 then
      n := { n with givenName := some "SpecialNPCName.GraniteSlime", defense := n.defDefense + 16, knockBackResist := 0 }
    else if item == 3081 then
      n := { n with givenName := some "SpecialNPCName.MarbleSlime", defense := n.defDefense + 16, knockBackResist := 0 }
  if n.direction == 0 then n := { n with direction := 1, netUpdate := true }
  if n.ai0 == -999 then
    return ({ n with frameY := 0, frameCounter := 0, rotation := 0 }, rng, none)
  -- Day, full health and above the surface line keep the slime passive.
  let aggressive := !(world.dayTime && n.life == n.lifeMax &&
    !(n.position.y.toFloat > world.worldSurface * 16)) || world.slimeRain
  if n.ai2 > 1 then n := { n with ai2 := n.ai2 - 1 }
  if n.wet then return (n, rng, some "wet slime")
  n := { n with aiAction := 0 }
  if n.ai2 == 0 then
    n := { n with ai0 := -100, ai2 := 1 }
    n := targetClosest players n
  if n.velocity.y == 0 then
    if n.collideY && n.oldVelocity.y != 0 && solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if n.ai3 == n.position.x then
      n := { n with direction := -n.direction, ai2 := 200 }
    n := { n with ai3 := 0 }
    n := { n with velocity.x := n.velocity.x * 0.8 }
    if n.velocity.x.toFloat > -0.1 && n.velocity.x.toFloat < 0.1 then n := { n with velocity.x := 0 }
    if aggressive then n := { n with ai0 := n.ai0 + 1 }
    n := { n with ai0 := n.ai0 + 1 }
    let threshold : Float32 := -1000
    let mut jump : Nat := 0
    if n.ai0 ≥ 0 then jump := 1
    if n.ai0 ≥ threshold && n.ai0 ≤ threshold * 0.5 then jump := 2
    if n.ai0 ≥ threshold * 2 && n.ai0 ≤ threshold * 1.5 then jump := 3
    if jump > 0 then
      n := { n with netUpdate := true }
      if aggressive && n.ai2 == 1 then n := targetClosest players n
      if jump == 3 then
        n := { n with velocity.y := -8, velocity.x := n.velocity.x + single (3 * n.direction), ai0 := -200,
                      ai3 := n.position.x }
      else
        n := { n with velocity.y := -6, velocity.x := n.velocity.x + single (2 * n.direction), ai0 := -120 }
        n := { n with ai0 := if jump == 1 then n.ai0 + threshold else n.ai0 + threshold * 2 }
    else if n.ai0 ≥ -30 then
      n := { n with aiAction := 1 }
  else if n.target < 255 && ((n.direction == 1 && n.velocity.x < 3) || (n.direction == -1 && n.velocity.x > -3)) then
    if n.collideX && n.velocity.x.abs == 0.2 then
      n := { n with position.x := n.position.x - 1.4 * single n.direction }
    if n.collideY && n.oldVelocity.y != 0 && solidCollision grid n.position n.width n.height then
      n := { n with position.x := n.position.x - (n.velocity.x + single n.direction) }
    if (n.direction == -1 && n.velocity.x.toFloat < 0.01) || (n.direction == 1 && n.velocity.x.toFloat > -0.01) then
      n := { n with velocity.x := n.velocity.x + 0.2 * single n.direction }
    else
      n := { n with velocity.x := n.velocity.x * 0.93 }
  return (n, rng, none)

/-- `NPC.UpdateCollision` for a dry slime: `Collision_WalkDownSlopes`, `ApplyTileCollision`
(`Collision.TileCollision` with `fallThrough = fall2 = false` and the hoik rule),
`Collision_MoveWhileDry` and `Collision_MoveSlopesAndStairFall(false)`. -/
def updateCollision (grid : Grid) (gravity : Float32) (n : Slime) : Slime := Id.run do
  let mut n := n
  n := { n with velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height gravity }
  n := { n with oldVelocity := n.velocity, collideX := false, collideY := false }
  let hit := OriginalCollision.tileCollision grid n.position n.velocity n.width n.height { hoik := true }
  n := { n with velocity := hit.displacement }
  if hit.hitAbove then n := { n with velocity.y := 0.01 }
  if n.oldVelocity.x != n.velocity.x then n := { n with collideX := true }
  if n.oldVelocity.y != n.velocity.y then n := { n with collideY := true }
  n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  let slope := OriginalCollision.slopeCollision grid n.position n.velocity n.width n.height n.stairFall
  n := { n with stairFall := slope.stairFall }
  if slope.stair && (slope.position.y - n.position.y).abs > 8 then
    n := { n with gfxOffY := n.gfxOffY - (slope.position.y - n.position.y), stepSpeed := 2 }
  n := { n with position := slope.position + ⟨0, 0⟩, velocity := slope.velocity }
  return n

/-- The slime branch of `NPC.FindFrame`. -/
def findFrame (world : World) (n : Slime) : Slime := Id.run do
  let motion : Nat :=
    if n.aiAction == 0 then
      if n.velocity.y < 0 then 2 else if n.velocity.y > 0 then 3 else if n.velocity.x != 0 then 1 else 0
    else if n.aiAction == 1 then 4 else 0
  let mut n := { n with frameCounter := n.frameCounter + 1 }
  if motion > 0 then n := { n with frameCounter := n.frameCounter + 1 }
  if motion == 4 then n := { n with frameCounter := n.frameCounter + 1 }
  if n.frameCounter ≥ 8 then n := { n with frameY := n.frameY + world.frameHeight, frameCounter := 0 }
  if n.frameY ≥ world.frameHeight * world.frameCount then n := { n with frameY := 0 }
  return n

private structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int

private def Rect.intersects (a b : Rect) : Bool :=
  b.x < a.x + a.w && a.x < b.x + b.w && b.y < a.y + a.h && a.y < b.y + b.h

/-- `NPC.CheckActive`: players in the active range keep the slime, players near the screen
rectangle refresh `timeLeft`, and `nearbyActiveNPCs` grows by `npcSlots`. -/
def checkActive (world : World) (players : Array Player) (n : Slime) : Slime × Array (Nat × Float32) := Id.run do
  let mut n := n
  let cx := n.position.x + single ((n.width : Int) / 2)
  let cy := n.position.y + single ((n.height : Int) / 2)
  let range : Rect := ⟨(cx - single world.activeRangeX).toInt32.toInt, (cy - single world.activeRangeY).toInt32.toInt,
    world.activeRangeX * 2, world.activeRangeY * 2⟩
  let screen : Rect := ⟨(cx.toFloat - Float.ofInt world.screenWidth * 0.5 - Float.ofNat n.width).toInt64.toInt,
    (cy.toFloat - Float.ofInt world.screenHeight * 0.5 - Float.ofNat n.height).toInt64.toInt,
    world.screenWidth + n.width * 2, world.screenHeight + n.height * 2⟩
  let mut keep := false
  let mut nearby := #[]
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.index ≥ 255 then continue
    let box : Rect := ⟨p.position.x.toInt32.toInt, p.position.y.toInt32.toInt, p.width, p.height⟩
    if range.intersects box then
      keep := true
      if n.lifeMax > 0 then nearby := nearby.push (p.index, n.npcSlots)
    if screen.intersects box then
      n := { n with timeLeft := world.activeTime, despawnEncouraged := false }
  n := { n with timeLeft := n.timeLeft - 1 }
  if n.timeLeft ≤ 0 then keep := false
  if !keep then n := { n with active := false }
  return (n, nearby)

/-- One `NPC.UpdateNPC(i)` call for an active, dry Green Slime. -/
def update (world : World) (players : Array Player) (rng : Rng) (n : Slime) : Step := Id.run do
  let (gravity, maxFall) := gravityFor world n
  if !n.active then return { slime := n, rng, gravity := none }
  let mut n := n
  if n.soundDelay > 0 then n := { n with soundDelay := n.soundDelay - 1 }
  if n.life ≤ 0 then
    return { slime := { n with active := false, netUpdate := false, justHit := false }, rng, gravity := some gravity }
  n := { n with oldTarget := n.target, oldDirection := n.direction, oldDirectionY := n.directionY }
  let ease := 1 + n.velocity.x.abs / 3
  if n.gfxOffY > 0 then
    n := { n with gfxOffY := n.gfxOffY - ease * n.stepSpeed }
    if n.gfxOffY < 0 then n := { n with gfxOffY := 0 }
  else if n.gfxOffY < 0 then
    n := { n with gfxOffY := n.gfxOffY + ease * n.stepSpeed }
    if n.gfxOffY > 0 then n := { n with gfxOffY := 0 }
  if n.gfxOffY > 16 then n := { n with gfxOffY := 16 }
  if n.gfxOffY < -16 then n := { n with gfxOffY := -16 }
  let (next, rng, unsupported) := ai world players gravity rng n
  if unsupported.isSome then return { slime := next, rng, gravity := some gravity, unsupported }
  n := next
  n := { n with immune := n.immune.map fun t => if t > 0 then t - 1 else t }
  n := { n with velocity.y := n.velocity.y + gravity }
  if n.velocity.y > maxFall then n := { n with velocity.y := maxFall }
  if n.velocity.x.toFloat < 0.005 && n.velocity.x.toFloat > -0.005 then n := { n with velocity.x := 0 }
  n := updateCollision world.grid gravity n
  n := findFrame world n
  let (checked, nearby) := checkActive world players n
  n := { checked with netUpdate := false, justHit := false }
  return { slime := n, rng, gravity := some gravity, nearby }

end Lean4Terria.OriginalGreenSlime
