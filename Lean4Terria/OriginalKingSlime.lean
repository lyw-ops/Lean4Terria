import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalRecipeCraft
import Lean4Terria.OriginalStarterMelee.LineOfSight
import Lean4Terria.OriginalEyeOfCthulhu

/-!
# King Slime (Terraria 1.4.5.8, desktop, Classic, single player)

Hand transcription of the desktop CIL (Terraria.exe `d52c7ea7…3e0`) for:

* the Slime Crown (item 560): `Player.SummonItemCheck` and the item-560 branch of
  `Player.ItemCheck_UseBossSpawners` (no time-of-day test);
* the natural spawns' callee `NPC.SpawnOnPlayer(plr, 50)` (generic search plus the type-50 sky and
  line-of-sight tests) and `NPC.DoDeathEvents_AdvanceSlimeRain` (the slime-rain kill counter); the
  surface outer-third roll of `Spawner.SpawnAnNPC` is given as `surfaceRoll`;
* one `NPC.UpdateNPC(i)` for King Slime (`AI_015_KingSlime`): life-scaled size, jumps, the
  out-of-reach teleport (`AI_015_KingSlime_FindTeleportSpot`, `BuildKingSlimeTeleportCache`), the
  far / dead-target despawn teleport, Blue Slime births every 5 % of life, dust, crown gore,
  `FindFrame`, gravity and tile collision (the released `OriginalGreenSlime` shell), `CheckActive`;
* `NPC.checkDead` for King Slime: death sound request, `NPCLoot` (Classic rules: trophy, saddle, mask,
  one ninja piece, hook or slime gun, solidifier, staff), `DoDeathEvents` (stop slime rain, the first
  town slime `ViolentlySpawnNerdySlime`, `downedSlimeKing`, potions and hearts, announcement), coins.

Blue Slimes (type 1) born here are ordinary `OriginalGreenSlime` actors (`netId 1`); their updates
are `OriginalGreenSlime.update`. Requests to the caller reuse the Eye of Cthulhu types (`Sound`, `Chat`,
`SlotView`, `ItemDrop`, `Effects`).
-/

namespace Lean4Terria.OriginalKingSlime

open OriginalCollision (Vector single Grid)
open OriginalGreenSlime (Slime)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle)
open OriginalEyeOfCthulhu (Actor PlayerView Sound Chat SlotView ItemDrop Gore Effects Rect playerAt availableSlot)

def kingType : Nat := 50
def blueSlimeType : Nat := 1
def townSlimeType : Nat := 670
def summonItem : Nat := 560
/-- Desktop `NPC_50` frame height (720 / 6). -/
def kingFrameHeight : Int := 120
/-- `NPC.kingSlimePointCacheSizeMax`. -/
def teleportCacheMax : Nat := 50

private def i32 (x : Float32) : Int := x.toInt32.toInt
private def sqrtf (x : Float32) : Float32 := (Float.sqrt x.toFloat).toFloat32
private def f0_1 : Float32 := 0.10000000149011612
private def f0_8 : Float32 := 0.800000011920929
private def f0_6 : Float32 := 0.6000000238418579
private def f0_4 : Float32 := 0.4000000059604645
private def f0_2 : Float32 := 0.20000000298023224
private def f0_93 : Float32 := 0.9300000071525574
private def f1_2 : Float32 := 1.2000000476837158
private def f0_05 : Float32 := 0.05000000074505806

/-- King Slime: the shared actor plus `hide` (= `dontTakeDamage`, set together each AI call). -/
structure King where
  actor : Actor
  hide : Bool := false
  deriving Repr

instance : Inhabited King := ⟨{ actor := default }⟩

/-- `SetDefaults(50)`: base 98 × 92 at scale 1.25 (native survey). -/
def kingDefaults : Actor :=
  { typeId := kingType, boss := true,
    body := { position := ⟨0, 0⟩, width := 122, height := 115, life := 2000, lifeMax := 2000, damage := 40,
              defense := 10, defDamage := 40, defDefense := 10, knockBackResist := 0, value := 10000,
              npcSlots := 5, scale := 1.25, alpha := 30, timeLeft := 750, netUpdate := true,
              color := ⟨0, 0, 0, 0⟩, netId := 50 } }

/-! ## Frame-shared helpers (same CIL calls as the Eye module) -/

private def next (fx : Effects) (lo hi : Int) : Int × Effects :=
  let (v, r) := fx.random.next lo hi
  (v, { fx with random := r })

private def sound (fx : Effects) (env : OriginalEyeOfCthulhu.Env) (s : Sound) : Effects :=
  { fx with sounds := fx.sounds.push s, random := env.playSound s fx.random }

/-- `Dust.NewDust`, then `noGravity = true` and `velocity *= k` on the created particle. -/
private def dust (fx : Effects) (req : DustRequest) (k : Float32) : Effects :=
  let (pool, particle, random) := fx.dust.create req fx.random
  match particle with
  | some p =>
    let p2 := { p with noGravity := true, velocity := ⟨p.velocity.x * k, p.velocity.y * k⟩ }
    { fx with dust := pool, random := random, particles := fx.particles.push p2 }
  | none => { fx with dust := pool, random }

/-- `Color(r, g, b, a).PackedValue`. -/
private def packed (r g b a : Nat) : UInt32 :=
  r.toUInt32 ||| (g.toUInt32 <<< 8) ||| (b.toUInt32 <<< 16) ||| (a.toUInt32 <<< 24)

/-- The teleport dust: `NewDust(position + UnitX * -20, width + 40, height, 4, velocity, 150, (78,136,255,80), 2)`. -/
private def teleportDust (n : Slime) : DustRequest :=
  let pos : Vector := ⟨n.position.x + (-20), n.position.y + (-0)⟩
  let base : DustRequest := { position := pos, width := n.width + 40, height := n.height, typeId := 4 }
  { base with speed := n.velocity, alpha := 150, color := packed 78 136 255 80, scale := 2 }

/-- The body dust: `NewDust(position, width, height, 4, velocity, 255, (0,80,255,80), scale * 1.2)`. -/
private def bodyDust (n : Slime) (scale : Float32) : DustRequest :=
  let base : DustRequest := { position := n.position, width := n.width, height := n.height, typeId := 4 }
  { base with speed := n.velocity, alpha := 255, color := packed 0 80 255 80, scale := scale }

private def newGore (fx : Effects) (worldW worldH : Nat) (typeId : Nat) (position velocity : Vector) : Effects :=
  let slot := (fx.goreOccupied.findIdx? (!·)).getD fx.goreOccupied.size
  if slot ≥ 600 then fx else
  let (up, random) := fx.random.next 10 31
  let (side, random) := random.next (-20) 21
  let g : Gore := { slot, typeId, position, velocity := ⟨velocity.x + single side * f0_1, velocity.y - single up * f0_1⟩ }
  let x := i32 position.x / 16
  let y := i32 position.y / 16
  let inside := 0 ≤ x && x < (worldW : Int) && 0 ≤ y && y < (worldH : Int)
  let occupied := fx.goreOccupied ++ Array.replicate (slot + 1 - fx.goreOccupied.size) false
  { fx with random, goreOccupied := occupied.set! slot inside, gores := if inside then fx.gores.push g else fx.gores }

private def netUpdateIgnoreSpamLimit (a : Actor) : Actor :=
  let n := if a.boss then OriginalEyeOfCthulhu.netSpamTicksBoss else OriginalEyeOfCthulhu.netSpamTicks
  let a := if !a.body.netUpdate || a.netSpam > n * OriginalEyeOfCthulhu.netSpamLimit then { a with netSpam := a.netSpam - n } else a
  { a with body.netUpdate := true }

/-- `Entity.Center`: `position + ((float)width / 2f, (float)height / 2f)` (float division). -/
private def center (n : Slime) : Vector :=
  ⟨n.position.x + single (n.width : Int) / 2, n.position.y + single (n.height : Int) / 2⟩

private def pcenter (p : PlayerView) : Vector :=
  ⟨p.position.x + single (p.width : Int) / 2, p.position.y + single (p.height : Int) / 2⟩

private def distance (a b : Vector) : Float32 :=
  let dx := a.x - b.x
  let dy := a.y - b.y
  sqrtf (dx * dx + dy * dy)

private def clamp01 (x : Float32) : Float32 := if x > 1 then 1 else if x < 0 then 0 else x

/-! ## Tiles -/

/-- `Main.tile[x, y]` reads of this module, from the collision grid (`rules`: `tileSolid`,
`tileSolidTop`; `platform` = `TileID.Sets.Platforms`). -/
private def tileAt (grid : Grid) (x y : Int) : Option OriginalWorld.Tile :=
  if 0 ≤ x && x < grid.width && 0 ≤ y && y < grid.height then some (grid.tileAt x.toNat y.toNat) else none

/-- `tile.nactive() && Main.tileSolid[type]`. -/
def nactiveSolid (grid : Grid) (x y : Int) : Bool :=
  match tileAt grid x y with
  | some t => match t.id with
    | some id => !t.actuated && (grid.rules id).solid
    | none => false
  | none => false

/-- `WorldGen.SolidTile(Tile)`: a missing cell counts as solid. -/
private def solidTileCell (grid : Grid) (x y : Int) : Bool :=
  match tileAt grid x y with
  | none => true
  | some t => match t.id with
    | some id => let r := grid.rules id
      !t.actuated && r.solid && !r.solidTop && t.shape == .full
    | none => false

private def lavaAt (grid : Grid) (x y : Int) : Bool :=
  match tileAt grid x y with
  | some t => match t.liquid with
    | some l => l.kind == .lava && l.amount > 0
    | none => false
  | none => false

def spawnTilesOf (grid : Grid) (houseWall : UInt16 → Bool) : OriginalEyeOfCthulhu.SpawnTiles :=
  { nactiveSolid := nactiveSolid grid
    houseWall := fun x y => match tileAt grid x y with | some t => houseWall t.wall | none => false
    lava := lavaAt grid }

/-! ## Spawning -/

/-- `NPC.SpawnOnPlayer(plr, 50)`: the generic search with the King Slime extras — on early passes a
spot under a wall (the original reads `Main.tile[y, y].wall`, x replaced by y) is skipped, and on the
first 400 passes the spot must see 20 tiles up and, from there, the player (`Collision.CanHit`). -/
def spawnOnPlayer (env : OriginalEyeOfCthulhu.Env) (grid : Grid) (houseWall : UInt16 → Bool) (plr : Nat)
    (random : UnifiedRandom) : OriginalEyeOfCthulhu.SpawnResult := Id.run do
  let tiles := spawnTilesOf grid houseWall
  let p := playerAt env.players plr
  let (rect, safe, sx, sy) := OriginalEyeOfCthulhu.spawnArea p env.worldWidth env.worldHeight
  let mut random := random
  let mut flag := false
  let mut bx : Int := 0
  let mut by_ : Int := 0
  for i in [0:500] do
    let last := i == 499
    for _ in [0:50] do
      let (x, r1) := random.next rect.x (rect.x + rect.w)
      let (y, r2) := r1.next rect.y (rect.y + rect.h)
      random := r2
      if tiles.nactiveSolid x y then continue
      if tiles.houseWall x y && i < 499 then continue
      if i < 250 then
        let wall := (tileAt grid y y).map (·.wall) |>.getD 0
        if wall > 0 then continue
      let mut yy := y
      while yy < rect.y + rect.h do
        if tiles.nactiveSolid x yy then
          if last || !safe.contains x y then
            bx := x
            by_ := yy
            flag := true
            break
        yy := yy + 1
      if flag && i < 400 then
        let spot : OriginalStarterMelee.LineOfSight.Box := { position := ⟨single (bx * 16), single ((by_ - 1) * 16)⟩, width := 16, height := 16 }
        let sky : OriginalStarterMelee.LineOfSight.Box := { position := ⟨single (bx * 16), single ((by_ - 1 - 20) * 16)⟩, width := 16, height := 16 }
        let eye : OriginalStarterMelee.LineOfSight.Box := { position := pcenter p, width := 0, height := 0 }
        if !(OriginalStarterMelee.LineOfSight.canHit grid spot sky && OriginalStarterMelee.LineOfSight.canHit grid sky eye) then
          bx := 0
          by_ := 0
          flag := false
          continue
      if flag && !last && !OriginalEyeOfCthulhu.hasTileSpawnSpace tiles env.worldWidth env.worldHeight bx by_ then flag := false
      if flag then break
    if flag && !last && !OriginalEyeOfCthulhu.notSpawningOnScreen env.players sx sy bx by_ then continue
    if flag then break
  if !flag then return { random }
  -- NPC.SpawnBoss(x*16+8, y*16, 50, plr): NewNPC from slot 1, timeLeft *= 20, no awoken message for 50
  let slot := availableSlot env.slots 1
  if slot < 0 then return { random }
  let a := kingDefaults
  let body := { a.body with
    position := ⟨single (bx * 16 + 8) - single (a.body.width : Int) / 2, single (by_ * 16) - single (a.body.height : Int)⟩
    active := true, timeLeft := (Float.ofInt OriginalEyeOfCthulhu.activeTime * 1.25).toInt64.toInt * 20, target := plr }
  -- NewNPC itself announces type 50 (Announcement.HasAwoken); SpawnBoss skips it for 50
  return { random, eye := some (slot.toNat, { a with body, targetSetFrame := env.ticker }), chats := #[.hasAwoken kingType] }

/-- `Player.SummonItemCheck(item 560)`: false while any active NPC is King Slime. -/
def summonItemCheck (activeTypes : Array Nat) : Bool := !activeTypes.contains kingType

/-- Item-560 branch of `ItemCheck_UseBossSpawners` (no day / night test): `ApplyItemTime` (the
caller's), the roar request at the player's position, `SpawnOnPlayer(i, 50)`. -/
def useSummonItem (env : OriginalEyeOfCthulhu.Env) (grid : Grid) (houseWall : UInt16 → Bool) (plr : Nat)
    (random : UnifiedRandom) : OriginalEyeOfCthulhu.SummonResult :=
  let p := playerAt env.players plr
  let s := Sound.legacy 15 (i32 p.position.x) (i32 p.position.y) 0
  { applyItemTime := true, sound := some s, spawn := spawnOnPlayer env grid houseWall plr (env.playSound s random),
    clearsSpawnEye := false }

/-- `NPC.DoDeathEvents_AdvanceSlimeRain(killer)` for a killed NPC: only during slime rain, for a
`Main.slimeRainNPC` type, with no King Slime alive. Returns the new `Main.slimeRainKillCount` and
whether `SpawnOnPlayer(killer, 50)` is called (then the count becomes `-limit / 2`). -/
def advanceSlimeRain (slimeRain isRainNpc kingAlive downedSlimeKing : Bool) (kills : Int) : Int × Bool :=
  if !slimeRain || !isRainNpc || kingAlive then (kills, false) else
  let limit : Int := if downedSlimeKing then 75 else 150
  let kills := kills + 1
  if kills < limit then (kills, false) else ((-limit).tdiv 2, true)

/-- The surface outer-third King Slime roll inside `Spawner.SpawnAnNPC` (IL_a3cc): reached for a
dry surface spot further than `maxTilesX / 3` from the spawn, `arg3 == 2`; draws `Next(300)`; true
means `SpawnOnPlayer(plr, 50)` is called (`kingAlive` = `NPC.AnyNPCs(50)`). -/
def surfaceRoll (kingAlive : Bool) (random : UnifiedRandom) : Bool × UnifiedRandom :=
  let (roll, random) := random.next 0 300
  (roll == 0 && !kingAlive, random)

/-! ## AI -/

/-- `BuildKingSlimeTeleportCache(x, y, outer, inner)`: up to 50 standing tiles in the four bands. -/
private def teleportCache (grid : Grid) (height : Nat) (target : Vector) (x y outer inner : Int) : Array (Int × Int) := Id.run do
  let mut cache : Array (Int × Int) := #[]
  for (x1, x2, y1, y2) in #[(x - outer, x - inner, y - outer, y + outer), (x + inner, x + outer, y - outer, y + outer),
                            (x - inner, x + inner, y - outer, y - inner), (x - inner, x + inner, y + inner, y + outer)] do
    let mut i := x1
    while i ≤ x2 do
      let mut j := y1
      while j ≤ y2 do
        if let some t := tileAt grid i j then
          if let some id := t.id then
            let r := grid.rules id
            if !t.actuated && cache.size < teleportCacheMax && ((r.solid && !r.solidTop) || r.platform) &&
                !solidTileCell grid i (j - 1) && !lavaAt grid i j then
              let from_ : Vector := ⟨single (i * 16 + 8), single (j * 16 - (height : Int) / 2)⟩
              if OriginalStarterMelee.LineOfSight.canHitLine grid from_ 0 0 target 0 0 then
                cache := cache.push (i, j)
        j := j + 1
      i := i + 1
  return cache

/-- `AI_015_KingSlime_FindTeleportSpot(toPlayer, ref x, ref y)`. -/
private def findTeleportSpot (grid : Grid) (players : Array PlayerView) (n : Slime) (toPlayer : Bool)
    (x y : Float32) (fx : Effects) : Float32 × Float32 × Effects := Id.run do
  let closest := fun _ : Unit => Id.run do
    -- Player.FindClosest(position, width, height): first strictly smaller Manhattan centre distance
    -- among active living players, player 0 when none
    let mut best : Option (Float32 × PlayerView) := none
    let nx := n.position.x + single (n.width : Int) / 2
    let ny := n.position.y + single (n.height : Int) / 2
    for p in players.qsort (·.index < ·.index) do
      if !p.active || p.dead then continue
      let d := (p.position.x + single (p.width : Int) / 2 - nx).abs + (p.position.y + single (p.height : Int) / 2 - ny).abs
      match best with
      | some (b, _) => if d < b then best := some (d, p)
      | none => best := some (d, p)
    let p := (best.map (·.2)).getD (playerAt players 0)
    return (p.position.x + single (p.width : Int) / 2, p.position.y + single (p.height : Int))
  let _ := x
  let _ := y
  if toPlayer then
    let (bx, by_) := closest ()
    return (bx, by_, fx)
  let tp := playerAt players n.target
  let c := pcenter tp
  let tx := i32 c.x >>> 4
  let ty := i32 c.y >>> 4
  for (outer, inner) in #[((10 : Int), (7 : Int)), (6, 2)] do
    let cache := teleportCache grid n.height c tx ty outer inner
    if cache.size > 0 then
      let (k, fx) := next fx 0 cache.size
      let (cx, cy) := cache[k.toNat]!
      return (single (cx * 16 + 8), single (cy * 16), fx)
  let (bx, by_) := closest ()
  return (bx, by_, fx)

/-- Births requested by an AI call: Blue Slimes as `OriginalGreenSlime` bodies. -/
abbrev Births := Array (Nat × Slime)

/-- `AI_015_KingSlime`, Classic. `world` is the Blue Slimes' `OriginalGreenSlime.World`. -/
def kingAI (env : OriginalEyeOfCthulhu.Env) (world : OriginalGreenSlime.World) (k : King) (fx : Effects) :
    Except String (King × Effects × Births) := do
  let grid := world.grid
  let players := env.players.map PlayerView.toSlime
  let mut a := k.actor
  let mut fx := fx
  let mut births : Births := #[]
  let mut num : Float32 := 1
  let mut first := false
  let mut busy := false
  let mut hidden := false
  a := { a with body.aiAction := 0 }
  if a.body.ai3 == 0 && a.body.life > 0 then a := { a with body.ai3 := single a.body.lifeMax }
  if a.localAI[3]! == 0 then
    a := { a with localAI := a.localAI.set! 3 1 }
    first := true
    a := { a with body.ai0 := -100 }
    a := { a with body := OriginalGreenSlime.targetClosest players a.body }
    a := { a with body.netUpdate := true }
  let far := fun (a : Actor) =>
    let tp := playerAt env.players a.body.target
    tp.dead || distance (center a.body) (pcenter tp) > 3000
  if far a then
    a := { a with body := OriginalGreenSlime.targetClosest players a.body }
    if far a then
      let n := a.body
      let n := if n.timeLeft > 10 then { n with timeLeft := 10 } else n
      a := { a with body := { n with despawnEncouraged := true } }
      let tp := playerAt env.players a.body.target
      a := { a with body.direction := if (pcenter tp).x < (center a.body).x then 1 else -1 }
      if a.body.ai1 != 5 then
        a := { a with body.netUpdate := true, body.ai2 := 0, body.ai0 := 0, body.ai1 := 5 }
        a := { a with localAI := (a.localAI.set! 1 (single ((world.grid.width : Int) * 16))).set! 2 (single ((world.grid.height : Int) * 16)) }
  let tp0 := playerAt env.players a.body.target
  if !tp0.dead && a.body.timeLeft > 10 && a.body.ai2 >= 300 && a.body.ai1 < 5 && a.body.velocity.y == 0 then
    a := { a with body.ai2 := 0, body.ai0 := 0, body.ai1 := 5 }
    a := { a with body := OriginalGreenSlime.targetClosest players a.body false }
    let tp := playerAt env.players a.body.target
    let d : Vector := ⟨(pcenter tp).x - (center a.body).x, (pcenter tp).y - (center a.body).y⟩
    let mut toPlayer := false
    if a.localAI[0]! >= 360 || sqrtf (d.x * d.x + d.y * d.y) > 2000 then
      if a.localAI[0]! >= 360 then a := { a with localAI := a.localAI.set! 0 360 }
      toPlayer := true
    let (x, y, fx2) := findTeleportSpot grid env.players a.body toPlayer a.localAI[1]! a.localAI[2]! fx
    fx := fx2
    a := { a with localAI := (a.localAI.set! 1 x).set! 2 y }
  let tp := playerAt env.players a.body.target
  let c := center a.body
  let top := a.body.position.y
  let tpBottom := tp.position.y + single (tp.height : Int)
  if !OriginalStarterMelee.LineOfSight.canHitLine grid c 0 0 (pcenter tp) 0 0 || (top - tpBottom).abs > 160 then
    a := { a with body.ai2 := a.body.ai2 + 1, localAI := a.localAI.set! 0 (a.localAI[0]! + 1) }
  else
    let v := a.localAI[0]! - 1
    a := { a with localAI := a.localAI.set! 0 (if v < 0 then 0 else v) }
  if a.body.timeLeft < 10 && (a.body.ai0 != 0 || a.body.ai1 != 0) then
    a := { a with body.ai0 := 0, body.ai1 := 0, body.netUpdate := true }
    busy := false
  if a.body.ai1 == 5 then
    busy := true
    a := { a with body.aiAction := 1, body.ai0 := a.body.ai0 + 1 }
    num := clamp01 ((60 - a.body.ai0) / 60)
    num := 0.5 + num * 0.5
    if a.body.ai0 >= 60 then hidden := true
    if a.body.ai0 == 60 then
      let cc := center a.body
      fx := newGore fx env.worldWidth env.worldHeight 734 ⟨cc.x + (-40), cc.y + single (-((a.body.height : Int) / 2))⟩ a.body.velocity
    if a.body.ai0 >= 60 then
      let bx := a.localAI[1]!
      let by_ := a.localAI[2]!
      a := { a with body.position := ⟨bx - single (a.body.width : Int) / 2, by_ - single (a.body.height : Int)⟩ }
      a := { a with body.ai1 := 6, body.ai0 := 0, body.netUpdate := true }
    if !hidden then
      for _ in [0:10] do
        fx := dust fx (teleportDust a.body) 0.5
  else if a.body.ai1 == 6 then
    busy := true
    a := { a with body.aiAction := 0, body.ai0 := a.body.ai0 + 1 }
    num := clamp01 (a.body.ai0 / 30)
    num := 0.5 + num * 0.5
    if a.body.ai0 >= 30 then
      a := { a with body.ai1 := 0, body.ai0 := 0, body.netUpdate := true }
      a := { a with body := OriginalGreenSlime.targetClosest players a.body }
    for _ in [0:10] do
      fx := dust fx (teleportDust a.body) 2
  if a.body.velocity.y == 0 then
    let vx := a.body.velocity.x * f0_8
    a := { a with body.velocity.x := if vx > -f0_1 && vx < f0_1 then 0 else vx }
    if !busy then
      a := { a with body.ai0 := a.body.ai0 + 2 }
      let life := single a.body.life
      let lifeMax := single a.body.lifeMax
      if life < lifeMax * f0_8 then a := { a with body.ai0 := a.body.ai0 + 1 }
      if life < lifeMax * f0_6 then a := { a with body.ai0 := a.body.ai0 + 1 }
      if life < lifeMax * f0_4 then a := { a with body.ai0 := a.body.ai0 + 2 }
      if life < lifeMax * f0_2 then a := { a with body.ai0 := a.body.ai0 + 3 }
      if life < lifeMax * f0_1 then a := { a with body.ai0 := a.body.ai0 + 4 }
      if a.body.ai0 >= 0 then
        a := { a with body.netUpdate := true }
        a := { a with body := OriginalGreenSlime.targetClosest players a.body }
        let dir := single a.body.direction
        if a.body.ai1 == 3 then
          a := { a with body.velocity := ⟨a.body.velocity.x + 3.5 * dir, -13⟩, body.ai0 := -200, body.ai1 := 0 }
        else if a.body.ai1 == 2 then
          a := { a with body.velocity := ⟨a.body.velocity.x + 4.5 * dir, -6⟩, body.ai0 := -120, body.ai1 := a.body.ai1 + 1 }
        else
          a := { a with body.velocity := ⟨a.body.velocity.x + 4 * dir, -8⟩, body.ai0 := -120, body.ai1 := a.body.ai1 + 1 }
      else if a.body.ai0 >= -30 then
        a := { a with body.aiAction := 1 }
  else if a.body.target < 255 then
    let dir := a.body.direction
    let vx := a.body.velocity.x
    if (dir == 1 && vx < 3) || (dir == -1 && vx > -3) then
      if (dir == -1 && vx < f0_1) || (dir == 1 && vx > -f0_1) then
        a := { a with body.velocity.x := vx + f0_2 * single dir }
      else
        a := { a with body.velocity.x := vx * f0_93 }
  fx := dust fx (bodyDust a.body (a.body.scale * f1_2)) 0.5
  if a.body.life > 0 then
    let mut s := single a.body.life / single a.body.lifeMax
    s := s * 0.5 + 0.75
    s := s * num
    s := s * 1
    if s != a.body.scale || first then
      let n := a.body
      let px := n.position.x + single ((n.width : Int) / 2)
      let py := n.position.y + single (n.height : Int)
      let w := (98 * s).toInt32.toInt.toNat
      let h := (92 * s).toInt32.toInt.toNat
      a := { a with body := { n with scale := s, width := w, height := h,
                                     position := ⟨px - single ((w : Int) / 2), py - single (h : Int)⟩ } }
    let step := (single a.body.lifeMax * f0_05).toInt32.toInt
    if single (a.body.life + step) < a.body.ai3 then
      a := { a with body.ai3 := single a.body.life }
      let (count, fx2) := next fx 1 4
      fx := fx2
      let mut slots := env.slots
      for _ in [0:count.toNat] do
        let (dx, f1) := next fx 0 ((a.body.width : Int) - 32)
        let x := i32 (a.body.position.x + single dx)
        let (dy, f2) := next f1 0 ((a.body.height : Int) - 32)
        let y := i32 (a.body.position.y + single dy)
        fx := f2
        let slot := availableSlot slots 0
        let (vx, f3) := next fx (-15) 16
        let (vy, f4) := next f3 (-30) 1
        let (k, f5) := next f4 0 3
        fx := f5
        if slot ≥ 0 then
          let born := OriginalGreenSlime.spawn world x y (single (-1000 * k)) (-1) 0 0 255 (netId := 1)
          -- the explicit second SetDefaults(1) resets timeLeft to activeTime (NewNPC's 1.25x is lost)
          births := births.push (slot.toNat, { born with active := true, timeLeft := OriginalEyeOfCthulhu.activeTime, velocity := ⟨single vx * f0_1, single vy * f0_1⟩ })
          let taken := slot.toNat
          slots := { slots with inUse := fun i => i == taken || slots.inUse i }
  return ({ k with actor := a, hide := hidden }, fx, births)

/-! ## `FindFrame` and the `UpdateNPC` shell -/

/-- King Slime branch of `NPC.FindFrame` (motion from `aiAction` / velocity as for slimes). -/
def findFrame (a : Actor) (frameHeight : Int) : Actor := Id.run do
  let mut n := a.body
  let motion : Nat :=
    if n.aiAction == 0 then
      if n.velocity.y < 0 then 2 else if n.velocity.y > 0 then 3 else if n.velocity.x != 0 then 1 else 0
    else if n.aiAction == 1 then 4 else 0
  if n.velocity.y != 0 then
    if n.frameY < frameHeight * 4 then n := { n with frameY := frameHeight * 4, frameCounter := 0 }
    n := { n with frameCounter := n.frameCounter + 1 }
    if n.frameCounter >= 4 then n := { n with frameY := frameHeight * 5 }
  else
    if n.frameY >= frameHeight * 5 then n := { n with frameY := frameHeight * 4, frameCounter := 0 }
    n := { n with frameCounter := n.frameCounter + 1 }
    if motion > 0 then n := { n with frameCounter := n.frameCounter + 1 }
    if motion == 4 then n := { n with frameCounter := n.frameCounter + 1 }
    if n.frameCounter >= 8 then
      n := { n with frameY := n.frameY + frameHeight, frameCounter := 0 }
      if n.frameY >= frameHeight * 4 then n := { n with frameY := 0 }
  return { a with body := n }

/-- `NPC.UpdateCollision` for a dry NPC with `Collision_DecideFallThroughPlatforms` = `fall`
(`Collision_MoveNormal` → `TileCollision(…, fall, fall, 1, false, false, true)`, then
`Collision_MoveSlopesAndStairFall(fall)`). -/
def updateCollisionFall (grid : Grid) (gravity : Float32) (fall : Bool) (n : Slime) : Slime := Id.run do
  let mut n := n
  n := { n with velocity := OriginalCollision.walkDownSlope grid n.position n.velocity n.width n.height gravity }
  n := { n with oldVelocity := n.velocity, collideX := false, collideY := false }
  let hit := OriginalCollision.tileCollision grid n.position n.velocity n.width n.height { fallThrough := fall, fall2 := fall, hoik := true }
  n := { n with velocity := hit.displacement }
  if hit.hitAbove then n := { n with velocity.y := 0.01 }
  if n.oldVelocity.x != n.velocity.x then n := { n with collideX := true }
  if n.oldVelocity.y != n.velocity.y then n := { n with collideY := true }
  n := { n with oldPosition := n.position, oldDirection := n.direction, position := n.position + n.velocity }
  if fall then n := { n with stairFall := true }
  let slope := OriginalCollision.slopeCollision grid n.position n.velocity n.width n.height n.stairFall
  n := { n with stairFall := slope.stairFall || fall }
  if slope.stair && (slope.position.y - n.position.y).abs > 8 then
    n := { n with gfxOffY := n.gfxOffY - (slope.position.y - n.position.y), stepSpeed := 2 }
  n := { n with position := slope.position + ⟨0, 0⟩, velocity := slope.velocity }
  return n

structure Step where
  king : King
  fx : Effects
  births : Births := #[]
  nearby : Array (Nat × Float32) := #[]
  deactivated : Bool := false
  gravity : Option Float32 := none

/-- One `NPC.UpdateNPC(i)` for King Slime: the slime shell (gravity, tile collision) around `kingAI`. -/
def update (env : OriginalEyeOfCthulhu.Env) (world : OriginalGreenSlime.World) (frameHeight : Int) (k : King)
    (fx : Effects) : Except String Step := do
  let a0 := k.actor
  if !a0.body.active then return { king := k, fx }
  let (gravity, maxFall) := OriginalGreenSlime.gravityFor world a0.body
  let mut a := a0
  if a.body.soundDelay > 0 then a := { a with body.soundDelay := a.body.soundDelay - 1 }
  if a.body.life ≤ 0 then
    return { king := { k with actor := { a with body.active := false, body.netUpdate := false, body.justHit := false } }, fx,
             deactivated := true, gravity := some gravity }
  a := { a with body.oldTarget := a.body.target, body.oldDirection := a.body.direction, body.oldDirectionY := a.body.directionY }
  let ease := 1 + a.body.velocity.x.abs / 3
  if a.body.gfxOffY > 0 then
    a := { a with body.gfxOffY := a.body.gfxOffY - ease * a.body.stepSpeed }
    if a.body.gfxOffY < 0 then a := { a with body.gfxOffY := 0 }
  else if a.body.gfxOffY < 0 then
    a := { a with body.gfxOffY := a.body.gfxOffY + ease * a.body.stepSpeed }
    if a.body.gfxOffY > 0 then a := { a with body.gfxOffY := 0 }
  if a.body.gfxOffY > 16 then a := { a with body.gfxOffY := 16 }
  if a.body.gfxOffY < -16 then a := { a with body.gfxOffY := -16 }
  let (k2, fx2, births) ← kingAI env world { k with actor := a } fx
  a := k2.actor
  a := { a with body.immune := a.body.immune.map fun t => if t > 0 then t - 1 else t }
  -- WorldGen.IsTileLoaded at the centre tile ((int)(pos + size / 2) / 16): outside the world the
  -- static NPC.gravity becomes 0 and the velocity is zeroed (e.g. after the off-world despawn teleport)
  let tx := i32 (a.body.position.x + single ((a.body.width : Int) / 2)) / 16
  let ty := i32 (a.body.position.y + single ((a.body.height : Int) / 2)) / 16
  let loaded := 0 ≤ tx && tx < (world.grid.width : Int) && 0 ≤ ty && ty < (world.grid.height : Int)
  let gravity := if loaded then gravity else 0
  if !loaded then a := { a with body.velocity := ⟨0, 0⟩ }
  a := { a with body.velocity.y := a.body.velocity.y + gravity }
  if a.body.velocity.y > maxFall then a := { a with body.velocity.y := maxFall }
  if a.body.velocity.x.toFloat < 0.005 && a.body.velocity.x.toFloat > -0.005 then a := { a with body.velocity.x := 0 }
  -- Collision_DecideFallThroughPlatforms for type 50: the target stands below the bottom edge
  let tp := playerAt env.players a.body.target
  let fall := a.body.target ≥ 0 && tp.position.y > a.body.position.y + single (a.body.height : Int)
  a := { a with body := updateCollisionFall world.grid gravity fall a.body }
  a := findFrame a frameHeight
  let (a3, nearby, gone) := OriginalEyeOfCthulhu.checkActive env.players a
  a := { a3 with body.netUpdate := false, body.justHit := false }
  return { king := { k2 with actor := a }, fx := fx2, births, nearby, deactivated := gone, gravity := some gravity }

/-! ## Death -/

structure DeathEnv where
  loot : OriginalCombat.Loot.Context
  downedSlimeKing : Bool
  /-- `NPC.unlockedSlimeBlueSpawn` before the kill. -/
  unlockedTownSlime : Bool
  slimeRain : Bool
  quietGlobals : Bool := true
  slots : SlotView
  ticker : Int := 0
  /-- The frame's dust pool (viewport = screen): the town-slime particle burst draws through `Dust.NewDust`. -/
  dust : DustPool := {}
  playSound : Sound → UnifiedRandom → UnifiedRandom := fun _ r => r

/-- The first town slime (`ViolentlySpawnNerdySlime`): slot, NPC type 670 at `Bottom`, velocity. -/
structure TownSlimeBirth where
  slot : Nat
  bottom : Int × Int
  velocity : Vector
  deriving Repr

structure Death where
  king : King
  random : UnifiedRandom
  items : Array ItemDrop := #[]
  sounds : Array Sound := #[]
  chats : Array Chat := #[]
  downedSlimeKing : Bool
  firstClear : Bool := false
  unlockedTownSlime : Bool
  townSlime : Option TownSlimeBirth := none
  /-- `Main.StopSlimeRain(true)` ran: the caller applies `slimeRainTime` (here) and the warning. -/
  stopSlimeRain : Option Float := none
  killCredit : Bool := false
  /-- Dust created by the town-slime burst (`NerdySlimeEffect`), and the pool after it. -/
  particles : Array DustParticle := #[]
  dust : DustPool := {}

private def newItem (center : Vector) (item stack : Nat) (rollPrefix : Bool) (r : UnifiedRandom) : ItemDrop × UnifiedRandom :=
  let (prefixId, r) := if rollPrefix then
      let (p, r) := OriginalRecipeCatalog.rollNaturalPrefix item.toUInt16 r
      (p.toNat, r)
    else (0, r)
  let (x, r) := r.next (-30) 31
  let (y, r) := r.next (-40) (-15)
  ({ item, stack, prefixId, center, velocity := ⟨single x * f0_1, single y * f0_1⟩ }, r)

/-- `CommonDrop` (`luck`) or `CommonDropNotScalingWithLuck` (`Next(d)`): (succeeded, items, stream). -/
private def common (luck : Option Float32) (center : Vector) (item d lo hi : Nat) (items : Array ItemDrop)
    (r : UnifiedRandom) : Bool × Array ItemDrop × UnifiedRandom :=
  let (roll, r) := match luck with
    | some l => let (v, res) := OriginalCombat.Loot.rollLuck l (d : Int) { random := r }; (v, res.random)
    | none => r.next 0 d
  if roll ≥ 1 then (false, items, r) else
  let (stack, r) := r.next (lo : Int) ((hi : Int) + 1)
  let (drop, r) := newItem center item stack.toNat true r
  (true, items.push drop, r)

private def fromDrop (d : OriginalItems.Drop) : ItemDrop :=
  let c : Vector := ⟨d.position.x + 8, d.position.y + 8⟩
  { item := d.stack.item.toNat, stack := d.stack.count, prefixId := d.stack.prefixId.toNat, center := c, velocity := d.velocity }

/-- `NPC.checkDead()` for King Slime with `life ≤ 0`. -/
def checkDead (env : DeathEnv) (k : King) (random : UnifiedRandom) : Except String Death := do
  let a := k.actor
  if !a.body.active || a.body.life > 0 then
    return { king := k, random, downedSlimeKing := env.downedSlimeKing, unlockedTownSlime := env.unlockedTownSlime }
  if !env.quietGlobals then throw "KingSlime: global drop rules active (hard mode / seasons) are not transcribed"
  let n := a.body
  let mut random := random
  let death := Sound.style 4 1 n.position
  random := env.playSound death random
  let luck := env.loot.luck
  let c : Vector := ⟨single (i32 n.position.x + (n.width : Int) / 2), single (i32 n.position.y + (n.height : Int) / 2)⟩
  let mut items : Array ItemDrop := #[]
  -- trophy (LegacyHack_IsABoss) 1/10, then the NotExpert leading rule's chain
  for (item, d) in #[(2489, 10), (2430, 4), (2493, 7)] do
    let (_, i2, r2) := common (some luck) c item d 1 1 items random
    items := i2
    random := r2
  -- OneFromOptions(1, 256 / 257 / 258): RollLuck(1), then Next(3)
  let (roll, res) := OriginalCombat.Loot.rollLuck luck 1 { random }
  random := res.random
  if roll < 1 then
    let (k3, r) := random.next 0 3
    let (drop, r) := newItem c (#[256, 257, 258][k3.toNat]!) 1 true r
    items := items.push drop
    random := r
  -- hook 1/3 not scaling with luck; on a failed roll the slime gun
  let (hook, i2, r2) := common none c 2585 3 1 1 items random
  items := i2
  random := r2
  if !hook then
    let (_, i3, r3) := common (some luck) c 2610 1 1 1 items random
    items := i3
    random := r3
  for (item, d) in #[(998, 1), (1309, 30)] do
    let (_, i4, r4) := common (some luck) c item d 1 1 items random
    items := i4
    random := r4
  -- DoDeathEvents (King Slime branch, then the boss tail)
  let mut stop : Option Float := none
  if env.slimeRain then
    let (t, r) := random.next 3024 6048
    random := r
    stop := some (Float.ofInt (-t * 100))
  let mut townSlime : Option TownSlimeBirth := none
  let mut unlocked := env.unlockedTownSlime
  let mut dustPool := env.dust
  let mut particles : Array DustParticle := #[]
  if !unlocked then
    unlocked := true
    let cc := center n
    let slot := availableSlot env.slots 0
    -- NewNPC(Center.X - 10, Center.Y, 670) → GiveTownUniqueDataToNPCsThatNeedIt: the town slime's name
    -- (getNewNPCName) and profile variation (VariantNPCProfile.RollVariation) take two Main.rand samples
    -- (their values are the town-NPC module's; measured against the capture: draws 17 and 18 of loot seed 50000)
    let (_, r) := random.sample
    let (_, r) := r.sample
    random := r
    -- Utils.NextFloatDirection: NextFloat() * 2 - 1
    let (f, r) := random.sample
    random := r
    let dirv := (f.toFloat32 * 2 - 1) * 3
    if slot ≥ 0 then
      townSlime := some { slot := slot.toNat, bottom := (i32 cc.x - 10, i32 cc.y), velocity := ⟨dirv, -10⟩ }
    -- ParticleOrchestrator: TownSlimeTransform / NerdySlimeEffect at the Center, 60 x
    -- NewDustPerfect(pos, 4, (move * 0.75 + NextVector2Circular(6, 6)) * NextFloat(), 175, (0,80,255,100), 0.6 + NextFloat() * 1.4)
    for _ in [0:60] do
      let (_, r1) := random.sample   -- NextVector2Unit: the rotation
      let (_, r2) := r1.sample       -- NextVector2Circular: the radius
      let (_, r3) := r2.sample       -- the velocity factor
      let (sc, r4) := r3.sample      -- the scale
      random := r4
      let scale : Float32 := 0.6000000238418579 + sc.toFloat32 * 1.399999976158142
      let base : DustRequest := { position := cc, width := 0, height := 0, typeId := 4 }
      let (pool, particle, r5) := dustPool.create { base with alpha := 175, color := packed 0 80 255 100, scale := scale } random
      dustPool := pool
      random := r5
      if let some p := particle then particles := particles.push p
  let first := !env.downedSlimeKing
  let (potions, r) := random.next 5 16
  random := r
  let (drop, r) := newItem c 28 potions.toNat false random
  items := items.push drop
  random := r
  let (hearts, r) := random.next 0 5
  random := r
  for _ in [0:(hearts + 5).toNat] do
    let (drop, r) := newItem c 58 1 false random
    items := items.push drop
    random := r
  let money := OriginalCombat.Loot.dropMoneyAt env.loot (OriginalCombat.Loot.dropPosition n.position n.width n.height) n.value { random }
  let heals := OriginalCombat.Loot.dropHealsAt env.loot (OriginalCombat.Loot.dropPosition n.position n.width n.height)
    (n.lifeMax > 1 && n.damage > 0) { money with drops := #[] }
  let conv := fun (d : OriginalItems.Drop) => fromDrop d
  items := items ++ money.drops.map conv ++ heals.drops.map conv
  random := heals.random
  return { king := { k with actor := { a with body.active := false } }, random, items, sounds := #[death],
           chats := #[.hasBeenDefeated kingType], downedSlimeKing := true, firstClear := first, unlockedTownSlime := unlocked,
           townSlime, stopSlimeRain := stop, killCredit := a.interacted, particles, dust := dustPool }

/-! ## Presentation requests -/

/-- Boss bar: `GetBossHeadTextureIndex()` = `Sets.BossHeadTextures[50]` (7, native survey). -/
def bossBar (slot : Nat) (k : King) : Option OriginalEyeOfCthulhu.BossBar :=
  if !k.actor.body.active then none
  else some { slot, life := k.actor.body.life, lifeMax := k.actor.body.lifeMax, headIcon := 7 }

/-- Boss music (category 1, Boss 1): same NPC scan as the Eye (`OriginalEyeOfCthulhu.requestsBossMusic`). -/
def requestsBossMusic (screen : Rect) (k : King) : Bool := OriginalEyeOfCthulhu.requestsBossMusic screen k.actor

end Lean4Terria.OriginalKingSlime
