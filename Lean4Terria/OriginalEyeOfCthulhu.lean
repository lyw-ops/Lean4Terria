import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalGreenSlime
import Lean4Terria.OriginalCombat.Dust
import Lean4Terria.OriginalCombat.Loot
import Lean4Terria.OriginalRecipeCraft

/-!
# Eye of Cthulhu (Terraria 1.4.5.8, desktop, Classic, single player)

Hand transcription of the desktop CIL (Terraria.exe `d52c7ea7…3e0`) for:

* the Suspicious Looking Eye (item 43): `Player.SummonItemCheck` (the `ItemCheck_CheckCanUse_Inner`
  gate) and the item-43 branch of `Player.ItemCheck_UseBossSpawners`;
* `NPC.SpawnOnPlayer(plr, 4)` with `Spawner.GetSpawnArea`, `HasTileSpawnSpace`,
  `CheckNotSpawningOnScreen`, `NPC.SpawnBoss`, `NPC.NewNPC` and `GetAvailableNPCSlot` — the callee that
  `OriginalWorldTimePhase` suspends on (`.spawnOnPlayer p 4`) and the summon item calls;
* one `NPC.UpdateNPC(i)` for the Eye (`aiStyle 4`) and the Servant of Cthulhu (type 5, the
  `AI_005_EaterOfSouls` path of type 5): the `UpdateNPC` shell for `noTileCollide`/`noGravity`
  actors, both Eye phases, the 100-tick transformation (sounds, gores 8/7/6, dust), dashes, servant
  birth, the dawn / dead-target retreat, `FindFrame`, `UpdateNPC_UpdateTrails` (mode 3) and
  `CheckActive`;
* `NPC.checkDead` for a dead Eye / Servant: death sound request, `NPCLoot` (the Classic Eye rules of
  `ItemDropDatabase`, `DoDeathEvents`: `downedBoss1`, potions and hearts, `EoCKilledToday`, the
  defeat announcement), coins and the common heart / star rolls.

The NPC pool, players, sound playback, chat, achievements and bestiary are the caller's. They meet
this module through typed requests (`Sound`, `Chat`, `SlotView`, births) at the exact original call
sites, so the `Main.rand` stream (`UnifiedRandom`) stays bit-exact.

Not transcribed (the functions return `Except.error` instead of guessing): Expert / Master /
For-the-worthy / remix paths, buffs on these NPCs, multiplayer, liquids touching the Eye, the
skyblock `lowTiles` midair search, held-item scopes other than the declared fields, and `HitEffect`
/ `StrikeNPC` (combat owns those).
-/

namespace Lean4Terria.OriginalEyeOfCthulhu

open OriginalCollision (Vector single)
open OriginalGreenSlime (Slime)
open OriginalForestGuideArrow (DustPool DustRequest DustParticle Viewport)

/-! ## Constants (`SetDefaults`, `NPCID.Sets`, `Main.npcFrameCount`, native survey) -/

def eyeType : Nat := 4
def servantType : Nat := 5
def summonItem : Nat := 43
/-- `NPC.activeTime`, `activeRangeX/Y`. -/
def activeTime : Int := 750
def activeRangeX : Int := 4032
def activeRangeY : Int := 2520
/-- `NPC.sWidth` / `NPC.sHeight` are constants on the desktop build. -/
def sWidth : Int := 1920
def sHeight : Int := 1200
/-- `Spawner.spawnSpaceX/Y`. -/
def spawnSpaceX : Int := 2
def spawnSpaceY : Int := 3
/-- `NPC.netSpamTicksPerPacket(ForBosses)`, `netSpamPacketLimit`. -/
def netSpamTicks : Int := 30
def netSpamTicksBoss : Int := 5
def netSpamLimit : Int := 3
/-- Desktop texture frame heights: `NPC_4` 110 × 996 / 6, `NPC_5` 20 × 64 / 2. -/
def eyeFrameHeight : Int := 166
def servantFrameHeight : Int := 32
/-- `NPC.GetAttackDamage_LerpBetweenFinalValues(23, 18)` then `CappedAtMaster` at Classic difficulty 1. -/
def phase2Damage : Int := 23

/-! ## Float32 literals exactly as the CIL `ldc.r4` operands -/

private def f1_57 : Float32 := 1.5700000524520874
private def f6_283 : Float32 := 6.2829999923706055
private def f0_02 : Float32 := 0.019999999552965164
private def f0_05 : Float32 := 0.05000000074505806
private def f0_08 : Float32 := 0.07999999821186066
private def f0_15 : Float32 := 0.15000000596046448
private def f0_04 : Float32 := 0.03999999910593033
private def f0_98 : Float32 := 0.9800000190734863
private def f0_97 : Float32 := 0.9700000286102295
private def f0_005 : Float32 := 0.004999999888241291
private def f0_2 : Float32 := 0.20000000298023224
private def f0_1 : Float32 := 0.10000000149011612
private def f0_4 : Float32 := 0.4000000059604645
private def f6_8 : Float32 := 6.800000190734863
private def f0_07 : Float32 := 0.07000000029802322
private def f0_03 : Float32 := 0.029999999329447746

private def i32 (x : Float32) : Int := x.toInt32.toInt
private def atan2f (y x : Float32) : Float32 := (Float.atan2 y.toFloat x.toFloat).toFloat32
private def sqrtf (x : Float32) : Float32 := (Float.sqrt x.toFloat).toFloat32

/-! ## Actors -/

/-- One Eye or Servant. `body` is the shared NPC body of the released NPC ports; the extra fields are
the ones these two types read or write beyond it. -/
structure Actor where
  typeId : Nat
  body : Slime
  boss : Bool
  localAI : Array Float32 := #[0, 0, 0, 0]
  oldPos : Array Vector := Array.replicate 10 ⟨0, 0⟩
  oldRot : Array Float32 := Array.replicate 10 0
  reflectsProjectiles : Bool := false
  netSpam : Int := 0
  targetSetFrame : Int := 0
  /-- `GetWereThereAnyInteractions()` (bestiary / banner credit on death). -/
  interacted : Bool := false
  deriving Repr

instance : Inhabited Actor := ⟨{ typeId := 0, body := { position := ⟨0, 0⟩ }, boss := false }⟩

def Actor.center (a : Actor) : Vector :=
  ⟨a.body.position.x + single ((a.body.width : Int) / 2), a.body.position.y + single ((a.body.height : Int) / 2)⟩

/-- `Main.player[i]` as these methods read it. -/
structure PlayerView where
  index : Nat
  active : Bool := true
  dead : Bool := false
  ghost : Bool := false
  position : Vector
  width : Nat := 20
  height : Nat := 42
  aggro : Int := 0
  itemAnimation : Int := 0
  /-- `inventory[selectedItem].type` and `scope` (they change `GetSpawnArea`). -/
  heldItem : Nat := 0
  scope : Bool := false
  deriving Repr

def PlayerView.toSlime (p : PlayerView) : OriginalGreenSlime.Player :=
  { index := p.index, active := p.active, dead := p.dead, ghost := p.ghost, position := p.position,
    width := p.width, height := p.height, aggro := p.aggro, itemAnimation := p.itemAnimation }

def PlayerView.center (p : PlayerView) : Vector :=
  ⟨p.position.x + single ((p.width : Int) / 2), p.position.y + single ((p.height : Int) / 2)⟩

private def emptyPlayer (i : Nat) : PlayerView := { index := i, active := false, position := ⟨0, 0⟩ }

def playerAt (players : Array PlayerView) (i : Int) : PlayerView :=
  (players.find? (fun p => (p.index : Int) == i)).getD (emptyPlayer i.toNat)

/-! ## Requests to the caller -/

/-- `SoundEngine.PlaySound` at its original site. `legacy` is `PlaySound(int type, int x, int y,
int style, 1f, 0f)`; `style` is `PlaySound(LegacySoundStyle, Vector2, 0f, 1f)` (death sounds). -/
inductive Sound where
  | legacy (soundId : Nat) (x y : Int) (style : Nat)
  | style (soundId style : Nat) (position : Vector)
  deriving Repr, BEq

/-- `ChatHelper.BroadcastChatMessage` (single player: the local chat), colour `ChatColors.BossOrEvent`. -/
inductive Chat where
  | hasAwoken (npcType : Nat)
  | hasBeenDefeated (npcType : Nat)
  deriving Repr, BEq

/-- `NPC.IsSpawnSlotInUse(i)` (= `npc[i].active || spawnSlotProtected[i] > 0`) and
`npc[i].CanBeReplacedByOtherNPCs`, as the pool stands at the call. -/
structure SlotView where
  inUse : Nat → Bool
  replaceable : Nat → Bool := fun _ => false

/-- An `Item.NewItem` call: `Center` and velocity as the game sets them. -/
structure ItemDrop where
  item : Nat
  stack : Nat
  prefixId : Nat := 0
  center : Vector
  velocity : Vector
  deriving Repr, BEq

/-- One `Gore.NewGore` (types 6, 7, 8 here): first free of 600, `Next(10, 31)` then `Next(-20, 21)`. -/
structure Gore where
  slot : Nat
  typeId : Nat
  position : Vector
  velocity : Vector
  scale : Float32 := 1
  deriving Repr

/-- The shared per-frame state the update touches besides the actor. -/
structure Effects where
  random : UnifiedRandom
  dust : DustPool
  particles : Array DustParticle := #[]
  /-- `Main.gore[i].active` for i < 600. -/
  goreOccupied : Array Bool := Array.replicate 600 false
  gores : Array Gore := #[]
  sounds : Array Sound := #[]
  chats : Array Chat := #[]

/-- Per-call environment. `playSound` is the caller's sound backend run AT the original call site
(it may draw `Main.rand`, e.g. an audible NPCHit pitch); identity means audio disabled. -/
structure Env where
  players : Array PlayerView
  /-- `Main.IsItDay()` (`!remixWorld && dayTime`). -/
  isDay : Bool
  worldWidth : Nat
  worldHeight : Nat
  slots : SlotView
  /-- `Main.EverLastingTicker` (copied into `targetSetFrame` by `NewNPC`). -/
  ticker : Int := 0
  playSound : Sound → UnifiedRandom → UnifiedRandom := fun _ r => r

private def Effects.sound (fx : Effects) (env : Env) (s : Sound) : Effects :=
  { fx with sounds := fx.sounds.push s, random := env.playSound s fx.random }

private def Effects.next (fx : Effects) (lo hi : Int) : Int × Effects :=
  let (v, r) := fx.random.next lo hi
  (v, { fx with random := r })

/-- `Dust.NewDust`; returns the created particle (if any) so the AI can scale its velocity. -/
private def Effects.newDust (fx : Effects) (req : DustRequest) : Effects × Option Nat :=
  let (pool, particle, random) := fx.dust.create req fx.random
  match particle with
  | some p => ({ fx with dust := pool, particles := fx.particles.push p, random }, some (fx.particles.size))
  | none => ({ fx with dust := pool, random }, none)

private def Effects.scaleDust (fx : Effects) (index : Option Nat) (sx sy : Float32) : Effects :=
  match index with
  | some i =>
    match fx.particles[i]? with
    | some p => { fx with particles := fx.particles.set! i { p with velocity := ⟨p.velocity.x * sx, p.velocity.y * sy⟩ } }
    | none => fx
  | none => fx

private def Effects.newGore (fx : Effects) (env : Env) (typeId : Nat) (position velocity : Vector) : Effects :=
  let slot := (fx.goreOccupied.findIdx? (!·)).getD fx.goreOccupied.size
  if slot ≥ 600 then fx else
  let (up, random) := fx.random.next 10 31
  let (side, random) := random.next (-20) 21
  let g : Gore := { slot, typeId, position, velocity := ⟨velocity.x + single side * f0_1, velocity.y - single up * f0_1⟩ }
  -- `DeactivateIfOutsideOfWorld`
  let x := i32 position.x / 16
  let y := i32 position.y / 16
  let inside := 0 ≤ x && x < (env.worldWidth : Int) && 0 ≤ y && y < (env.worldHeight : Int)
  let occupied := fx.goreOccupied ++ Array.replicate (slot + 1 - fx.goreOccupied.size) false
  { fx with random, goreOccupied := occupied.set! slot inside, gores := if inside then fx.gores.push g else fx.gores }

/-! ## Birth: `NPC.NewNPC` / `SetDefaults(4 | 5)` -/

/-- `GetAvailableNPCSlot(type, start)` for types 4 and 5 (not reverse, slot 0 allowed): -1 = none. -/
def availableSlot (slots : SlotView) (start : Nat) : Int := Id.run do
  for i in [start:200] do
    if !slots.inUse i then return i
  for i in [start:200] do
    if slots.replaceable i then return i
  return -1

/-- `SetDefaults(type)` for 4 / 5 (native survey). -/
def defaults (typeId : Nat) : Actor :=
  if typeId == eyeType then
    { typeId, boss := true,
      body := { position := ⟨0, 0⟩, width := 100, height := 110, life := 2800, lifeMax := 2800, damage := 15,
                defense := 12, defDamage := 15, defDefense := 12, knockBackResist := 0, value := 30000,
                npcSlots := 5, scale := 1, alpha := 0, timeLeft := activeTime, netUpdate := true,
                color := ⟨0, 0, 0, 0⟩, netId := 4 } }
  else
    { typeId, boss := false,
      body := { position := ⟨0, 0⟩, width := 20, height := 20, life := 8, lifeMax := 8, damage := 12,
                defense := 0, defDamage := 12, defDefense := 0, knockBackResist := 1, value := 0,
                npcSlots := 1, scale := 1, alpha := 0, timeLeft := activeTime, netUpdate := true,
                color := ⟨0, 0, 0, 0⟩, netId := 5 } }

/-- `NPC.NewNPC(source, x, y, type, start, ai0..3, target)`: slot (200 = none) and the new actor.
The caller stores it in that slot and sets `spawnSlotProtected[slot] = 2`. -/
def newNPC (env : Env) (x y : Int) (typeId : Nat) (start : Nat) (target : Int := 255) : Nat × Option Actor :=
  let slot := availableSlot env.slots start
  if slot < 0 then (200, none) else
  let a := defaults typeId
  let w := a.body.width
  let h := a.body.height
  let body := { a.body with
    position := ⟨single x - single (w : Int) / 2, single y - single (h : Int)⟩
    active := true
    timeLeft := (Float.ofInt activeTime * 1.25).toInt64.toInt
    target := target }
  (slot.toNat, some { a with body, targetSetFrame := env.ticker })

/-! ## `NPC.SpawnOnPlayer(plr, 4)` -/

/-- Tile reads of the spawn search. `nactiveSolid` = `tile.nactive() && Main.tileSolid[type]`;
`solidTile` = `WorldGen.SolidTile(x, y, false)`; `houseWall` = `Main.wallHouse[tile.wall]`. -/
structure SpawnTiles where
  nactiveSolid : Int → Int → Bool
  houseWall : Int → Int → Bool := fun _ _ => false
  lava : Int → Int → Bool := fun _ _ => false

structure Rect where
  x : Int
  y : Int
  w : Int
  h : Int
  deriving Repr, BEq

def Rect.intersects (a b : Rect) : Bool := b.x < a.x + a.w && a.x < b.x + b.w && b.y < a.y + a.h && a.y < b.y + b.h
def Rect.contains (r : Rect) (x y : Int) : Bool := r.x ≤ x && x < r.x + r.w && r.y ≤ y && y < r.y + r.h

private def centered (cx cy w h : Int) : Rect := ⟨cx - w / 2, cy - h / 2, w, h⟩

private def clampToWorld (r : Rect) (width height : Int) : Rect :=
  let l := max 0 (min r.x width)
  let t := max 0 (min r.y height)
  let rr := max 0 (min (r.x + r.w) width)
  let b := max 0 (min (r.y + r.h) height)
  ⟨l, t, rr - l, b - t⟩

/-- `Spawner.GetSpawnArea`: spawn rectangle, safe rectangle, `NPC.safeRangeX/Y`. -/
def spawnArea (p : PlayerView) (worldWidth worldHeight : Nat) : Rect × Rect × Int × Int := Id.run do
  let tw := Float.ofInt (sWidth / 16)
  let th := Float.ofInt (sHeight / 16)
  let mut rx := (tw * 0.7).toInt64.toInt
  let mut ry := (th * 0.7).toInt64.toInt
  let mut sx := (tw * 0.52).toInt64.toInt
  let mut sy := (th * 0.52).toInt64.toInt
  if p.heldItem == 1254 || p.heldItem == 1299 || p.scope then
    let zoom : Float32 :=
      if p.heldItem == 1254 && p.scope then 1.25
      else if p.heldItem == 1254 then 1.5
      else if p.heldItem == 1299 then 1.5
      else 2
    rx := rx + (tw * 0.5 / zoom.toFloat).toInt64.toInt
    ry := ry + (th * 0.5 / zoom.toFloat).toInt64.toInt
    sx := sx + (tw * 0.5 / zoom.toFloat).toInt64.toInt
    sy := sy + (th * 0.5 / zoom.toFloat).toInt64.toInt
  let tx := i32 p.position.x >>> 4
  let ty := i32 p.position.y >>> 4
  let spawn := clampToWorld (centered tx ty (rx * 2) (ry * 2)) worldWidth worldHeight
  let safe := centered tx ty (sx * 2) (sy * 2)
  return (spawn, safe, sx, sy)

private def canSpawnInTiles (tiles : SpawnTiles) (r : Rect) : Bool := Id.run do
  for x in [0:r.w.toNat] do
    for y in [0:r.h.toNat] do
      let tx := r.x + x
      let ty := r.y + y
      if tiles.nactiveSolid tx ty then return false
      if tiles.lava tx ty then return false
  return true

/-- `Spawner.HasTileSpawnSpace(x, y)`. -/
def hasTileSpawnSpace (tiles : SpawnTiles) (w h : Nat) (x y : Int) : Bool :=
  let r : Rect := ⟨x - spawnSpaceX / 2, y - spawnSpaceY, spawnSpaceX, spawnSpaceY⟩
  let inWorld := r.x ≥ 0 && r.x + r.w < (w : Int) && r.y ≥ 0 && r.y + r.h < (h : Int)
  inWorld && canSpawnInTiles tiles r

/-- `Spawner.CheckNotSpawningOnScreen(x, y)` with the `safeRangeX/Y` statics just set. -/
def notSpawningOnScreen (players : Array PlayerView) (sx sy x y : Int) : Bool := Id.run do
  let r : Rect := ⟨x * 16, y * 16, 16, 16⟩
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.index ≥ 255 then continue
    let c := p.center
    let screen : Rect := ⟨i32 (c.x - single (sWidth / 2) - single sx), i32 (c.y - single (sHeight / 2) - single sy),
      sWidth + sx * 2, sHeight + sy * 2⟩
    if r.intersects screen then return false
  return true

/-- Result of a boss spawn (`SpawnOnPlayer` / summon). -/
structure SpawnResult where
  random : UnifiedRandom
  /-- Slot and actor of the Eye (none: no spot found, or no free slot). -/
  eye : Option (Nat × Actor) := none
  chats : Array Chat := #[]

/-- `NPC.SpawnOnPlayer(plr, 4, 0, 0, 0, 0)` in single player (the generic search branch). -/
def spawnOnPlayer (env : Env) (tiles : SpawnTiles) (plr : Nat) (random : UnifiedRandom) : SpawnResult := Id.run do
  let p := playerAt env.players plr
  let (rect, safe, sx, sy) := spawnArea p env.worldWidth env.worldHeight
  let mut random := random
  let mut flag := false
  let mut bx : Int := 0
  let mut by_ : Int := 0
  let outer : Nat := 500
  let inner : Nat := 50
  for i in [0:outer] do
    let last := i == outer - 1
    for _ in [0:inner] do
      let (x, r1) := random.next rect.x (rect.x + rect.w)
      let (y, r2) := r1.next rect.y (rect.y + rect.h)
      random := r2
      if tiles.nactiveSolid x y then continue
      if tiles.houseWall x y && i < outer - 1 then continue
      let mut yy := y
      while yy < rect.y + rect.h do
        if tiles.nactiveSolid x yy then
          if last || !safe.contains x y then
            bx := x
            by_ := yy
            flag := true
            break
        yy := yy + 1
      if flag && !last && !hasTileSpawnSpace tiles env.worldWidth env.worldHeight bx by_ then flag := false
      if flag then break
    if flag && !last && !notSpawningOnScreen env.players sx sy bx by_ then continue
    if flag then break
  if !flag then return { random }
  -- NPC.SpawnBoss(x*16+8, y*16, 4, plr): NewNPC from slot 1, timeLeft *= 20, announcement
  let (slot, actor) := newNPC env (bx * 16 + 8) (by_ * 16) eyeType 1 plr
  match actor with
  | none => return { random }
  | some a =>
    let a := { a with body.timeLeft := a.body.timeLeft * 20 }
    return { random, eye := some (slot, a), chats := #[.hasAwoken eyeType] }

/-! ## Suspicious Looking Eye -/

/-- `Player.SummonItemCheck(item 43)`: false while any active NPC is an Eye (the CanUse gate). -/
def summonItemCheck (activeTypes : Array Nat) : Bool := !activeTypes.contains eyeType

/-- Item-43 branch of `ItemCheck_UseBossSpawners` once `ItemTimeIsZero && itemAnimation > 0 &&
SummonItemCheck`: `none` by day (no item time, so `ItemCheck` consumes nothing); otherwise
`ApplyItemTime` (reported as `applyItemTime := true`), the roar request at the player's position,
`SpawnOnPlayer(i, 4)`, then `WorldGen.spawnEye = false`. -/
structure SummonResult where
  applyItemTime : Bool
  sound : Option Sound := none
  spawn : SpawnResult
  clearsSpawnEye : Bool

def useSummonItem (env : Env) (tiles : SpawnTiles) (plr : Nat) (random : UnifiedRandom) : SummonResult :=
  if env.isDay then { applyItemTime := false, spawn := { random }, clearsSpawnEye := false } else
  let p := playerAt env.players plr
  let s := Sound.legacy 15 (i32 p.position.x) (i32 p.position.y) 0
  let random := env.playSound s random
  { applyItemTime := true, sound := some s, spawn := spawnOnPlayer env tiles plr random, clearsSpawnEye := true }

/-! ## AI -/

private def netUpdateIgnoreSpamLimit (a : Actor) : Actor :=
  let n := if a.boss then netSpamTicksBoss else netSpamTicks
  let a := if !a.body.netUpdate || a.netSpam > n * netSpamLimit then { a with netSpam := a.netSpam - n } else a
  { a with body.netUpdate := true }

private def encourageDespawn (n : Slime) (time : Int) : Slime :=
  let n := if n.timeLeft > time then { n with timeLeft := time } else n
  { n with despawnEncouraged := true }

/-- Accelerate one velocity component toward `target` by `acc` (the doubled turn-around step). -/
private def steer (v target acc : Float32) : Float32 := Id.run do
  let mut v := v
  if v < target then
    v := v + acc
    if v < 0 && target > 0 then v := v + acc
  else if v > target then
    v := v - acc
    if v > 0 && target < 0 then v := v - acc
  return v

private def zeroSmall (v : Float32) : Float32 :=
  if v.toFloat > -0.1 && v.toFloat < 0.1 then 0 else v

private def eyeDustRequest (n : Slime) : DustRequest :=
  { position := ⟨n.position.x, n.position.y + single (n.height : Int) * 0.25⟩, width := n.width,
    height := (i32 (single (n.height : Int) * 0.5)).toNat, typeId := 5, speed := ⟨n.velocity.x, 2⟩ }

private def dustAt (position : Vector) (width height : Nat) (sx sy : Float32) : DustRequest :=
  { position, width, height, typeId := 5, speed := ⟨sx, sy⟩ }

/-- Births requested by this update (`NewNPC` slot and actor). -/
abbrev Births := Array (Nat × Actor)

/-- `aiStyle 4`, Classic. -/
def eyeAI (env : Env) (a : Actor) (fx : Effects) : Except String (Actor × Effects × Births) := do
  let players := env.players.map PlayerView.toSlime
  let mut a := a
  let mut fx := fx
  let mut births : Births := #[]
  let n0 := a.body
  let tgt0 := playerAt env.players n0.target
  if n0.target < 0 || n0.target == 255 || tgt0.dead || !tgt0.active then
    a := { a with body := OriginalGreenSlime.targetClosest players a.body }
  let tp := playerAt env.players a.body.target
  let dead := tp.dead
  let n := a.body
  let num5 := n.position.x + single ((n.width : Int) / 2) - tp.position.x - single ((tp.width : Int) / 2)
  let num6 := n.position.y + single (n.height : Int) - 59 - tp.position.y - single ((tp.height : Int) / 2)
  let mut num7 := atan2f num6 num5 + f1_57
  if num7 < 0 then num7 := num7 + f6_283
  else if num7.toFloat > 6.283 then num7 := num7 - f6_283
  let ai0 := n.ai0
  let ai1 := n.ai1
  let ai2 := n.ai2
  let mut num8 : Float32 := 0
  if ai0 == 0 && ai1 == 0 then num8 := f0_02
  if ai0 == 0 && ai1 == 2 && ai2 > 40 then num8 := f0_05
  if ai0 == 3 && ai1 == 0 then num8 := f0_05
  if ai0 == 3 && ai1 == 2 && ai2 > 40 then num8 := f0_08
  if ai0 == 3 && ai1 == 4 && ai2 > 20 then num8 := f0_15
  if ai0 == 3 && ai1 == 5 then num8 := f0_05
  let mut rot := n.rotation
  if rot < num7 then
    rot := if (num7 - rot).toFloat > 3.1415 then rot - num8 else rot + num8
  else if rot > num7 then
    rot := if (rot - num7).toFloat > 3.1415 then rot + num8 else rot - num8
  if rot > num7 - num8 && rot < num7 + num8 then rot := num7
  if rot < 0 then rot := rot + f6_283
  else if rot.toFloat > 6.283 then rot := rot - f6_283
  if rot > num7 - num8 && rot < num7 + num8 then rot := num7
  a := { a with body.rotation := rot }
  let (roll, fx1) := fx.next 0 5
  fx := fx1
  if roll == 0 then
    let (fx2, d) := fx.newDust (eyeDustRequest a.body)
    fx := fx2.scaleDust d 0.5 f0_1
  a := { a with reflectsProjectiles := false }
  if env.isDay || dead then
    a := { a with body := encourageDespawn { a.body with velocity.y := a.body.velocity.y - f0_04 } 10 }
    return (a, fx, births)
  let tcx := tp.position.x + single ((tp.width : Int) / 2)
  let tcy := tp.position.y + single ((tp.height : Int) / 2)
  let vec : Vector := ⟨a.body.position.x + single (a.body.width : Int) * 0.5, a.body.position.y + single (a.body.height : Int) * 0.5⟩
  if a.body.ai0 == 0 then
    if a.body.ai1 == 0 then
      let num12 := tcx - vec.x
      let num13 := tcy - 200 - vec.y
      let num14 := sqrtf (num12 * num12 + num13 * num13)
      let num15 := num14
      let k := 5 / num14
      let num12 := num12 * k
      let num13 := num13 * k
      a := { a with body.velocity := ⟨steer a.body.velocity.x num12 f0_04, steer a.body.velocity.y num13 f0_04⟩ }
      a := { a with body.ai2 := a.body.ai2 + 1 }
      if a.body.ai2 >= 600 then
        a := { a with body.ai1 := 1, body.ai2 := 0, body.ai3 := 0, body.target := 255, body.netUpdate := true }
      else if a.body.position.y + single (a.body.height : Int) < tp.position.y && num15 < 500 then
        if !tp.dead then a := { a with body.ai3 := a.body.ai3 + 1 }
        if a.body.ai3 >= 110 then
          a := { a with body.ai3 := 0, body.rotation := num7 }
          let num19 := tcx - vec.x
          let num20 := tcy - vec.y
          let num21 := 5 / sqrtf (num19 * num19 + num20 * num20)
          let v3 : Vector := ⟨num19 * num21, num20 * num21⟩
          let v2 : Vector := ⟨vec.x + v3.x * 10, vec.y + v3.y * 10⟩
          let (slot, servant) := newNPC env (i32 v2.x) (i32 v2.y) servantType 0
          if let some s := servant then
            births := births.push (slot, { s with body.velocity := v3 })
          fx := fx.sound env (.legacy 3 (i32 v2.x) (i32 v2.y) 1)
          for _ in [0:10] do
            let (fx2, _) := fx.newDust (dustAt v2 20 20 (v3.x * f0_4) (v3.y * f0_4))
            fx := fx2
    else if a.body.ai1 == 1 then
      a := { a with body.rotation := num7 }
      let dx := tcx - vec.x
      let dy := tcy - vec.y
      let k := 6 / sqrtf (dx * dx + dy * dy)
      a := { a with body.velocity := ⟨dx * k, dy * k⟩, body.ai1 := 2 }
      a := netUpdateIgnoreSpamLimit a
    else if a.body.ai1 == 2 then
      a := { a with body.ai2 := a.body.ai2 + 1 }
      if a.body.ai2 >= 40 then
        let v := a.body.velocity
        let v : Vector := ⟨v.x * f0_98, v.y * f0_98⟩
        a := { a with body.velocity := ⟨zeroSmall v.x, zeroSmall v.y⟩ }
      else
        a := { a with body.rotation := atan2f a.body.velocity.y a.body.velocity.x - f1_57 }
      if a.body.ai2 >= 150 then
        a := { a with body.ai3 := a.body.ai3 + 1, body.ai2 := 0, body.target := 255, body.rotation := num7 }
        if a.body.ai3 >= 3 then a := { a with body.ai1 := 0, body.ai3 := 0 }
        else a := { a with body.ai1 := 1 }
    if single a.body.life < single a.body.lifeMax * 0.5 then
      a := { a with body.ai0 := 1, body.ai1 := 0, body.ai2 := 0, body.ai3 := 0 }
      a := netUpdateIgnoreSpamLimit a
    return (a, fx, births)
  if a.body.ai0 == 1 || a.body.ai0 == 2 then
    if a.body.ai0 == 1 || a.body.ai3 == 1 then
      a := { a with body.ai2 := a.body.ai2 + f0_005 }
      if a.body.ai2.toFloat > 0.5 then a := { a with body.ai2 := 0.5 }
    else
      a := { a with body.ai2 := a.body.ai2 - f0_005 }
      if a.body.ai2 < 0 then a := { a with body.ai2 := 0 }
    a := { a with body.rotation := a.body.rotation + a.body.ai2, body.ai1 := a.body.ai1 + 1 }
    if a.body.ai1 >= 100 then
      if a.body.ai3 == 1 then
        a := { a with body.ai3 := 0, body.ai1 := 0 }
      else
        a := { a with body.ai0 := a.body.ai0 + 1, body.ai1 := 0 }
        if a.body.ai0 == 3 then
          a := { a with body.ai2 := 0 }
        else
          let pos := a.body.position
          fx := fx.sound env (.legacy 3 (i32 pos.x) (i32 pos.y) 1)
          for _ in [0:2] do
            for g in [8, 7, 6] do
              let (vx, f1) := fx.next (-30) 31
              let (vy, f2) := f1.next (-30) 31
              fx := f2.newGore env g pos ⟨single vx * f0_2, single vy * f0_2⟩
          for _ in [0:20] do
            let (vx, f1) := fx.next (-30) 31
            let (vy, f2) := f1.next (-30) 31
            let (f3, _) := f2.newDust (dustAt pos a.body.width a.body.height (single vx * f0_2) (single vy * f0_2))
            fx := f3
          fx := fx.sound env (.legacy 15 (i32 pos.x) (i32 pos.y) 0)
    let (vx, f1) := fx.next (-30) 31
    let (vy, f2) := f1.next (-30) 31
    let (f3, _) := f2.newDust (dustAt a.body.position a.body.width a.body.height (single vx * f0_2) (single vy * f0_2))
    fx := f3
    let v := a.body.velocity
    let v : Vector := ⟨v.x * f0_98, v.y * f0_98⟩
    a := { a with body.velocity := ⟨zeroSmall v.x, zeroSmall v.y⟩ }
    return (a, fx, births)
  -- phase two (ai0 = 3)
  a := { a with body.defense := 0, body.damage := phase2Damage }
  if a.body.ai1 == 0 then
    let dx := tcx - vec.x
    let dy := tcy - 120 - vec.y
    let k := 6 / sqrtf (dx * dx + dy * dy)
    let dx := dx * k
    let dy := dy * k
    a := { a with body.velocity := ⟨steer a.body.velocity.x dx f0_07, steer a.body.velocity.y dy f0_07⟩ }
    a := { a with body.ai2 := a.body.ai2 + 1 }
    if a.body.ai2 >= 200 then
      a := { a with body.ai1 := 1, body.ai2 := 0, body.ai3 := 0, body.target := 255, body.netUpdate := true }
  else if a.body.ai1 == 1 then
    let pos := a.body.position
    fx := fx.sound env (.legacy 36 (i32 pos.x) (i32 pos.y) 0)
    a := { a with body.rotation := num7 }
    let dx := tcx - vec.x
    let dy := tcy - vec.y
    let k := f6_8 / sqrtf (dx * dx + dy * dy)
    a := { a with body.velocity := ⟨dx * k, dy * k⟩, body.ai1 := 2 }
    a := netUpdateIgnoreSpamLimit a
  else if a.body.ai1 == 2 then
    a := { a with body.ai2 := a.body.ai2 + 1 }
    if a.body.ai2 >= 40 then
      let v := a.body.velocity
      let v : Vector := ⟨v.x * f0_97, v.y * f0_97⟩
      a := { a with body.velocity := ⟨zeroSmall v.x, zeroSmall v.y⟩ }
    else
      a := { a with body.rotation := atan2f a.body.velocity.y a.body.velocity.x - f1_57 }
    if a.body.ai2 >= 130 then
      a := { a with body.ai3 := a.body.ai3 + 1, body.ai2 := 0, body.target := 255, body.rotation := num7 }
      if a.body.ai3 >= 3 then
        a := { a with body.ai1 := 0, body.ai3 := 0 }
        a := netUpdateIgnoreSpamLimit a
      else a := { a with body.ai1 := 1 }
  else
    throw s!"EyeOfCthulhu: phase-two state ai1={a.body.ai1} is Expert-only"
  return (a, fx, births)

/-- `AI_005_EaterOfSouls` for type 5. -/
def servantAI (env : Env) (a : Actor) (fx : Effects) : Actor × Effects := Id.run do
  let players := env.players.map PlayerView.toSlime
  let mut a := a
  let mut fx := fx
  -- `target < 0 || target <= 255 || player[target].dead`: always retargets
  a := { a with body := OriginalGreenSlime.targetClosest players a.body }
  let tp := playerAt env.players a.body.target
  -- GetTargetData(true): a valid player target, else the default struct (Position 0, size 0)
  let valid := 0 ≤ a.body.target && a.body.target < 255 && tp.active && !tp.dead && !tp.ghost
  let (tx, ty, tw, th) : Float32 × Float32 × Int × Int :=
    if valid then (tp.position.x, tp.position.y, tp.width, tp.height) else (0, 0, 0, 0)
  let targetDead := false -- loc1 is only set for a valid (hence living) player target
  let speed : Float32 := 5
  let acc : Float32 := f0_03
  let q (v : Float32) : Float32 := single (i32 (v / 8) * 8)
  let vx0 := q (a.body.position.x + single (a.body.width : Int) * 0.5)
  let vy0 := q (a.body.position.y + single (a.body.height : Int) * 0.5)
  let mut dx := q (tx + single (tw / 2)) - vx0
  let mut dy := q (ty + single (th / 2)) - vy0
  let dist := sqrtf (dx * dx + dy * dy)
  if dist == 0 then
    dx := a.body.velocity.x
    dy := a.body.velocity.y
  else
    let k := speed / dist
    dx := dx * k
    dy := dy * k
  if targetDead then
    dx := single a.body.direction * speed / 2
    dy := -speed / 2
  a := { a with body.velocity := ⟨steer a.body.velocity.x dx acc, steer a.body.velocity.y dy acc⟩ }
  a := { a with body.rotation := atan2f a.body.velocity.y a.body.velocity.x - f1_57 }
  let (roll, fx1) := fx.next 0 40
  fx := fx1
  if roll == 0 then
    let (fx2, d) := fx.newDust (eyeDustRequest a.body)
    fx := fx2.scaleDust d 0.5 f0_1
  if env.isDay || targetDead then
    a := { a with body := encourageDespawn { a.body with velocity.y := a.body.velocity.y - acc * 2 } 10 }
  let v := a.body.velocity
  let o := a.body.oldVelocity
  if ((v.x > 0 && o.x < 0) || (v.x < 0 && o.x > 0) || (v.y > 0 && o.y < 0) || (v.y < 0 && o.y > 0)) &&
      !a.body.justHit then
    a := { a with body.netUpdate := true }
  return (a, fx)

/-! ## `UpdateNPC` shell, `FindFrame`, trails, `CheckActive` -/

/-- `FindFrame` for 4 / 5 (`frameHeight`: 166 / 32 on the desktop client, 1 on a server). -/
def findFrame (a : Actor) (frameHeight : Int) : Actor := Id.run do
  let mut n := a.body
  if a.typeId == eyeType then
    n := { n with frameCounter := n.frameCounter + 1 }
    if n.frameCounter < 7 then n := { n with frameY := 0 }
    else if n.frameCounter < 14 then n := { n with frameY := frameHeight }
    else if n.frameCounter < 21 then n := { n with frameY := frameHeight * 2 }
    else n := { n with frameCounter := 0, frameY := 0 }
    if n.ai0 > 1 then n := { n with frameY := n.frameY + frameHeight * 3 }
  else
    n := { n with frameCounter := n.frameCounter + 1 }
    if n.frameCounter >= 8 then n := { n with frameY := n.frameY + frameHeight, frameCounter := 0 }
    if n.frameY >= frameHeight * 2 then n := { n with frameY := 0 }
  return { a with body := n }

/-- `UpdateNPC_UpdateTrails`: mode 3 for the Eye (shift `oldPos`/`oldRot`), none (-1) for type 5. -/
def updateTrails (a : Actor) : Actor := Id.run do
  if a.typeId != eyeType then return a
  let mut pos := a.oldPos
  let mut rot := a.oldRot
  for k in [0:pos.size - 1] do
    let i := pos.size - 1 - k
    pos := pos.set! i (pos[i - 1]?.getD ⟨0, 0⟩)
    rot := rot.set! i (rot[i - 1]?.getD 0)
  return { a with oldPos := pos.set! 0 a.body.position, oldRot := rot.set! 0 a.body.rotation }

/-- `NPC.CheckActive` for 4 / 5: actor, `Player.nearbyActiveNPCs` increments, and whether it
deactivated (the caller then sets `NPC.noSpawnCycle`). -/
def checkActive (players : Array PlayerView) (a : Actor) : Actor × Array (Nat × Float32) × Bool := Id.run do
  if !a.body.active then return (a, #[], false)
  let mut n := a.body
  let cx := n.position.x + single ((n.width : Int) / 2)
  let cy := n.position.y + single ((n.height : Int) / 2)
  let range : Rect := ⟨i32 (cx - single activeRangeX), i32 (cy - single activeRangeY), activeRangeX * 2, activeRangeY * 2⟩
  let screen : Rect := ⟨(cx.toFloat - Float.ofInt sWidth * 0.5 - Float.ofNat n.width).toInt64.toInt,
    (cy.toFloat - Float.ofInt sHeight * 0.5 - Float.ofNat n.height).toInt64.toInt,
    sWidth + n.width * 2, sHeight + n.height * 2⟩
  let mut keep := false
  let mut nearby := #[]
  for p in players.qsort (·.index < ·.index) do
    if !p.active || p.index ≥ 255 then continue
    let box : Rect := ⟨i32 p.position.x, i32 p.position.y, p.width, p.height⟩
    if range.intersects box then
      keep := true
      if n.lifeMax > 0 then nearby := nearby.push (p.index, n.npcSlots)
    if screen.intersects box then
      n := { n with timeLeft := activeTime, despawnEncouraged := false }
    if a.boss then keep := true
  n := { n with timeLeft := n.timeLeft - 1 }
  if n.timeLeft ≤ 0 then keep := false
  let gone := !keep
  if gone then n := { n with active := false }
  return ({ a with body := n }, nearby, gone)

/-- Output of one `UpdateNPC(i)`. -/
structure Step where
  actor : Actor
  fx : Effects
  births : Births := #[]
  nearby : Array (Nat × Float32) := #[]
  /-- `CheckActive` (or `life ≤ 0`) deactivated the actor: the caller sets `NPC.noSpawnCycle`. -/
  deactivated : Bool := false

/-- One `NPC.UpdateNPC(i)` for an Eye or Servant (`frameHeight` per type: see `eyeFrameHeight`). -/
def update (env : Env) (frameHeight : Int) (a : Actor) (fx : Effects) : Except String Step := do
  if !a.body.active then return { actor := a, fx }
  let mut a := a
  if a.body.soundDelay > 0 then a := { a with body.soundDelay := a.body.soundDelay - 1 }
  if a.body.life ≤ 0 then
    return { actor := { a with body.active := false, body.netUpdate := false, body.justHit := false }, fx, deactivated := true }
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
  let mut fx := fx
  let mut births : Births := #[]
  if a.typeId == eyeType then
    let (a2, fx2, b2) ← eyeAI env a fx
    a := a2
    fx := fx2
    births := b2
  else if a.typeId == servantType then
    let (a2, fx2) := servantAI env a fx
    a := a2
    fx := fx2
  else throw s!"EyeOfCthulhu: type {a.typeId} is not handled here"
  a := { a with body.immune := a.body.immune.map fun t => if t > 0 then t - 1 else t }
  if a.body.velocity.x.toFloat < 0.005 && a.body.velocity.x.toFloat > -0.005 then a := { a with body.velocity.x := 0 }
  a := { a with body.oldPosition := a.body.position, body.oldDirection := a.body.direction }
  a := { a with body.position := a.body.position + a.body.velocity }
  a := findFrame a frameHeight
  a := updateTrails a
  let (a2, nearby, gone) := checkActive env.players a
  a := { a2 with body.netUpdate := false, body.justHit := false }
  return { actor := a, fx, births, nearby, deactivated := gone }

/-! ## Death: `checkDead` → `NPCLoot` → `DoDeathEvents` -/

/-- World / player facts the death path reads. -/
structure DeathEnv where
  /-- Luck and needs of `Player.FindClosest(npc.position, w, h)` (the drop rules' player). -/
  loot : OriginalCombat.Loot.Context
  /-- `WorldGen.crimson`. -/
  crimson : Bool
  /-- `NPC.downedBoss1` before the kill. -/
  downedBoss1 : Bool
  /-- `NPC.WoFKilledToday`. -/
  wofKilledToday : Bool := false
  /-- The 19 global rules are quiet: not hard mode, no Halloween / Christmas, no slime rain. -/
  quietGlobals : Bool := true
  playSound : Sound → UnifiedRandom → UnifiedRandom := fun _ r => r

structure Death where
  actor : Actor
  random : UnifiedRandom
  items : Array ItemDrop := #[]
  sounds : Array Sound := #[]
  chats : Array Chat := #[]
  /-- `NPC.downedBoss1` after the kill, and whether this kill set it (`OnGameEventClearedForTheFirstTime(13)`). -/
  downedBoss1 : Bool
  firstClear : Bool := false
  eocKilledToday : Bool := false
  /-- `NPC.noSpawnCycle = true`. -/
  noSpawnCycle : Bool := true
  /-- Bestiary kill and banner kill credit (only after an interaction). -/
  killCredit : Bool := false

private def prefixFor (item : Nat) (r : UnifiedRandom) : Nat × UnifiedRandom :=
  let (p, r) := OriginalRecipeCatalog.rollNaturalPrefix item.toUInt16 r
  (p.toNat, r)

/-- `Item.NewItem` at `center`: prefix (`-1` rolls a natural prefix, `0` none), then the velocity. -/
private def newItem (center : Vector) (item stack : Nat) (rollPrefix : Bool) (r : UnifiedRandom) : ItemDrop × UnifiedRandom :=
  let (prefixId, r) := if rollPrefix then prefixFor item r else (0, r)
  let (x, r) := r.next (-30) 31
  let (y, r) := r.next (-40) (-15)
  ({ item, stack, prefixId, center, velocity := ⟨single x * f0_1, single y * f0_1⟩ }, r)

/-- `ItemDropWithConditionRule` / `CommonDrop` with the condition true: `RollLuck(d) < 1`, then the
amount, then `DropItemFromNPC` (centre `(int)pos + size / 2`, natural prefix). -/
private def commonDrop (luck : Float32) (center : Vector) (item d lo hi : Nat) (items : Array ItemDrop) (r : UnifiedRandom) :
    Array ItemDrop × UnifiedRandom :=
  let (roll, res) := OriginalCombat.Loot.rollLuck luck (d : Int) { random := r }
  let r := res.random
  if roll ≥ 1 then (items, r) else
  let (stack, r) := r.next (lo : Int) ((hi : Int) + 1)
  let (drop, r) := newItem center item stack.toNat true r
  (items.push drop, r)

private def lootPosition (n : Slime) : Vector :=
  OriginalCombat.Loot.dropPosition n.position n.width n.height

private def fromLoot (res : OriginalCombat.Loot.Result) : Array ItemDrop :=
  res.drops.map fun d => { item := d.stack.item.toNat, stack := d.stack.count, prefixId := d.stack.prefixId.toNat,
                           center := ⟨d.position.x + 8, d.position.y + 8⟩, velocity := d.velocity }

/-- `NPC.checkDead()` for an Eye or Servant with `life ≤ 0`. -/
def checkDead (env : DeathEnv) (a : Actor) (random : UnifiedRandom) : Except String Death := do
  if !a.body.active || a.body.life > 0 then
    return { actor := a, random, downedBoss1 := env.downedBoss1, noSpawnCycle := false }
  if !env.quietGlobals then throw "EyeOfCthulhu: global drop rules active (hard mode / seasons) are not transcribed"
  let n := a.body
  let mut random := random
  let mut sounds : Array Sound := #[]
  let death := Sound.style 4 1 n.position
  sounds := sounds.push death
  random := env.playSound death random
  let mut items : Array ItemDrop := #[]
  let mut chats : Array Chat := #[]
  let mut downed := env.downedBoss1
  let mut first := false
  let mut eocToday := false
  let luck := env.loot.luck
  let center : Vector := ⟨single (i32 n.position.x + (n.width : Int) / 2), single (i32 n.position.y + (n.height : Int) / 2)⟩
  if a.typeId == eyeType then
    -- NPCLoot_DropItems: the specific rules after the quiet globals
    let rules : Array (Nat × Nat × Nat × Nat) :=
      #[(1360, 10, 1, 1), (2112, 7, 1, 1), (1299, 40, 1, 1), (47, 1, 20, 50)] ++
      (if env.crimson then #[(880, 1, 30, 90), (2171, 1, 1, 3)] else #[(56, 1, 30, 90), (59, 1, 1, 3)])
    for (item, d, lo, hi) in rules do
      let (i2, r2) := commonDrop luck center item d lo hi items random
      items := i2
      random := r2
    -- DoDeathEvents: downedBoss1, then (boss) potions and hearts, celebration
    first := !downed
    downed := true
    let (potions, r) := random.next 5 16
    random := r
    let (drop, r) := newItem center 28 potions.toNat false random
    items := items.push drop
    random := r
    let (hearts, r) := random.next 0 5
    random := r
    for _ in [0:(hearts + 5).toNat] do
      let (drop, r) := newItem center 58 1 false random
      items := items.push drop
      random := r
    eocToday := true
    if env.wofKilledToday then throw "EyeOfCthulhu: badger hat (EoC and WoF killed today) is not transcribed"
    chats := chats.push (.hasBeenDefeated eyeType)
  -- coins and the common heart / star rolls
  let money := OriginalCombat.Loot.dropMoneyAt env.loot (lootPosition n) n.value { random }
  let heals := OriginalCombat.Loot.dropHealsAt env.loot (lootPosition n) (n.lifeMax > 1 && n.damage > 0) { money with drops := #[] }
  items := items ++ fromLoot money ++ fromLoot heals
  random := heals.random
  return { actor := { a with body.active := false }, random, items, sounds, chats, downedBoss1 := downed,
           firstClear := first, eocKilledToday := eocToday, killCredit := a.interacted }

/-! ## Presentation requests (typed; drawing and playback are the caller's) -/

/-- What the boss health bar (`BigProgressBarSystem` tracking a boss NPC) shows for an active Eye.
`headIcon` is `NPC.GetBossHeadTextureIndex()`: `Sets.BossHeadTextures[4]` (0, native survey), or 1
once `ai[0] ≥ 2` (the mouth form). -/
structure BossBar where
  slot : Nat
  life : Int
  lifeMax : Int
  headIcon : Nat
  deriving Repr, BEq

def bossBar (slot : Nat) (a : Actor) : Option BossBar :=
  if a.typeId != eyeType || !a.body.active then none
  else some { slot, life := a.body.life, lifeMax := a.body.lifeMax, headIcon := if a.body.ai0 >= 2 then 1 else 0 }

/-- `Main.UpdateAudio_DecideOnNewMusic` NPC scan for an Eye: it is a boss of no special music group, so it
selects boss-music category 1 (Boss 1) when the 10000 × 10000 box around its centre
(`(int)(position + size / 2) - 5000`) intersects the screen rectangle `(screenPosition, screenWidth, screenHeight)`. -/
def requestsBossMusic (screen : Rect) (a : Actor) : Bool :=
  a.body.active && a.boss &&
    (Rect.mk (i32 (a.body.position.x + single ((a.body.width : Int) / 2)) - 5000)
      (i32 (a.body.position.y + single ((a.body.height : Int) / 2)) - 5000) 10000 10000).intersects screen

end Lean4Terria.OriginalEyeOfCthulhu
