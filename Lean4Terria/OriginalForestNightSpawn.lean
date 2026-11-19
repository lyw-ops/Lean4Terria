import Init

/-!
Terraria 1.4.5.8 Classic ordinary surface night selection.
The caller owns tile selection, special-biome and event gates, and the native
NPC pool. Thread the existing SpawnNPC stream through the supplied bounded
draw so the settings, selected entities, and rejections share that stream.
-/

namespace Lean4Terria.OriginalForestNightSpawn

abbrev Next (m : Type → Type) := String → Int → m Int

structure ZombieSettings where
  family : Int
  canSpawnArmed : Bool
  torchDenominator : Int
  graveyardDenominator : Int
  deriving BEq, Repr

/-- GetZombieSettings after its initial Next(7), with Skyblock.lowTiles off.
For a single active player with at most 100 base HP the torch-zombie bound is 5. -/
def zombieSettings (family : Int) (startingHealth : Bool) (numberOfActivePlayers : Int) :
    ZombieSettings :=
  ⟨family, true, if startingHealth then max 2 (5 - numberOfActivePlayers.tdiv 2) else 12, 20⟩

def getZombieSettings [Monad m] (next : Next m) (startingHealth : Bool)
    (numberOfActivePlayers : Int := 1) : m ZombieSettings := do
  let family ← next "Spawner::GetZombieSettings IL_0016" 7
  return zombieSettings family startingHealth numberOfActivePlayers

structure Birth where
  typeId : Int
  xOffset : Int := 0
  yOffset : Int := 0
  deriving BEq, Repr

/-- Ordinary inland water after the deep-water and ocean branches. The caller
must check waterTile first. A failed roll continues ordinary selection. -/
def chooseInlandWater [Monad m] (next : Next m) (groundType : Nat)
    (y : Int) (worldSurface : Float) (goldCritterChance : Int)
    (friendly : Bool := false) : m (Option Int) := do
  if !friendly && y.toFloat > worldSurface then
    if (← next "Spawner::SpawnAnNPC IL_1f45" 3) == 0 then return some 63
  let roll ← next "Spawner::SpawnAnNPC IL_1fce" 4
  if roll != 0 then return none
  if y > 50 && y.toFloat < worldSurface then
    let _ ← next "Spawner::SpawnAnNPC IL_208d" 3
  if groundType == 53 then return some 607
  let gold ← next "Spawner::SpawnAnNPC IL_23ef" goldCritterChance
  return some (if gold == 0 then 592 else 55)

structure Surface where
  groundType : Nat
  moonPhase : Nat
  tooWindyForFireFlies : Bool
  fireFlyChance : Int
  fireFlyMultiple : Int

private def fireFlies [Monad m] (next : Next m) (view : Surface) : m (Option (List Birth)) := do
  unless !view.tooWindyForFireFlies && ([2, 477, 109, 492] : List Nat).contains view.groundType do
    return none
  if (← next "Spawner::SpawnAnNPC IL_aa8c" view.fireFlyChance) != 0 then return none
  let id := if view.groundType == 109 then 358 else 355
  let mut births : List Birth := [{ typeId := id }]
  for (site, x, y) in [("aaec", -16, 0), ("ab2d", 16, 0), ("ab6e", 0, -16), ("abaf", 0, 16)] do
    if (← next s!"Spawner::SpawnAnNPC IL_{site}" view.fireFlyMultiple) == 0 then
      births := births ++ [⟨id, x, y⟩]
  return some births

private def demonEyes [Monad m] (next : Next m) : m (List Birth) := do
  if (← next "Spawner::SpawnAnNPC IL_ad9b" 2) == 0 then
    if (← next "Spawner::SpawnAnNPC IL_ada8" 4) == 0 then return [{ typeId := -43 }]
    return [{ typeId := 2 }]
  let variant ← next "Spawner::SpawnAnNPC IL_ae10" 5
  let some (base, small, offset) := (#[ (190, -38, "ae21"), (191, -39, "ae91"),
    (192, -40, "af01"), (193, -41, "af71"), (194, -42, "afe4") ] : Array (Int × Int × String))[variant.toNat]?
    | return []
  -- Each small variant here is an extra call at the same position.
  if (← next s!"Spawner::SpawnAnNPC IL_{offset}" 3) == 0 then return [{ typeId := small }, { typeId := base }]
  return [{ typeId := base }]

private def zombies [Monad m] (next : Next m) (settings : ZombieSettings) : m (List Birth) := do
  -- The Blood Moon and Graveyard checks are after these luck-zero draws.
  let _ ← next "Spawner::SpawnAnNPC IL_b0a2" 300
  let _ ← next "Spawner::SpawnAnNPC IL_b0ec" 300
  if (← next "Spawner::SpawnAnNPC IL_b5d2" settings.torchDenominator) == 0 then
    return [{ typeId := 590 }]
  let (base, small, big) := (#[ (3, -26, -27), (132, -28, -29), (186, -30, -31),
    (187, -32, -33), (188, -34, -35), (189, -36, -37), (200, -44, -45) ] :
      Array (Int × Int × Int))[settings.family.toNat]?.getD (3, -26, -27)
  let mut id := base
  if (← next "Spawner::SpawnAnNPC IL_b854" 3) == 0 then
    id := if (← next "Spawner::SpawnAnNPC IL_b861" 2) == 0 then small else big
  return [{ typeId := id }]

/-- SpawnAnNPC from IL_aa39 in dry, non-event, pre-Hardmode Classic forest at
zero luck. The non-friendly caller supplies a selected surface tile and the
settings it drew at SpawnAnNPC entry. Entries are ordered calls to the NPC
allocator, with offsets relative to the tile's bottom-center in pixels. -/
def choose [Monad m] (next : Next m) (view : Surface) (settings : ZombieSettings) :
    m (List Birth) := do
  if let some births ← fireFlies next view then return births
  let mut eye := (← next "Spawner::SpawnAnNPC IL_acda" 6) == 0
  if !eye && view.moonPhase == 4 then
    eye := (← next "Spawner::SpawnAnNPC IL_acf2" 2) == 0
  if eye then demonEyes next else zombies next settings

end Lean4Terria.OriginalForestNightSpawn
