import Lean4Terria.OriginalForestClock

namespace Lean4Terria.OriginalWorldTimePhase

open OriginalForestClock

structure Item where
  typeId : Int := 0
  stack : Int := 0
  ammo : Int := 0
  useAmmo : Int := 0
  dye : Nat := 0
  countsAsBomb : Bool := false
  deriving Repr

structure Player where
  index : Nat
  active : Bool
  dead : Bool
  lifeMax : Int
  lifeMax2 : Int
  defense : Int
  y : Float32
  zoneSnow : Bool
  townNPCs : Int
  canSpawnEnemiesNear : Bool
  inventory : Array Item := #[]
  effectiveDyes : Array Item := #[]
  miscDyes : Array Item := #[]
  deriving Repr

structure Npc where
  index : Nat
  typeId : Nat
  active : Bool
  town : Bool
  boss : Bool
  homeless : Bool
  aiStyle : Int
  townPet : Bool
  deriving Repr, Inhabited

structure Progress where
  eye : Bool := false
  evil : Bool := false
  skeletron : Bool := false
  slimeKing : Bool := false
  queenBee : Bool := false
  mech1 : Bool := false
  mech2 : Bool := false
  mech3 : Bool := false
  mechAny : Bool := false
  plant : Bool := false
  moonLord : Bool := false
  deerclops : Bool := false
  goblins : Bool := false
  pirates : Bool := false
  frost : Bool := false
  savedStylist : Bool := false
  savedAngler : Bool := false
  savedGoblin : Bool := false
  savedTaxCollector : Bool := false
  savedWizard : Bool := false
  savedMech : Bool := false
  savedBartender : Bool := false
  savedGolfer : Bool := false
  unlocked : Array Nat := #[]
  boughtCat : Bool := false
  boughtDog : Bool := false
  boughtBunny : Bool := false
  deriving Repr

structure LogTile where
  active : Bool := false
  typeId : Nat := 0
  liquid : Nat := 0
  frameX : Int := 0
  frameY : Int := 0

structure LogGrid where
  width : Nat
  height : Nat
  tileAt : Int → Int → LogTile

structure Calendar where
  month : Nat
  day : Nat
  deriving Repr

structure Party where
  genuine : Bool := false
  manual : Bool := false
  cooldown : Int := 0
  celebrating : Array Nat := #[]
  deriving Repr

structure Lanterns where
  genuine : Bool := false
  manual : Bool := false
  nextNight : Bool := false
  cooldown : Int := 0
  deriving Repr

structure State where
  world : WorldTimeState
  normalEventsBlocked : Bool := false
  crimson : Bool := false
  spawnEye : Bool := false
  spawnHardBoss : Int := 0
  spawnMeteor : Bool := false
  canShowMeteorFall : Bool := false
  meteorShowerCount : Int := 0
  eclipse : Bool := false
  bloodMoon : Bool := false
  pumpkinMoon : Bool := false
  snowMoon : Bool := false
  moonWave : Int := 0
  moonWaveKills : Float32 := 0
  totalInvasionPoints : Float32 := 0
  forceHalloweenToday : Bool := false
  forceXmasToday : Bool := false
  halloween : Bool := false
  xmas : Bool := false
  fastForwardDawn : Bool := false
  fastForwardDusk : Bool := false
  dayRate : Int := 1
  sundialCooldown : Int := 0
  moondialCooldown : Int := 0
  invasionDelay : Int := 0
  invasionType : Int := 0
  checkForSpawns : Int := 0
  prioritizedTown : Nat := 0
  townCanSpawn : Array Bool := #[]
  anglerQuest : Nat := 0
  anglerQuestFinished : Bool := false
  anglerWhoFinished : Array String := #[]
  starfallBoost : Float32 := 1
  starFallCount : Int := 0
  critters : CritterChances
  eyeKilledToday : Bool := false
  wallKilledToday : Bool := false
  freeCake : Bool := false
  treeShakes : Int := 0
  party : Party := {}
  lanterns : Lanterns := {}
  mysticCanSpawn : Bool := false
  mysticDelay : Int := 0
  stumps : Array (Int × Int) := #[]
  fairyLog : Bool := false
  blockLight718 : Bool := false

def State.fastForwarding (state : State) : Bool := state.fastForwardDawn || state.fastForwardDusk
def State.lanternsUp (state : State) : Bool := state.lanterns.genuine || state.lanterns.manual

structure Context where
  netMode : Int
  worldUpdateRate : Int
  worldWidth : Nat
  worldSurface : Float
  rockLayer : Float
  spawnTileY : Int
  dungeonX : Int
  dungeonY : Int
  players : Array Player
  npcs : Array Npc
  progress : Progress
  hardMode : Bool
  anniversary : Bool
  good : Bool
  remix : Bool
  drunk : Bool
  crimson : Bool
  surface : Bool
  lowTiles : Bool
  noAltars : Bool
  altarCount : Int
  shadowOrbSmashed : Bool
  bossesKeepSpawning : Bool
  mechdusa : Bool
  dd2 : Bool
  lunarApocalypse : Bool
  moonLordCountdown : Int
  anyDanger : Bool
  infected : Bool
  vampire : Bool
  forceHalloweenForever : Bool
  forceXmasForever : Bool
  npcTypeCount : Nat
  xmasCalendar : Option Calendar := none
  halloweenCalendar : Option Calendar := none
  logGrid : Option LogGrid := none
  bestiaryCompletion : Option Float32 := none

inductive Message where
  | misc (index : Nat)
  | gen (index : Nat)
  | key (key : String)
  | birthday (slots : Array Nat)
  | moonScore (snow : Bool) (points : Float32)
  deriving Repr

inductive ChatColor where
  | world | party | bossOrEvent
  deriving BEq, Repr

inductive Effect where
  | chat (message : Message) (color : ChatColor := .world)
  | achievement (index : Nat)
  | syncWorld
  | syncAngler
  | npcHome (slot : Nat) (x y : Int)
  | birthdayAchievement
  deriving Repr

structure Census where
  counts : Array Nat
  total : Nat
  merchant : Bool
  armsDealer : Bool
  nurse : Bool
  dyeTrader : Bool
  demolitionist : Bool
  bestiary : Float32
  deriving Repr

inductive Call where
  | updateTimeRate
  | scanMysticLogs
  | checkXmas
  | checkHalloween
  | spawnOnPlayer (player npcType : Nat)
  | spawnMechQueen (player : Nat)
  | meteorAmbience
  | dropMeteor
  | quickFindHome (npcSlot : Nat)
  | bestiaryReport
  | spawnOldMan (x y : Int)
  | startInvasion (kind : Int)
  deriving BEq, Repr

inductive Continuation where
  | finish
  | nightAfterRate
  | nightAfterLogs
  | dayAfterRate
  | dayAfterXmas
  | dayAfterCalendar
  | dayAfterGoblin
  | dayAfterPirates
  | afterEye
  | afterMech
  | afterTwinFirst (player : Nat)
  | afterDeerclops
  | afterMeteorAmbience
  | afterMeteorDrop
  | censusScan (nextSlot : Nat) (counts : Array Nat) (total : Nat)
  | censusAfterHome (slot : Nat) (counts : Array Nat) (total : Nat)
  | censusAfterBestiary (census : Census)
  | censusAfterOldMan (census : Census)
  | nightToTown
  | dayToMeteor
  deriving Repr

structure Pending where
  call : Call
  continuation : Continuation
  /-- Original UpdateTime local1, preserved across every external call. -/
  entryTime : Float
  /-- Despite the original method's name, true BLOCKS ordinary events. -/
  normalEventsBlocked : Bool
  /-- Where a transition helper resumes after its own nested pending calls. -/
  returnTo : Continuation

structure Step where
  state : State
  effects : Array Effect := #[]
  pending : Option Pending := none
  complete : Bool := false

inductive Receipt where
  | unit
  | meteorDropped (success : Bool)
  | bestiary (completion : Float32)
  | npcSlot (slot : Nat)
  deriving Repr

def normalEventsBlocked (state : State) (context : Context) : Bool :=
  context.lunarApocalypse || context.npcs.any (fun n => n.active && n.typeId == 398) ||
    context.moonLordCountdown > 0 || state.lanternsUp

def eligibleSurfacePlayer (context : Context) (player : Player) : Bool :=
  player.active && !player.dead &&
    (player.y.toFloat < context.worldSurface*16 || context.spawnTileY.toFloat ≤ context.worldSurface) &&
    player.canSpawnEnemiesNear

end Lean4Terria.OriginalWorldTimePhase
