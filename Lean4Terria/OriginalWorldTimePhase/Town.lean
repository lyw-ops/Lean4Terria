import Lean4Terria.OriginalWorldTimePhase.Transitions

namespace Lean4Terria.OriginalWorldTimePhase

private def carried (p : Player) : Array Item := p.inventory.extract 0 58

def merchantAllowed (context : Context) : Bool := Id.run do
  if context.progress.unlocked.contains 17 then return true
  let mut coins : Int := 0
  for player in context.players do
    if !player.active then continue
    for item in carried player do
      if item.stack ≤ 0 then continue
      let value := if item.typeId == 71 then 1 else if item.typeId == 72 then 100
        else if item.typeId == 73 then 10000 else if item.typeId == 74 then 1000000 else 0
      coins := (Int32.ofInt (coins+(Int32.ofInt (item.stack*value)).toInt)).toInt
      if coins ≥ 5000 then return true
  return false

def nurseAllowed (context : Context) : Bool :=
  context.progress.unlocked.contains 18 || context.players.any (fun p => p.active && p.lifeMax.tdiv 20 > 5)

def armsAllowed (context : Context) : Bool :=
  context.progress.unlocked.contains 19 || context.players.any fun p =>
    p.active && (carried p).any (fun i => i.stack > 0 && (i.ammo == 97 || i.useAmmo == 97))

def demolitionistAllowed (context : Context) : Bool :=
  context.progress.unlocked.contains 38 || context.players.any fun p =>
    p.active && (carried p).any (fun i => i.stack > 0 && i.countsAsBomb)

def dyeAllowed (context : Context) : Bool :=
  context.progress.unlocked.contains 207 || context.players.any fun p =>
    p.active && ((carried p).any (fun i => i.stack > 0 && (i.dye > 0 ||
      (1107 ≤ i.typeId && i.typeId ≤ 1120) || (3385 ≤ i.typeId && i.typeId ≤ 3388))) ||
      (p.effectiveDyes.extract 0 10).any (fun i => i.stack > 0 && i.dye > 0) ||
      (p.miscDyes.extract 0 5).any (fun i => i.stack > 0 && i.dye > 0))

private def count (census : Census) (kind : Nat) : Nat := census.counts[kind]?.getD 0

private def eligibleTown (state : State) (context : Context) (c : Census) (party : Bool) : Array (Nat × Bool) := Id.run do
  let missing : Nat → Bool := fun kind => count c kind < 1
  let progress := context.progress
  let princess := #[17,18,20,19,22,38,54,108,107,124,160,178,207,208,209,227,228,229,
      353,369,441,550,588,633].all (fun kind => count c kind > 0) ||
    (context.anniversary && !context.good) || progress.unlocked.contains 663
  let green := state.party.genuine || progress.unlocked.contains 678
  let mut result : Array (Nat × Bool) := #[
    (22,missing 22), (17,c.merchant && missing 17),
    (18,c.nurse && missing 18 && count c 17 > 0), (19,c.armsDealer && missing 19),
    (20,(context.infected || progress.eye || progress.evil || progress.skeletron) && missing 20),
    (38,c.demolitionist && count c 17 > 0 && missing 38),
    (353,progress.savedStylist && missing 353),(369,progress.savedAngler && missing 369),
    (54,progress.skeletron && missing 54),(107,progress.savedGoblin && missing 107),
    (441,progress.savedTaxCollector && missing 441),(108,progress.savedWizard && missing 108),
    (124,progress.savedMech && missing 124),(142,progress.frost && missing 142 && state.xmas),
    (178,((context.anniversary && !context.good) || progress.mechAny) && missing 178),
    (207,c.dyeTrader && missing 207 && c.total ≥ 4),(228,progress.queenBee && missing 228),
    (229,progress.pirates && missing 229),(160,context.hardMode && missing 160),
    (209,context.hardMode && progress.plant && missing 209),(227,c.total ≥ 8 && missing 227),
    (208,party && missing 208),(550,progress.savedBartender && missing 550),
    (588,progress.savedGolfer && missing 588),
    (633,((context.vampire && !context.infected) || c.bestiary ≥ (0.1 : Float32)) && missing 633),
    (637,progress.boughtCat && missing 637),(638,progress.boughtDog && missing 638),
    (656,progress.boughtBunny && missing 656),(678,green && missing 678),(663,princess && missing 663)]
  for kind in #[670,679,680,681,682,683,684] do
    result := result.push (kind,progress.unlocked.contains kind && missing kind)
  return result

def censusFinish (state : State) (context : Context) (c : Census) : Step := Id.run do
  let (roll,rng) := state.world.random.next 0 40
  let state := {state with world.random:=rng}
  let party := (roll == 0 && c.total ≥ 20) || context.progress.unlocked.contains 208
  let allowed := eligibleTown state context c party
  let table := allowed.foldl (init:=state.townCanSpawn) fun table (kind,canSpawn) =>
    if canSpawn && kind < table.size then table.set! kind true else table
  let mut priority := state.prioritizedTown
  let missing : Nat → Bool := fun kind => count c kind < 1
  let progress := context.progress
  if priority == 0 && context.infected && missing 20 then priority := 20
  if priority == 0 && context.vampire && !context.infected && missing 633 then priority := 633
  if priority == 0 && missing 22 then priority := 22
  -- The priority order differs from the allowed-flag table, including the
  -- anniversary override for178 and the special-seed override for633.
  for (kind,canSpawn) in #[
    (17,c.merchant),(18,c.nurse && count c 17 > 0),(19,c.armsDealer),
    (107,progress.savedGoblin),(108,progress.savedWizard),
    (20,progress.eye || progress.evil || progress.skeletron),
    (38,c.demolitionist && count c 17 > 0),(228,progress.queenBee),
    (178,progress.mechAny),(124,progress.savedMech),(369,progress.savedAngler),
    (209,context.hardMode && progress.plant),(229,progress.pirates),(54,progress.skeletron),
    (353,progress.savedStylist),(207,c.total ≥ 4 && c.dyeTrader),(227,c.total ≥ 8),
    (208,party),(142,progress.frost && state.xmas),(550,progress.savedBartender),
    (588,progress.savedGolfer),(441,progress.savedTaxCollector),(160,context.hardMode),
    (633,c.bestiary ≥ (0.1 : Float32)),
    (663,(allowed.find? (fun x => x.1 == 663)).any (·.2)),
    (684,progress.unlocked.contains 684),(670,progress.unlocked.contains 670),
    (678,state.party.genuine || progress.unlocked.contains 678),
    (679,progress.unlocked.contains 679),(680,progress.unlocked.contains 680),
    (682,progress.unlocked.contains 682),(683,progress.unlocked.contains 683),
    (681,progress.unlocked.contains 681),(656,progress.boughtBunny),
    (637,progress.boughtCat),(638,progress.boughtDog)] do
    if priority == 0 && canSpawn && missing kind then priority := kind
  return {state:={state with townCanSpawn:=table,prioritizedTown:=priority}}

def censusWithReport (state : State) (context : Context) (beforeReport : Census)
    (completion : Float32) (entryTime : Float) (returnTo : Continuation) : Step :=
  let c := {beforeReport with bestiary:=completion}
  if !context.progress.skeletron && c.counts[37]?.getD 0 == 0 && context.dungeonX ≥ 0 && context.dungeonY ≥ 0 then
    suspend state (.spawnOldMan (context.dungeonX*16+8) (context.dungeonY*16)) (.censusAfterOldMan c) entryTime returnTo
  else censusFinish state context c

def censusAfterScan (state : State) (context : Context) (counts : Array Nat) (total : Nat)
    (entryTime : Float) (returnTo : Continuation) : Step :=
  if state.prioritizedTown != 0 then {state} else
  let c : Census := {
    counts := counts
    total := total
    merchant := merchantAllowed context
    armsDealer := armsAllowed context
    nurse := nurseAllowed context
    dyeTrader := dyeAllowed context
    demolitionist := demolitionistAllowed context
    bestiary := 0 }
  match context.bestiaryCompletion with
  | none => suspend state .bestiaryReport (.censusAfterBestiary c) entryTime returnTo
  | some completion => censusWithReport state context c completion entryTime returnTo

private def addTown (counts : Array Nat) (npc : Npc) : Array Nat :=
  if npc.typeId < counts.size then counts.set! npc.typeId (counts[npc.typeId]?.getD 0+1) else counts

def censusScan (state : State) (context : Context) (start : Nat) (counts : Array Nat) (total : Nat)
    (entryTime : Float) (returnTo : Continuation) : Step := Id.run do
  let mut counts := counts
  let mut total := total
  for slot in [start:context.npcs.size] do
    let npc := context.npcs[slot]!
    if !npc.active || !npc.town then continue
    if npc.typeId != 368 && npc.typeId != 37 && npc.typeId != 453 && !npc.homeless then
      return suspend state (.quickFindHome npc.index) (.censusAfterHome slot counts total) entryTime returnTo
    counts := addTown counts npc
    total := total+1
  return censusAfterScan state context counts total entryTime returnTo

def censusAfterHome (state : State) (context : Context) (slot : Nat) (counts : Array Nat) (total : Nat)
    (entryTime : Float) (returnTo : Continuation) : Step :=
  let npc := context.npcs[slot]!
  censusScan state context (slot+1) (addTown counts npc) (total+1) entryTime returnTo

def spawnTownNpcs (state : State) (context : Context) (entryTime : Float)
    (returnTo : Continuation := .nightToTown) : Step :=
  if context.netMode == 1 || context.worldUpdateRate ≤ 0 then {state} else
  let check := (Int32.ofInt (state.checkForSpawns+1)).toInt
  if check < (7200 : Int).tdiv context.worldUpdateRate then {state:={state with checkForSpawns:=check}} else
  let state := {state with checkForSpawns:=0,prioritizedTown:=0,townCanSpawn:=Array.replicate context.npcTypeCount false}
  censusScan state context 0 (Array.replicate context.npcTypeCount 0) 0 entryTime returnTo

end Lean4Terria.OriginalWorldTimePhase
