import Lean.Data.Json
import Lean4Terria.Game

namespace Lean4Terria.Save

open Lean (Json FromJson toJson fromJson?)

private def version : Nat := 73

private def maxBoneSerpentSegments : Nat := 24

private def encodePlayer (player : Player) : Json :=
  Json.mkObj [
    ("x", toJson player.x), ("y", toJson player.y),
    ("vx", toJson player.vx), ("vy", toJson player.vy),
    ("grounded", toJson player.grounded)
  ]

private def encodeSlot : Option Stack → Json
  | none => .null
  | some stack => Json.mkObj [
    ("item", toJson stack.item.code.toNat), ("count", toJson stack.count)
  ]

private def encodeCrawler : Option Crawler → Json
  | none => .null
  | some crawler => Json.mkObj [
    ("body", encodePlayer crawler.body),
    ("health", toJson crawler.health),
    ("hopIn", toJson crawler.hopIn)
  ]

private def encodeCombat (combat : CombatState) : Json :=
  Json.mkObj [
    ("health", toJson combat.health),
    ("lifeCrystals", toJson combat.lifeCrystals),
    ("onFire", toJson combat.onFire),
    ("fireProgress", toJson combat.fireProgress),
    ("lavaCooldown", toJson combat.lavaCooldown),
    ("regenTime", toJson combat.regenTime),
    ("regenProgress", toJson combat.regenProgress),
    ("invulnerable", toJson combat.invulnerable),
    ("swing", toJson combat.swing),
    ("facing", toJson combat.facing),
    ("crawler", encodeCrawler combat.crawler),
    ("cleared", toJson combat.cleared)
  ]

private def encodeWardenPhase : WardenPhase → String
  | .approach => "approach"
  | .windup => "windup"
  | .charge => "charge"
  | .recover => "recover"

private def encodeWarden : WardenEncounter → Json
  | .ready => Json.mkObj [("state", toJson "ready")]
  | .fighting boss => Json.mkObj [
    ("state", toJson "fighting"), ("body", encodePlayer boss.body),
    ("health", toJson boss.health), ("phase", toJson (encodeWardenPhase boss.phase)),
    ("ticks", toJson boss.ticks), ("facing", toJson boss.facing)
  ]
  | .reward position => Json.mkObj [
    ("state", toJson "reward"), ("x", toJson position.x), ("y", toJson position.y)
  ]
  | .cleared => Json.mkObj [("state", toJson "cleared")]

private def encodePosition (position : TilePos) : Json :=
  Json.mkObj [("x", toJson position.x), ("y", toJson position.y)]

private def encodeTownGuide : Option GuideNPC → Json
  | none => .null
  | some guide => Json.mkObj [
    ("body", encodePlayer guide.body), ("health", toJson guide.health), ("direction", toJson guide.direction),
    ("attackCooldown", toJson guide.attackCooldown), ("hurtCooldown", toJson guide.hurtCooldown),
    ("doorToClose", (guide.doorToClose.map encodePosition).getD .null)
  ]

private def encodeFurnitureKind : FurnitureKind → String
  | .bed => "bed"
  | .furnace => "furnace"
  | .ironAnvil => "ironAnvil"
  | .leadAnvil => "leadAnvil"
  | .sawmill => "sawmill"
  | .loom => "loom"
  | .woodChair => "woodChair"
  | .woodTable => "woodTable"
  | .hellforge => "hellforge"
  | .mythrilAnvil => "mythrilAnvil"
  | .orichalcumAnvil => "orichalcumAnvil"
  | .adamantiteForge => "adamantiteForge"
  | .titaniumForge => "titaniumForge"
  | .woodenCrate => "woodenCrate"
  | .ironCrate => "ironCrate"
  | .goldenCrate => "goldenCrate"
  | .pearlwoodCrate => "pearlwoodCrate"
  | .mythrilCrate => "mythrilCrate"
  | .titaniumCrate => "titaniumCrate"
  | .biomeCrate _ => "biomeCrate"

private def encodeFurniture (furniture : Furniture) : Json :=
  let base := [("kind", toJson (encodeFurnitureKind furniture.kind)),
    ("position", encodePosition furniture.position), ("flipped", toJson furniture.flipped)]
  match furniture.kind with
  | .biomeCrate item => Json.mkObj (base ++ [("item", toJson item.code.toNat)])
  | _ => Json.mkObj base

private def encodeTileHit (hit : TileHit) : Json :=
  Json.mkObj [("position", encodePosition hit.position), ("tile", toJson hit.tile.code.toNat),
    ("damage", toJson hit.damage), ("ticks", toJson hit.ticks)]

private def encodeGelDrop (drop : GelDrop) : Json :=
  Json.mkObj [("position", encodePosition drop.position), ("count", toJson drop.count)]

private def encodeNightDrop (drop : NightDrop) : Json :=
  Json.mkObj [("position", encodePosition drop.position), ("coins", toJson drop.coins),
    ("items", .arr (drop.items.map (fun stack => encodeSlot (some stack))))]

private def encodeChest (chest : Chest) : Json :=
  Json.mkObj [("position", encodePosition chest.position), ("slots", .arr (chest.slots.map encodeSlot))]

private def encodeSurfaceKind : SurfaceKind → String
  | .slime => "slime"
  | .greenSlime => "greenSlime"
  | .blueSlime => "blueSlime"
  | .cedarHare => "cedarHare"
  | .brambleCrawler => "brambleCrawler"
  | .zombie => "zombie"
  | .demonEye => "demonEye"

private def encodeNightCreature : Option NightCreature → Json
  | none => .null
  | some creature => Json.mkObj [
    ("body", encodePlayer creature.body), ("health", toJson creature.health),
    ("hopIn", toJson creature.hopIn), ("kind", toJson (encodeSurfaceKind creature.kind))
  ]

private def encodeSurface (surface : SurfaceState) : Json :=
  Json.mkObj [("clock", toJson surface.clock), ("creature", encodeNightCreature surface.creature),
    ("flying", encodeNightCreature surface.flying)]

private def encodeSparseBytes (bytes : ByteArray) : Json := Id.run do
  let mut entries := #[]
  for index in [:bytes.size] do
    let value := bytes[index]!.toNat
    if value > 0 then entries := entries.push (.arr #[toJson index, toJson value])
  return .arr entries

private def encodeQuarry (quarry : QuarryState) : Json :=
  let creature := quarry.creature.map fun creature => Json.mkObj [
    ("body", encodePlayer creature.body), ("health", toJson creature.health),
    ("hopIn", toJson creature.hopIn)
  ]
  Json.mkObj [("cleared", toJson quarry.cleared), ("creature", creature.getD .null)]

private def encodeSentry (sentry : SentryState) : Json :=
  let creature := sentry.creature.map fun creature => Json.mkObj [
    ("body", encodePlayer creature.body), ("health", toJson creature.health), ("aim", toJson creature.aim)
  ]
  Json.mkObj [("cleared", toJson sentry.cleared), ("creature", creature.getD .null)]

private def encodeStormPhase : StormPhase → String
  | .aim => "aim"
  | .barrage => "barrage"
  | .recover => "recover"

private def encodeStorm : StormEncounter → Json
  | .ready => Json.mkObj [("state", toJson "ready")]
  | .fighting boss => Json.mkObj [
    ("state", toJson "fighting"), ("body", encodePlayer boss.body),
    ("health", toJson boss.health), ("phase", toJson (encodeStormPhase boss.phase)),
    ("ticks", toJson boss.ticks), ("facing", toJson boss.facing)
  ]
  | .reward position => Json.mkObj [("state", toJson "reward"), ("position", encodePosition position)]
  | .cleared => Json.mkObj [("state", toJson "cleared")]

private def encodeRuinPhase : RuinPhase → String
  | .windup => "windup"
  | .slam => "slam"
  | .recover => "recover"

private def encodeRuin : RuinEncounter → Json
  | .ready => Json.mkObj [("state", toJson "ready")]
  | .fighting boss => Json.mkObj [
    ("state", toJson "fighting"), ("body", encodePlayer boss.body),
    ("health", toJson boss.health), ("phase", toJson (encodeRuinPhase boss.phase)),
    ("ticks", toJson boss.ticks), ("targetX", toJson boss.targetX)
  ]
  | .reward position => Json.mkObj [("state", toJson "reward"), ("position", encodePosition position)]
  | .cleared => Json.mkObj [("state", toJson "cleared")]

private def encodeCavernPhase : CavernPhase → String
  | .windup => "windup"
  | .beam => "beam"
  | .recover => "recover"

private def encodeCavern : CavernEncounter → Json
  | .ready => Json.mkObj [("state", toJson "ready")]
  | .fighting boss => Json.mkObj [
    ("state", toJson "fighting"), ("body", encodePlayer boss.body),
    ("health", toJson boss.health), ("phase", toJson (encodeCavernPhase boss.phase)),
    ("ticks", toJson boss.ticks)
  ]
  | .reward position => Json.mkObj [("state", toJson "reward"), ("position", encodePosition position)]
  | .cleared => Json.mkObj [("state", toJson "cleared")]

private def encodeEyeMotion (motion : EyeMotion) : Json :=
  Json.mkObj [("x", toJson motion.x), ("y", toJson motion.y),
    ("vx", toJson motion.vx), ("vy", toJson motion.vy)]

private def encodeEyePhase : EyePhase → String
  | .firstHover => "firstHover"
  | .firstCharge => "firstCharge"
  | .transformSpinUp => "transformSpinUp"
  | .transformSpinDown => "transformSpinDown"
  | .secondHover => "secondHover"
  | .secondCharge => "secondCharge"
  | .retreat => "retreat"

private def encodeEyeServant (servant : EyeServant) : Json :=
  Json.mkObj [("id", toJson servant.id), ("motion", encodeEyeMotion servant.motion),
    ("health", toJson servant.health)]

private def encodeEye : Option EyeOfCthulhu → Json
  | none => .null
  | some eye => Json.mkObj [
    ("motion", encodeEyeMotion eye.motion), ("health", toJson eye.health),
    ("phase", toJson (encodeEyePhase eye.phase)), ("ticks", toJson eye.ticks),
    ("servantTicks", toJson eye.servantTicks),
    ("charges", toJson eye.charges), ("servants", .arr (eye.servants.map encodeEyeServant)),
    ("nextServantId", toJson eye.nextServantId)
  ]

private def encodeWormSegment (segment : WormSegment) : Json :=
  Json.mkObj [("id", toJson segment.id), ("motion", encodeEyeMotion segment.motion),
    ("health", toJson segment.health)]

private def encodeEater : Option EaterOfWorlds → Json
  | none => .null
  | some boss => Json.mkObj [
    ("chains", .arr (boss.chains.map fun chain => .arr (chain.map encodeWormSegment))),
    ("retreating", toJson boss.retreating), ("motionTick", toJson boss.motionTick)
  ]

private def encodeBrainPhase : BrainPhase → String
  | .shielded => "shielded"
  | .exposed => "exposed"
  | .retreat => "retreat"

private def encodeBrainCreeper (creeper : BrainCreeper) : Json :=
  Json.mkObj [("id", toJson creeper.id), ("motion", encodeEyeMotion creeper.motion),
    ("health", toJson creeper.health), ("rush", toJson creeper.rush)]

private def encodeBrain : Option BrainOfCthulhu → Json
  | none => .null
  | some boss => Json.mkObj [
    ("motion", encodeEyeMotion boss.motion), ("health", toJson boss.health),
    ("phase", toJson (encodeBrainPhase boss.phase)), ("timer", toJson boss.timer),
    ("warp", toJson boss.warp), ("serial", toJson boss.serial),
    ("creepers", .arr (boss.creepers.map encodeBrainCreeper))
  ]

private def encodeEvilFlyer (enemy : EvilFlyer) : Json :=
  let kind := match enemy.kind with
    | .eaterOfSouls => "eaterOfSouls"
    | .crimera => "crimera"
  Json.mkObj [("id", toJson enemy.id), ("kind", toJson kind), ("motion", encodeEyeMotion enemy.motion),
    ("health", toJson enemy.health), ("ticks", toJson enemy.ticks), ("diving", toJson enemy.diving)]

private def encodeSkeletronPhase : SkeletronPhase → String
  | .hover => "hover"
  | .spin => "spin"
  | .enraged => "enraged"
  | .retreat => "retreat"

private def encodeSkeletronHandPhase : SkeletronHandPhase → String
  | .hover => "hover"
  | .lift => "lift"
  | .slam => "slam"
  | .reset => "reset"
  | .windup => "windup"
  | .swipe => "swipe"

private def encodeFloatMotionFields (x y vx vy : Float32) : List (String × Json) :=
  [("xBits", toJson x.toBits.toNat), ("yBits", toJson y.toBits.toNat),
    ("vxBits", toJson vx.toBits.toNat), ("vyBits", toJson vy.toBits.toNat)]

private def encodeSkeletronFloatFields (x y vx vy rotation : Float32) : List (String × Json) :=
  encodeFloatMotionFields x y vx vy ++ [("rotationBits", toJson rotation.toBits.toNat)]

private def encodeSkeletronHand : Option SkeletronHand → Json
  | none => .null
  | some hand => Json.mkObj (encodeSkeletronFloatFields hand.x hand.y hand.vx hand.vy hand.rotation ++ [
    ("health", toJson hand.health), ("phase", toJson (encodeSkeletronHandPhase hand.phase)),
    ("ticks", toJson hand.ticks)
  ])

private def encodeSkeletronHead (head : SkeletronHead) : Json :=
  Json.mkObj (encodeSkeletronFloatFields head.x head.y head.vx head.vy head.rotation ++
    [("direction", toJson head.direction)])

private def encodeSkeletron : Option Skeletron → Json
  | none => .null
  | some boss => Json.mkObj [
    ("head", encodeSkeletronHead boss.head), ("health", toJson boss.health),
    ("phase", toJson (encodeSkeletronPhase boss.phase)), ("ticks", toJson boss.ticks),
    ("leftHand", encodeSkeletronHand boss.leftHand), ("rightHand", encodeSkeletronHand boss.rightHand),
    ("headDefense", toJson boss.headDefense), ("headDamage", toJson boss.headDamage),
    ("timeLeft", toJson boss.timeLeft)
  ]

private def encodeDungeonSlime (slime : DungeonSlime) : Json :=
  Json.mkObj (encodeFloatMotionFields slime.x slime.y slime.vx slime.vy ++ [
    ("id", toJson slime.id), ("health", toJson slime.health),
    ("lastJumpXBits", toJson slime.lastJumpX.toBits.toNat),
    ("jumpClock", toJson slime.jumpClock), ("retargetClock", toJson slime.retargetClock),
    ("direction", toJson slime.direction), ("frameClock", toJson slime.frameClock),
    ("frame", toJson slime.frame), ("timeLeft", toJson slime.timeLeft),
    ("collideX", toJson slime.collideX), ("collideY", toJson slime.collideY), ("wet", toJson slime.wet)
  ])

private def encodeDarkCaster (caster : DarkCaster) : Json :=
  Json.mkObj (encodeFloatMotionFields caster.x caster.y caster.vx caster.vy ++ [
    ("id", toJson caster.id), ("health", toJson caster.health), ("timeLeft", toJson caster.timeLeft),
    ("clock", toJson caster.clock), ("casting", toJson caster.casting), ("direction", toJson caster.direction),
    ("spriteDirection", toJson caster.spriteDirection),
    ("destination", (caster.destination.map encodePosition).getD .null)
  ])

private def encodeWaterSphere (sphere : WaterSphere) : Json :=
  Json.mkObj (encodeFloatMotionFields sphere.x sphere.y sphere.vx sphere.vy ++ [
    ("id", toJson sphere.id), ("timeLeft", toJson sphere.timeLeft)
  ])

private def encodeAngryBonesKind : AngryBonesKind → String
  | .regular => "regular"
  | .short => "short"
  | .bigBoned => "bigBoned"
  | .big => "big"
  | .bigMuscle => "bigMuscle"
  | .bigHelmet => "bigHelmet"

private def encodeAngryBones (bones : AngryBones) : Json :=
  Json.mkObj (encodeFloatMotionFields bones.x bones.y bones.vx bones.vy ++ [
    ("id", toJson bones.id), ("kind", toJson (encodeAngryBonesKind bones.kind)),
    ("health", toJson bones.health), ("timeLeft", toJson bones.timeLeft),
    ("direction", toJson bones.direction), ("directionY", toJson bones.directionY),
    ("spriteDirection", toJson bones.spriteDirection), ("oldXBits", toJson bones.oldX.toBits.toNat),
    ("ai0", toJson bones.ai0), ("ai1", toJson bones.ai1), ("ai2", toJson bones.ai2), ("ai3", toJson bones.ai3),
    ("justHit", toJson bones.justHit), ("wet", toJson bones.wet),
    ("frame", toJson bones.frame), ("frameClockBits", toJson bones.frameClock.toBits)
  ])

private def encodeCursedSkull (skull : CursedSkull) : Json :=
  Json.mkObj (encodeFloatMotionFields skull.x skull.y skull.vx skull.vy ++ [
    ("id", toJson skull.id), ("rotationBits", toJson skull.rotation.toBits.toNat),
    ("orbitBits", toJson skull.orbit.toBits.toNat), ("clock", toJson skull.clock),
    ("health", toJson skull.health), ("timeLeft", toJson skull.timeLeft),
    ("frame", toJson skull.frame), ("frameClock", toJson skull.frameClock),
    ("direction", toJson skull.direction), ("spriteDirection", toJson skull.spriteDirection)
  ])

private def encodeLavaSlime (slime : LavaSlime) : Json :=
  Json.mkObj (encodeFloatMotionFields slime.x slime.y slime.vx slime.vy ++ [
    ("id", toJson slime.id), ("lastJumpXBits", toJson slime.lastJumpX.toBits.toNat),
    ("health", toJson slime.health), ("jumpClock", toJson slime.jumpClock), ("retargetClock", toJson slime.retargetClock),
    ("direction", toJson slime.direction), ("directionY", toJson slime.directionY),
    ("collideX", toJson slime.collideX), ("collideY", toJson slime.collideY), ("wet", toJson slime.wet),
    ("frameClock", toJson slime.frameClock), ("frame", toJson slime.frame), ("timeLeft", toJson slime.timeLeft)
  ])

private def encodeUnderworldFlyer (flyer : UnderworldFlyer) : Json :=
  let kind := match flyer.kind with
    | .hellbat => "hellbat"
    | .demon => "demon"
    | .voodooDemon => "voodooDemon"
  Json.mkObj (encodeFloatMotionFields flyer.x flyer.y flyer.vx flyer.vy ++ [
    ("id", toJson flyer.id), ("kind", toJson kind), ("oldVxBits", toJson flyer.oldVx.toBits.toNat),
    ("oldVyBits", toJson flyer.oldVy.toBits.toNat), ("rotationBits", toJson flyer.rotation.toBits.toNat),
    ("health", toJson flyer.health), ("direction", toJson flyer.direction), ("directionY", toJson flyer.directionY),
    ("spriteDirection", toJson flyer.spriteDirection), ("collideX", toJson flyer.collideX),
    ("collideY", toJson flyer.collideY), ("wet", toJson flyer.wet), ("attackClock", toJson flyer.attackClock),
    ("searchClock", toJson flyer.searchClock), ("orbitClock", toJson flyer.orbitClock),
    ("frameClock", toJson flyer.frameClock), ("frame", toJson flyer.frame), ("timeLeft", toJson flyer.timeLeft)
  ])

private def encodeFireImp (imp : FireImp) : Json :=
  Json.mkObj (encodeFloatMotionFields imp.x imp.y imp.vx imp.vy ++ [
    ("id", toJson imp.id), ("health", toJson imp.health), ("timeLeft", toJson imp.timeLeft),
    ("clock", toJson imp.clock), ("casting", toJson imp.casting),
    ("destination", (imp.destination.map encodePosition).getD .null),
    ("direction", toJson imp.direction), ("spriteDirection", toJson imp.spriteDirection),
    ("frame", toJson imp.frame), ("frameClock", toJson imp.frameClock)
  ])

private def encodeBurningSphere (sphere : BurningSphere) : Json :=
  Json.mkObj (encodeFloatMotionFields sphere.x sphere.y sphere.vx sphere.vy ++ [
    ("id", toJson sphere.id), ("timeLeft", toJson sphere.timeLeft)])

private def encodeBoneSerpentSegment (segment : BoneSerpentSegment) : Json :=
  Json.mkObj [("xBits", toJson segment.x.toBits.toNat), ("yBits", toJson segment.y.toBits.toNat),
    ("rotationBits", toJson segment.rotation.toBits.toNat)]

private def encodeBoneSerpent (serpent : BoneSerpent) : Json :=
  Json.mkObj [("id", toJson serpent.id), ("segments", .arr (serpent.segments.map encodeBoneSerpentSegment)),
    ("bodyCount", toJson serpent.bodyCount),
    ("vxBits", toJson serpent.vx.toBits.toNat), ("vyBits", toJson serpent.vy.toBits.toNat),
    ("health", toJson serpent.health), ("timeLeft", toJson serpent.timeLeft), ("direction", toJson serpent.direction),
    ("directionY", toJson serpent.directionY), ("buried", toJson serpent.buried), ("soundDelay", toJson serpent.soundDelay)]

private def encodeUnderworldCritter (critter : UnderworldCritter) : Json :=
  let kind := match critter.kind with
    | .hellButterfly => "hellButterfly"
    | .lavafly => "lavafly"
    | .magmaSnail => "magmaSnail"
  Json.mkObj (encodeFloatMotionFields critter.x critter.y critter.vx critter.vy ++ [
    ("id", toJson critter.id), ("kind", toJson kind), ("rotationBits", toJson critter.rotation.toBits.toNat),
    ("health", toJson critter.health), ("timeLeft", toJson critter.timeLeft), ("direction", toJson critter.direction),
    ("directionY", toJson critter.directionY), ("spriteDirection", toJson critter.spriteDirection),
    ("collideX", toJson critter.collideX), ("collideY", toJson critter.collideY), ("wet", toJson critter.wet),
    ("frame", toJson critter.frame), ("frameClockBits", toJson critter.frameClock.toBits.toNat),
    ("ai0Bits", toJson critter.ai0.toBits.toNat), ("ai1Bits", toJson critter.ai1.toBits.toNat),
    ("ai2Bits", toJson critter.ai2.toBits.toNat), ("ai3Bits", toJson critter.ai3.toBits.toNat),
    ("local0Bits", toJson critter.local0.toBits.toNat), ("local1Bits", toJson critter.local1.toBits.toNat),
    ("local2Bits", toJson critter.local2.toBits.toNat), ("local3Bits", toJson critter.local3.toBits.toNat)])

private def encodeDemonSickle (shot : DemonSickle) : Json :=
  Json.mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("rotationBits", toJson shot.rotation.toBits.toNat), ("life", toJson shot.life),
    ("hits", toJson shot.hits), ("hostile", toJson shot.hostile), ("slot", toJson shot.slot)])

private def encodeWallEye (eye : WallEye) : Json :=
  Json.mkObj [("xBits", toJson eye.x.toBits.toNat), ("yBits", toJson eye.y.toBits.toNat),
    ("vyBits", toJson eye.vy.toBits.toNat), ("rotationBits", toJson eye.rotation.toBits.toNat),
    ("upper", toJson eye.upper), ("laserClock", toJson eye.laserClock), ("volley", toJson eye.volley),
    ("frame", toJson eye.frame), ("frameClock", toJson eye.frameClock)]

private def encodeWallOfFlesh (wall : WallOfFlesh) : Json :=
  Json.mkObj (encodeFloatMotionFields wall.x wall.y wall.vx wall.vy ++ [
    ("rotationBits", toJson wall.rotation.toBits.toNat), ("direction", toJson wall.direction),
    ("health", toJson wall.health), ("top", toJson wall.top), ("bottom", toJson wall.bottom),
    ("eyes", .arr (wall.eyes.map encodeWallEye)), ("leechClock", toJson wall.leechClock),
    ("leeches", toJson wall.leeches), ("roarClockBits", toJson wall.roarClock.toBits.toNat),
    ("deadFadeBits", toJson wall.deadFade.toBits.toNat),
    ("frame", toJson wall.frame), ("frameClock", toJson wall.frameClock)])

private def encodeWallHungry (hungry : WallHungry) : Json :=
  Json.mkObj (encodeFloatMotionFields hungry.x hungry.y hungry.vx hungry.vy ++ [
    ("id", toJson hungry.id), ("fractionBits", toJson hungry.fraction.toBits.toNat),
    ("rotationBits", toJson hungry.rotation.toBits.toNat), ("health", toJson hungry.health),
    ("hitPause", toJson hungry.hitPause), ("clock", toJson hungry.clock), ("attached", toJson hungry.attached)])

private def encodeWallLeech (leech : WallLeech) : Json :=
  Json.mkObj [("id", toJson leech.id), ("segments", .arr (leech.segments.map encodeBoneSerpentSegment)),
    ("vxBits", toJson leech.vx.toBits.toNat), ("vyBits", toJson leech.vy.toBits.toNat),
    ("health", toJson leech.health), ("timeLeft", toJson leech.timeLeft)]

private def encodeWallLaser (laser : WallLaser) : Json :=
  Json.mkObj (encodeFloatMotionFields laser.x laser.y laser.vx laser.vy ++ [
    ("damage", toJson laser.damage), ("life", toJson laser.life), ("penetration", toJson laser.penetration),
    ("slot", toJson laser.slot)])

private def encodeDroppedVoodooDoll (doll : DroppedVoodooDoll) : Json :=
  Json.mkObj [("x", toJson doll.x), ("y", toJson doll.y), ("vx", toJson doll.vx), ("vy", toJson doll.vy),
    ("count", toJson doll.count), ("pickupDelay", toJson doll.pickupDelay)]

private def encodeWraith (wraith : Wraith) : Json :=
  Json.mkObj (encodeFloatMotionFields wraith.x wraith.y wraith.vx wraith.vy ++ [
    ("id", toJson wraith.id), ("rotationBits", toJson wraith.rotation.toBits.toNat),
    ("anchorXBits", toJson wraith.anchorX.toBits.toNat), ("anchorYBits", toJson wraith.anchorY.toBits.toNat),
    ("health", toJson wraith.health), ("timeLeft", toJson wraith.timeLeft),
    ("direction", toJson wraith.direction), ("directionY", toJson wraith.directionY),
    ("spriteDirection", toJson wraith.spriteDirection),
    ("stuckClock", toJson wraith.stuckClock), ("justHit", toJson wraith.justHit),
    ("frame", toJson wraith.frame), ("frameClock", toJson wraith.frameClock)])

private def encodeCavernEnemyKind : CavernEnemyKind → String
  | .illuminantBat => "illuminantBat"
  | .illuminantSlime => "illuminantSlime"
  | .clinger => "clinger"
  | .floatyGross => "floatyGross"

private def encodeCavernEnemy (enemy : CavernEnemy) : Json :=
  Json.mkObj (encodeFloatMotionFields enemy.x enemy.y enemy.vx enemy.vy ++ [
    ("id", toJson enemy.id), ("kind", toJson (encodeCavernEnemyKind enemy.kind)),
    ("oldVxBits", toJson enemy.oldVx.toBits.toNat), ("oldVyBits", toJson enemy.oldVy.toBits.toNat),
    ("rotationBits", toJson enemy.rotation.toBits.toNat), ("lastJumpXBits", toJson enemy.lastJumpX.toBits.toNat),
    ("hoverAnchorXBits", toJson enemy.hoverAnchorX.toBits.toNat),
    ("hoverAnchorYBits", toJson enemy.hoverAnchorY.toBits.toNat),
    ("health", toJson enemy.health), ("timeLeft", toJson enemy.timeLeft), ("frame", toJson enemy.frame),
    ("frameClock", toJson enemy.frameClock), ("retargetClock", toJson enemy.retargetClock),
    ("searchClock", toJson enemy.searchClock), ("attackClock", toJson enemy.attackClock),
    ("direction", toJson enemy.direction), ("directionY", toJson enemy.directionY),
    ("spriteDirection", toJson enemy.spriteDirection), ("jumpClock", toJson enemy.jumpClock),
    ("orbitClock", toJson enemy.orbitClock), ("collideX", toJson enemy.collideX),
    ("collideY", toJson enemy.collideY), ("wet", toJson enemy.wet), ("justHit", toJson enemy.justHit),
    ("anchor", (enemy.anchor.map encodePosition).getD .null)])

private def encodeCavernFlame (flame : CavernFlame) : Json :=
  Json.mkObj (encodeFloatMotionFields flame.x flame.y flame.vx flame.vy ++ [
    ("life", toJson flame.life), ("clock", toJson flame.clock), ("slot", toJson flame.slot)])

private def encodeQueenPhase : QueenPhase → Nat
  | .rest => 0 | .grow => 1 | .shrink => 2 | .jumps => 3 | .smash => 4 | .gel => 5

private def encodeQueenSlime (queen : QueenSlime) : Json :=
  Json.mkObj (encodeFloatMotionFields queen.x queen.y queen.vx queen.vy ++ [
    ("phase", toJson (encodeQueenPhase queen.phase)), ("timer", toJson queen.timer),
    ("sequence", toJson queen.sequence), ("blocked", toJson queen.blocked),
    ("scaleBits", toJson queen.scale.toBits.toNat), ("rotationBits", toJson queen.rotation.toBits.toNat),
    ("health", toJson queen.health), ("minionHealthMark", toJson queen.minionHealthMark),
    ("destination", (queen.destination.map encodePosition).getD .null),
    ("timeLeft", toJson queen.timeLeft), ("direction", toJson queen.direction),
    ("wingClock", toJson queen.wingClock), ("wet", toJson queen.wet)])

private def encodeQueenMinion (slime : QueenMinion) : Json :=
  Json.mkObj (encodeFloatMotionFields slime.x slime.y slime.vx slime.vy ++ [
    ("id", toJson slime.id), ("kind", toJson slime.kind.sprite),
    ("health", toJson slime.health), ("timer", toJson slime.timer), ("jumps", toJson slime.jumps),
    ("charge", toJson slime.charge), ("timeLeft", toJson slime.timeLeft),
    ("direction", toJson slime.direction), ("wet", toJson slime.wet),
    ("frameClock", toJson slime.frameClock)])

private def encodeQueenShot (shot : QueenShot) : Json :=
  Json.mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("kind", toJson shot.kind.sprite), ("age", toJson shot.age),
    ("bounced", toJson shot.bounced), ("slot", toJson shot.slot)])

private def encodeTwinKind : TwinKind → String
  | .retinazer => "retinazer"
  | .spazmatism => "spazmatism"

private def encodeTwin (twin : Twin) : Json :=
  Json.mkObj (encodeFloatMotionFields twin.x twin.y twin.vx twin.vy ++ [
    ("kind", toJson (encodeTwinKind twin.kind)), ("rotationBits", toJson twin.rotation.toBits.toNat),
    ("ai0Bits", toJson twin.ai0.toBits.toNat), ("ai1Bits", toJson twin.ai1.toBits.toNat),
    ("ai2Bits", toJson twin.ai2.toBits.toNat), ("ai3Bits", toJson twin.ai3.toBits.toNat),
    ("localAI1Bits", toJson twin.localAI1.toBits.toNat), ("localAI2Bits", toJson twin.localAI2.toBits.toNat),
    ("powered", toJson twin.powered), ("health", toJson twin.health), ("timeLeft", toJson twin.timeLeft),
    ("frame", toJson twin.frame), ("frameClock", toJson twin.frameClock)])

private def encodeDestroyerSegment (part : DestroyerSegment) : Json :=
  Json.mkObj [
    ("xBits", toJson part.x.toBits.toNat), ("yBits", toJson part.y.toBits.toNat),
    ("rotationBits", toJson part.rotation.toBits.toNat), ("alpha", toJson part.alpha),
    ("laserClock", toJson part.laserClock), ("released", toJson part.released)]

private def encodeDestroyer (boss : Destroyer) : Json :=
  Json.mkObj [
    ("segments", .arr (boss.segments.map encodeDestroyerSegment)), ("health", toJson boss.health),
    ("vxBits", toJson boss.vx.toBits.toNat), ("vyBits", toJson boss.vy.toBits.toNat),
    ("burrowSoundDelay", toJson boss.burrowSoundDelay)]

private def encodeDestroyerProbe (probe : DestroyerProbe) : Json :=
  Json.mkObj (encodeFloatMotionFields probe.x probe.y probe.vx probe.vy ++ [
    ("id", toJson probe.id), ("health", toJson probe.health), ("rotationBits", toJson probe.rotation.toBits.toNat),
    ("facing", toJson probe.facing),
    ("laserClock", toJson probe.laserClock), ("timeLeft", toJson probe.timeLeft), ("justHit", toJson probe.justHit)])

private def encodePrimePhase : PrimePhase → String
  | .hover => "hover"
  | .spin => "spin"
  | .guardian => "guardian"
  | .retreat => "retreat"

private def encodePrimeHead (head : PrimeHead) : Json :=
  Json.mkObj (encodeFloatMotionFields head.x head.y head.vx head.vy ++ [
    ("rotationBits", toJson head.rotation.toBits.toNat), ("phase", toJson (encodePrimePhase head.phase)),
    ("timer", toJson head.timer), ("health", toJson head.health), ("timeLeft", toJson head.timeLeft),
    ("direction", toJson head.direction), ("defense", toJson head.defense), ("damage", toJson head.contactDamage),
    ("initialized", toJson head.initialized), ("frame", toJson head.frame), ("frameClock", toJson head.frameClock)])

private def encodePrimeArm (arm : PrimeArm) : Json :=
  Json.mkObj (encodeFloatMotionFields arm.x arm.y arm.vx arm.vy ++ [
    ("kind", toJson arm.kind.id), ("rotationBits", toJson arm.rotation.toBits.toNat),
    ("health", toJson arm.health), ("phase", toJson arm.phase), ("timer", toJson arm.timer),
    ("shotClock", toJson arm.shotClock), ("facing", toJson arm.facing), ("direction", toJson arm.direction),
    ("frame", toJson arm.frame), ("frameClock", toJson arm.frameClock)])

private def encodePrimeShot (shot : PrimeShot) : Json :=
  Json.mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("kind", toJson shot.kind.sprite), ("rotationBits", toJson shot.rotation.toBits.toNat),
    ("damage", toJson shot.damage), ("life", toJson shot.life), ("age", toJson shot.age),
    ("slot", toJson shot.slot), ("honeyWet", toJson shot.honeyWet)])

private def encodeTwinAttackKind : TwinAttackKind → String
  | .eyeLaser => "eyeLaser"
  | .deathLaser => "deathLaser"
  | .cursedFlame => "cursedFlame"
  | .eyeFire => "eyeFire"
  | .destroyerLaser => "destroyerLaser"
  | .probeLaser => "probeLaser"

private def encodeTwinShot (shot : TwinShot) : Json :=
  Json.mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("kind", toJson (encodeTwinAttackKind shot.kind)), ("damage", toJson shot.damage),
    ("life", toJson shot.life), ("age", toJson shot.age), ("slot", toJson shot.slot),
    ("penetration", toJson shot.penetration)])

private def encodePlanteraHead (actor : PlanteraHead) : Json :=
  Json.mkObj (encodeFloatMotionFields actor.x actor.y actor.vx actor.vy ++ [
    ("rotationBits", toJson actor.rotation.toBits.toNat),
    ("health", toJson actor.health), ("attackClock", toJson actor.attackClock),
    ("stage", toJson actor.stage), ("timeLeft", toJson actor.timeLeft),
    ("defense", toJson actor.defense), ("damage", toJson actor.contactDamage),
    ("enraged", toJson actor.enraged), ("frame", toJson actor.frame),
    ("frameClock", toJson actor.frameClock)])

private def encodePlanteraHook (actor : PlanteraHook) : Json :=
  Json.mkObj (encodeFloatMotionFields actor.x actor.y actor.vx actor.vy ++ [
    ("rotationBits", toJson actor.rotation.toBits.toNat),
    ("anchor", (actor.anchor.map encodePosition).getD .null), ("timer", toJson actor.timer),
    ("frame", toJson actor.frame), ("frameClock", toJson actor.frameClock)])

private def encodePlanteraTentacle (actor : PlanteraTentacle) : Json :=
  Json.mkObj (encodeFloatMotionFields actor.x actor.y actor.vx actor.vy ++ [
    ("rotationBits", toJson actor.rotation.toBits.toNat),
    ("id", toJson actor.id), ("health", toJson actor.health),
    ("timer", toJson actor.timer), ("offsetX", toJson actor.offsetX),
    ("offsetY", toJson actor.offsetY), ("facing", toJson actor.facing),
    ("frame", toJson actor.frame), ("frameClock", toJson actor.frameClock),
    ("timeLeft", toJson actor.timeLeft)])

private def encodePlanteraSpore (actor : PlanteraSpore) : Json :=
  Json.mkObj (encodeFloatMotionFields actor.x actor.y actor.vx actor.vy ++ [
    ("rotationBits", toJson actor.rotation.toBits.toNat),
    ("id", toJson actor.id), ("timeLeft", toJson actor.timeLeft)])

private def encodePlanteraShot (actor : PlanteraShot) : Json :=
  Json.mkObj (encodeFloatMotionFields actor.x actor.y actor.vx actor.vy ++ [
    ("rotationBits", toJson actor.rotation.toBits.toNat),
    ("kind", toJson actor.kind.sprite), ("damage", toJson actor.damage),
    ("life", toJson actor.life), ("age", toJson actor.age),
    ("slot", toJson actor.slot), ("honeyWet", toJson actor.honeyWet)])

private def encodeGolemBody (body : GolemBody) : Json :=
  .mkObj (encodeFloatMotionFields body.x body.y body.vx body.vy ++ [
    ("health", toJson body.health), ("alpha", toJson body.alpha),
    ("jumpState", toJson body.jumpState), ("jumpClock", toJson body.jumpClock),
    ("initialized", toJson body.initialized), ("noTileCollide", toJson body.noTileCollide),
    ("invulnerable", toJson body.invulnerable), ("enraged", toJson body.enraged),
    ("direction", toJson body.direction), ("frameY", toJson body.frameY), ("timeLeft", toJson body.timeLeft)])

private def encodeGolemHead (head : GolemHead) : Json :=
  .mkObj (encodeFloatMotionFields head.x head.y head.vx head.vy ++ [
    ("rotationBits", toJson head.rotation.toBits.toNat), ("fireClockBits", toJson head.fireClock.toBits.toNat),
    ("laserClockBits", toJson head.laserClock.toBits.toNat), ("health", toJson head.health),
    ("alpha", toJson head.alpha), ("frame", toJson head.frame), ("frameCounter", toJson head.frameCounter),
    ("detached", toJson head.detached), ("fast", toJson head.fast),
    ("fireMouth", toJson head.fireMouth), ("noTileCollide", toJson head.noTileCollide), ("look", toJson head.look)])

private def encodeGolemFist (fist : GolemFist) : Json :=
  .mkObj (encodeFloatMotionFields fist.x fist.y fist.vx fist.vy ++ [
    ("kind", toJson fist.kind.id), ("rotationBits", toJson fist.rotation.toBits.toNat), ("health", toJson fist.health),
    ("phase", toJson fist.phase), ("timer", toJson fist.timer), ("alpha", toJson fist.alpha),
    ("noTileCollide", toJson fist.noTileCollide), ("collideX", toJson fist.collideX), ("collideY", toJson fist.collideY)])

private def encodeGolemShot (shot : GolemShot) : Json :=
  .mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("kind", toJson shot.kind.sprite), ("rotationBits", toJson shot.rotation.toBits.toNat),
    ("damage", toJson shot.damage), ("life", toJson shot.life), ("age", toJson shot.age), ("alpha", toJson shot.alpha),
    ("direction", toJson shot.direction), ("slot", toJson shot.slot), ("bounces", toJson shot.bounces),
    ("honeyWet", toJson shot.honeyWet)])

private def encodeTempleTrapShot (shot : TempleTraps.Shot) : Json :=
  .mkObj (encodeFloatMotionFields shot.position.x shot.position.y shot.velocity.x shot.velocity.y ++ [
    ("kind", toJson shot.kind.sprite), ("rotationBits", toJson shot.rotation.toBits.toNat),
    ("damage", toJson shot.damage), ("life", toJson shot.life), ("age", toJson shot.age),
    ("direction", toJson shot.direction), ("slot", toJson shot.slot), ("retracting", toJson shot.retracting),
    ("anchor", (shot.anchor.map fun point => .mkObj [
      ("xBits", toJson point.x.toBits.toNat), ("yBits", toJson point.y.toBits.toNat)]).getD .null)])

private def encodeTempleEnemy (enemy : TempleEnemy) : Json :=
  .mkObj (encodeFloatMotionFields enemy.x enemy.y enemy.vx enemy.vy ++ [
    ("id", toJson enemy.id), ("kind", toJson enemy.kind.npcId), ("health", toJson enemy.health),
    ("previousXBits", toJson enemy.previousX.toBits.toNat), ("previousVxBits", toJson enemy.previousVx.toBits.toNat),
    ("previousVyBits", toJson enemy.previousVy.toBits.toNat), ("direction", toJson enemy.direction),
    ("directionY", toJson enemy.directionY), ("spriteDirection", toJson enemy.spriteDirection),
    ("collideX", toJson enemy.collideX), ("collideY", toJson enemy.collideY),
    ("ai0", toJson enemy.ai0), ("ai1", toJson enemy.ai1), ("ai2", toJson enemy.ai2), ("ai3", toJson enemy.ai3),
    ("frame", toJson enemy.frame), ("frameClockBits", toJson enemy.frameClock.toBits.toNat),
    ("rotationBits", toJson enemy.rotation.toBits.toNat), ("timeLeft", toJson enemy.timeLeft),
    ("wet", toJson enemy.wet), ("gfxOffsetYBits", toJson enemy.gfxOffsetY.toBits.toNat),
    ("stepSpeedBits", toJson enemy.stepSpeed.toBits.toNat), ("justHit", toJson enemy.justHit)])

private def encodeTempleFight (state : GameState) : Json :=
  .mkObj [("body", (state.golem.map encodeGolemBody).getD .null),
    ("head", (state.golemHead.map encodeGolemHead).getD .null),
    ("fists", .arr (state.golemFists.map encodeGolemFist)),
    ("golemShots", .arr (state.golemShots.map encodeGolemShot)), ("defeated", toJson state.golemDefeated),
    ("wires", encodeSparseBytes state.templeWires), ("cooldowns", .arr (state.templeCooldowns.map
      (fun (p, ticks) => .mkObj [("position", encodePosition p), ("ticks", toJson ticks)]))),
    ("trapShots", .arr (state.templeTrapShots.map encodeTempleTrapShot)),
    ("enemies", .arr (state.templeEnemies.map encodeTempleEnemy)), ("nextEnemyId", toJson state.nextTempleEnemyId),
    ("hellfireTicks", toJson state.hellfireTicks), ("hellfireProgress", toJson state.hellfireProgress),
    ("bleedingTicks", toJson state.bleedingTicks), ("spikeCooldown", toJson state.spikeCooldown)]

private def encodeAquaStream (stream : AquaStream) : Json :=
  Json.mkObj (encodeFloatMotionFields stream.x stream.y stream.vx stream.vy ++ [
    ("age", toJson stream.age), ("hitsLeft", toJson stream.hitsLeft),
    ("lavaSeed", toJson stream.lavaSeed.toNat), ("lavaBounceAge", toJson stream.lavaBounceAge),
    ("slot", toJson stream.slot)
  ])

private def encodeWorldAim (aim : WorldAim) : Json :=
  Json.mkObj [("x", toJson aim.x), ("y", toJson aim.y)]

private def encodeShortsword (thrust : ShortswordThrust) : Json :=
  Json.mkObj [
    ("vxBits", toJson thrust.vx.toBits.toNat), ("vyBits", toJson thrust.vy.toBits.toNat),
    ("age", toJson thrust.age), ("damage", toJson thrust.damage), ("slot", toJson thrust.slot)
  ]

private def encodeFlailPhase : FlailPhase → String
  | .spinning => "spinning"
  | .outbound => "outbound"
  | .returning => "returning"
  | .tethered => "tethered"
  | .forcedReturn => "forcedReturn"
  | .ricochet => "ricochet"
  | .dropped => "dropped"

private def encodeFlail (flail : Flail) : Json :=
  Json.mkObj (encodeFloatMotionFields flail.x flail.y flail.vx flail.vy ++ [
    ("rotationBits", toJson flail.rotation.toBits.toNat), ("phase", toJson (encodeFlailPhase flail.phase)),
    ("phaseTicks", toJson flail.phaseTicks), ("spinTicks", toJson flail.spinTicks),
    ("collisionCount", toJson flail.collisionCount), ("slot", toJson flail.slot),
    ("tileCollide", toJson flail.tileCollide), ("spriteDirection", toJson flail.spriteDirection)
  ])

private def encodeShotKind : ShotKind → String
  | .arrow => "arrow"
  | .woodenArrow => "woodenArrow"
  | .unholyArrow => "unholyArrow"
  | .hellfireArrow => "hellfireArrow"
  | .musketBullet => "musketBullet"
  | .undertakerBullet => "undertakerBullet"
  | .handgunBullet => "handgunBullet"
  | .thrownBone => "thrownBone"
  | .stormArrow => "stormArrow"
  | .hunterArrow => "hunterArrow"
  | .sentry => "sentry"
  | .herald => "herald"
  | .raidBolt => "raidBolt"
  | .ruinWave => "ruinWave"
  | .highlandBolt => "highlandBolt"
  | .cavernBeam => "cavernBeam"
  | .guideArrow => "guideArrow"

private def encodeProjectile (shot : Projectile) : Json :=
  Json.mkObj [
    ("kind", toJson (encodeShotKind shot.kind)), ("x", toJson shot.x), ("y", toJson shot.y),
    ("vx", toJson shot.vx), ("vy", toJson shot.vy), ("life", toJson shot.life),
    ("damageBonus", toJson shot.damageBonus), ("hitsLeft", toJson shot.hitsLeft), ("slot", toJson shot.slot)
  ]

private def encodeBomb (bomb : ActiveBomb) : Json :=
  Json.mkObj [
    ("x", toJson bomb.x), ("y", toJson bomb.y), ("vx", toJson bomb.vx), ("vy", toJson bomb.vy),
    ("fuse", toJson bomb.fuse), ("sticky", toJson bomb.sticky), ("dynamite", toJson bomb.dynamite),
    ("attached", (bomb.attached.map encodePosition).getD .null), ("slot", toJson bomb.slot)
  ]

private def encodeEquippedItem (item : Option Item) : Json :=
  (item.map fun value => toJson value.code.toNat).getD .null

private def encodeEquipment (equipment : Equipment) : Json :=
  Json.mkObj [
    ("head", encodeEquippedItem equipment.head),
    ("armor", encodeEquippedItem equipment.armor),
    ("legs", encodeEquippedItem equipment.legs),
    ("accessory", encodeEquippedItem equipment.accessory),
    ("hook", encodeEquippedItem equipment.hook),
    ("mount", encodeEquippedItem equipment.mount)
  ]

private def encodeWingedSlime (mount : WingedSlime) : Json :=
  Json.mkObj [
    ("vxBits", toJson mount.vx.toBits.toNat), ("vyBits", toJson mount.vy.toBits.toNat),
    ("remainderXBits", toJson mount.remainderX.toBits.toNat), ("remainderYBits", toJson mount.remainderY.toBits.toNat),
    ("flight", toJson mount.flight), ("jump", toJson mount.jump), ("wetSlime", toJson mount.wetSlime),
    ("jumpHeld", toJson mount.jumpHeld), ("stompTicks", toJson mount.stompTicks),
    ("stompCount", toJson mount.stompCount), ("frame", toJson mount.frame),
    ("frameCounterBits", toJson mount.frameCounter.toBits.toNat), ("wingFrame", toJson mount.wingFrame),
    ("wingCounter", toJson mount.wingCounter)]

private def encodeAssassinDash (dash : AssassinDash) : Json :=
  Json.mkObj [("tap", toJson dash.tap), ("velocityBits", toJson dash.velocity.toBits.toNat),
    ("cooldown", toJson dash.cooldown)]

private def encodeRaidFoeKind : RaidFoeKind → String
  | .runner => "runner"
  | .slinger => "slinger"
  | .brute => "brute"

private def encodeRaider (foe : Raider) : Json :=
  Json.mkObj [
    ("kind", toJson (encodeRaidFoeKind foe.kind)), ("body", encodePlayer foe.body),
    ("health", toJson foe.health), ("cooldown", toJson foe.cooldown)
  ]

private def encodeNpcTarget (target : NpcTarget) : Json :=
  let tagged := fun (kind : String) (fields : List (String × Json)) =>
    Json.mkObj (("kind", toJson kind) :: fields)
  let simple := fun kind => tagged kind []
  let identified := fun kind (id : Nat) => tagged kind [("id", toJson id)]
  match target with
  | .crawler => simple "crawler"
  | .warden => simple "warden"
  | .quarry => simple "quarry"
  | .sentry => simple "sentry"
  | .herald => simple "herald"
  | .sentinel => simple "sentinel"
  | .colossus => simple "colossus"
  | .surface kind => tagged "surface" [("surface", toJson (encodeSurfaceKind kind))]
  | .eye => simple "eye"
  | .eyeServant id => identified "eyeServant" id
  | .eaterSegment id => identified "eaterSegment" id
  | .brainCore => simple "brainCore"
  | .brainCreeper id => identified "brainCreeper" id
  | .evilFlyer id => identified "evilFlyer" id
  | .skeletron part =>
    let name := match part with
      | .head => "head"
      | .leftHand => "leftHand"
      | .rightHand => "rightHand"
    tagged "skeletron" [("part", toJson name)]
  | .dungeonGuardian => simple "dungeonGuardian"
  | .dungeonSlime id => identified "dungeonSlime" id
  | .darkCaster id => identified "darkCaster" id
  | .waterSphere id => identified "waterSphere" id
  | .angryBones id => identified "angryBones" id
  | .cursedSkull id => identified "cursedSkull" id
  | .lavaSlime id => identified "lavaSlime" id
  | .underworldFlyer id => identified "underworldFlyer" id
  | .fireImp id => identified "fireImp" id
  | .burningSphere id => identified "burningSphere" id
  | .boneSerpent id part => tagged "boneSerpent" [("id", toJson id), ("part", toJson part)]
  | .underworldCritter id => identified "underworldCritter" id
  | .wall part =>
    let name := match part with
      | .mouth => "mouth"
      | .upperEye => "upperEye"
      | .lowerEye => "lowerEye"
    tagged "wall" [("part", toJson name)]
  | .wallHungry id => identified "wallHungry" id
  | .wallLeech id part => tagged "wallLeech" [("id", toJson id), ("part", toJson part)]
  | .wraith id => identified "wraith" id
  | .hardmodeCavern id => identified "hardmodeCavern" id
  | .twin kind => tagged "twin" [("twin", toJson (encodeTwinKind kind))]
  | .destroyerSegment part => tagged "destroyerSegment" [("part", toJson part)]
  | .destroyerProbe id => tagged "destroyerProbe" [("id", toJson id)]
  | .primeHead => tagged "primeHead" []
  | .primeArm kind => tagged "primeArm" [("part", toJson kind.id)]
  | .planteraHead => tagged "planteraHead" []
  | .golemBody => tagged "golemBody" []
  | .golemHead => tagged "golemHead" []
  | .golemLeftFist => tagged "golemLeftFist" []
  | .golemRightFist => tagged "golemRightFist" []
  | .templeEnemy id => tagged "templeEnemy" [("id", toJson id)]
  | .planteraTentacle id => tagged "planteraTentacle" [("id", toJson id)]
  | .planteraSpore id => tagged "planteraSpore" [("id", toJson id)]
  | .queenSlime => tagged "queenSlime" []
  | .queenMinion id => tagged "queenMinion" [("id", toJson id)]
  | .raider wave kind => tagged "raider" [("wave", toJson wave), ("foe", toJson (encodeRaidFoeKind kind))]
  | .cave index => tagged "cave" [("index", toJson index)]

private def encodeNpcImmunity (cooldowns : Array (NpcTarget × Nat)) : Json :=
  .arr (cooldowns.map fun (target, ticks) =>
    Json.mkObj [("target", encodeNpcTarget target), ("ticks", toJson ticks)])

private def encodeDaggerPose (pose : DaggerPose) : Json :=
  Json.mkObj [("xBits", toJson pose.x.toBits.toNat), ("yBits", toJson pose.y.toBits.toNat),
    ("rotationBits", toJson pose.rotation.toBits.toNat)]

private def encodeDagger (summon : DaggerSummon) : Json :=
  let tagged (kind : String) (fields : List (String × Json)) := Json.mkObj (("kind", toJson kind) :: fields)
  let phase := match summon.phase with
    | .idle => tagged "idle" []
    | .chase target => tagged "chase" [("target", encodeNpcTarget target)]
    | .recoil elapsed => tagged "recoil" [("elapsed", toJson elapsed)]
  Json.mkObj [("slot", toJson summon.slot), ("pose", encodeDaggerPose summon.blade.pose),
    ("vxBits", toJson summon.blade.vx.toBits.toNat), ("vyBits", toJson summon.blade.vy.toBits.toNat),
    ("trail", .arr (summon.blade.trail.map encodeDaggerPose)),
    ("phase", phase), ("cooldowns", encodeNpcImmunity summon.cooldowns)]

private def encodeNpcBurns (burns : Array NpcBurn) : Json :=
  .arr (burns.map fun burn => Json.mkObj [
    ("target", encodeNpcTarget burn.target), ("onFire", toJson burn.onFire),
    ("shadowflame", toJson burn.shadowflame), ("hellfire", toJson burn.hellfire), ("progress", toJson burn.progress)])

private def encodeFlailAttack (attack : BlueMoonAttack) : Json :=
  Json.mkObj [("flail", encodeFlail attack.flail), ("aim", encodeWorldAim attack.aim),
    ("cooldowns", encodeNpcImmunity attack.cooldowns)]

private def encodeDarkLance (attack : DarkLance) : Json :=
  Json.mkObj [("vxBits", toJson attack.vx.toBits.toNat), ("vyBits", toJson attack.vy.toBits.toNat),
    ("age", toJson attack.age), ("animation", toJson attack.animation), ("facing", toJson attack.facing),
    ("slot", toJson attack.slot)]

private def encodeMagicMissileTrailPoint (point : MagicMissileTrailPoint) : Json :=
  Json.mkObj [("xBits", toJson point.x.toBits.toNat), ("yBits", toJson point.y.toBits.toNat),
    ("rotationBits", toJson point.rotation.toBits.toNat)]

private def encodeMissile (missile : MagicMissile) : Json :=
  Json.mkObj (encodeFloatMotionFields missile.x missile.y missile.vx missile.vy ++ [
    ("rotationBits", toJson missile.rotation.toBits.toNat), ("life", toJson missile.life),
    ("guided", toJson missile.guided), ("slot", toJson missile.slot), ("soundDelay", toJson missile.soundDelay),
    ("trail", .arr (missile.trail.map encodeMagicMissileTrailPoint))
  ])

private def encodeMagicMissile (attack : MagicMissileAttack) : Json :=
  Json.mkObj [("missile", encodeMissile attack.missile), ("aim", encodeWorldAim attack.aim),
    ("target", (attack.target.map encodeNpcTarget).getD .null)]

private def encodeFlowerFireball (shot : FlowerFireball) : Json :=
  Json.mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("life", toJson shot.life), ("bounces", toJson shot.bounces), ("wet", toJson shot.wet),
    ("slot", toJson shot.slot)])

private def encodeHellwingShot (shot : HellwingShot) : Json :=
  Json.mkObj (encodeFloatMotionFields shot.x shot.y shot.vx shot.vy ++ [
    ("goalVxBits", toJson shot.goalVx.toBits.toNat), ("goalVyBits", toJson shot.goalVy.toBits.toNat),
    ("life", toJson shot.life), ("hits", toJson shot.hits), ("unholy", toJson shot.unholy), ("slot", toJson shot.slot)])

private def encodeFlamelash (attack : FlamelashAttack) : Json :=
  let shot := attack.projectile
  let projectile := Json.mkObj [("missile", encodeMissile shot.missile), ("hitsLeft", toJson shot.hitsLeft),
    ("wet", toJson shot.wet), ("frame", toJson shot.frame), ("frameTicks", toJson shot.frameTicks),
    ("empoweredTicks", toJson shot.empoweredTicks)]
  Json.mkObj [("projectile", projectile), ("aim", encodeWorldAim attack.aim),
    ("target", (attack.target.map encodeNpcTarget).getD .null), ("cooldowns", encodeNpcImmunity attack.cooldowns)]

private def encodeValor (attack : ValorAttack) : Json :=
  let yoyo := attack.yoyo
  let body := Json.mkObj (encodeFloatMotionFields yoyo.x yoyo.y yoyo.vx yoyo.vy ++ [
    ("rotationBits", toJson yoyo.rotation.toBits.toNat),
    ("aimXBits", toJson yoyo.aimX.toBits.toNat), ("aimYBits", toJson yoyo.aimY.toBits.toNat),
    ("age", toJson yoyo.age), ("returning", toJson yoyo.returning), ("slot", toJson yoyo.slot),
    ("recoilStalled", toJson yoyo.recoilStalled)
  ])
  Json.mkObj [("yoyo", body), ("aim", encodeWorldAim attack.aim)]

private def encodeRaid : RaidEncounter → Json
  | .ready => Json.mkObj [("state", toJson "ready")]
  | .active wave => Json.mkObj [
    ("state", toJson "active"), ("wave", toJson wave.number),
    ("countdown", toJson wave.countdown), ("foes", .arr (wave.foes.map encodeRaider))
  ]
  | .reward position => Json.mkObj [("state", toJson "reward"), ("position", encodePosition position)]
  | .cleared => Json.mkObj [("state", toJson "cleared")]

private def encodeCaveSite : CaveSite → Json
  | .waiting cooldown => Json.mkObj [("state", toJson "waiting"), ("cooldown", toJson cooldown)]
  | .alive resident =>
    let kind := match resident.kind with
      | .slime => "slime"
      | .beetle => "beetle"
      | .skitter => "skitter"
      | .oreback => "oreback"
    Json.mkObj [
      ("state", toJson "alive"), ("body", encodePlayer resident.body), ("kind", toJson kind),
      ("health", toJson resident.health), ("hopIn", toJson resident.hopIn)
    ]
  | .loot position => Json.mkObj [("state", toJson "loot"), ("position", encodePosition position)]

private def encodeFishing : FishingState → Json
  | .idle => Json.mkObj [("state", toJson "idle")]
  | .waiting position ticks => Json.mkObj [
    ("state", toJson "waiting"), ("position", encodePosition position), ("ticks", toJson ticks)
  ]
  | .biting position ticks => Json.mkObj [
    ("state", toJson "biting"), ("position", encodePosition position), ("ticks", toJson ticks)
  ]
  | .soaking position clock => Json.mkObj [
    ("state", toJson "soaking"), ("position", encodePosition position), ("ticks", toJson clock)
  ]
  | .hooked position ticks fish baitItem => Json.mkObj [
    ("state", toJson "hooked"), ("position", encodePosition position), ("ticks", toJson ticks),
    ("catch", toJson fish.code.toNat), ("bait", toJson baitItem.code.toNat)
  ]
  | .bobber motion phase => Json.mkObj <| [
    ("state", toJson "bobber"), ("xBits", toJson motion.x.toBits.toNat), ("yBits", toJson motion.y.toBits.toNat),
    ("vxBits", toJson motion.vx.toBits.toNat), ("vyBits", toJson motion.vy.toBits.toNat),
    ("rotationBits", toJson motion.rotation.toBits.toNat), ("wet", toJson motion.wet),
    ("reelTicks", toJson motion.reelTicks), ("slot", toJson motion.slot)
  ] ++ (match phase with
    | .drifting clock => [("phase", toJson "drifting"), ("ticks", toJson clock)]
    | .hooked ticks fish baitItem => [
      ("phase", toJson "hooked"), ("ticks", toJson ticks),
      ("catch", toJson fish.code.toNat), ("bait", toJson baitItem.code.toNat)]
    | .returning fish => [("phase", toJson "returning"), ("catch", toJson (fish.map (·.code.toNat)))])

private def encodeCrop (crop : Crop) : Json :=
  Json.mkObj [("position", encodePosition crop.position), ("ticks", toJson crop.ticks)]

private def encodeRide : Option RailRide → Json
  | none => .null
  | some ride => Json.mkObj [
    ("rail", encodePosition ride.rail), ("next", (ride.next.map encodePosition).getD .null),
    ("progress", toJson ride.progress), ("speed", toJson ride.speed)
  ]

private def encodeHookKind : HookKind → Json
  | .basic => toJson "basic"
  | .dissonance => toJson "dissonance"

private def encodeHookAnchor (anchor : HookAnchor) : List (String × Json) :=
  [("tile", encodePosition anchor.tile), ("x", toJson anchor.x), ("y", toJson anchor.y),
    ("kind", encodeHookKind anchor.kind), ("life", toJson anchor.life)]

private def encodeGrapple : GrappleState → Json
  | .idle => Json.mkObj [("state", toJson "idle")]
  | .flying hook => Json.mkObj [
    ("state", toJson "flying"), ("x", toJson hook.x), ("y", toJson hook.y),
    ("vx", toJson hook.vx), ("vy", toJson hook.vy), ("life", toJson hook.life),
    ("kind", encodeHookKind hook.kind), ("returning", toJson hook.returning),
    ("held", (hook.held.map (Json.mkObj ∘ encodeHookAnchor)).getD .null)
  ]
  | .latched anchor => Json.mkObj (("state", toJson "latched") :: encodeHookAnchor anchor)

/-- Serialize the state needed to continue playing. The current status message is transient. -/
def encode (state : GameState) : String :=
  Json.compress <| Json.mkObj [
    ("version", toJson version),
    ("seed", toJson state.seed),
    ("worldName", toJson state.worldName),
    ("worldCreated", toJson state.worldCreated),
    ("worldFavorite", toJson state.worldFavorite),
    ("width", toJson state.world.width),
    ("height", toJson state.world.height),
    ("tiles", .arr (state.world.tiles.data.map fun byte => toJson byte.toNat)),
    ("walls", encodeSparseBytes state.walls),
    ("furniture", .arr (state.furniture.map encodeFurniture)),
    ("bedSpawnPoint", (state.bedSpawnPoint.map encodePosition).getD .null),
    ("legacyEarly", toJson state.legacyEarly),
    ("player", encodePlayer state.player),
    ("respawnTicks", toJson state.respawnTicks),
    ("hotTileTicks", toJson state.hotTileTicks),
    ("burningTicks", toJson state.burningTicks),
    ("toolCooldown", toJson state.toolCooldown),
    ("tileHits", .arr (state.tileHits.map encodeTileHit)),
    ("gelDrops", .arr (state.gelDrops.map encodeGelDrop)),
    ("nightDrops", .arr (state.nightDrops.map encodeNightDrop)),
    ("eye", encodeEye state.eye),
    ("eyeDefeated", toJson state.eyeDefeated),
    ("evilRelicsBroken", toJson state.evilRelicsBroken),
    ("eater", encodeEater state.eater),
    ("brain", encodeBrain state.brain),
    ("evilBossDefeated", toJson state.evilBossDefeated),
    ("evilEnemies", .arr (state.evilEnemies.map encodeEvilFlyer)),
    ("dungeonInstalled", toJson state.dungeonInstalled),
    ("lowerDungeonInstalled", toJson state.lowerDungeonInstalled),
    ("blueMoonDungeonInstalled", toJson state.blueMoonDungeonInstalled),
    ("magicMissileDungeonInstalled", toJson state.magicMissileDungeonInstalled),
    ("deepDungeonInstalled", toJson state.deepDungeonInstalled),
    ("shadowKeyRecovery", toJson state.shadowKeyRecovery),
    ("skeletron", encodeSkeletron state.skeletron),
    ("skeletronDefeated", toJson state.skeletronDefeated),
    ("oldManWaiting", toJson state.oldManWaiting),
    ("dungeonGuardian", (state.dungeonGuardian.map encodeEyeMotion).getD .null),
    ("dungeonGuardianHealth", toJson state.dungeonGuardianHealth),
    ("dungeonSlimes", .arr (state.dungeonSlimes.map encodeDungeonSlime)),
    ("darkCasters", .arr (state.darkCasters.map encodeDarkCaster)),
    ("waterSpheres", .arr (state.waterSpheres.map encodeWaterSphere)),
    ("nextDarkCasterId", toJson state.nextDarkCasterId),
    ("nextWaterSphereId", toJson state.nextWaterSphereId),
    ("angryBones", .arr (state.angryBones.map encodeAngryBones)),
    ("nextAngryBonesId", toJson state.nextAngryBonesId),
    ("cursedSkulls", .arr (state.cursedSkulls.map encodeCursedSkull)),
    ("nextCursedSkullId", toJson state.nextCursedSkullId),
    ("cursedTicks", toJson state.cursedTicks),
    ("darknessTicks", toJson state.darknessTicks),
    ("obsidianSkinTicks", toJson state.obsidianSkinTicks),
    ("netSwingTicks", toJson state.netSwingTicks),
    ("lavaSlimes", .arr (state.lavaSlimes.map encodeLavaSlime)),
    ("underworldFlyers", .arr (state.underworldFlyers.map encodeUnderworldFlyer)),
    ("fireImps", .arr (state.fireImps.map encodeFireImp)),
    ("burningSpheres", .arr (state.burningSpheres.map encodeBurningSphere)),
    ("boneSerpent", (state.boneSerpent.map encodeBoneSerpent).getD .null),
    ("underworldCritters", .arr (state.underworldCritters.map encodeUnderworldCritter)),
    ("nextUnderworldNpcId", toJson state.nextUnderworldNpcId),
    ("demonSickles", .arr (state.demonSickles.map encodeDemonSickle)),
    ("wallOfFlesh", (state.wallOfFlesh.map encodeWallOfFlesh).getD .null),
    ("wallHungry", .arr (state.wallHungry.map encodeWallHungry)),
    ("wallLeeches", .arr (state.wallLeeches.map encodeWallLeech)),
    ("wallLasers", .arr (state.wallLasers.map encodeWallLaser)),
    ("hardmode", toJson state.hardmode), ("horrified", toJson state.horrified),
    ("altarsSmashed", toJson state.altarsSmashed), ("hardmodeOreMask", toJson state.hardmodeOreMask),
    ("tongued", toJson state.tongued), ("guideRespawnPending", toJson state.guideRespawnPending),
    ("voodooDolls", .arr (state.voodooDolls.map encodeDroppedVoodooDoll)),
    ("wraiths", .arr (state.wraiths.map encodeWraith)), ("nextWraithId", toJson state.nextWraithId),
    ("slowTicks", toJson state.slowTicks),
    ("hardmodeCavern", .arr (state.hardmodeCavern.map encodeCavernEnemy)),
    ("nextHardmodeCavernId", toJson state.nextHardmodeCavernId),
    ("cavernFlames", .arr (state.cavernFlames.map encodeCavernFlame)),
    ("cursedInfernoTicks", toJson state.cursedInfernoTicks),
    ("cursedInfernoProgress", toJson state.cursedInfernoProgress),
    ("weakTicks", toJson state.weakTicks),
    ("retinazer", (state.retinazer.map encodeTwin).getD .null),
    ("spazmatism", (state.spazmatism.map encodeTwin).getD .null),
    ("twinsDefeated", toJson state.twinsDefeated), ("twinsPending", toJson state.twinsPending),
    ("mechanicalNights", toJson state.mechanicalNights),
    ("twinShots", .arr (state.twinShots.map encodeTwinShot)),
    ("destroyer", (state.destroyer.map encodeDestroyer).getD .null),
    ("destroyerPending", toJson state.destroyerPending), ("destroyerDefeated", toJson state.destroyerDefeated),
    ("destroyerProbes", .arr (state.destroyerProbes.map encodeDestroyerProbe)),
    ("nextDestroyerProbeId", toJson state.nextDestroyerProbeId),
    ("skeletronPrime", (state.skeletronPrime.map encodePrimeHead).getD .null),
    ("primeArms", .arr (state.primeArms.map encodePrimeArm)),
    ("primeShots", .arr (state.primeShots.map encodePrimeShot)),
    ("primePending", toJson state.primePending),
    ("primeDefeated", toJson state.primeDefeated),
    ("plantera", (state.plantera.map encodePlanteraHead).getD .null),
    ("planteraHooks", .arr (state.planteraHooks.map encodePlanteraHook)),
    ("planteraTentacles", .arr (state.planteraTentacles.map encodePlanteraTentacle)),
    ("planteraSpores", .arr (state.planteraSpores.map encodePlanteraSpore)),
    ("planteraShots", .arr (state.planteraShots.map encodePlanteraShot)),
    ("jungleTemple", encodeTempleFight state),
    ("nextPlanteraPartId", toJson state.nextPlanteraPartId),
    ("planteraBulbs", .arr (state.planteraBulbs.map encodePosition)),
    ("planteraUnlocked", toJson state.planteraUnlocked),
    ("planteraDefeated", toJson state.planteraDefeated),
    ("poisonedTicks", toJson state.poisonedTicks),
    ("poisonProgress", toJson state.poisonProgress),
    ("queenSlime", (state.queenSlime.map encodeQueenSlime).getD .null),
    ("queenMinions", .arr (state.queenMinions.map encodeQueenMinion)),
    ("queenShots", .arr (state.queenShots.map encodeQueenShot)),
    ("queenSlimeDefeated", toJson state.queenSlimeDefeated),
    ("nextQueenMinionId", toJson state.nextQueenMinionId),
    ("dagger", (state.dagger.map encodeDagger).getD .null),
    ("minionTarget", (state.minionTarget.map encodeNpcTarget).getD .null),
    ("aquaStreams", .arr (state.aquaStreams.map encodeAquaStream)),
    ("aquaBurst", (state.aquaBurst.map encodeWorldAim).getD .null),
    ("aquaAnimation", toJson state.aquaAnimation),
    ("aquaShotDelay", toJson state.aquaShotDelay),
    ("mana", toJson state.mana),
    ("manaRegenDelay", toJson state.manaRegenDelay),
    ("manaRegenProgress", toJson state.manaRegenProgress),
    ("bombs", .arr (state.bombs.map encodeBomb)),
    ("inventory", .arr (state.inventory.map encodeSlot)),
    ("cursorStack", encodeSlot state.cursorStack),
    ("selected", toJson state.selected),
    ("combat", encodeCombat state.combat),
    ("warden", encodeWarden state.warden),
    ("surface", encodeSurface state.surface),
    ("weather", Json.mkObj [("day", toJson state.weather.day), ("rainTicks", toJson state.weather.rainTicks),
      ("rainStrength", toJson state.weather.rainStrength),
      ("cloudActivityBits", toJson state.weather.cloudActivity.toBits.toNat),
      ("cloudOpacityBits", toJson state.weather.cloudOpacity.toBits.toNat)]),
    ("crateLifeCrystals", toJson state.crateLifeCrystals),
    ("chests", .arr (state.chests.map encodeChest)),
    ("homeDoor", (state.homeDoor.map encodePosition).getD .null),
    ("townGuide", encodeTownGuide state.townGuide),
    ("water", encodeSparseBytes state.water.levels),
    ("magma", encodeSparseBytes state.magma.levels),
    ("honey", encodeSparseBytes state.honey.levels),
    ("breath", toJson state.breath),
    ("quarry", encodeQuarry state.quarry),
    ("sentry", encodeSentry state.sentry),
    ("storm", encodeStorm state.storm),
    ("projectiles", .arr (state.projectiles.map encodeProjectile)),
    ("shortsword", (state.shortsword.map encodeShortsword).getD .null),
    ("blueMoon", (state.blueMoon.map encodeFlailAttack).getD .null),
    ("sunfury", (state.sunfury.map encodeFlailAttack).getD .null),
    ("darkLance", (state.darkLance.map encodeDarkLance).getD .null),
    ("magicMissiles", .arr (state.magicMissiles.map encodeMagicMissile)),
    ("flowerFireballs", .arr (state.flowerFireballs.map encodeFlowerFireball)),
    ("hellwingShots", .arr (state.hellwingShots.map encodeHellwingShot)),
    ("flamelashes", .arr (state.flamelashes.map encodeFlamelash)),
    ("valor", (state.valor.map encodeValor).getD .null),
    ("npcImmunity", encodeNpcImmunity state.npcImmunity),
    ("npcBurns", encodeNpcBurns state.npcBurns),
    ("bowCooldown", toJson state.bowCooldown),
    ("chart", encodeSparseBytes state.chart.cells),
    ("coins", toJson state.coins),
    ("merchantStock", toJson state.merchantStock),
    ("merchantDoor", (state.merchantDoor.map encodePosition).getD .null),
    ("equipment", encodeEquipment state.equipment),
    ("wingedSlime", (state.wingedSlime.map encodeWingedSlime).getD .null),
    ("assassinDash", encodeAssassinDash state.assassinDash),
    ("weaponArmorStats", .arr (state.weaponArmorStats.map fun (slot, stats) =>
      .arr #[toJson slot, toJson stats.damageBonus, toJson stats.criticalChance])),
    ("raid", encodeRaid state.raid),
    ("homeAnchor", (state.homeAnchor.map encodePosition).getD .null),
    ("ecology", .arr (state.ecology.sites.map encodeCaveSite)),
    ("fishing", encodeFishing state.fishing),
    ("crops", .arr (state.crops.map encodeCrop)),
    ("gills", toJson state.gills),
    ("cratePotion", toJson state.cratePotion),
    ("fishingPotion", toJson state.fishingPotion),
    ("sonarPotion", toJson state.sonarPotion),
    ("angler", Json.mkObj [
      ("rescued", toJson state.angler.rescued), ("questIndex", toJson state.angler.questIndex),
      ("completed", toJson state.angler.completed), ("turnedIn", toJson state.angler.turnedIn),
      ("position", (state.angler.position.map encodePosition).getD .null),
      ("home", (state.angler.home.map encodePosition).getD .null)]),
    ("barkskin", toJson state.barkskin),
    ("potionSickness", toJson state.potionSickness),
    ("honeyBuff", toJson state.honeyBuff),
    ("ruin", encodeRuin state.ruin),
    ("carts", .arr (state.carts.map encodePosition)),
    ("ride", encodeRide state.ride),
    ("grapple", encodeGrapple state.grapple),
    ("cavern", encodeCavern state.cavern)
  ]

private def readField {α : Type} [FromJson α] (json : Json) (key : String) : Except String α :=
  (json.getObjVal? key >>= fromJson?).mapError fun error => s!"Invalid '{key}' field ({error})"

private def decodeProjectileSlot (json : Json) (saveVersion : Nat) : Except String (Option Nat) := do
  if saveVersion < 41 then return none
  let slot : Option Nat ← readField json "slot"
  if slot.any (· ≥ maxProjectileSlots) then throw "Projectile slot lies outside the shared pool."
  return slot

private def decodeTileBytes (array : Array Json) (expectedSize maxCode : Nat) :
    Except String ByteArray := do
  if array.size != expectedSize then
    throw s!"Expected {expectedSize} tiles, found {array.size}."
  let mut tiles := ByteArray.emptyWithCapacity expectedSize
  for entry in array do
    let code ← entry.getNat?
    let byte := UInt8.ofNat code
    if code > maxCode || (Tile.ofCode? byte).isNone then
      throw s!"Unknown tile code {code} at index {tiles.size}."
    tiles := tiles.push byte
  return tiles

private def decodeSlot (maxCode : Nat) : Json → Except String (Option Stack)
  | .null => pure none
  | json => do
    let code : Nat ← readField json "item"
    let some item := if code ≤ maxCode then Item.ofCode? code.toUInt16 else none
      | throw s!"Unknown item code {code}."
    if item == .heart then throw "A Heart is collected directly and cannot be stored."
    let count : Nat ← readField json "count"
    if count == 0 || count > Inventory.stackLimit item then
      throw s!"Invalid stack size {count}."
    return some { item, count }

private def decodeInventory (json : Json) (saveVersion maxItemCode : Nat) : Except String Inventory := do
  let slots ← (← json.getObjVal? "inventory").getArr?
  let expected := if saveVersion == 1 then 6 else if saveVersion == 2 then 8
    else if saveVersion < 30 then 24 else Inventory.slotCount
  if slots.size != expected then
    throw s!"Expected {expected} inventory slots, found {slots.size}."
  let inventory ← slots.mapM (decodeSlot maxItemCode)
  let coins : Nat ← if saveVersion < 8 then pure 0 else readField json "coins"
  if saveVersion ≥ 30 then
    for index in [:inventory.size] do
      if let some stack := inventory[index]! then
        if !Inventory.acceptsSlot index stack.item then
          throw s!"{stack.item.name} cannot be stored in inventory slot {index}."
    if Inventory.coinBalance inventory != coins then
      throw "Saved coin balance does not match the inventory."
    return inventory
  -- The first row gained two hotbar slots. Keep legacy backpack offsets after them.
  let inventory := if saveVersion == 1 then (inventory.push none).push (some ⟨.wood, startingWood⟩)
    else inventory
  let inventory := inventory.extract 0 8 ++ #[none, none] ++ inventory.extract 8 inventory.size
  let inventory := inventory ++ Array.replicate (Inventory.slotCount - inventory.size) none
  if coins == 0 then return inventory
  let some credited := Inventory.creditCoins inventory coins
    | throw "The legacy coin balance exceeds the inventory's coin capacity."
  return credited

private def decodePlayer (json : Json) (world : World) (maxVx := maxHorizontalSpeed)
    (allowTerrainOverlap := false) (maxVy := maxVerticalSpeed) :
    Except String Player := do
  let x : Int ← readField json "x"
  let y : Int ← readField json "y"
  let vx : Int ← readField json "vx"
  let vy : Int ← readField json "vy"
  let grounded : Bool ← readField json "grounded"
  if vx.natAbs > maxVx.toNat || vy.natAbs > maxVy.toNat then
    throw "Player velocity exceeds the simulation bounds."
  let player : Player := { x, y, vx, vy, grounded }
  let obstructed := if allowTerrainOverlap then
    x < 0 || y < 0 || x + playerWidth > (world.width : Int) * tileUnits ||
      y + playerHeight > (world.height : Int) * tileUnits
    else player.collides world
  if obstructed then
    throw "Player position overlaps terrain or lies outside the world."
  return player

private def decodeCrawler (world : World) : Json → Except String (Option Crawler)
  | .null => pure none
  | json => do
    let body ← (decodePlayer (← json.getObjVal? "body") world).mapError fun error =>
      s!"Invalid crawler body ({error})"
    let health : Nat ← readField json "health"
    if health == 0 || health > crawlerMaxHealth then
      throw s!"Invalid crawler health {health}."
    let hopIn : Nat ← readField json "hopIn"
    if hopIn > crawlerHopDelay then
      throw s!"Invalid crawler hop timer {hopIn}."
    return some { body, health, hopIn }

private def decodeCombat (json : Json) (world : World) (saveVersion : Nat) : Except String CombatState := do
  let rawHealth : Nat ← readField json "health"
  let lifeCrystals : Nat ← if saveVersion < 18 then pure 0 else readField json "lifeCrystals"
  let crystalLimit := if saveVersion < 24 then 5 else maxLifeCrystals
  if lifeCrystals > crystalLimit then throw "Too many Life Crystal upgrades."
  if saveVersion < 24 && (rawHealth == 0 || rawHealth > 5 + lifeCrystals) then
    throw s!"Invalid legacy player health {rawHealth}."
  let health := if saveVersion < 24 then rawHealth * 20 else rawHealth
  let onFire : Nat ← if saveVersion < 24 then pure 0 else readField json "onFire"
  let fireProgress : Nat ← if saveVersion < 24 then pure 0 else readField json "fireProgress"
  let lavaCooldown : Nat ← if saveVersion < 24 then pure 0 else readField json "lavaCooldown"
  let regenTime : Nat ← if saveVersion < 24 then pure 0 else readField json "regenTime"
  let regenProgress : Nat ← if saveVersion < 24 then pure 0 else readField json "regenProgress"
  if onFire > lavaBurnTicks || fireProgress ≥ burnDamageInterval || (onFire == 0 && fireProgress != 0) then
    throw "The burning timer exceeds its simulation bounds."
  if lavaCooldown > lavaHurtCooldown then throw "The lava cooldown exceeds its simulation bounds."
  if regenTime > maxRegenTime || regenProgress ≥ regenPointThreshold then
    throw "The regeneration timer exceeds its simulation bounds."
  let invulnerable : Nat ← readField json "invulnerable"
  let swing : Nat ← readField json "swing"
  let invulnerabilityLimit := if saveVersion < 24 then 48 else spawnInvulnerability
  let swingLimit := if saveVersion < 25 then swingCooldown
    else if saveVersion == 25 then 21
    else if saveVersion < 48 then 22
    else
      let normal := max maxSwordUseTime Item.batBat.swordUseTime
      -- A swing begun while Weak keeps its full animation even after the buff expires.
      if saveVersion < 52 then normal else (1000 * normal + 948) / 949
  if invulnerable > invulnerabilityLimit || swing > swingLimit then
    throw "Combat cooldown exceeds the simulation bounds."
  let facing : Int ← readField json "facing"
  if facing != -1 && facing != 1 then
    throw s!"Invalid facing direction {facing}."
  let crawler ← decodeCrawler world (← json.getObjVal? "crawler")
  let cleared : Bool ← readField json "cleared"
  if cleared && crawler.isSome then
    throw "A cleared world cannot have an active crawler."
  let combat : CombatState := {
    health, lifeCrystals, invulnerable, swing, facing, crawler, cleared
    onFire, fireProgress, lavaCooldown, regenTime, regenProgress }
  if health > combat.healthLimit then throw s!"Invalid player health {health}."
  return combat

private def decodeWardenPhase : String → Except String WardenPhase
  | "approach" => pure .approach
  | "windup" => pure .windup
  | "charge" => pure .charge
  | "recover" => pure .recover
  | value => throw s!"Unknown Warden phase '{value}'."

private def decodeWarden (json : Json) (world : World) : Except String WardenEncounter := do
  match ← readField (α := String) json "state" with
  | "ready" => pure .ready
  | "fighting" => do
    let body ← (decodePlayer (← json.getObjVal? "body") world wardenMaxSpeed).mapError fun error =>
      s!"Invalid Warden body ({error})"
    if !inWardenArena body.centerTile then
      throw "Warden body lies outside the arena."
    let health : Nat ← readField json "health"
    if health == 0 || health > wardenMaxHealth then
      throw s!"Invalid Warden health {health}."
    let facing : Int ← readField json "facing"
    if facing != -1 && facing != 1 then
      throw s!"Invalid Warden facing direction {facing}."
    let phase ← decodeWardenPhase (← readField json "phase")
    let ticks : Nat ← readField json "ticks"
    let boss : Warden := { body, health, phase, ticks, facing }
    let duration := boss.phaseDuration
    if ticks > duration || (duration > 0 && ticks == 0) then
      throw s!"Invalid Warden phase timer {ticks}."
    pure (.fighting boss)
  | "reward" => do
    let position : TilePos := ⟨← readField json "x", ← readField json "y"⟩
    if !inWardenArena position then
      throw "Warden reward lies outside the arena."
    pure (.reward position)
  | "cleared" => pure .cleared
  | value => throw s!"Unknown Warden encounter state '{value}'."

private def decodePosition (json : Json) : Except String TilePos := do
  return ⟨← readField json "x", ← readField json "y"⟩

private def decodeWorldAim (json : Json) : Except String WorldAim := do
  return ⟨← readField json "x", ← readField json "y"⟩

private def decodeBombs (json : Json) (world : World) (saveVersion : Nat) : Except String (Array ActiveBomb) := do
  let entries ← json.getArr?
  if entries.size > maxActiveBombs then throw "Too many lit bombs."
  entries.mapM fun entry => do
    let x : Int ← readField entry "x"
    let y : Int ← readField entry "y"
    let vx : Int ← readField entry "vx"
    let vy : Int ← readField entry "vy"
    let fuse : Nat ← readField entry "fuse"
    let sticky : Bool ← readField entry "sticky"
    let dynamite : Bool ← if saveVersion < 49 then pure false else readField entry "dynamite"
    let attached ← match ← entry.getObjVal? "attached" with
      | .null => pure none
      | position => some <$> decodePosition position
    let slot ← decodeProjectileSlot entry saveVersion
    let bomb : ActiveBomb := { x, y, vx, vy, fuse, sticky, dynamite, attached, slot }
    if !bomb.valid world then throw "Invalid lit bomb position, motion or fuse."
    return bomb

private def decodeEyeMotion (json : Json) : Except String EyeMotion := do
  return ⟨← readField json "x", ← readField json "y", ← readField json "vx", ← readField json "vy"⟩

private def decodeEyePhase : String → Except String EyePhase
  | "firstHover" => pure .firstHover
  | "firstCharge" => pure .firstCharge
  | "transformSpinUp" => pure .transformSpinUp
  | "transformSpinDown" => pure .transformSpinDown
  | "secondHover" => pure .secondHover
  | "secondCharge" => pure .secondCharge
  | "retreat" => pure .retreat
  | value => throw s!"Unknown Eye of Cthulhu phase '{value}'."

private def decodeEye (json : Json) (world : World) (saveVersion : Nat) : Except String (Option EyeOfCthulhu) := do
  if json == .null then return none
  let motion ← decodeEyeMotion (← json.getObjVal? "motion")
  let health : Nat ← readField json "health"
  let phase ← decodeEyePhase (← readField json "phase")
  let ticks : Nat ← readField json "ticks"
  let servantTicks : Nat ← readField json "servantTicks"
  let charges : Nat ← readField json "charges"
  let entries ← (← json.getObjVal? "servants").getArr?
  if entries.size > eyeMaxServants then throw "Too many Servants of Cthulhu."
  let servants : Array EyeServant ← entries.mapIdxM fun index entry => do
    let id : Nat ← if saveVersion < 41 then pure (index + 1) else readField entry "id"
    let motion ← decodeEyeMotion (← entry.getObjVal? "motion")
    let health : Nat ← readField entry "health"
    return { id, motion, health }
  let nextServantId : Nat ← if saveVersion < 41 then pure (servants.size + 1) else readField json "nextServantId"
  let eye : EyeOfCthulhu := { motion, health, phase, ticks, servantTicks, charges, servants, nextServantId }
  if !eye.valid world then throw "Invalid Eye of Cthulhu motion, health or phase state."
  return some eye

private def decodeEater (json : Json) (world : World) : Except String (Option EaterOfWorlds) := do
  if json == .null then return none
  let retreating : Bool ← readField json "retreating"
  let motionTick : Nat ← readField json "motionTick"
  let entries ← (← json.getObjVal? "chains").getArr?
  if entries.size > eaterSegmentCount then throw "Too many Eater of Worlds chains."
  let chains : Array (Array WormSegment) ← entries.mapM fun entry => do
    let segments ← entry.getArr?
    if segments.size > eaterSegmentCount then throw "Too many Eater of Worlds segments."
    segments.mapM fun data => do
      let id : Nat ← readField data "id"
      let motion ← decodeEyeMotion (← data.getObjVal? "motion")
      let health : Nat ← readField data "health"
      return { id, motion, health }
  let boss : EaterOfWorlds := { chains, retreating, motionTick }
  if !boss.valid world then throw "Invalid Eater of Worlds chain, motion or health."
  return some boss

private def decodeBrainPhase : String → Except String BrainPhase
  | "shielded" => pure .shielded
  | "exposed" => pure .exposed
  | "retreat" => pure .retreat
  | value => throw s!"Unknown Brain of Cthulhu phase '{value}'."

private def decodeBrain (json : Json) (world : World) : Except String (Option BrainOfCthulhu) := do
  if json == .null then return none
  let motion ← decodeEyeMotion (← json.getObjVal? "motion")
  let health : Nat ← readField json "health"
  let phase ← decodeBrainPhase (← readField json "phase")
  let timer : Nat ← readField json "timer"
  let warp : Nat ← readField json "warp"
  let serial : Nat ← readField json "serial"
  let entries ← (← json.getObjVal? "creepers").getArr?
  if entries.size > brainCreeperCount then throw "Too many Creepers."
  let creepers : Array BrainCreeper ← entries.mapM fun data => do
    let id : Nat ← readField data "id"
    let motion ← decodeEyeMotion (← data.getObjVal? "motion")
    let health : Nat ← readField data "health"
    let rush : Nat ← readField data "rush"
    return { id, motion, health, rush }
  let boss : BrainOfCthulhu := { motion, health, phase, timer, warp, serial, creepers }
  if !boss.valid world then throw "Invalid Brain of Cthulhu phase, motion or Creepers."
  return some boss

private def decodeEvilFlyers (json : Json) (world : World) : Except String (Array EvilFlyer) := do
  let entries ← json.getArr?
  if entries.size > maxEvilFlyers then throw "Too many evil biome enemies."
  let mut enemies : Array EvilFlyer := #[]
  for data in entries do
    let id : Nat ← readField data "id"
    let kind : EvilFlyerKind ← match ← readField (α := String) data "kind" with
      | "eaterOfSouls" => pure .eaterOfSouls
      | "crimera" => pure .crimera
      | value => throw s!"Unknown evil biome enemy '{value}'."
    let motion ← decodeEyeMotion (← data.getObjVal? "motion")
    let health : Nat ← readField data "health"
    let ticks : Nat ← readField data "ticks"
    let diving : Bool ← readField data "diving"
    let enemy : EvilFlyer := { id, kind, motion, health, ticks, diving }
    if !enemy.valid world || enemies.any (·.id == id) then throw "Invalid or repeated evil biome enemy."
    enemies := enemies.push enemy
  return enemies

private def decodeFloat32Bits (json : Json) (part key : String) : Except String Float32 := do
  let bits : Nat ← readField json key
  if bits ≥ UInt32.size then throw s!"{part} field '{key}' exceeds 32 bits."
  let value := Float32.ofBits (UInt32.ofNat bits)
  if !value.isFinite then throw s!"{part} field '{key}' must be finite."
  return value

private def decodeFloatMotion (json : Json) (part : String) : Except String (Float32 × Float32 × Float32 × Float32) := do
  let readBits := decodeFloat32Bits json part
  return (← readBits "xBits", ← readBits "yBits", ← readBits "vxBits", ← readBits "vyBits")

private def decodeWeather (json : Json) (legacyEarly : Bool) : Except String Weather := do
  let weather : Weather := {
    day := ← readField json "day", rainTicks := ← readField json "rainTicks"
    rainStrength := ← readField json "rainStrength"
    cloudActivity := ← decodeFloat32Bits json "Weather" "cloudActivityBits"
    cloudOpacity := ← decodeFloat32Bits json "Weather" "cloudOpacityBits" }
  if weather.rainTicks > 223000 || weather.rainStrength > 90 ||
      weather.raining != (weather.rainStrength ≥ 5) ||
      (!weather.raining && weather.rainStrength != 0) ||
      weather.cloudActivity < -345600 || weather.cloudActivity > 172800 ||
      weather.cloudOpacity < 0 || weather.cloudOpacity > 1 ||
      (legacyEarly && weather != {}) then throw "Invalid world weather."
  return weather

private def decodeSkeletronHand (data : Json) (world : World) (head : SkeletronHead)
    (side : Int) (saveVersion : Nat) : Except String (Option SkeletronHand) := do
  if data == .null then return none
  let health : Nat ← readField data "health"
  let phase : SkeletronHandPhase ← match ← readField (α := String) data "phase" with
    | "hover" => pure .hover
    | "lift" => pure .lift
    | "slam" => pure .slam
    | "reset" => pure .reset
    | "windup" => pure .windup
    | "swipe" => pure .swipe
    | value => throw s!"Unknown Skeletron hand phase '{value}'."
  let ticks : Nat ← readField data "ticks"
  if saveVersion < 33 then
    let motion ← decodeEyeMotion (← data.getObjVal? "motion")
    let speed := match phase with
      | .slam => 18 * 16
      | .swipe => 17 * 16
      | _ => 512
    let clockValid := match phase with
      | .hover | .reset => ticks < 300
      | _ => ticks == 0
    if !clockValid || !motion.valid world speed then
      throw "Invalid historical Skeletron hand motion or timer."
    return some (SkeletronHand.ofMotion motion health phase ticks head side)
  let readBits := decodeFloat32Bits data "Skeletron hand"
  let x ← readBits "xBits"
  let y ← readBits "yBits"
  let vx ← readBits "vxBits"
  let vy ← readBits "vyBits"
  let rotation ← readBits "rotationBits"
  let hand : SkeletronHand := { x, y, vx, vy, rotation, health, phase, ticks }
  if saveVersion < 34 && !hand.motion.valid world (64 * 16) then
    throw "Invalid historical Skeletron hand motion."
  return some hand

private def decodeSkeletronHead (json : Json) : Except String SkeletronHead := do
  let readBits := decodeFloat32Bits json "Skeletron head"
  let x ← readBits "xBits"
  let y ← readBits "yBits"
  let vx ← readBits "vxBits"
  let vy ← readBits "vyBits"
  let rotation ← readBits "rotationBits"
  let direction : Int ← readField json "direction"
  return { x, y, vx, vy, rotation, direction }

private def decodeSkeletron (json : Json) (world : World) (player : Player)
    (saveVersion : Nat) : Except String (Option Skeletron) := do
  if json == .null then return none
  let health : Nat ← readField json "health"
  let phase : SkeletronPhase ← match ← readField (α := String) json "phase" with
    | "hover" => pure .hover
    | "spin" => pure .spin
    | "enraged" => pure .enraged
    | "retreat" => pure .retreat
    | value => throw s!"Unknown Skeletron phase '{value}'."
  let ticks : Nat ← readField json "ticks"
  if saveVersion < 33 then
    let motionTick : Nat ← readField json "motionTick"
    if motionTick ≥ 100 then throw "Invalid historical Skeletron hand motion tick."
  let (head, headDefense) ← if saveVersion < 32 then do
      let motion ← decodeEyeMotion (← json.getObjVal? "motion")
      let (defense, speed) : Nat × Int := match phase with
        | .spin => if ticks == 0 then (10, 512) else (0, 24)
        | .enraged => (9999, 128)
        | .hover | .retreat => (10, 512)
      -- Check the old integer bounds before converting through Int32.
      if !motion.valid world speed then throw "Invalid historical Skeletron head motion."
      let direction := if bodyCenterX player < motion.x then -1 else 1
      pure (SkeletronHead.ofMotion motion direction, defense)
    else do
      let head ← decodeSkeletronHead (← json.getObjVal? "head")
      let defense : Nat ← readField json "headDefense"
      pure (head, defense)
  let (ticks, headDamage, timeLeft) ← if saveVersion < 34 then do
      let phaseValid := match phase with
        | .hover => headDefense == 10 || (ticks == 0 && headDefense == 0)
        | .spin => headDefense == (if ticks == 0 then 10 else 0)
        | .enraged => headDefense == 9999 && ticks == 0
        | .retreat => headDefense == 10 && ticks < 50
      if !phaseValid then throw "Invalid historical Skeletron head defense or phase timer."
      -- Earlier withdrawals used the phase counter to count up to despawn.
      if phase == .retreat then pure (0, 32, 50 - ticks)
      else pure (ticks, if phase == .enraged then 9999 else 32, 749)
    else do
      let damage : Nat ← readField json "headDamage"
      let timeLeft : Nat ← readField json "timeLeft"
      pure (ticks, damage, timeLeft)
  let leftHand ← decodeSkeletronHand (← json.getObjVal? "leftHand") world head (-1) saveVersion
  let rightHand ← decodeSkeletronHand (← json.getObjVal? "rightHand") world head 1 saveVersion
  let boss : Skeletron := { head, health, phase, ticks, leftHand, rightHand, headDefense, headDamage, timeLeft }
  if 32 ≤ saveVersion && saveVersion < 34 && !boss.motion.valid world (64 * 16) then
    throw "Invalid historical Skeletron head motion."
  if !boss.valid world then throw "Invalid Skeletron head or hand motion, health or phase state."
  return some boss

private def decodeDungeonSlimes (json : Json) (world : World) : Except String (Array DungeonSlime) := do
  let entries ← json.getArr?
  if entries.size > maxDungeonSlimes then throw "Too many Dungeon Slimes."
  let mut slimes : Array DungeonSlime := #[]
  for data in entries do
    let id : Nat ← readField data "id"
    let health : Nat ← readField data "health"
    let readBits := decodeFloat32Bits data "Dungeon Slime"
    let x ← readBits "xBits"
    let y ← readBits "yBits"
    let vx ← readBits "vxBits"
    let vy ← readBits "vyBits"
    let lastJumpX ← readBits "lastJumpXBits"
    let jumpClock : Int ← readField data "jumpClock"
    let retargetClock : Nat ← readField data "retargetClock"
    let direction : Int ← readField data "direction"
    let frameClock : Nat ← readField data "frameClock"
    let frame : Nat ← readField data "frame"
    let timeLeft : Nat ← readField data "timeLeft"
    let collideX : Bool ← readField data "collideX"
    let collideY : Bool ← readField data "collideY"
    let wet : Bool ← readField data "wet"
    let slime : DungeonSlime := {
      id, health, x, y, vx, vy, lastJumpX, jumpClock, retargetClock, direction
      frameClock, frame, timeLeft, collideX, collideY, wet }
    if !slime.valid world || slimes.any (·.id == id) then throw "Invalid or repeated Dungeon Slime."
    slimes := slimes.push slime
  return slimes

private def decodeDarkCasters (json : Json) (world : World) : Except String (Array DarkCaster) := do
  let entries ← json.getArr?
  if entries.size > maxDarkCasters then throw "Too many Dark Casters."
  let mut casters : Array DarkCaster := #[]
  for data in entries do
    let id : Nat ← readField data "id"
    let health : Nat ← readField data "health"
    let timeLeft : Nat ← readField data "timeLeft"
    let clock : Nat ← readField data "clock"
    let casting : Nat ← readField data "casting"
    let readBits := decodeFloat32Bits data "Dark Caster"
    let x ← readBits "xBits"
    let y ← readBits "yBits"
    let vx ← readBits "vxBits"
    let vy ← readBits "vyBits"
    let direction : Int ← readField data "direction"
    let spriteDirection : Int ← readField data "spriteDirection"
    let destination ← match ← data.getObjVal? "destination" with
      | .null => pure none
      | position => do
        let tile ← decodePosition position
        if !world.contains tile || tile == ⟨0, 0⟩ then throw "Invalid Dark Caster destination."
        pure (some tile)
    let caster : DarkCaster := {
      id, health, timeLeft, clock, casting, x, y, vx, vy, destination, direction, spriteDirection }
    if id == 0 || !caster.valid world || casters.any (·.id == id) then
      throw "Invalid or repeated Dark Caster."
    casters := casters.push caster
  return casters

private def decodeWaterSpheres (json : Json) (world : World) : Except String (Array WaterSphere) := do
  let entries ← json.getArr?
  if entries.size > maxWaterSpheres then throw "Too many Water Spheres."
  let mut spheres : Array WaterSphere := #[]
  for data in entries do
    let id : Nat ← readField data "id"
    let timeLeft : Nat ← readField data "timeLeft"
    let readBits := decodeFloat32Bits data "Water Sphere"
    let x ← readBits "xBits"
    let y ← readBits "yBits"
    let vx ← readBits "vxBits"
    let vy ← readBits "vyBits"
    let sphere : WaterSphere := { id, timeLeft, x, y, vx, vy }
    if id == 0 || !sphere.valid world || spheres.any (·.id == id) then
      throw "Invalid or repeated Water Sphere."
    spheres := spheres.push sphere
  return spheres

private def decodeAngryBones (json : Json) (world : World) : Except String (Array AngryBones) := do
  let entries ← json.getArr?
  if entries.size > maxAngryBones then throw "Too many Angry Bones."
  let mut bones : Array AngryBones := #[]
  let mut lastId := 0
  for data in entries do
    let id : Nat ← readField data "id"
    if id ≤ lastId then throw "Angry Bones IDs must be positive and strictly increasing."
    let kind : AngryBonesKind ← match ← readField (α := String) data "kind" with
      | "regular" => pure .regular
      | "short" => pure .short
      | "bigBoned" => pure .bigBoned
      | "big" => pure .big
      | "bigMuscle" => pure .bigMuscle
      | "bigHelmet" => pure .bigHelmet
      | value => throw s!"Unknown Angry Bones kind '{value}'."
    let readBits := decodeFloat32Bits data "Angry Bones"
    let x ← readBits "xBits"
    let y ← readBits "yBits"
    let vx ← readBits "vxBits"
    let vy ← readBits "vyBits"
    let oldX ← readBits "oldXBits"
    let frameClockBits : UInt64 ← readField data "frameClockBits"
    let frameClock := Float.ofBits frameClockBits
    if !frameClock.isFinite then throw "Angry Bones frameClockBits must be finite."
    let health : Nat ← readField data "health"
    let timeLeft : Nat ← readField data "timeLeft"
    let direction : Int ← readField data "direction"
    let directionY : Int ← readField data "directionY"
    let spriteDirection : Int ← readField data "spriteDirection"
    let ai0 : Nat ← readField data "ai0"
    let ai1 : Nat ← readField data "ai1"
    let ai2 : Nat ← readField data "ai2"
    let ai3 : Nat ← readField data "ai3"
    let justHit : Bool ← readField data "justHit"
    let wet : Bool ← readField data "wet"
    let frame : Nat ← readField data "frame"
    let actor : AngryBones := {
      id, kind, x, y, vx, vy, health, timeLeft, direction, directionY, spriteDirection, oldX
      ai0, ai1, ai2, ai3, justHit, wet, frame, frameClock }
    if !actor.valid world then throw "Invalid Angry Bones motion, health or AI state."
    bones := bones.push actor
    lastId := id
  return bones

private def decodeCursedSkulls (json : Json) (world : World) : Except String (Array CursedSkull) := do
  let entries ← json.getArr?
  if entries.size > maxCursedSkulls then throw "Too many Cursed Skulls."
  let mut skulls : Array CursedSkull := #[]
  let mut lastId := 0
  for data in entries do
    let id : Nat ← readField data "id"
    if id ≤ lastId then throw "Cursed Skull IDs must be positive and strictly increasing."
    let readBits := decodeFloat32Bits data "Cursed Skull"
    let x ← readBits "xBits"
    let y ← readBits "yBits"
    let vx ← readBits "vxBits"
    let vy ← readBits "vyBits"
    let rotation ← readBits "rotationBits"
    let orbit ← readBits "orbitBits"
    let clock : Nat ← readField data "clock"
    let health : Nat ← readField data "health"
    let timeLeft : Nat ← readField data "timeLeft"
    let frame : Nat ← readField data "frame"
    let frameClock : Nat ← readField data "frameClock"
    let direction : Int ← readField data "direction"
    let spriteDirection : Int ← readField data "spriteDirection"
    let skull : CursedSkull := {
      id, x, y, vx, vy, rotation, orbit, clock, health, timeLeft, frame, frameClock
      direction, spriteDirection }
    if !skull.valid world then throw "Invalid Cursed Skull motion, health or AI state."
    skulls := skulls.push skull
    lastId := id
  return skulls

private def decodeActorArray (json : Json) (name : String) (limit : Nat) (parse : Json → Except String α) :
    Except String (Array α) := do
  let entries ← json.getArr?
  if entries.size > limit then throw s!"Too many active {name}."
  entries.mapM parse

private def decodeLavaSlimes (json : Json) : Except String (Array LavaSlime) :=
  decodeActorArray json "Lava Slimes" maxUnderworldNpcs fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Lava Slime"
    return {
      x, y, vx, vy, id := ← readField entry "id", health := ← readField entry "health"
      lastJumpX := ← decodeFloat32Bits entry "Lava Slime" "lastJumpXBits"
      jumpClock := ← readField entry "jumpClock", retargetClock := ← readField entry "retargetClock"
      direction := ← readField entry "direction", directionY := ← readField entry "directionY"
      collideX := ← readField entry "collideX", collideY := ← readField entry "collideY", wet := ← readField entry "wet"
      frameClock := ← readField entry "frameClock", frame := ← readField entry "frame", timeLeft := ← readField entry "timeLeft" }

private def decodeUnderworldFlyers (json : Json) : Except String (Array UnderworldFlyer) :=
  decodeActorArray json "Underworld flyers" maxUnderworldNpcs fun entry => do
    let readBits := decodeFloat32Bits entry "Underworld flyer"
    let (x, y, vx, vy) ← decodeFloatMotion entry "Underworld flyer"
    let kind ← match ← readField (α := String) entry "kind" with
      | "hellbat" => pure UnderworldFlyerKind.hellbat
      | "demon" => pure .demon
      | "voodooDemon" => pure .voodooDemon
      | value => throw s!"Unknown Underworld flyer kind '{value}'."
    return {
      x, y, vx, vy, kind, id := ← readField entry "id", health := ← readField entry "health"
      oldVx := ← readBits "oldVxBits", oldVy := ← readBits "oldVyBits", rotation := ← readBits "rotationBits"
      direction := ← readField entry "direction", directionY := ← readField entry "directionY"
      spriteDirection := ← readField entry "spriteDirection", collideX := ← readField entry "collideX"
      collideY := ← readField entry "collideY", wet := ← readField entry "wet", attackClock := ← readField entry "attackClock"
      searchClock := ← readField entry "searchClock", orbitClock := ← readField entry "orbitClock"
      frameClock := ← readField entry "frameClock", frame := ← readField entry "frame", timeLeft := ← readField entry "timeLeft" }

private def decodeFireImps (json : Json) : Except String (Array FireImp) :=
  decodeActorArray json "Fire Imps" maxFireImps fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Fire Imp"
    let destination ← match ← entry.getObjVal? "destination" with
      | .null => pure none
      | value => some <$> decodePosition value
    return {
      x, y, vx, vy, destination, id := ← readField entry "id", health := ← readField entry "health"
      timeLeft := ← readField entry "timeLeft", clock := ← readField entry "clock", casting := ← readField entry "casting"
      direction := ← readField entry "direction", spriteDirection := ← readField entry "spriteDirection"
      frame := ← readField entry "frame", frameClock := ← readField entry "frameClock" }

private def decodeBurningSpheres (json : Json) : Except String (Array BurningSphere) :=
  decodeActorArray json "Burning Spheres" maxBurningSpheres fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Burning Sphere"
    return {x, y, vx, vy, id := ← readField entry "id", timeLeft := ← readField entry "timeLeft"}

private def decodeLinkedSegment (json : Json) (name : String) : Except String BoneSerpentSegment := do
  let readBits := decodeFloat32Bits json name
  return {x := ← readBits "xBits", y := ← readBits "yBits", rotation := ← readBits "rotationBits"}

private def decodeBoneSerpent (json : Json) : Except String (Option BoneSerpent) := do
  if json == .null then return none
  let segments ← decodeActorArray (← json.getObjVal? "segments") "Bone Serpent segments" maxBoneSerpentSegments
    (decodeLinkedSegment · "Bone Serpent segment")
  let readBits := decodeFloat32Bits json "Bone Serpent"
  return some {
    segments, id := ← readField json "id", vx := ← readBits "vxBits", vy := ← readBits "vyBits"
    bodyCount := ← readField json "bodyCount", health := ← readField json "health", timeLeft := ← readField json "timeLeft"
    direction := ← readField json "direction", directionY := ← readField json "directionY"
    buried := ← readField json "buried", soundDelay := ← readField json "soundDelay"}

private def decodeUnderworldCritters (json : Json) : Except String (Array UnderworldCritter) :=
  decodeActorArray json "Underworld critters" maxUnderworldCritters fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Underworld critter"
    let readBits := decodeFloat32Bits entry "Underworld critter"
    let kind ← match ← readField (α := String) entry "kind" with
      | "hellButterfly" => pure UnderworldCritterKind.hellButterfly
      | "lavafly" => pure .lavafly
      | "magmaSnail" => pure .magmaSnail
      | value => throw s!"Unknown Underworld critter kind '{value}'."
    let frameClockBits : Nat ← readField entry "frameClockBits"
    if frameClockBits ≥ UInt64.size then throw "Underworld critter frameClockBits exceeds 64 bits."
    let frameClock := Float.ofBits frameClockBits.toUInt64
    if !frameClock.isFinite then throw "Underworld critter frameClockBits must be finite."
    return {
      x, y, vx, vy, kind, frameClock, id := ← readField entry "id", health := ← readField entry "health"
      rotation := ← readBits "rotationBits", timeLeft := ← readField entry "timeLeft"
      direction := ← readField entry "direction", directionY := ← readField entry "directionY"
      spriteDirection := ← readField entry "spriteDirection", collideX := ← readField entry "collideX"
      collideY := ← readField entry "collideY", wet := ← readField entry "wet"
      frame := ← readField entry "frame"
      ai0 := ← readBits "ai0Bits", ai1 := ← readBits "ai1Bits", ai2 := ← readBits "ai2Bits", ai3 := ← readBits "ai3Bits"
      local0 := ← readBits "local0Bits", local1 := ← readBits "local1Bits", local2 := ← readBits "local2Bits"
      local3 := ← readBits "local3Bits" }

private def decodeWallEye (json : Json) : Except String WallEye := do
  let readBits := decodeFloat32Bits json "Wall eye"
  return {
    x := ← readBits "xBits", y := ← readBits "yBits", vy := ← readBits "vyBits",
    rotation := ← readBits "rotationBits", upper := ← readField json "upper",
    laserClock := ← readField json "laserClock", volley := ← readField json "volley",
    frame := ← readField json "frame", frameClock := ← readField json "frameClock" }

private def decodeWallOfFlesh (json : Json) : Except String (Option WallOfFlesh) := do
  if json == .null then return none
  let (x, y, vx, vy) ← decodeFloatMotion json "Wall of Flesh"
  let readBits := decodeFloat32Bits json "Wall of Flesh"
  let eyes ← decodeActorArray (← json.getObjVal? "eyes") "Wall eyes" 2 decodeWallEye
  return some {
    x, y, vx, vy, eyes, rotation := ← readBits "rotationBits", direction := ← readField json "direction",
    health := ← readField json "health", top := ← readField json "top", bottom := ← readField json "bottom",
    leechClock := ← readField json "leechClock", leeches := ← readField json "leeches",
    roarClock := ← readBits "roarClockBits", deadFade := ← readBits "deadFadeBits",
    frame := ← readField json "frame", frameClock := ← readField json "frameClock" }

private def decodeWallHungry (json : Json) : Except String (Array WallHungry) :=
  decodeActorArray json "Hungry" maxWallHungry fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Hungry"
    let readBits := decodeFloat32Bits entry "Hungry"
    return {
      x, y, vx, vy, id := ← readField entry "id", fraction := ← readBits "fractionBits",
      rotation := ← readBits "rotationBits", health := ← readField entry "health",
      hitPause := ← readField entry "hitPause", clock := ← readField entry "clock",
      attached := ← readField entry "attached" }

private def decodeWallLeeches (json : Json) : Except String (Array WallLeech) :=
  decodeActorArray json "Wall leeches" maxWallLeeches fun entry => do
    let segments ← decodeActorArray (← entry.getObjVal? "segments") "Wall leech segments" maxWallLeechSegments
      (decodeLinkedSegment · "Wall leech segment")
    let readBits := decodeFloat32Bits entry "Wall leech"
    return {
      segments, id := ← readField entry "id", vx := ← readBits "vxBits", vy := ← readBits "vyBits",
      health := ← readField entry "health", timeLeft := ← readField entry "timeLeft" }

private def decodeWallLasers (json : Json) (saveVersion : Nat) : Except String (Array WallLaser) :=
  decodeActorArray json "Wall lasers" maxProjectileSlots fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Wall laser"
    return {
      x, y, vx, vy, damage := ← readField entry "damage", life := ← readField entry "life",
      penetration := ← readField entry "penetration", slot := ← decodeProjectileSlot entry saveVersion }

private def decodeDroppedVoodooDolls (json : Json) : Except String (Array DroppedVoodooDoll) :=
  decodeActorArray json "dropped Guide Voodoo Dolls" maxDroppedVoodooDolls fun entry => do
    return {
      x := ← readField entry "x", y := ← readField entry "y", vx := ← readField entry "vx",
      vy := ← readField entry "vy", count := ← readField entry "count", pickupDelay := ← readField entry "pickupDelay" }

private def decodeWraiths (json : Json) : Except String (Array Wraith) :=
  decodeActorArray json "Wraiths" maxWraiths fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Wraith"
    let readBits := decodeFloat32Bits entry "Wraith"
    return {
      x, y, vx, vy, id := ← readField entry "id", health := ← readField entry "health",
      rotation := ← readBits "rotationBits", anchorX := ← readBits "anchorXBits", anchorY := ← readBits "anchorYBits",
      timeLeft := ← readField entry "timeLeft", direction := ← readField entry "direction",
      directionY := ← readField entry "directionY", spriteDirection := ← readField entry "spriteDirection",
      stuckClock := ← readField entry "stuckClock",
      justHit := ← readField entry "justHit",
      frame := ← readField entry "frame", frameClock := ← readField entry "frameClock" }

private def decodeCavernEnemyKind : String → Except String CavernEnemyKind
  | "illuminantBat" => pure .illuminantBat
  | "illuminantSlime" => pure .illuminantSlime
  | "clinger" => pure .clinger
  | "floatyGross" => pure .floatyGross
  | kind => throw s!"Unknown Hardmode cavern enemy kind '{kind}'."

private def decodeCavernEnemies (json : Json) : Except String (Array CavernEnemy) :=
  decodeActorArray json "Hardmode cavern enemies" maxCavernEnemies fun entry => do
    let kind ← decodeCavernEnemyKind (← readField entry "kind")
    let (x, y, vx, vy) ← decodeFloatMotion entry "Hardmode cavern enemy"
    let readBits := decodeFloat32Bits entry "Hardmode cavern enemy"
    let anchor ← match ← entry.getObjVal? "anchor" with
      | .null => pure none
      | cell => some <$> decodePosition cell
    return {
      x, y, vx, vy, kind, anchor, id := ← readField entry "id",
      oldVx := ← readBits "oldVxBits", oldVy := ← readBits "oldVyBits",
      rotation := ← readBits "rotationBits", lastJumpX := ← readBits "lastJumpXBits",
      hoverAnchorX := ← readBits "hoverAnchorXBits", hoverAnchorY := ← readBits "hoverAnchorYBits",
      health := ← readField entry "health", timeLeft := ← readField entry "timeLeft",
      frame := ← readField entry "frame", frameClock := ← readField entry "frameClock",
      retargetClock := ← readField entry "retargetClock", searchClock := ← readField entry "searchClock",
      attackClock := ← readField entry "attackClock", direction := ← readField entry "direction",
      directionY := ← readField entry "directionY", spriteDirection := ← readField entry "spriteDirection",
      jumpClock := ← readField entry "jumpClock", orbitClock := ← readField entry "orbitClock",
      collideX := ← readField entry "collideX", collideY := ← readField entry "collideY",
      wet := ← readField entry "wet", justHit := ← readField entry "justHit" }

private def decodeCavernFlames (json : Json) (saveVersion : Nat) : Except String (Array CavernFlame) :=
  decodeActorArray json "Clinger flames" maxCavernFlames fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Clinger flame"
    return {
      x, y, vx, vy, life := ← readField entry "life", clock := ← readField entry "clock",
      slot := ← decodeProjectileSlot entry saveVersion }

private def decodeQueenSlime (json : Json) : Except String (Option QueenSlime) := do
  if json == .null then return none
  let (x, y, vx, vy) ← decodeFloatMotion json "Queen Slime"
  let phaseCode : Nat ← readField json "phase"
  let some phase := #[QueenPhase.rest, .grow, .shrink, .jumps, .smash, .gel][phaseCode]?
    | throw "Unknown Queen Slime phase."
  let destination ← match ← json.getObjVal? "destination" with
    | .null => pure none
    | value => some <$> decodePosition value
  let bits := decodeFloat32Bits json "Queen Slime"
  return some {
    x, y, vx, vy, phase, destination, timer := ← readField json "timer",
    sequence := ← readField json "sequence", blocked := ← readField json "blocked",
    scale := ← bits "scaleBits", rotation := ← bits "rotationBits",
    health := ← readField json "health", minionHealthMark := ← readField json "minionHealthMark",
    timeLeft := ← readField json "timeLeft", direction := ← readField json "direction",
    wingClock := ← readField json "wingClock", wet := ← readField json "wet" }

private def decodeQueenMinions (json : Json) : Except String (Array QueenMinion) :=
  decodeActorArray json "Queen Slime minions" maxQueenMinions fun entry => do
    let kindCode : Nat ← readField entry "kind"
    let kind ← match kindCode with
      | 658 => pure QueenMinionKind.crystal
      | 659 => pure QueenMinionKind.bouncy
      | 660 => pure QueenMinionKind.heavenly
      | _ => throw "Unknown Queen Slime minion."
    let (x, y, vx, vy) ← decodeFloatMotion entry "Queen Slime minion"
    return {
      x, y, vx, vy, kind, id := ← readField entry "id", health := ← readField entry "health",
      timer := ← readField entry "timer", jumps := ← readField entry "jumps", charge := ← readField entry "charge",
      timeLeft := ← readField entry "timeLeft", direction := ← readField entry "direction",
      wet := ← readField entry "wet", frameClock := ← readField entry "frameClock" }

private def decodeQueenShots (json : Json) (saveVersion : Nat) : Except String (Array QueenShot) :=
  decodeActorArray json "Queen Slime projectiles" maxQueenShots fun entry => do
    let kindCode : Nat ← readField entry "kind"
    let kind ← match kindCode with
      | 920 => pure QueenShotKind.spike
      | 921 => pure QueenShotKind.bouncy
      | 922 => pure QueenShotKind.stomp
      | 926 => pure QueenShotKind.regal
      | _ => throw "Unknown Queen Slime projectile."
    let (x, y, vx, vy) ← decodeFloatMotion entry "Queen Slime projectile"
    return {
      x, y, vx, vy, kind, age := ← readField entry "age",
      bounced := ← readField entry "bounced", slot := ← decodeProjectileSlot entry saveVersion }

private def decodeTwinKind : String → Except String TwinKind
  | "retinazer" => pure .retinazer
  | "spazmatism" => pure .spazmatism
  | kind => throw s!"Unknown Twin kind '{kind}'."

private def decodeTwin (json : Json) : Except String (Option Twin) := do
  if json == .null then return none
  let kind ← decodeTwinKind (← readField json "kind")
  let (x, y, vx, vy) ← decodeFloatMotion json "Twin"
  let readBits := decodeFloat32Bits json "Twin"
  return some {
    kind, x, y, vx, vy, rotation := ← readBits "rotationBits",
    ai0 := ← readBits "ai0Bits", ai1 := ← readBits "ai1Bits",
    ai2 := ← readBits "ai2Bits", ai3 := ← readBits "ai3Bits",
    localAI1 := ← readBits "localAI1Bits", localAI2 := ← readBits "localAI2Bits",
    powered := ← readField json "powered", health := ← readField json "health",
    timeLeft := ← readField json "timeLeft",
    frame := ← readField json "frame", frameClock := ← readField json "frameClock" }

private def decodeDestroyer (json : Json) : Except String (Option Destroyer) := do
  if json == .null then return none
  let segments ← decodeActorArray (← json.getObjVal? "segments") "Destroyer segments" destroyerSegmentCount
    fun entry => do
      let readBits := decodeFloat32Bits entry "Destroyer segment"
      return {
        x := ← readBits "xBits"
        y := ← readBits "yBits"
        rotation := ← readBits "rotationBits"
        alpha := ← readField entry "alpha"
        laserClock := ← readField entry "laserClock"
        released := ← readField entry "released" : DestroyerSegment }
  let readBits := decodeFloat32Bits json "Destroyer"
  return some {
    segments
    health := ← readField json "health"
    vx := ← readBits "vxBits"
    vy := ← readBits "vyBits"
    burrowSoundDelay := ← readField json "burrowSoundDelay" }

private def decodeDestroyerProbes (json : Json) : Except String (Array DestroyerProbe) :=
  decodeActorArray json "Destroyer probes" destroyerBodyCount fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Destroyer probe"
    return {
      x, y, vx, vy
      id := ← readField entry "id"
      health := ← readField entry "health"
      rotation := ← decodeFloat32Bits entry "Destroyer probe" "rotationBits"
      facing := ← readField entry "facing"
      laserClock := ← readField entry "laserClock"
      timeLeft := ← readField entry "timeLeft"
      justHit := ← readField entry "justHit" }

private def decodePrimePhase : String → Except String PrimePhase
  | "hover" => pure .hover
  | "spin" => pure .spin
  | "guardian" => pure .guardian
  | "retreat" => pure .retreat
  | _ => throw "Unknown Skeletron Prime phase."

private def decodePrimeArmKind (id : Nat) : Except String PrimeArmKind :=
  match PrimeArmKind.all.find? (·.id == id) with
  | some kind => pure kind
  | none => throw "Unknown Skeletron Prime arm."

private def decodePrimeHead (json : Json) : Except String (Option PrimeHead) := do
  if json == .null then return none
  let (x, y, vx, vy) ← decodeFloatMotion json "Skeletron Prime head"
  return some {
    x, y, vx, vy
    rotation := ← decodeFloat32Bits json "Skeletron Prime head" "rotationBits"
    phase := ← decodePrimePhase (← readField json "phase")
    timer := ← readField json "timer"
    health := ← readField json "health"
    timeLeft := ← readField json "timeLeft"
    direction := ← readField json "direction"
    defense := ← readField json "defense"
    contactDamage := ← readField json "damage"
    initialized := ← readField json "initialized"
    frame := ← readField json "frame"
    frameClock := ← readField json "frameClock" }

private def decodePrimeArms (json : Json) : Except String (Array PrimeArm) :=
  decodeActorArray json "Skeletron Prime arms" 4 fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Skeletron Prime arm"
    return {
      kind := ← decodePrimeArmKind (← readField entry "kind")
      x, y, vx, vy
      rotation := ← decodeFloat32Bits entry "Skeletron Prime arm" "rotationBits"
      health := ← readField entry "health"
      phase := ← readField entry "phase"
      timer := ← readField entry "timer"
      shotClock := ← readField entry "shotClock"
      facing := ← readField entry "facing"
      direction := ← readField entry "direction"
      frame := ← readField entry "frame"
      frameClock := ← readField entry "frameClock" }

private def decodePrimeShots (json : Json) (saveVersion : Nat) : Except String (Array PrimeShot) :=
  decodeActorArray json "Skeletron Prime projectiles" maxProjectileSlots fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Skeletron Prime projectile"
    let id : Nat ← readField entry "kind"
    let kind : PrimeAttackKind ← match id with
      | 100 => pure .laser
      | 102 => pure .bomb
      | _ => throw "Unknown Skeletron Prime projectile."
    return {
      kind, x, y, vx, vy
      rotation := ← decodeFloat32Bits entry "Skeletron Prime projectile" "rotationBits"
      damage := ← readField entry "damage"
      life := ← readField entry "life"
      age := ← readField entry "age"
      slot := ← decodeProjectileSlot entry saveVersion
      honeyWet := ← readField entry "honeyWet" }

private def decodePlanteraHead (json : Json) : Except String PlanteraHead := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Plantera head"
  return {
    x, y, vx, vy
    rotation := ← decodeFloat32Bits json "Plantera head" "rotationBits"
    health := ← readField json "health"
    attackClock := ← readField json "attackClock"
    stage := ← readField json "stage"
    timeLeft := ← readField json "timeLeft"
    defense := ← readField json "defense"
    contactDamage := ← readField json "damage"
    enraged := ← readField json "enraged"
    frame := ← readField json "frame"
    frameClock := ← readField json "frameClock" }

private def decodePlanteraHook (json : Json) : Except String PlanteraHook := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Plantera hook"
  let cell ← json.getObjVal? "anchor"
  let anchor ← if cell == .null then pure none else some <$> decodePosition cell
  return {
    x, y, vx, vy
    rotation := ← decodeFloat32Bits json "Plantera hook" "rotationBits"
    anchor := anchor
    timer := ← readField json "timer"
    frame := ← readField json "frame"
    frameClock := ← readField json "frameClock" }

private def decodePlanteraTentacle (json : Json) : Except String PlanteraTentacle := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Plantera tentacle"
  return {
    x, y, vx, vy
    rotation := ← decodeFloat32Bits json "Plantera tentacle" "rotationBits"
    id := ← readField json "id"
    health := ← readField json "health"
    timer := ← readField json "timer"
    offsetX := ← readField json "offsetX"
    offsetY := ← readField json "offsetY"
    facing := ← readField json "facing"
    frame := ← readField json "frame"
    frameClock := ← readField json "frameClock"
    timeLeft := ← readField json "timeLeft" }

private def decodePlanteraSpore (json : Json) : Except String PlanteraSpore := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Plantera spore"
  return {
    x, y, vx, vy
    rotation := ← decodeFloat32Bits json "Plantera spore" "rotationBits"
    id := ← readField json "id"
    timeLeft := ← readField json "timeLeft" }

private def decodePlanteraShot (json : Json) (saveVersion : Nat) : Except String PlanteraShot := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Plantera projectile"
  let type : Nat ← readField json "kind"
  let kind : PlanteraShotKind ← match type with
    | 275 => pure .seed
    | 276 => pure .poisonSeed
    | 277 => pure .thornBall
    | _ => throw "Unknown Plantera projectile."
  return {
    x, y, vx, vy
    rotation := ← decodeFloat32Bits json "Plantera projectile" "rotationBits"
    kind := kind
    damage := ← readField json "damage"
    life := ← readField json "life"
    age := ← readField json "age"
    slot := ← decodeProjectileSlot json saveVersion
    honeyWet := ← readField json "honeyWet" }

private def restorePlantera (state : GameState) (json : Json) (saveVersion : Nat) :
    Except String GameState := do
  if saveVersion < 70 then return state
  let data ← json.getObjVal? "plantera"
  let plantera ← if data == .null then pure none else some <$> decodePlanteraHead data
  let planteraHooks ← decodeActorArray (← json.getObjVal? "planteraHooks") "Plantera hooks" 3 decodePlanteraHook
  let planteraTentacles ← decodeActorArray (← json.getObjVal? "planteraTentacles")
    "Plantera tentacles" 8 decodePlanteraTentacle
  let planteraSpores ← decodeActorArray (← json.getObjVal? "planteraSpores") "Plantera spores" 200 decodePlanteraSpore
  let planteraShots ← decodeActorArray (← json.getObjVal? "planteraShots") "Plantera projectiles" maxProjectileSlots
    (fun entry => decodePlanteraShot entry saveVersion)
  let planteraBulbs ← decodeActorArray (← json.getObjVal? "planteraBulbs") "Plantera bulbs" 16 decodePosition
  return { state with
    plantera, planteraHooks, planteraTentacles, planteraSpores, planteraShots, planteraBulbs
    nextPlanteraPartId := ← readField json "nextPlanteraPartId"
    planteraUnlocked := ← readField json "planteraUnlocked"
    planteraDefeated := ← readField json "planteraDefeated"
    poisonedTicks := ← readField json "poisonedTicks"
    poisonProgress := ← readField json "poisonProgress" }

private def decodeGolemBody (json : Json) : Except String GolemBody := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Golem body"
  return {
    x, y, vx, vy
    health := ← readField json "health", alpha := ← readField json "alpha"
    jumpState := ← readField json "jumpState", jumpClock := ← readField json "jumpClock"
    initialized := ← readField json "initialized", noTileCollide := ← readField json "noTileCollide"
    invulnerable := ← readField json "invulnerable", enraged := ← readField json "enraged"
    direction := ← readField json "direction", frameY := ← readField json "frameY"
    timeLeft := ← readField json "timeLeft" }

private def decodeGolemHead (json : Json) : Except String GolemHead := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Golem head"
  let bits := decodeFloat32Bits json "Golem head"
  return {
    x, y, vx, vy
    rotation := ← bits "rotationBits", fireClock := ← bits "fireClockBits", laserClock := ← bits "laserClockBits"
    health := ← readField json "health", alpha := ← readField json "alpha"
    frame := ← readField json "frame", frameCounter := ← readField json "frameCounter"
    detached := ← readField json "detached", fast := ← readField json "fast", fireMouth := ← readField json "fireMouth"
    noTileCollide := ← readField json "noTileCollide", look := ← readField json "look" }

private def decodeGolemFist (json : Json) : Except String GolemFist := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Golem fist"
  let sprite : Nat ← readField json "kind"
  let kind : GolemFistKind ← match sprite with
    | 247 => pure .left
    | 248 => pure .right
    | _ => throw "Unknown Golem fist."
  return {
    x, y, vx, vy
    kind, rotation := ← decodeFloat32Bits json "Golem fist" "rotationBits"
    health := ← readField json "health", phase := ← readField json "phase"
    timer := ← readField json "timer", alpha := ← readField json "alpha"
    noTileCollide := ← readField json "noTileCollide"
    collideX := ← readField json "collideX", collideY := ← readField json "collideY" }

private def decodeGolemShot (json : Json) : Except String GolemShot := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Golem projectile"
  let sprite : Nat ← readField json "kind"
  let kind : GolemShotKind ← match sprite with
    | 258 => pure .fireball
    | 259 => pure .eyeBeam
    | _ => throw "Unknown Golem projectile."
  return {
    x, y, vx, vy
    kind, rotation := ← decodeFloat32Bits json "Golem projectile" "rotationBits"
    damage := ← readField json "damage", life := ← readField json "life"
    age := ← readField json "age", alpha := ← readField json "alpha"
    direction := ← readField json "direction", slot := ← decodeProjectileSlot json 72
    bounces := ← readField json "bounces", honeyWet := ← readField json "honeyWet" }

private def decodeTempleTrapShot (json : Json) : Except String TempleTraps.Shot := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Temple trap projectile"
  let sprite : Nat ← readField json "kind"
  let kind : TempleTraps.ShotKind ← match sprite with
    | 184 => pure .dart
    | 185 => pure .spikyBall
    | 186 => pure .spear
    | 187 => pure .flameEmitter
    | 188 => pure .flame
    | _ => throw "Unknown temple trap projectile."
  let raw ← json.getObjVal? "anchor"
  let anchor ← if raw == .null then pure none else do
    let x ← decodeFloat32Bits raw "Spear trap anchor" "xBits"
    let y ← decodeFloat32Bits raw "Spear trap anchor" "yBits"
    pure (some (⟨x, y⟩ : OriginalCollision.Vector))
  return {
    kind, position := ⟨x, y⟩, velocity := ⟨vx, vy⟩, anchor
    rotation := ← decodeFloat32Bits json "Temple trap projectile" "rotationBits"
    damage := ← readField json "damage", life := ← readField json "life"
    age := ← readField json "age", direction := ← readField json "direction"
    slot := ← decodeProjectileSlot json 72, retracting := ← readField json "retracting" }

private def decodeTempleWires (json : Json) (world : World) : Except String ByteArray := do
  let entries ← json.getArr?
  if entries.isEmpty then return .empty
  let size := world.width * world.height
  if entries.size > size then throw "Too many wire cells."
  let mut wires := ByteArray.mk (Array.replicate size 0)
  for entry in entries do
    let #[rawIndex, rawMask] ← entry.getArr? | throw "A wire entry needs an index and a color mask."
    let index ← rawIndex.getNat?
    let mask ← rawMask.getNat?
    if index ≥ size || mask == 0 || mask ≥ 16 || wires[index]! != 0 then
      throw "Invalid or repeated wire cell."
    wires := wires.set! index mask.toUInt8
  return wires

private def decodeTempleEnemy (json : Json) : Except String TempleEnemy := do
  let (x, y, vx, vy) ← decodeFloatMotion json "Temple enemy"
  let id ← readField json "id"
  let type ← readField json "kind"
  let some kind := TempleEnemyKind.ofNpcId? type | throw "Unknown Temple enemy."
  let bits := decodeFloat32Bits json "Temple enemy"
  let clock : Nat ← readField json "frameClockBits"
  if clock ≥ 2^64 then throw "Temple enemy animation clock exceeds Float64 storage."
  return {
    id, kind, x, y, vx, vy
    health := ← readField json "health", previousX := ← bits "previousXBits"
    previousVx := ← bits "previousVxBits", previousVy := ← bits "previousVyBits"
    direction := ← readField json "direction", directionY := ← readField json "directionY"
    spriteDirection := ← readField json "spriteDirection"
    collideX := ← readField json "collideX", collideY := ← readField json "collideY"
    ai0 := ← readField json "ai0", ai1 := ← readField json "ai1", ai2 := ← readField json "ai2", ai3 := ← readField json "ai3"
    frame := ← readField json "frame", frameClock := Float.ofBits clock.toUInt64
    rotation := ← bits "rotationBits", timeLeft := ← readField json "timeLeft", wet := ← readField json "wet"
    gfxOffsetY := ← bits "gfxOffsetYBits", stepSpeed := ← bits "stepSpeedBits", justHit := ← readField json "justHit" }

private def restoreTempleFight (state : GameState) (json : Json) (saveVersion : Nat) :
    Except String GameState := do
  if saveVersion < 72 then
    if state.poisonedTicks > 419 then throw "Invalid pre-temple Poisoned duration."
    return state
  let data ← json.getObjVal? "jungleTemple"
  let body ← data.getObjVal? "body"
  let head ← data.getObjVal? "head"
  let golem ← if body == .null then pure none else some <$> decodeGolemBody body
  let golemHead ← if head == .null then pure none else some <$> decodeGolemHead head
  let golemFists ← decodeActorArray (← data.getObjVal? "fists") "Golem fists" 2 decodeGolemFist
  let golemShots ← decodeActorArray (← data.getObjVal? "golemShots") "Golem projectiles"
    maxProjectileSlots decodeGolemShot
  let templeTrapShots ← decodeActorArray (← data.getObjVal? "trapShots") "Temple trap projectiles"
    maxProjectileSlots decodeTempleTrapShot
  let templeWires ← decodeTempleWires (← data.getObjVal? "wires") state.world
  let templeEnemies ← if saveVersion < 73 then pure #[] else
    decodeActorArray (← data.getObjVal? "enemies") "Temple enemies" 30 decodeTempleEnemy
  let nextTempleEnemyId ← if saveVersion < 73 then pure 1 else readField data "nextEnemyId"
  let templeCooldowns ← decodeActorArray (← data.getObjVal? "cooldowns") "Temple mechanisms" 999 fun entry => do
    let position ← decodePosition (← entry.getObjVal? "position")
    let ticks : Nat ← readField entry "ticks"
    pure (position, ticks)
  let state := { state with
    golem, golemHead, golemFists, golemShots, templeTrapShots, templeWires, templeCooldowns
    templeEnemies, nextTempleEnemyId
    golemDefeated := ← readField data "defeated"
    hellfireTicks := ← readField data "hellfireTicks", hellfireProgress := ← readField data "hellfireProgress"
    bleedingTicks := ← readField data "bleedingTicks", spikeCooldown := ← readField data "spikeCooldown" }
  return state

private def validateTempleFight (state : GameState) : Except String Unit := do
  let world := state.world
  let ids := state.templeEnemies.map (·.id)
  if state.nextTempleEnemyId == 0 || ids.any (· ≥ state.nextTempleEnemyId) ||
      ids.toList.eraseDups.length != ids.size || !state.templeEnemies.all (·.valid world) then
    throw "Invalid Temple enemy motion or ID."
  if (state.golem.isSome || state.golemDefeated || !state.golemShots.isEmpty) &&
      (!state.hardmode || !state.planteraDefeated) then throw "Golem requires a Plantera victory in Hardmode."
  if let some body := state.golem then
    if !body.valid world || body.initialized != state.golemHead.isSome ||
        (state.golemHead.any (!·.detached) && !body.invulnerable) then throw "Invalid Golem body state."
  else if state.golemHead.isSome || !state.golemFists.isEmpty then
    throw "Golem parts require an active body."
  let sides := state.golemFists.toList.map (·.kind)
  if sides.eraseDups.length != sides.length ||
      !state.golemFists.all (·.valid world) || !state.golemHead.all (·.valid world) ||
      !state.golemShots.all (fun shot => shot.slot.isSome && shot.valid world) then
    throw "Invalid Golem parts or projectiles."
  let sites := state.templeCooldowns.toList.map Prod.fst
  if sites.eraseDups.length != sites.length ||
      state.templeCooldowns.any (fun (p, ticks) => !world.contains p || ticks == 0 || ticks > 300) then
    throw "Invalid temple mechanism cooldown."
  for shot in state.templeTrapShots do
    if !shot.slot.isSome || shot.life == 0 || shot.age + shot.life > shot.kind.lifetime ||
        !(shot.damage == if shot.kind == .spear then 60 else 40) ||
        ![-1, 1].contains shot.direction ||
        !shot.position.x.isFinite || !shot.position.y.isFinite ||
        !shot.velocity.x.isFinite || !shot.velocity.y.isFinite || !shot.rotation.isFinite ||
        shot.velocity.x.abs > 32 || shot.velocity.y.abs > 32 then
      throw "Invalid temple trap projectile."
  if state.hellfireTicks > 1200 || state.hellfireProgress ≥ 15 ||
      (state.hellfireTicks == 0 && state.hellfireProgress != 0) ||
      state.bleedingTicks > 1199 || state.spikeCooldown > 40 ||
      (state.respawnTicks > 0 && (state.hellfireTicks > 0 || state.bleedingTicks > 0 || state.spikeCooldown > 0)) then
    throw "Invalid temple trap debuff."

private def decodeTwinAttackKind : String → Except String TwinAttackKind
  | "eyeLaser" => pure .eyeLaser
  | "deathLaser" => pure .deathLaser
  | "cursedFlame" => pure .cursedFlame
  | "eyeFire" => pure .eyeFire
  | "destroyerLaser" => pure .destroyerLaser
  | "probeLaser" => pure .probeLaser
  | kind => throw s!"Unknown Twins projectile kind '{kind}'."

private def decodeTwinShots (json : Json) (saveVersion : Nat) : Except String (Array TwinShot) :=
  decodeActorArray json "Twins projectiles" maxTwinShots fun entry => do
    let kind ← decodeTwinAttackKind (← readField entry "kind")
    if saveVersion < 68 && (kind == .destroyerLaser || kind == .probeLaser) then
      throw "Destroyer lasers require a version 68 save."
    let (x, y, vx, vy) ← decodeFloatMotion entry "Mechanical boss projectile"
    return {
      kind, x, y, vx, vy, damage := ← readField entry "damage", life := ← readField entry "life",
      age := ← readField entry "age", slot := ← decodeProjectileSlot entry saveVersion,
      penetration := ← readField entry "penetration" }

private def decodeAquaStreams (json : Json) (world : World) (saveVersion : Nat) :
    Except String (Array AquaStream) := do
  let entries ← json.getArr?
  if entries.size > maxAquaStreams then throw "Too many Aqua Streams."
  entries.mapM fun data => do
    let readBits := decodeFloat32Bits data "Aqua Stream"
    let x ← readBits "xBits"
    let y ← readBits "yBits"
    let vx ← readBits "vxBits"
    let vy ← readBits "vyBits"
    let age : Nat ← readField data "age"
    let hitsLeft : Nat ← readField data "hitsLeft"
    let lavaSeed : Nat ← if saveVersion < 40 then pure 0 else readField data "lavaSeed"
    if lavaSeed ≥ aquaLavaSeedLimit then throw "Invalid Aqua Stream lava seed."
    let lavaBounceAge : Option Nat ← if saveVersion < 40 then pure none else readField data "lavaBounceAge"
    let slot ← decodeProjectileSlot data saveVersion
    let stream : AquaStream := {x, y, vx, vy, age, hitsLeft, lavaSeed := lavaSeed.toUInt32, lavaBounceAge, slot}
    if !stream.valid world then throw "Invalid Aqua Stream motion, age or remaining hits."
    return stream

private def decodeShortsword (json : Json) (saveVersion : Nat) : Except String (Option ShortswordThrust) := do
  if json == .null then return none
  let vx ← decodeFloat32Bits json "Shortsword" "vxBits"
  let vy ← decodeFloat32Bits json "Shortsword" "vyBits"
  let age : Nat ← readField json "age"
  let damage : Nat ← readField json "damage"
  let slot ← decodeProjectileSlot json saveVersion
  if slot.isNone then throw "An active shortsword must have a projectile slot."
  let thrust : ShortswordThrust := {vx, vy, age, damage, slot}
  if !thrust.valid || age < shortswordSubupdates || age % shortswordSubupdates != 0 ||
      damage != Item.copperShortsword.swordDamage then
    throw "Invalid active shortsword motion, age or damage."
  return some thrust

private def decodeTownGuide (json : Json) (world : World) (legacyEarly : Bool) :
    Except String (Option GuideNPC) := do
  if json == .null then return none
  if legacyEarly then throw "A live Guide requires a Classic world."
  let body ← decodePlayer (← json.getObjVal? "body") world 24
  let health : Nat ← readField json "health"
  let direction : Int ← readField json "direction"
  let attackCooldown : Nat ← readField json "attackCooldown"
  let hurtCooldown : Nat ← readField json "hurtCooldown"
  if health == 0 || health > guideMaxHealth || (direction != -1 && direction != 1) ||
      attackCooldown > guideAttackCooldown || hurtCooldown > guideHurtCooldown then
    throw "Invalid Guide health, direction or cooldown."
  let doorToClose ← match ← json.getObjVal? "doorToClose" with
    | .null => pure none
    | data => do
      let position ← decodePosition data
      if doorBase world position != some position then throw "The Guide's pending door does not exist."
      pure (some position)
  return some { body, health, direction, attackCooldown, hurtCooldown, doorToClose }

private def decodeWalls (json : Json) (world : World) (saveVersion : Nat) : Except String ByteArray := do
  let entries ← json.getArr?
  let size := world.width * world.height
  if entries.size > size then throw "Too many background wall cells."
  let mut cells := ByteArray.mk (Array.replicate size 0)
  for entry in entries do
    let pair ← entry.getArr?
    let #[rawIndex, rawCode] := pair | throw "A background wall must have an index and a material."
    let index ← rawIndex.getNat?
    let code ← rawCode.getNat?
    if index ≥ size || (code != 1 && (saveVersion < 71 || code != TempleLayout.wallCode.toNat)) ||
        cells[index]! != 0 then
      throw "Invalid or repeated background wall cell."
    cells := cells.set! index code.toUInt8
  return cells

private def decodeFurnitureKind : String → Except String FurnitureKind
  | "bed" => pure .bed
  | "furnace" => pure .furnace
  | "ironAnvil" => pure .ironAnvil
  | "leadAnvil" => pure .leadAnvil
  | "sawmill" => pure .sawmill
  | "loom" => pure .loom
  | "woodChair" => pure .woodChair
  | "woodTable" => pure .woodTable
  | "hellforge" => pure .hellforge
  | "mythrilAnvil" => pure .mythrilAnvil
  | "orichalcumAnvil" => pure .orichalcumAnvil
  | "adamantiteForge" => pure .adamantiteForge
  | "titaniumForge" => pure .titaniumForge
  | "woodenCrate" => pure .woodenCrate
  | "ironCrate" => pure .ironCrate
  | "goldenCrate" => pure .goldenCrate
  | "pearlwoodCrate" => pure .pearlwoodCrate
  | "mythrilCrate" => pure .mythrilCrate
  | "titaniumCrate" => pure .titaniumCrate
  | value => throw s!"Unknown furniture kind '{value}'."

private def decodeFurniture (json : Json) (world : World) (saveVersion : Nat) : Except String (Array Furniture) := do
  let entries ← json.getArr?
  let count := world.tiles.data.foldl (fun total code =>
    total + if (Tile.bed.code ≤ code && code ≤ Tile.loom.code) ||
      (Tile.leadAnvil.code ≤ code && code ≤ Tile.woodTable.code) || code == Tile.hellforge.code ||
      (Tile.mythrilAnvil.code ≤ code && code ≤ Tile.titaniumForge.code) ||
      code == Tile.fishingCrate.code then 1 else 0)
    (0 : Nat)
  if entries.size > count then throw "Too many furniture records for the placed tiles."
  let mut furniture := #[]
  let mut occupied : Std.HashSet TilePos := {}
  for entry in entries do
    let label : String ← readField entry "kind"
    let kind ← if saveVersion ≥ 58 && label == "biomeCrate" then do
        let code : Nat ← readField entry "item"
        let some item := if code ≤ Item.mudBlock.code.toNat then Item.ofCode? code.toUInt16 else none
          | throw "Unknown placed crate item."
        if !item.isFishingCrate || item.biomeFishingTextureId.isNone then throw "Invalid placed biome crate."
        pure (.biomeCrate item)
      else decodeFurnitureKind label
    let position ← decodePosition (← entry.getObjVal? "position")
    let flipped : Bool ← readField entry "flipped"
    let piece : Furniture := { kind, position, flipped }
    let cells := piece.cells
    if !piece.intact world || cells.any occupied.contains then
      throw "Furniture has an incomplete, unsupported or overlapping footprint."
    occupied := cells.foldl (fun seen cell => seen.insert cell) occupied
    furniture := furniture.push piece
  if occupied.size != count then throw "A furniture tile has no matching record."
  return furniture

private def validatePlacedBottles (world : World) : Except String Unit := do
  for index in [:world.tiles.size] do
    if world.tiles[index]! == Tile.placedBottle.code then
      let support : TilePos := ⟨index % world.width, index / world.width + 1⟩
      if !(world.tile support).bottleSupport then throw "A placed bottle needs a table, work bench or platform."

private def validateCavernCrystals (world : World) : Except String Unit := do
  for index in [:world.tiles.size] do
    let code := world.tiles[index]!
    if code == Tile.crystalShard.code || code == Tile.gelatinCrystal.code then
      let position : TilePos := ⟨index % world.width, index / world.width⟩
      if !world.crystalShardSupport position then throw "A cavern crystal needs a valid supporting tile."

private def decodeBedSpawnPoint (json : Json) (world : World) (walls : ByteArray)
    (furniture : Array Furniture) : Except String (Option TilePos) := do
  if json == .null then return none
  let anchor ← decodePosition json
  let some bed := furniture.find? (fun piece => piece.kind == .bed && piece.position == anchor)
    | throw "The spawn bed does not exist."
  if (bedSpawn world walls bed).isNone then throw "The spawn bed is outside a valid bedroom."
  return some anchor

private def decodeTileHits (json : Json) (world : World) : Except String (Array TileHit) := do
  let entries ← json.getArr?
  if entries.size > tileHitLimit then throw "Too many recent tile hits."
  let mut hits := #[]
  let mut seen : Std.HashSet TilePos := {}
  for entry in entries do
    let position ← decodePosition (← entry.getObjVal? "position")
    let code : Nat ← readField entry "tile"
    let some tile := if code < 256 then Tile.ofCode? code.toUInt8 else none
      | throw "Unknown tile code in a recent hit."
    let damage : Nat ← readField entry "damage"
    let ticks : Nat ← readField entry "ticks"
    if seen.contains position || world.tile? position != some tile || !tile.allowsToolCracks then
      throw "A recent hit must match a unique, crackable tile."
    if damage == 0 || damage ≥ 100 || ticks == 0 || ticks > tileHitLifetime then
      throw "A recent tile hit exceeds its simulation bounds."
    seen := seen.insert position
    hits := hits.push { position, tile, damage, ticks }
  return hits

private def decodeGelDrops (json : Json) (world : World) : Except String (Array GelDrop) := do
  let entries ← json.getArr?
  if entries.size > maxGelDrops then throw "Too many uncollected Gel drops."
  let mut drops := #[]
  let mut seen : Std.HashSet TilePos := {}
  for entry in entries do
    let position ← decodePosition (← entry.getObjVal? "position")
    let count : Nat ← readField entry "count"
    if !world.contains position || seen.contains position || count == 0 || count > 9999 then
      throw "Invalid or repeated Gel drop."
    seen := seen.insert position
    drops := drops.push { position, count }
  return drops

private def decodeNightDropItems (json : Json) (saveVersion : Nat) : Except String (Array Stack) := do
  let entries ← json.getArr?
  let itemLimit := if saveVersion < 27 then 4 else if saveVersion == 27 then 16
    else if saveVersion == 28 then 48 else if saveVersion < 50 then 64 else 128
  if entries.size > itemLimit then throw "Too many item types in a night drop."
  let mut items : Array Stack := #[]
  for entry in entries do
    let code : Nat ← readField entry "item"
    let some item := if code < UInt16.size then Item.ofCode? code.toUInt16 else none
      | throw "Unknown item in a night drop."
    let count : Nat ← readField entry "count"
    let v26Item := Item.lens.code.toNat ≤ code && code ≤ Item.zombieArm.code.toNat
    let v27Item := v26Item || (Item.demoniteOre.code.toNat ≤ code && code ≤ Item.unholyArrow.code.toNat) ||
      item == .heart
    let valid := if saveVersion < 27 then v26Item
      else if saveVersion == 27 then v27Item
      else if saveVersion == 28 then v27Item ||
        (Item.ebonstone.code.toNat ≤ code && code ≤ Item.rottedFork.code.toNat) ||
        ([.stone, .dirt, .wood, .ice, .torch, .platform, .woodWall, .copper, .tinOre, .iron, .leadOre,
          .silver, .tungstenOre, .goldOre, .platinumOre, .crystal, .cobweb, .arrow] : List Item).contains item
      else if saveVersion < 31 then code ≤ Item.bottle.code.toNat && item.isNightDrop
      else
        let maxCode := if saveVersion < 36 then Item.bookOfSkulls.code.toNat
          else if saveVersion < 38 then Item.bone.code.toNat
          else if saveVersion < 39 then Item.nazar.code.toNat
          else if saveVersion < 43 then Item.aquaScepter.code.toNat
          else if saveVersion < 44 then Item.blueMoon.code.toNat
          else if saveVersion < 45 then Item.magicMissile.code.toNat
          else if saveVersion < 46 then Item.handgun.code.toNat
          else if saveVersion < 47 then Item.obsidianSkull.code.toNat
          else if saveVersion < 48 then Item.hellwingBow.code.toNat
          else if saveVersion < 49 then Item.batBat.code.toNat
          else if saveVersion < 50 then Item.goldenBugNet.code.toNat
          else if saveVersion < 51 then Item.wallOfFleshTrophy.code.toNat
          else if saveVersion < 52 then Item.trifoldMap.code.toNat
          else if saveVersion < 53 then Item.monsterMeat.code.toNat
          else if saveVersion < 54 then Item.spazmatismTrophy.code.toNat
          else if saveVersion < 57 then Item.goldWorm.code.toNat
          else if saveVersion < 58 then Item.manaPotion.code.toNat
          else if saveVersion < 59 then Item.mudBlock.code.toNat
          else if saveVersion < 60 then Item.theDuke.code.toNat
          else if saveVersion < 61 then Item.itsScragglinTime.code.toNat
          else if saveVersion < 62 then Item.crispyHoneyBlock.code.toNat
          else if saveVersion < 68 then Item.queenSlimeTrophy.code.toNat
          else if saveVersion < 69 then Item.destroyerTrophy.code.toNat
          else if saveVersion < 70 then Item.skeletronPrimeTrophy.code.toNat
          else if saveVersion < 71 then Item.planteraTrophy.code.toNat
          else if saveVersion < 72 then Item.titanPotion.code.toNat
          else if saveVersion < 73 then Item.golemTrophy.code.toNat
          else Item.lizardEgg.code.toNat
        code ≤ maxCode && (item.isNightDrop || (saveVersion ≥ 57 && item.isCrateLoot) ||
          (saveVersion ≥ 59 && (item.anglerTextureId.isSome || item == .goldenBugNet)) ||
          (saveVersion ≥ 60 && item.waterCatchTextureId.isSome) ||
          (saveVersion ≥ 61 && (item == .honeyBlock || item == .crispyHoneyBlock)) ||
          (saveVersion ≥ 62 && item.queenSlimeTextureId.isSome)) &&
          (saveVersion ≥ 35 || item != .goldenKey) &&
          (saveVersion ≥ 50 || (item != .ashBlock && item != .obsidianBrick)) &&
          (saveVersion ≥ 52 || item != .caveGel)
    if !valid || count == 0 || count > 9999 || items.any (·.item == item) then
      throw "Invalid or repeated item in a night drop."
    items := items.push { item, count }
  return items

private def decodeNightDrops (json : Json) (world : World) (saveVersion : Nat) : Except String (Array NightDrop) := do
  let entries ← json.getArr?
  if entries.size > maxNightDrops then throw "Too many uncollected night drops."
  let coinLimit := if saveVersion < 27 then 9999 else maxDropCoins
  let mut drops := #[]
  let mut seen : Std.HashSet TilePos := {}
  for entry in entries do
    let position ← decodePosition (← entry.getObjVal? "position")
    let coins : Nat ← readField entry "coins"
    if !world.contains position || seen.contains position || coins > coinLimit then
      throw "Invalid or repeated night drop."
    let items ← decodeNightDropItems (← entry.getObjVal? "items") saveVersion
    if coins == 0 && items.isEmpty then throw "An empty night drop cannot be saved."
    seen := seen.insert position
    drops := drops.push { position, coins, items }
  return drops

private def decodeSurfaceKind : String → Except String SurfaceKind
  | "slime" => pure .slime
  | "greenSlime" => pure .greenSlime
  | "blueSlime" => pure .blueSlime
  | "cedarHare" => pure .cedarHare
  | "brambleCrawler" => pure .brambleCrawler
  | "zombie" => pure .zombie
  | "demonEye" => pure .demonEye
  | value => throw s!"Unknown surface creature '{value}'."

private def decodeSurfaceCreature (data : Json) (world : World) (seed : UInt64) (player : Player)
    (saveVersion clock : Nat) (legacyEarly flying : Bool) : Except String (Option NightCreature) := do
  if data == .null then return none
  let kind ← if saveVersion < 19 then pure .slime
    else do decodeSurfaceKind (← readField data "kind")
  if saveVersion < 25 && kind == .greenSlime then throw "Green Slimes require a version 25 world."
  let newKind := kind == .blueSlime || kind == .zombie || kind == .demonEye
  if ((saveVersion < 26 || legacyEarly) && newKind) || (saveVersion ≥ 26 && !legacyEarly && kind == .slime) then
    throw "The surface creature does not belong to this world mode."
  if (kind == .demonEye) != flying then throw "The surface creature is in the wrong movement slot."
  if !kind.activeAt clock (saveVersion < 26 || legacyEarly) then
    throw "The surface creature is outside its active time."
  let body ← decodePlayer (← data.getObjVal? "body") world kind.maxSpeed
  let health : Nat ← readField data "health"
  let hopIn : Nat ← readField data "hopIn"
  if health == 0 || health > kind.maxHealth || hopIn > kind.hopDelay then
    throw "Invalid surface creature health or hop timer."
  if flying && (body.grounded || body.vy.natAbs > demonEyeMaxVerticalSpeed.toNat || hopIn != 0) then
    throw "Invalid flying creature movement."
  let creature : NightCreature := { body, health, hopIn, kind }
  if !creature.staysNear seed player then throw "The surface creature is outside its habitat or active range."
  return some creature

private def decodeSurface (json : Json) (world : World) (seed : UInt64) (player : Player)
    (saveVersion : Nat) (legacyEarly : Bool) : Except String SurfaceState := do
  let clock : Nat ← readField json "clock"
  if clock ≥ phaseCycleTicks (saveVersion < 26 || legacyEarly) then throw s!"Invalid world clock {clock}."
  let creature ← decodeSurfaceCreature (← json.getObjVal? "creature") world seed player saveVersion clock legacyEarly false
  let flying ← if saveVersion < 26 then pure none
    else do decodeSurfaceCreature (← json.getObjVal? "flying") world seed player saveVersion clock legacyEarly true
  let creature := if saveVersion == 25 && !legacyEarly then creature.filter (·.kind != .slime) else creature
  let oldDay := phaseDayTicks true
  let clock := if saveVersion ≥ 26 || legacyEarly then clock
    else if clock < oldDay then clock * classicDayTicks / oldDay
    else classicDayTicks + (clock - oldDay) * classicNightTicks / (phaseCycleTicks true - oldDay)
  return { clock, creature, flying }

private def decodeChests (json : Json) (world : World) (saveVersion maxItemCode : Nat)
    (dungeonChests shadowSites : Array TilePos) (allowShadow : Bool) : Except String (Array Chest) := do
  let entries ← json.getArr?
  let (count, shadowCells, templeCells) := world.tiles.data.foldl (fun (count, shadowCells, templeCells) code =>
    if code == Tile.chest.code then (count + 1, shadowCells, templeCells)
    else if code == Tile.shadowChest.code || code == Tile.shadowOpenChest.code then (count, shadowCells + 1, templeCells)
    else if code == Tile.lihzahrdChest.code then (count, shadowCells, templeCells + 1)
    else (count, shadowCells, templeCells)) (0, 0, 0)
  if !allowShadow && shadowCells > 0 then throw "Shadow Chests require an expanded world."
  if shadowCells % 4 != 0 then throw "A Shadow Chest is missing part of its footprint."
  if templeCells % 4 != 0 then throw "A Lihzahrd Chest is missing part of its footprint."
  if entries.size != count + dungeonChests.size + shadowCells / 4 + templeCells / 4 then
    throw "Chest records do not match the number of chest sites."
  let mut chests := #[]
  let mut seen : Std.HashSet TilePos := {}
  let mut covered : Std.HashSet TilePos := {}
  for entry in entries do
    let position ← decodePosition (← entry.getObjVal? "position")
    let tile := world.tile position
    let shadow := allowShadow && (tile == .shadowChest || tile == .shadowOpenChest)
    let temple := tile == .lihzahrdChest
    if (tile != .chest && !dungeonChests.contains position && !shadow && !temple) || seen.contains position then
      throw "A chest record has a missing or duplicate tile."
    if shadow || temple then
      if tile == .shadowChest && !shadowSites.contains position then
        throw "A locked Shadow Chest is outside its generated site."
      for cell in dungeonChestCells position do
        if world.tile cell != tile || covered.contains cell then
          throw "A chest footprint is incomplete or overlaps another chest."
        covered := covered.insert cell
    let slots ← (← entry.getObjVal? "slots").getArr?
    let expectedSlots := if saveVersion < 31 then 16 else chestSlots
    if slots.size != expectedSlots then throw s!"A chest must have {expectedSlots} slots."
    let slots ← slots.mapM (decodeSlot maxItemCode)
    let slots := slots ++ Array.replicate (chestSlots - slots.size) none
    seen := seen.insert position
    chests := chests.push { position, slots }
  if covered.size != shadowCells + templeCells then throw "A chest tile has no matching chest record."
  return chests

private def validateDoors (world : World) : Except String Unit := do
  for index in [:world.tiles.size] do
    let code := world.tiles[index]!
    if (Tile.ofCode? code).any Tile.isDoor then
      let position : TilePos := ⟨index % world.width, index / world.width⟩
      let some base := doorBase world position | throw "A door is missing a matching part."
      if base == position && !doorSupportValid world base then
        throw "A door is missing its floor or required roof."
    else if (Tile.ofCode? code).any (fun tile => (TempleDoor.kind tile).isSome) then
      let position : TilePos := ⟨index % world.width, index / world.width⟩
      let some (hinge, _) := TempleDoor.atCell world position
        | throw "A Lihzahrd Door has an incomplete footprint."
      if !TempleDoor.supports world hinge then
        throw "A Lihzahrd Door is missing a support."
    else if (Tile.ofCode? code).any Tile.isTempleAltar then
      let position : TilePos := ⟨index % world.width, index / world.width⟩
      if (TempleLayout.altarAnchor world position).isNone then throw "An altar has an incomplete footprint."

private def decodeHomeDoor (json : Json) (world : World) (classic : Bool) : Except String (Option TilePos) := do
  if json == .null then return none
  let door ← decodePosition json
  let valid := if classic then isClassicEntrance world door else doorBase world door == some door
  if !valid then throw "The registered home entrance does not exist."
  return some door

private def decodeHomeAnchor (json : Json) (world : World) : Except String (Option TilePos) := do
  if json == .null then return none
  let anchor ← decodePosition json
  if world.tile? anchor != some .homeBeacon then throw "The bound Home Beacon does not exist."
  return some anchor

private def decodeLiquid (json : Json) (world : World) (name : String) (legacy : Bool)
    (other : Option Water := none) : Except String Water := do
  let entries ← json.getArr?
  let size := world.width * world.height
  let capacity := if legacy then 8 else Water.capacity
  if entries.size > size then throw s!"Too many {name} cells."
  let mut levels := ByteArray.mk (Array.replicate size 0)
  for entry in entries do
    let pair ← entry.getArr?
    let #[rawIndex, rawAmount] := pair | throw s!"A {name} cell must have an index and an amount."
    let index ← rawIndex.getNat?
    let amount ← rawAmount.getNat?
    if index ≥ size || amount == 0 || amount > capacity || levels[index]! != 0 then
      throw s!"Invalid or repeated {name} cell."
    let position : TilePos := ⟨index % world.width, index / world.width⟩
    if (world.tile position).solid then throw s!"A solid tile cannot contain {name}."
    if other.any (fun liquid => liquid.levels[index]! != 0) then
      throw s!"{name} overlaps another liquid."
    let fill := if legacy then Water.fromEighths amount else amount
    levels := levels.set! index fill.toUInt8
  return { levels }

private def decodeQuarry (json : Json) (world : World) : Except String QuarryState := do
  let cleared : Bool ← readField json "cleared"
  let data ← json.getObjVal? "creature"
  if data == .null then return { cleared }
  if cleared then throw "The quarry crawler is already defeated."
  let body ← decodePlayer (← data.getObjVal? "body") world
  let health : Nat ← readField data "health"
  let hopIn : Nat ← readField data "hopIn"
  if !inQuarryCreatureRange body.centerTile ||
      health == 0 || health > quarryCreatureMaxHealth || hopIn > quarryCreatureHopDelay then
    throw "Invalid quarry crawler position, health or hop timer."
  return { creature := some { body, health, hopIn }, cleared }

private def decodeSentry (json : Json) (world : World) : Except String SentryState := do
  let cleared : Bool ← readField json "cleared"
  let data ← json.getObjVal? "creature"
  if data == .null then return { cleared }
  if cleared then throw "A defeated sentry cannot be active."
  let body ← decodePlayer (← data.getObjVal? "body") world 0
  let health : Nat ← readField data "health"
  let aim : Nat ← readField data "aim"
  if body.vy != 0 || !body.grounded || !inSentryRange body.centerTile ||
      health == 0 || health > sentryMaxHealth || aim > sentryAimLimit then
    throw "Invalid sentry position, health or aim timer."
  return { creature := some { body, health, aim }, cleared }

private def decodeStormPhase : String → Except String StormPhase
  | "aim" => pure .aim
  | "barrage" => pure .barrage
  | "recover" => pure .recover
  | value => throw s!"Unknown Storm Herald phase '{value}'."

private def decodeStorm (json : Json) (world : World) (player : Player) : Except String StormEncounter := do
  match ← readField (α := String) json "state" with
  | "ready" => pure .ready
  | "fighting" => do
    let body ← decodePlayer (← json.getObjVal? "body") world 0
    let health : Nat ← readField json "health"
    let facing : Int ← readField json "facing"
    let phase ← decodeStormPhase (← readField json "phase")
    let ticks : Nat ← readField json "ticks"
    let boss : StormHerald := { body, health, phase, ticks, facing }
    if !stormPlayerInRange player || !inStormArena body.centerTile ||
        health == 0 || health > stormMaxHealth ||
        (facing != -1 && facing != 1) || ticks == 0 || ticks > boss.phaseDuration then
      throw "Invalid Storm Herald position, health, facing or phase timer."
    return .fighting boss
  | "reward" => do
    let position ← decodePosition (← json.getObjVal? "position")
    if !inStormArena position || (world.tile position).solid then
      throw "The Storm Bow must rest in an open tile in the arena."
    return .reward position
  | "cleared" => pure .cleared
  | value => throw s!"Unknown Storm Herald encounter state '{value}'."

private def decodeRuinPhase : String → Except String RuinPhase
  | "windup" => pure .windup
  | "slam" => pure .slam
  | "recover" => pure .recover
  | value => throw s!"Unknown Ruin Sentinel phase '{value}'."

private def validRuinTarget (x : Int) : Bool :=
  (ruinArenaStart : Int) * tileUnits ≤ x && x < ((ruinArenaEnd : Int) + 1) * tileUnits

private def decodeRuin (json : Json) (world : World) (player : Player) : Except String RuinEncounter := do
  let state : String ← readField json "state"
  if state != "ready" && world.tile ruinAltar != .ruinAltar then
    throw "The Ruin Sentinel encounter needs its original altar."
  match state with
  | "ready" => pure .ready
  | "fighting" => do
    let body ← decodePlayer (← json.getObjVal? "body") world 0
    let health ← readField json "health"
    let phase ← decodeRuinPhase (← readField json "phase")
    let ticks ← readField json "ticks"
    let targetX ← readField json "targetX"
    let boss : RuinSentinel := { body, health, phase, ticks, targetX }
    if !ruinPlayerInRange player || !inRuinArena body.centerTile || !validRuinTarget targetX ||
        health == 0 || health > ruinMaxHealth || ticks == 0 || ticks > boss.phaseDuration then
      throw "Invalid Ruin Sentinel position, health, target or phase timer."
    return .fighting boss
  | "reward" => do
    let position ← decodePosition (← json.getObjVal? "position")
    if !inRuinArena position || (world.tile position).solid then
      throw "The Ruin Hammer must rest in an open tile in the arena."
    return .reward position
  | "cleared" => pure .cleared
  | value => throw s!"Unknown Ruin Sentinel encounter state '{value}'."

private def decodeCavernPhase : String → Except String CavernPhase
  | "windup" => pure .windup
  | "beam" => pure .beam
  | "recover" => pure .recover
  | value => throw s!"Unknown Deepstone Colossus phase '{value}'."

private def decodeCavern (json : Json) (world : World) (player : Player) (altar : TilePos) :
    Except String CavernEncounter := do
  let state : String ← readField json "state"
  if state != "ready" && world.tile altar != .cavernAltar then
    throw "The Deepstone Colossus encounter needs its original altar."
  match state with
  | "ready" => pure .ready
  | "fighting" => do
    let body ← decodePlayer (← json.getObjVal? "body") world 0
    let health ← readField json "health"
    let phase ← decodeCavernPhase (← readField json "phase")
    let ticks ← readField json "ticks"
    let boss : DeepstoneColossus := { body, health, phase, ticks }
    let footing : TilePos := ⟨body.centerTile.x, cavernArenaFloor⟩
    if !cavernPlayerInRange player || !cavernBossPositionValid body || !(world.tile footing).solid ||
        health == 0 || health > cavernMaxHealth || ticks == 0 || ticks > boss.phaseDuration then
      throw "Invalid Deepstone Colossus position, health or phase timer."
    return .fighting boss
  | "reward" => do
    let position ← decodePosition (← json.getObjVal? "position")
    if !inCavernArena position || (world.tile position).solid then
      throw "The Deepstone Aegis must rest in an open tile in the arena."
    return .reward position
  | "cleared" => pure .cleared
  | value => throw s!"Unknown Deepstone Colossus encounter state '{value}'."

private def validCavernBeam (encounter : CavernEncounter) (shot : Projectile) : Bool :=
  match encounter with
  | .fighting boss =>
    let elapsed := match boss.phase with
      | .windup => 0
      | .beam => boss.phaseDuration - boss.ticks
      | .recover => ({ boss with phase := .beam }).phaseDuration + (boss.phaseDuration - boss.ticks)
    let age := shot.kind.lifetime - shot.life
    let expectedAge := if shot.y == cavernHighBeamY then elapsed - cavernHighBeamDelay else elapsed
    (shot.y == cavernLowBeamY || shot.y == cavernHighBeamY) && age > 0 && age == expectedAge &&
      shot.x - shot.vx * (age : Int) == bodyCenterX boss.body
  | _ => false

private def decodeShotKind : String → Except String ShotKind
  | "arrow" => pure .arrow
  | "woodenArrow" => pure .woodenArrow
  | "unholyArrow" => pure .unholyArrow
  | "hellfireArrow" => pure .hellfireArrow
  | "musketBullet" => pure .musketBullet
  | "undertakerBullet" => pure .undertakerBullet
  | "handgunBullet" => pure .handgunBullet
  | "thrownBone" => pure .thrownBone
  | "stormArrow" => pure .stormArrow
  | "hunterArrow" => pure .hunterArrow
  | "sentry" => pure .sentry
  | "herald" => pure .herald
  | "raidBolt" => pure .raidBolt
  | "ruinWave" => pure .ruinWave
  | "highlandBolt" => pure .highlandBolt
  | "cavernBeam" => pure .cavernBeam
  | "guideArrow" => pure .guideArrow
  | value => throw s!"Unknown projectile kind '{value}'."

private def decodeProjectiles (json : Json) (world : World) (storm : StormEncounter) (raid : RaidEncounter)
    (ruin : RuinEncounter) (cavern : CavernEncounter) (saveVersion : Nat) : Except String (Array Projectile) := do
  let entries ← json.getArr?
  if entries.size > maxProjectiles then throw "Too many active projectiles."
  let stormActive := match storm with
    | .fighting _ => true
    | _ => false
  let raidActive := match raid with
    | .active _ => true
    | _ => false
  let ruinActive := match ruin with
    | .fighting _ => true
    | _ => false
  let shots ← entries.mapM fun entry => do
    let kind ← decodeShotKind (← readField entry "kind")
    let x ← readField entry "x"
    let y ← readField entry "y"
    let vx ← readField entry "vx"
    let vy ← readField entry "vy"
    let life ← readField entry "life"
    let damageBonus : Nat ← if saveVersion < 27 then pure 0 else readField entry "damageBonus"
    let hitsLeft : Nat ← if saveVersion < 27 then pure 1 else readField entry "hitsLeft"
    let slot ← decodeProjectileSlot entry saveVersion
    let shot : Projectile := { kind, x, y, vx, vy, life, damageBonus, hitsLeft, slot }
    let validWave := if kind == .ruinWave then
        -- The spawning tick moves the wave and spends its first tick of life.
        let age : Int := kind.lifetime - life
        ruinActive && life < kind.lifetime && y == (ruinArenaFloor : Int) * tileUnits - 64 &&
          validRuinTarget (x - vx * age)
      else true
    if !shot.valid world || (kind == .herald && !stormActive) ||
        (kind == .stormArrow && storm != .cleared) || (kind == .raidBolt && !raidActive) ||
        (kind == .hunterArrow && saveVersion < 12) ||
        (kind == .highlandBolt && saveVersion < 20) || !validWave ||
        ((kind == .guideArrow || kind == .woodenArrow) && saveVersion < 26) ||
        (kind == .unholyArrow && saveVersion < 27) ||
        (kind == .hellfireArrow && saveVersion < 49) ||
        (saveVersion < 49 && damageBonus > 0 && kind.isBullet) ||
        ((kind == .musketBullet || kind == .undertakerBullet) && saveVersion < 28) ||
        (kind == .handgunBullet && (saveVersion < 45 || life ≥ kind.lifetime || life % 2 != 0 || slot.isNone)) ||
        (kind == .thrownBone && saveVersion < 36) ||
        (kind == .cavernBeam && (saveVersion < 21 || !validCavernBeam cavern shot)) then
      throw "Invalid or orphaned projectile."
    return shot
  let mut beamDirections : Array (Int × Int) := #[]
  for shot in shots do
    if shot.kind == .cavernBeam then
      let direction := (shot.y, shot.vx)
      if beamDirections.contains direction then throw "Duplicate Deepstone Colossus beam."
      beamDirections := beamDirections.push direction
  return shots

private def decodeChart (json : Json) (world : World) (maxTileCode : Nat) : Except String Chart := do
  let entries ← json.getArr?
  let size := world.width * world.height
  if entries.size > size then throw "Too many explored map cells."
  let mut cells := (Chart.empty world).cells
  for entry in entries do
    let pair ← entry.getArr?
    let #[rawIndex, rawCode] := pair | throw "An explored cell must have an index and a tile."
    let index ← rawIndex.getNat?
    let code ← rawCode.getNat?
    if index ≥ size || code == 0 || code > maxTileCode + 1 ||
        cells[index]! != 0 || (Tile.ofCode? ((code - 1).toUInt8)).isNone then
      throw "Invalid or repeated explored map cell."
    cells := cells.set! index code.toUInt8
  return { cells }

private def decodeMerchantStock (json : Json) (saveVersion : Nat) (legacyEarly : Bool) :
    Except String (Array Nat) := do
  let stock : Array Nat ← readField json "merchantStock"
  let currentOffers := shopOffersFor legacyEarly
  let oldCount := if saveVersion < 9 then 3 else if saveVersion < 14 then 5
    else if saveVersion < 49 then (shopOffersFor true).size else currentOffers.size
  let offers := currentOffers.extract 0 oldCount
  if stock.size != offers.size ||
      (stock.zip offers).any (fun (quantity, offer) => quantity > offer.limit ||
        (offer.item == .bugNet && quantity != 1)) then
    throw "Merchant stock exceeds its daily offer limits."
  let initial := initialMerchantStockFor legacyEarly
  return stock ++ initial.extract oldCount initial.size

private def decodeEquippedItem (data : Json) (slot : GearSlot) (maxCode : Nat) :
    Except String (Option Item) := do
  if data == .null then return none
  let code ← data.getNat?
  if code > maxCode then throw "Unknown equipped item."
  let some item := Item.ofCode? code.toUInt16 | throw "Unknown equipped item."
  if item.gearSlot != some slot then throw "Item does not match its equipment slot."
  return some item

private def decodeEquipment (json : Json) (saveVersion : Nat) : Except String Equipment := do
  let maxCode := if saveVersion < 21 then Item.raidCrest.code.toNat
    else if saveVersion < 28 then Item.deepstoneAegis.code.toNat
    else if saveVersion < 31 then Item.panicNecklace.code.toNat
    else if saveVersion < 38 then Item.cobaltShield.code.toNat
    else if saveVersion < 46 then Item.nazar.code.toNat
    else if saveVersion < 48 then Item.obsidianSkull.code.toNat
    else if saveVersion < 49 then Item.magmaStone.code.toNat
    else if saveVersion < 51 then Item.treasureMagnet.code.toNat
    else if saveVersion < 52 then Item.trifoldMap.code.toNat
    else if saveVersion < 58 then Item.vitamins.code.toNat
    else if saveVersion < 59 then Item.lavaproofFishingHook.code.toNat
    else if saveVersion < 60 then Item.theDuke.code.toNat
    else if saveVersion < 61 then Item.itsScragglinTime.code.toNat
    else if saveVersion < 64 then Item.crispyHoneyBlock.code.toNat
    else Item.crystalAssassinPants.code.toNat
  let head ← if saveVersion < 64 then pure none else decodeEquippedItem (← json.getObjVal? "head") .head maxCode
  let armor ← decodeEquippedItem (← json.getObjVal? "armor") .armor maxCode
  let accessory ← decodeEquippedItem (← json.getObjVal? "accessory") .accessory maxCode
  let hook ← if saveVersion < 63 then pure none
    else decodeEquippedItem (← json.getObjVal? "hook") .hook Item.hookOfDissonance.code.toNat
  let legs ← if saveVersion < 64 then pure none else decodeEquippedItem (← json.getObjVal? "legs") .legs maxCode
  let mount ← if saveVersion < 65 then pure none
    else decodeEquippedItem (← json.getObjVal? "mount") .mount Item.gelatinousPillion.code.toNat
  return { head, armor, legs, accessory, hook, mount }

private def decodeWingedSlime (json : Json) : Except String (Option WingedSlime) := do
  if json == .null then return none
  let bits := decodeFloat32Bits json "Winged Slime"
  let mount : WingedSlime := {
    vx := ← bits "vxBits", vy := ← bits "vyBits",
    remainderX := ← bits "remainderXBits", remainderY := ← bits "remainderYBits",
    flight := ← readField json "flight", jump := ← readField json "jump",
    wetSlime := ← readField json "wetSlime", jumpHeld := ← readField json "jumpHeld",
    stompTicks := ← readField json "stompTicks", stompCount := ← readField json "stompCount",
    frame := ← readField json "frame", frameCounter := ← bits "frameCounterBits",
    wingFrame := ← readField json "wingFrame", wingCounter := ← readField json "wingCounter" }
  if !mount.valid then throw "Invalid Winged Slime mount state."
  return some mount

private def decodeAssassinDash (json : Json) (equipment : Equipment) : Except String AssassinDash := do
  let tap ← readField json "tap"
  let velocity ← decodeFloat32Bits json "Crystal Assassin dash" "velocityBits"
  let cooldown ← readField json "cooldown"
  let dash : AssassinDash := { tap, velocity, cooldown }
  if !dash.valid || (dash != {} && !equipment.crystalAssassinSet) then
    throw "Invalid Crystal Assassin dash state."
  return dash

private def decodeWeaponArmorStats (json : Json) : Except String (Array (Nat × WeaponArmorStats)) := do
  let entries ← json.getArr?
  if entries.size > maxProjectileSlots then throw "Too many projectile armor stat entries."
  let mut stats := #[]
  for entry in entries do
    let #[index, damage, crit] ← entry.getArr? | throw "Malformed projectile armor stats."
    let slot ← index.getNat?
    let profile : WeaponArmorStats := { damageBonus := ← damage.getNat?, criticalChance := ← crit.getNat? }
    if slot ≥ maxProjectileSlots || !profile.valid || stats.any (·.1 == slot) then
      throw "Invalid or repeated projectile armor stats."
    stats := stats.push (slot, profile)
  return stats

private def decodeRaidFoeKind : String → Except String RaidFoeKind
  | "runner" => pure .runner
  | "slinger" => pure .slinger
  | "brute" => pure .brute
  | _ => throw "Unknown raid enemy kind."

private def decodeRaider (json : Json) (world : World) : Except String Raider := do
  let kind ← decodeRaidFoeKind (← readField json "kind")
  let body ← decodePlayer (← json.getObjVal? "body") world kind.maxSpeed
  let health : Nat ← readField json "health"
  let cooldown : Nat ← readField json "cooldown"
  if !inRaidFoeRange body.centerTile || health == 0 || health > kind.maxHealth || cooldown > kind.maxCooldown then
    throw "Invalid raider position, health or cooldown."
  return { kind, body, health, cooldown }

private def decodeNpcTarget (json : Json) (saveVersion : Nat) : Except String NpcTarget := do
  let id := readField (α := Nat) json "id"
  let underworldId := fun introduced => do
    if saveVersion < introduced then throw s!"Underworld NPC targets require a version {introduced} save."
    let number ← id
    if number == 0 then throw "An Underworld NPC target requires a positive ID."
    return number
  let kind : String ← readField json "kind"
  match kind with
  | "crawler" => pure .crawler
  | "warden" => pure .warden
  | "quarry" => pure .quarry
  | "sentry" => pure .sentry
  | "herald" => pure .herald
  | "sentinel" => pure .sentinel
  | "colossus" => pure .colossus
  | "surface" => .surface <$> decodeSurfaceKind (← readField json "surface")
  | "eye" => pure .eye
  | "eyeServant" => .eyeServant <$> id
  | "eaterSegment" => .eaterSegment <$> id
  | "brainCore" => pure .brainCore
  | "brainCreeper" => .brainCreeper <$> id
  | "evilFlyer" => .evilFlyer <$> id
  | "skeletron" =>
    match ← readField (α := String) json "part" with
    | "head" => pure (.skeletron .head)
    | "leftHand" => pure (.skeletron .leftHand)
    | "rightHand" => pure (.skeletron .rightHand)
    | part => throw s!"Unknown Skeletron part '{part}'."
  | "dungeonGuardian" => pure .dungeonGuardian
  | "dungeonSlime" => .dungeonSlime <$> id
  | "darkCaster" => .darkCaster <$> id
  | "waterSphere" => .waterSphere <$> id
  | "angryBones" => .angryBones <$> id
  | "cursedSkull" => .cursedSkull <$> id
  | "lavaSlime" => .lavaSlime <$> underworldId 48
  | "underworldFlyer" => .underworldFlyer <$> underworldId 48
  | "fireImp" => .fireImp <$> underworldId 48
  | "burningSphere" => .burningSphere <$> underworldId 48
  | "boneSerpent" => do
    let id ← underworldId 49
    let part : Nat ← readField json "part"
    if part ≥ maxBoneSerpentSegments then throw "A Bone Serpent target has an invalid segment index."
    return .boneSerpent id part
  | "underworldCritter" => .underworldCritter <$> underworldId 49
  | "wall" => do
    if saveVersion < 50 then throw "Wall of Flesh targets require a version 50 save."
    match ← readField (α := String) json "part" with
    | "mouth" => pure (.wall .mouth)
    | "upperEye" => pure (.wall .upperEye)
    | "lowerEye" => pure (.wall .lowerEye)
    | part => throw s!"Unknown Wall of Flesh part '{part}'."
  | "wallHungry" => .wallHungry <$> underworldId 50
  | "wallLeech" => do
    let id ← underworldId 50
    let part : Nat ← readField json "part"
    if part ≥ maxWallLeechSegments then throw "A Wall leech target has an invalid segment index."
    return .wallLeech id part
  | "wraith" => do
    if saveVersion < 51 then throw "Wraith targets require a version 51 save."
    let number ← id
    if number == 0 then throw "A Wraith target requires a positive ID."
    return .wraith number
  | "hardmodeCavern" => do
    if saveVersion < 52 then throw "Hardmode cavern targets require a version 52 save."
    let number ← id
    if number == 0 then throw "A Hardmode cavern target requires a positive ID."
    return .hardmodeCavern number
  | "twin" => do
    if saveVersion < 53 then throw "Twin targets require a version 53 save."
    return .twin (← decodeTwinKind (← readField json "twin"))
  | "destroyerSegment" => do
    if saveVersion < 68 then throw "Destroyer targets require a version 68 save."
    let part : Nat ← readField json "part"
    if part ≥ destroyerSegmentCount then throw "Destroyer segment index is out of range."
    return .destroyerSegment part
  | "destroyerProbe" => do
    if saveVersion < 68 then throw "Probe targets require a version 68 save."
    let number ← id
    if number == 0 then throw "A Probe target requires a positive ID."
    return .destroyerProbe number
  | "primeHead" => do
    if saveVersion < 69 then throw "Skeletron Prime targets require a version 69 save."
    return .primeHead
  | "primeArm" => do
    if saveVersion < 69 then throw "Skeletron Prime targets require a version 69 save."
    return .primeArm (← decodePrimeArmKind (← readField json "part"))
  | "planteraHead" => do
    if saveVersion < 70 then throw "Plantera targets require a version 70 save."
    return .planteraHead
  | "golemBody" | "golemHead" | "golemLeftFist" | "golemRightFist" => do
    if saveVersion < 72 then throw "Golem targets require a version 72 save."
    return match kind with
      | "golemBody" => .golemBody
      | "golemHead" => .golemHead
      | "golemLeftFist" => .golemLeftFist
      | _ => .golemRightFist
  | "templeEnemy" => do
    if saveVersion < 73 then throw "Temple enemy targets require a version 73 save."
    let number ← id
    if number == 0 then throw "A Temple enemy requires a positive ID."
    return .templeEnemy number
  | "planteraTentacle" | "planteraSpore" => do
    if saveVersion < 70 then throw "Plantera targets require a version 70 save."
    let number ← id
    if number == 0 then throw "A Plantera part requires a positive ID."
    return if kind == "planteraTentacle" then .planteraTentacle number else .planteraSpore number
  | "queenSlime" => do
    if saveVersion < 62 then throw "Queen Slime targets require a version 62 save."
    return .queenSlime
  | "queenMinion" => do
    if saveVersion < 62 then throw "Queen Slime minion targets require a version 62 save."
    let number ← id
    if number == 0 then throw "A Queen Slime minion needs a positive ID."
    return .queenMinion number
  | "raider" => return .raider (← readField json "wave") (← decodeRaidFoeKind (← readField json "foe"))
  | "cave" => .cave <$> readField json "index"
  | kind => throw s!"Unknown NPC target kind '{kind}'."

private def decodeNpcImmunity (json : Json) (saveVersion : Nat) (tickLimit : Nat := npcHitCooldown) :
    Except String (Array (NpcTarget × Nat)) := do
  let entries ← json.getArr?
  if entries.size > maxNpcImmunity then throw "Too many NPC hit cooldowns."
  let mut cooldowns : Array (NpcTarget × Nat) := #[]
  for entry in entries do
    let target ← decodeNpcTarget (← entry.getObjVal? "target") saveVersion
    let ticks : Nat ← readField entry "ticks"
    if ticks == 0 || ticks > tickLimit then throw "NPC hit cooldown lies outside its tick range."
    if cooldowns.any (fun (existing, _) => existing == target) then throw "Duplicate NPC hit cooldown."
    cooldowns := cooldowns.push (target, ticks)
  return cooldowns

private def decodeDaggerPose (json : Json) : Except String DaggerPose := do
  let bits := decodeFloat32Bits json "Enchanted Dagger"
  return ⟨← bits "xBits", ← bits "yBits", ← bits "rotationBits"⟩

private def decodeDagger (json : Json) (saveVersion : Nat) : Except String (Option DaggerSummon) := do
  if json == .null then return none
  let bits := decodeFloat32Bits json "Enchanted Dagger"
  let trail ← (← json.getObjVal? "trail").getArr?
  if trail.size > bladeStaffTrailLength then throw "Enchanted Dagger trail exceeds its frame count."
  let blade : EnchantedDagger := {
    pose := ← decodeDaggerPose (← json.getObjVal? "pose"), vx := ← bits "vxBits", vy := ← bits "vyBits",
    trail := ← trail.mapM decodeDaggerPose }
  let phaseData ← json.getObjVal? "phase"
  let kind : String ← readField phaseData "kind"
  let phase ← match kind with
    | "idle" => pure DaggerPhase.idle
    | "chase" => pure (DaggerPhase.chase (← decodeNpcTarget (← phaseData.getObjVal? "target") saveVersion))
    | "recoil" => pure (DaggerPhase.recoil (← readField phaseData "elapsed"))
    | _ => throw "Unknown Enchanted Dagger phase."
  let cooldowns ← decodeNpcImmunity (← json.getObjVal? "cooldowns") saveVersion bladeStaffLocalImmunity
  return some { blade, phase, cooldowns, slot := ← readField json "slot" }

private def decodeNpcBurns (json : Json) (saveVersion : Nat) : Except String (Array NpcBurn) := do
  let entries ← json.getArr?
  if entries.size > maxNpcImmunity then throw "Too many NPC burn effects."
  let maxOnFire := if saveVersion < 47 then 300 else 479
  entries.mapM fun entry => do
    let target ← decodeNpcTarget (← entry.getObjVal? "target") saveVersion
    let onFire : Nat ← readField entry "onFire"
    let shadowflame : Nat ← readField entry "shadowflame"
    let hellfire : Nat ← if saveVersion < 48 then pure 0 else readField entry "hellfire"
    let progress : Nat ← readField entry "progress"
    if onFire > maxOnFire || shadowflame > 299 || hellfire > 360 || progress ≥ 300 ||
        (onFire == 0 && shadowflame == 0 && hellfire == 0) then
      throw "Invalid NPC burn duration or progress."
    return {target, onFire, shadowflame, hellfire, progress}

private def decodeFlailPhase : String → Except String FlailPhase
  | "spinning" => pure .spinning
  | "outbound" => pure .outbound
  | "returning" => pure .returning
  | "tethered" => pure .tethered
  | "forcedReturn" => pure .forcedReturn
  | "ricochet" => pure .ricochet
  | "dropped" => pure .dropped
  | phase => throw s!"Unknown Blue Moon flail phase '{phase}'."

private def decodeFlail (json : Json) (saveVersion : Nat) : Except String Flail := do
  let readBits := decodeFloat32Bits json "Blue Moon flail"
  let x ← readBits "xBits"
  let y ← readBits "yBits"
  let vx ← readBits "vxBits"
  let vy ← readBits "vyBits"
  let rotation ← readBits "rotationBits"
  let phase ← decodeFlailPhase (← readField json "phase")
  let phaseTicks : Nat ← readField json "phaseTicks"
  let spinTicks : Nat ← readField json "spinTicks"
  let collisionCount : Nat ← readField json "collisionCount"
  let slot ← decodeProjectileSlot json saveVersion
  let tileCollide : Bool ← readField json "tileCollide"
  let spriteDirection : Int ← readField json "spriteDirection"
  return {x, y, vx, vy, rotation, phase, phaseTicks, spinTicks, collisionCount, slot, tileCollide, spriteDirection}

private def decodeFlailAttack (json : Json) (saveVersion : Nat) : Except String (Option BlueMoonAttack) := do
  if json == .null then return none
  let flail ← decodeFlail (← json.getObjVal? "flail") saveVersion
  let aim ← decodeWorldAim (← json.getObjVal? "aim")
  let cooldowns ← decodeNpcImmunity (← json.getObjVal? "cooldowns") saveVersion blueMoonHitCooldown
  return some {flail, aim, cooldowns}

private def decodeDarkLance (json : Json) (saveVersion : Nat) : Except String (Option DarkLance) := do
  if json == .null then return none
  let readBits := decodeFloat32Bits json "Dark Lance"
  let vx ← readBits "vxBits"
  let vy ← readBits "vyBits"
  let age : Nat ← readField json "age"
  let facing : Int ← readField json "facing"
  let slot ← decodeProjectileSlot json saveVersion
  let animation ← if saveVersion < 64 then pure darkLanceUseTime else readField json "animation"
  return some {vx, vy, age, facing, slot, animation}

private def decodeMagicMissileTrail (json : Json) : Except String (Array MagicMissileTrailPoint) := do
  let entries ← json.getArr?
  if entries.size > magicMissileTrailLength then throw "Magic Missile trail exceeds its history limit."
  entries.mapM fun entry => do
    let readBits := decodeFloat32Bits entry "Magic Missile trail point"
    return {x := ← readBits "xBits", y := ← readBits "yBits", rotation := ← readBits "rotationBits"}

private def decodeProjectileArray (json : Json) (name : String) (decode : Json → Except String α) :
    Except String (Array α) :=
  decodeActorArray json name maxProjectileSlots decode

private def decodeMissile (body : Json) (saveVersion : Nat) (name : String) : Except String MagicMissile := do
  let readBits := decodeFloat32Bits body name
  let x ← readBits "xBits"
  let y ← readBits "yBits"
  let vx ← readBits "vxBits"
  let vy ← readBits "vyBits"
  let rotation ← readBits "rotationBits"
  let life : Nat ← readField body "life"
  let guided : Bool ← readField body "guided"
  let slot ← decodeProjectileSlot body saveVersion
  let soundDelay : Nat ← readField body "soundDelay"
  let trail ← decodeMagicMissileTrail (← body.getObjVal? "trail")
  return {x, y, vx, vy, rotation, life, guided, slot, soundDelay, trail}

private def decodeMagicMissiles (json : Json) (saveVersion : Nat) : Except String (Array MagicMissileAttack) :=
  decodeProjectileArray json "Magic Missiles" fun entry => do
    let missile ← decodeMissile (← entry.getObjVal? "missile") saveVersion "Magic Missile"
    let aim ← decodeWorldAim (← entry.getObjVal? "aim")
    let target ← match ← entry.getObjVal? "target" with
      | .null => pure none
      | target => some <$> decodeNpcTarget target saveVersion
    return {missile, aim, target}

private def decodeFlowerFireballs (json : Json) (saveVersion : Nat) : Except String (Array FlowerFireball) :=
  decodeProjectileArray json "Flower of Fire fireballs" fun entry => do
    let readBits := decodeFloat32Bits entry "Flower of Fire fireball"
    return {
      x := ← readBits "xBits", y := ← readBits "yBits", vx := ← readBits "vxBits", vy := ← readBits "vyBits"
      life := ← readField entry "life", bounces := ← readField entry "bounces", wet := ← readField entry "wet"
      slot := ← decodeProjectileSlot entry saveVersion }

private def decodeHellwingShots (json : Json) (saveVersion : Nat) : Except String (Array HellwingShot) :=
  decodeProjectileArray json "Hellwing Bow shots" fun entry => do
    let readBits := decodeFloat32Bits entry "Hellwing Bow shot"
    return {
      x := ← readBits "xBits", y := ← readBits "yBits", vx := ← readBits "vxBits", vy := ← readBits "vyBits"
      goalVx := ← readBits "goalVxBits", goalVy := ← readBits "goalVyBits", life := ← readField entry "life"
      hits := ← readField entry "hits", unholy := ← readField entry "unholy", slot := ← decodeProjectileSlot entry saveVersion }

private def decodeDemonSickles (json : Json) (saveVersion : Nat) : Except String (Array DemonSickle) :=
  decodeProjectileArray json "Demon Sickles" fun entry => do
    let (x, y, vx, vy) ← decodeFloatMotion entry "Demon Sickle"
    return {
      x, y, vx, vy, rotation := ← decodeFloat32Bits entry "Demon Sickle" "rotationBits"
      life := ← readField entry "life", hits := ← readField entry "hits"
      hostile := ← readField entry "hostile", slot := ← decodeProjectileSlot entry saveVersion }

private def decodeFlamelashes (json : Json) (saveVersion : Nat) : Except String (Array FlamelashAttack) :=
  decodeProjectileArray json "Flamelash projectiles" fun entry => do
    let data ← entry.getObjVal? "projectile"
    let projectile : Flamelash := {
      missile := ← decodeMissile (← data.getObjVal? "missile") saveVersion "Flamelash"
      hitsLeft := ← readField data "hitsLeft", wet := ← readField data "wet", frame := ← readField data "frame"
      frameTicks := ← readField data "frameTicks", empoweredTicks := ← readField data "empoweredTicks" }
    let aim ← decodeWorldAim (← entry.getObjVal? "aim")
    let target ← match ← entry.getObjVal? "target" with
      | .null => pure none
      | target => some <$> decodeNpcTarget target saveVersion
    let cooldowns ← decodeNpcImmunity (← entry.getObjVal? "cooldowns") saveVersion flamelashHitCooldown
    return {projectile, aim, target, cooldowns}

private def decodeValor (json : Json) (saveVersion : Nat) : Except String (Option ValorAttack) := do
  if json == .null then return none
  let body ← json.getObjVal? "yoyo"
  let readBits := decodeFloat32Bits body "Valor"
  let yoyo : Yoyo := {
    x := ← readBits "xBits", y := ← readBits "yBits", vx := ← readBits "vxBits", vy := ← readBits "vyBits"
    rotation := ← readBits "rotationBits", aimX := ← readBits "aimXBits", aimY := ← readBits "aimYBits"
    age := ← readField body "age", returning := ← readField body "returning"
    slot := ← decodeProjectileSlot body saveVersion, recoilStalled := ← readField body "recoilStalled" }
  let aim ← decodeWorldAim (← json.getObjVal? "aim")
  return some {yoyo, aim}

private def decodeRaid (json : Json) (world : World) (seed : UInt64) (player : Player) :
    Except String RaidEncounter := do
  match ← readField (α := String) json "state" with
  | "ready" => pure .ready
  | "active" => do
    let number : Nat ← readField json "wave"
    let countdown : Nat ← readField json "countdown"
    let entries ← (← json.getObjVal? "foes").getArr?
    if !raidPlayerInRange seed player || number == 0 || number > raidWaveCount ||
        countdown > raidCountdownTicks || entries.size > raidMaxFoes ||
        (countdown > 0 && (!entries.isEmpty || number == 1)) || (countdown == 0 && entries.isEmpty) then
      throw "Invalid Amber Raid wave or player position."
    let foes ← entries.mapM (fun entry => decodeRaider entry world)
    let kinds := foes.map (·.kind)
    if (number == 1 && kinds.contains .brute) ||
        (#[RaidFoeKind.runner, .slinger, .brute]).any (fun kind =>
          (kinds.filter (· == kind)).size > 1) then
      throw "Invalid composition of an Amber Raid wave."
    return .active { number, countdown, foes }
  | "reward" => do
    let position ← decodePosition (← json.getObjVal? "position")
    if !inRaidRewardRange position || (world.tile position).solid then
      throw "The Raid Crest must be in an open tile in Amber Bluffs."
    return .reward position
  | "cleared" => pure .cleared
  | _ => throw "Unknown Amber Raid encounter state."

private def decodeCaveSite (json : Json) (world : World) (habitat : CaveHabitat) :
    Except String CaveSite := do
  match ← readField (α := String) json "state" with
  | "waiting" =>
    let cooldown : Nat ← readField json "cooldown"
    if cooldown > caveRespawnTicks then throw "Invalid cave resident respawn timer."
    return .waiting cooldown
  | "alive" =>
    let tag : String ← readField json "kind"
    let kind ← match tag with
      | "slime" => pure CaveMobKind.slime
      | "beetle" => pure .beetle
      | "skitter" => pure .skitter
      | "oreback" => pure .oreback
      | _ => throw "Unknown cave resident kind."
    if kind != habitat.kind then throw "The cave resident does not match this habitat."
    let body ← decodePlayer (← json.getObjVal? "body") world kind.maxSpeed
    let health : Nat ← readField json "health"
    let hopIn : Nat ← readField json "hopIn"
    if !habitat.contains body.centerTile || health == 0 || health > kind.maxHealth || hopIn > kind.maxHopIn then
      throw "Invalid cave resident position, health or hop timer."
    return .alive { body, kind, health, hopIn }
  | "loot" =>
    let position ← decodePosition (← json.getObjVal? "position")
    if !habitat.contains position || (world.tile position).solid then
      throw "Cave resident loot is outside its open habitat."
    return .loot position
  | _ => throw "Unknown cave site state."

private def decodeCaveEcology (json : Json) (world : World) (seed : UInt64) (saveVersion : Nat) :
    Except String CaveEcology := do
  let entries ← json.getArr?
  let initial := initialCaveEcology seed
  let count := if saveVersion < 20 then 3 else initial.habitats.size
  if entries.size != count then throw "Wrong number of cave habitats."
  let sites ← (entries.zip initial.habitats).mapM fun (entry, habitat) =>
    decodeCaveSite entry world habitat
  return { initial with sites := sites ++ initial.sites.extract count initial.sites.size }

private def decodeFishingItem (code saveVersion : Nat) (valid : Item → Bool) : Except String Item := do
  let maxCode := if saveVersion < 57 then Item.goldWorm.code.toNat
    else if saveVersion < 58 then Item.manaPotion.code.toNat
    else if saveVersion < 59 then Item.mudBlock.code.toNat
    else if saveVersion < 60 then Item.theDuke.code.toNat
    else if saveVersion < 61 then Item.itsScragglinTime.code.toNat
    else Item.crispyHoneyBlock.code.toNat
  let some item := if code ≤ maxCode then Item.ofCode? code.toUInt16 else none
    | throw "Unknown fishing item."
  if !valid item then throw "Invalid fishing item."
  return item

private def decodeFishing (json : Json) (world : World) (water : Water) (player : Player)
    (inventory : Inventory) (held : Option Stack) (legacyEarly : Bool) (saveVersion : Nat) :
    Except String FishingState := do
  let state : String ← readField json "state"
  if state == "idle" then return .idle
  if !held.any (·.item.isFishingPole) then throw "A cast needs a selected fishing pole."
  let maxBite := if saveVersion < 59 then 350 else fishingMaxBite
  if state == "bobber" then
    if saveVersion < 55 || legacyEarly then throw "Invalid Classic fishing projectile."
    let bits := decodeFloat32Bits json "Fishing bobber"
    let motion : FishingBobber := {
      x := ← bits "xBits", y := ← bits "yBits", vx := ← bits "vxBits", vy := ← bits "vyBits",
      rotation := ← bits "rotationBits", wet := ← readField json "wet"
      reelTicks := ← readField json "reelTicks", slot := ← readField json "slot" }
    let speedLimit : Float32 := max 16 ((held.map (·.item.fishingCastSpeed)).getD 0 + 1)
    if !motion.inWorld world || motion.vx.abs > speedLimit || motion.vy.abs > speedLimit ||
        motion.rotation < -1.58 || motion.rotation > 4.72 || motion.slot ≥ maxProjectileSlots ||
        motion.reelTicks > 100 then
      throw "Invalid fishing bobber motion."
    let phase : String ← readField json "phase"
    let phase ← match phase with
      | "drifting" => do
        let ticks : Nat ← readField json "ticks"
        if ticks ≥ fishingCheckClock || motion.reelTicks != 0 then throw "Invalid fishing bobber clock."
        pure (.drifting ticks : BobberPhase)
      | "hooked" => do
        let ticks : Nat ← readField json "ticks"
        if ticks == 0 || ticks > maxBite || motion.reelTicks != 0 then throw "Invalid fishing bite timer."
        let fish ← decodeFishingItem (← readField json "catch") saveVersion Item.isFishingCatch
        let baitItem ← decodeFishingItem (← readField json "bait") saveVersion Item.isFishingBait
        pure (.hooked ticks fish baitItem)
      | "returning" => do
        let code : Option Nat ← readField json "catch"
        pure (.returning (← code.mapM fun item => decodeFishingItem item saveVersion Item.isFishingCatch))
      | _ => throw "Unknown fishing bobber phase."
    return .bobber motion phase
  let position ← decodePosition (← json.getObjVal? "position")
  let ticks : Nat ← readField json "ticks"
  if state == "soaking" || state == "hooked" then
    if saveVersion < 54 || legacyEarly || !world.contains position || (world.tile position).solid then
      throw "Invalid Classic fishing float."
    if state == "soaking" then
      if ticks ≥ fishingCheckClock then throw "Invalid Classic fishing clock."
      return .soaking position ticks
    let fish ← decodeFishingItem (← readField json "catch") saveVersion Item.isFishingCatch
    let baitItem ← decodeFishingItem (← readField json "bait") saveVersion Item.isFishingBait
    if ticks == 0 || ticks > maxBite then
      throw "Invalid Classic fishing catch."
    return .hooked position ticks fish baitItem
  let biting ← match state with
    | "waiting" => pure false
    | "biting" => pure true
    | _ => throw "Unknown fishing state."
  let limit := if biting then fishingBiteTicks else fishingWaitTicks
  if ticks == 0 || ticks > limit then throw "Invalid fishing timer."
  let hasBait := (inventory.extract 0 Inventory.trashSlot).any (fun slot => slot.any (·.item.isFishingBait))
  if !hasBait then
    throw "A cast needs a selected Fishing Rod and bait."
  if legacyFishingSpot world water player position != some position then
    throw "The fishing float is outside a valid water surface."
  return if biting then .biting position ticks else .waiting position ticks

private def decodeCrops (json : Json) (world : World) : Except String (Array Crop) := do
  let entries ← json.getArr?
  let count := world.tiles.data.foldl (fun count code =>
    if code == Tile.sunleafSprout.code then count + 1 else count) (0 : Nat)
  if entries.size != count then throw "Crop records do not match the number of Sunleaf sprouts."
  let mut seen : Std.HashSet TilePos := {}
  let mut crops := #[]
  for entry in entries do
    let position ← decodePosition (← entry.getObjVal? "position")
    let floor := { position with y := position.y + 1 }
    if world.tile? position != some .sunleafSprout || world.tile? floor != some .dirt ||
        seen.contains position then
      throw "A crop must have a unique Sunleaf sprout directly above dirt."
    let ticks : Nat ← readField entry "ticks"
    if ticks ≥ cropGrowthTicks then throw "A Sunleaf sprout has invalid growth progress."
    seen := seen.insert position
    crops := crops.push { position, ticks }
  return crops

private def decodeCarts (json : Json) (world : World) : Except String (Array TilePos) := do
  let entries ← json.getArr?
  if entries.size > maxMinecarts then throw "Too many deployed minecarts."
  let mut positions := #[]
  for entry in entries do
    let position ← decodePosition entry
    if positions.contains position || !canParkCart world position then
      throw "A parked minecart must occupy a unique clear rail position."
    positions := positions.push position
  return positions

private def decodeRide (json : Json) (world : World) (player : Player) (parked : Array TilePos) :
    Except String (Option RailRide) := do
  if json == .null then return none
  if parked.size ≥ maxMinecarts then throw "Too many deployed minecarts."
  let rail ← decodePosition (← json.getObjVal? "rail")
  let next ← match ← json.getObjVal? "next" with
    | .null => pure none
    | position => some <$> decodePosition position
  let progress ← readField json "progress"
  let speed ← readField json "speed"
  let ride : RailRide := { rail, next, progress, speed }
  if !ride.valid world parked || player != ride.body then
    throw "The minecart ride is invalid or does not match the player position."
  return some ride

private def decodeHookKind (json : Json) : Except String HookKind := do
  match ← readField (α := String) json "kind" with
  | "basic" => pure .basic
  | "dissonance" => pure .dissonance
  | name => throw s!"Unknown hook kind '{name}'."

private def decodeHookAnchor (json : Json) (saveVersion : Nat) : Except String HookAnchor := do
  let tile ← decodePosition (← json.getObjVal? "tile")
  let x ← readField json "x"
  let y ← readField json "y"
  let kind ← if saveVersion < 63 then pure .basic else decodeHookKind json
  let life ← if saveVersion < 63 then pure 0 else readField json "life"
  return { tile, x, y, kind, life }

private def decodeGrapple (json : Json) (saveVersion : Nat) : Except String GrappleState := do
  match ← readField (α := String) json "state" with
  | "idle" => pure .idle
  | "flying" => do
    let x ← readField json "x"
    let y ← readField json "y"
    let vx ← readField json "vx"
    let vy ← readField json "vy"
    let life ← readField json "life"
    let kind ← if saveVersion < 63 then pure .basic else decodeHookKind json
    let returning ← if saveVersion < 63 then pure false else readField json "returning"
    let held ← if saveVersion < 63 then pure none else
      match ← json.getObjVal? "held" with
      | .null => pure none
      | anchor => some <$> decodeHookAnchor anchor saveVersion
    return .flying { x, y, vx, vy, life, kind, returning, held }
  | "latched" => .latched <$> decodeHookAnchor json saveVersion
  | value => throw s!"Unknown grappling hook state '{value}'."

private def retainsBodyTile (body : Player) (position : TilePos) (current replacement : Tile) : Bool :=
  if replacement.solid then
    body.overlapsTile position
  else
    -- Keep the floor beneath a grounded body when the new cavern would excavate it.
    current.solid && body.grounded && body.y + playerHeight == position.y * tileUnits &&
      body.overlapsTile { position with y := position.y - 1 }

private def adjacentTiles (position : TilePos) : List TilePos :=
  [⟨position.x - 1, position.y⟩, ⟨position.x + 1, position.y⟩,
    ⟨position.x, position.y - 1⟩, ⟨position.x, position.y + 1⟩]

private def hasTorchSupport (world : World) (position : TilePos) : Bool :=
  (adjacentTiles position).any fun neighbor =>
    (world.tile? neighbor).any fun tile => tile.solid && !tile.isDoor

private def keepTorchSupport (before after : World) (position : TilePos) : World :=
  if hasTorchSupport after position then after
  else
    match (adjacentTiles position).find? (fun neighbor =>
        (before.tile? neighbor).any fun tile => tile.solid && !tile.isDoor) with
    | some support => after.setTile support (before.tile support)
    | none => after

/-- Add the terrain introduced since this save while preserving local edits and occupied tiles. -/
private def migrateTerrain (saveVersion : Nat) (seed : UInt64) (world generated : World)
    (bodies : List Player) (homeDoor : Option TilePos) (water : Option Water)
    (projectiles : Array Projectile) : World := Id.run do
  let baseline := if saveVersion < 3 then generateLegacyTerrain seed
    else if saveVersion == 3 then generateV3Terrain seed
    else if saveVersion == 4 then generateV4Terrain seed
    else if saveVersion == 5 then generateV5Terrain seed
    else if saveVersion == 6 then generateV6Terrain seed
    else generateV7Terrain seed
  let home := (homeDoor.map (homeFootprint world)).getD {}
  let shotTiles := projectiles.foldl
    (fun (seen : Std.HashSet TilePos) shot => seen.insert ⟨shot.x / tileUnits, shot.y / tileUnits⟩) {}
  let mut tiles := ByteArray.emptyWithCapacity (generated.width * generated.height)
  for y in [:world.height] do
    for x in [:generated.width] do
      let position : TilePos := ⟨x, y⟩
      let replacement := generated.tile position
      if x ≥ world.width then
        tiles := tiles.push replacement.code
        continue
      let legacyTile := baseline.tile position
      let current := world.tile position
      -- Version 1 generated air where version 2 added timber.
      let original := if saveVersion == 1 && legacyTile == .timber then .air else legacyTile
      let door := { position with y := position.y - 1 }
      let supportsDoor := current.solid && !replacement.solid && doorBase world door == some door
      let wouldFillWater := replacement.solid && water.any (fun fluid => fluid.level world position > 0)
      let wouldBlockShot := replacement.solid && shotTiles.contains position
      let tile := if legacyTile != replacement && current == original && !supportsDoor &&
          !home.contains position && !wouldFillWater && !wouldBlockShot &&
          !bodies.any (fun body => retainsBodyTile body position current replacement) then replacement
        else current
      tiles := tiles.push tile.code
  let mut migrated := { generated with tiles }
  for y in [:world.height] do
    for x in [:world.width] do
      let position : TilePos := ⟨x, y⟩
      if world.tile position == .torch then
        migrated := keepTorchSupport world migrated position
  -- Old excavations may remove the intended support of newly generated torches.
  for y in [:migrated.height] do
    for x in [:migrated.width] do
      let position : TilePos := ⟨x, y⟩
      if migrated.tile position == .torch && world.tile position != .torch &&
          !hasTorchSupport migrated position then
        migrated := migrated.setTile position .air
  -- An old player or building may already occupy the generated altar site.
  if !migrated.tiles.data.any (· == Tile.stormAltar.code) then
    for x in [stormArenaStart + 1:stormArenaEnd] do
      let position : TilePos := ⟨x, stormArenaFloor - 1⟩
      let floor : TilePos := ⟨x, stormArenaFloor⟩
      if migrated.tile position == .air && (migrated.tile floor).solid &&
          world.tile position == baseline.tile position && !home.contains position &&
          !shotTiles.contains position &&
          !water.any (fun fluid => fluid.level world position > 0) &&
          !bodies.any (fun body => body.overlapsTile position) then
        migrated := migrated.setTile position .stormAltar
        break
  return migrated

private def validateLandmarks (cells : ByteArray) (width : Nat) (pockets : Array SupplyPocket)
    (crystalSites : Array LifeCrystalSite) (berryPositions : Array TilePos) (mine : DeepsteelMine)
    (cavernAltar : TilePos)
    (codeOffset : UInt8 := 0) : Except String Unit := do
  let centers := pockets.map (·.cache)
  let crystalPositions := crystalSites.map (·.position)
  let closed := Tile.supplyCache.code + codeOffset
  let opened := Tile.openedCache.code + codeOffset
  let altar := Tile.ruinAltar.code + codeOffset
  let crystal := Tile.lifeCrystal.code + codeOffset
  let berry := Tile.highlandBerry.code + codeOffset
  let ore := Tile.deepsteel.code + codeOffset
  let marker := Tile.mineMarker.code + codeOffset
  let colossusAltar := Tile.cavernAltar.code + codeOffset
  let galleryMarker := Tile.magmaMarker.code + codeOffset
  let altarIndex := ruinAltar.y.toNat * width + ruinAltar.x.toNat
  let markerIndex := mine.marker.y.toNat * width + mine.marker.x.toNat
  let colossusAltarIndex := cavernAltar.y.toNat * width + cavernAltar.x.toNat
  let galleryMarkerIndex := magmaMarker.y.toNat * width + magmaMarker.x.toNat
  for index in [:cells.size] do
    let code := cells[index]!
    if code == closed || code == opened then
      let position : TilePos := ⟨index % width, index / width⟩
      if !centers.contains position then throw "A supply cache is outside its generated pocket."
    if code == altar && index != altarIndex then throw "The ruin altar is outside its generated site."
    if code == crystal then
      let position : TilePos := ⟨index % width, index / width⟩
      if !crystalPositions.contains position then throw "A Life Crystal is outside its generated site."
    if code == berry then
      let position : TilePos := ⟨index % width, index / width⟩
      if !berryPositions.contains position then throw "A Highland Berry bush is outside its generated site."
    if code == ore then
      let position : TilePos := ⟨index % width, index / width⟩
      if !mine.ore.contains position then throw "A Deepsteel deposit is outside its generated vein."
    if code == marker && index != markerIndex then
      throw "The Deepsteel Mine marker is outside its generated site."
    if code == colossusAltar && index != colossusAltarIndex then
      throw "The Deepstone Cavern altar is outside its generated site."
    if code == galleryMarker && index != galleryMarkerIndex then
      throw "The Magma Gallery marker is outside its generated site."

private def validateEyeAltarCells (cells : ByteArray) (width : Nat) (sites : Array TilePos)
    (tile : Tile) (offset : UInt8 := 0) : Except String Unit := do
  let allowed := sites.flatMap altarCells
  for index in [:cells.size] do
    let code := cells[index]!
    if code == Tile.demonAltar.code + offset || code == Tile.crimsonAltar.code + offset then
      let position : TilePos := ⟨index % width, index / width⟩
      if code != tile.code + offset || !allowed.contains position then
        throw "An evil altar is outside its generated site or has the wrong type."

private def validateEvilObjectCells (cells : ByteArray) (width : Nat) (relics pots : Array TilePos)
    (relic : Tile) (offset : UInt8 := 0) : Except String Unit := do
  let relicCells := relics.flatMap evilRelicCells
  let potCells := pots.flatMap evilPotCells
  for index in [:cells.size] do
    let code := cells[index]!
    if code == Tile.shadowOrb.code + offset || code == Tile.crimsonHeart.code + offset then
      let position : TilePos := ⟨index % width, index / width⟩
      if code != relic.code + offset || !relicCells.contains position then
        throw "An evil relic is outside its generated site or has the wrong type."
    if code == Tile.clayPot.code + offset then
      let position : TilePos := ⟨index % width, index / width⟩
      if !potCells.contains position then throw "A clay pot is outside its generated site."

private def dungeonKeySites (layout : DungeonLayout) (deepInstalled : Bool) : Array TilePos :=
  #[layout.keyChest] ++ (if deepInstalled then #[layout.deepKeyChest] else #[])

private def dungeonGoldSites (layout : DungeonLayout)
    (lowerInstalled blueMoonInstalled magicMissileInstalled deepInstalled : Bool) : Array TilePos :=
  #[layout.goldChest] ++ (if lowerInstalled then #[layout.lowerGoldChest] else #[]) ++
    (if blueMoonInstalled then #[layout.blueMoonChest] else #[]) ++
    (if magicMissileInstalled then #[layout.magicMissileChest] else #[]) ++
    (if deepInstalled then #[layout.valorChest, layout.handgunChest] else #[])

private def validateDungeonChestCells (cells : ByteArray) (width : Nat) (layout : DungeonLayout)
    (installed lowerInstalled blueMoonInstalled magicMissileInstalled deepInstalled : Bool) (offset : UInt8 := 0) :
    Except String Unit := do
  let wooden := (dungeonKeySites layout deepInstalled).flatMap dungeonChestCells
  let gold := (dungeonGoldSites layout lowerInstalled blueMoonInstalled magicMissileInstalled deepInstalled).flatMap
    dungeonChestCells
  for index in [:cells.size] do
    let code := cells[index]!
    if code == Tile.dungeonKeyChest.code + offset || code == Tile.dungeonGoldChest.code + offset ||
        code == Tile.dungeonOpenChest.code + offset then
      let position : TilePos := ⟨index % width, index / width⟩
      let footprint := if code == Tile.dungeonKeyChest.code + offset then wooden else gold
      if !installed || !footprint.contains position then
        throw "A dungeon chest tile is outside its generated site."

private def dungeonChestSites (world : World) (layout : DungeonLayout)
    (installed lowerInstalled blueMoonInstalled magicMissileInstalled deepInstalled : Bool) :
    Except String (Array TilePos) := do
  validateDungeonChestCells world.tiles world.width layout installed lowerInstalled blueMoonInstalled
    magicMissileInstalled deepInstalled
  if !installed then return #[]
  let keySites := dungeonKeySites layout deepInstalled
  for anchor in keySites do
    if !(dungeonChestCells anchor).all (fun position => world.tile position == .dungeonKeyChest) then
      throw "A dungeon chest is missing part of its footprint."
  let goldSites := dungeonGoldSites layout lowerInstalled blueMoonInstalled magicMissileInstalled deepInstalled
  for anchor in goldSites do
    let tile := world.tile anchor
    if (tile != .dungeonGoldChest && tile != .dungeonOpenChest) ||
        !(dungeonChestCells anchor).all (fun position => world.tile position == tile) then
      throw "A dungeon chest is missing part of its footprint."
  return keySites ++ goldSites

private def validateShadowChestCells (cells : ByteArray) (width : Nat) (sites : Array TilePos)
    (allowShadow : Bool) : Except String Unit := do
  let footprint := sites.flatMap dungeonChestCells
  for index in [:cells.size] do
    let code := cells[index]!
    if code == Tile.shadowChest.code + 1 || code == Tile.shadowOpenChest.code + 1 then
      let position : TilePos := ⟨index % width, index / width⟩
      if !allowShadow || (code == Tile.shadowChest.code + 1 && !footprint.contains position) then
        throw "A Shadow Chest observation is outside its generated site."

private def deliverMigratedShadowKey (anchor : TilePos) (chests : Array Chest) (inventory : Inventory)
    (cursor : Option Stack) (cursorIdle : Bool) : Array Chest × Inventory × Option Stack × Bool := Id.run do
  let key : Stack := ⟨.shadowKey, 1⟩
  if let some index := chests.findIdx? (·.position == anchor) then
    if let some chest := chests[index]? then
      if let some slot := chest.slots.findIdx? Option.isNone then
        let updated := { chest with slots := chest.slots.set! slot (some key) }
        return (chests.set! index updated, inventory, cursor, false)
  if let some (inventory, _) := Inventory.addOne inventory .shadowKey then
    return (chests, inventory, cursor, false)
  if cursor.isNone && cursorIdle then return (chests, inventory, some key, false)
  return (chests, inventory, cursor, true)

private def countWholeSites (world : World) (sites : Array TilePos) (cellsAt : TilePos → Array TilePos)
    (tile : Tile) (description : String) : Except String Nat := do
  let mut count := 0
  for site in sites do
    let cells := cellsAt site
    if cells.any (fun position => world.tile position == tile) then
      if !cells.all (fun position => world.tile position == tile) then
        throw s!"{description} is missing part of its footprint."
      count := count + 1
  return count

private def nearBeacon (world : World) (position : TilePos) : Bool :=
  world.tile position == .homeBeacon ||
    world.tile { position with y := position.y - 1 } == .homeBeacon ||
    world.tile { position with y := position.y + 1 } == .homeBeacon

private def pristineFootprint (world baseline : World) (water : Water) (bodies : List Player)
    (rooms : List (Std.HashSet TilePos)) (fixedTiles : List TilePos) (footprint : Array TilePos) : Bool :=
  footprint.all fun position =>
    world.contains position && world.tile? position == baseline.tile? position &&
      water.level world position == 0 && !nearBeacon world position &&
      !rooms.any (·.contains position) && !fixedTiles.contains position &&
      !bodies.any (·.overlapsTile position)

/-- A pocket is a single terrain edit. Preserve the whole original area if any
of its cells are edited, flooded, occupied or part of a house or beacon. -/
private def retrofitSupplyPockets (world baseline : World) (pockets : Array SupplyPocket)
    (water : Water) (bodies : List Player) (homes fixedTiles : List TilePos) : World := Id.run do
  let rooms := homes.map (homeFootprint world)
  let mut migrated := world
  for pocket in pockets do
    if pristineFootprint world baseline water bodies rooms fixedTiles pocket.footprint then
      for (position, tile) in pocket.edits do
        migrated := migrated.setTile position tile
  return migrated

/-- Install the pond's terrain and water together without disturbing a used site. -/
private def retrofitPond (seed : UInt64) (world : World) (water : Water) (bodies : List Player)
    (homes fixedTiles : List TilePos) : World × Water := Id.run do
  let baseline := generatePreFishingTerrain seed
  let footprint := pondFootprint seed
  let rooms := homes.map (homeFootprint world)
  if !pristineFootprint world baseline water bodies rooms fixedTiles footprint then
    return (world, water)
  let terrain := generatePreGardenTerrain seed
  let world := footprint.foldl (fun updated position => updated.setTile position (terrain.tile position)) world
  let filled := Water.initial world (seed := seed)
  let mut levels := water.levels
  for position in footprint do
    if surfacePondContains seed position then
      let index := position.y.toNat * world.width + position.x.toNat
      levels := levels.set! index (filled.level world position).toUInt8
  return (world, { levels })

private def retrofitSunleaf (seed : UInt64) (world : World) (water : Water) (bodies : List Player)
    (homes fixedTiles : List TilePos) : World := Id.run do
  let baseline := generatePreGardenTerrain seed
  let rooms := homes.map (homeFootprint world)
  let mut updated := world
  for position in wildSunleaf seed do
    let floor := { position with y := position.y + 1 }
    if pristineFootprint world baseline water bodies rooms fixedTiles #[position, floor] then
      updated := updated.setTile position .sunleaf
  return updated

private def retainsFishing (world : World) (water : Water) (player : Player) : FishingState → Bool
  | .idle | .soaking .. | .hooked .. | .bobber .. => true
  | .waiting position _ | .biting position _ => legacyFishingSpot world water player position == some position

/-- Install the ruins as one edit, preserving the original terrain when it is in use. -/
private def retrofitRuins (seed : UInt64) (world : World) (water : Water) (bodies : List Player)
    (homes fixedTiles : List TilePos) (player : Player) (fishing : FishingState) : World :=
  let baseline := generatePreRuinsTerrain seed
  let rooms := homes.map (homeFootprint world)
  if !pristineFootprint world baseline water bodies rooms fixedTiles ruinFootprint then world
  else
    let updated := ruinEdits.foldl (fun terrain (position, tile) => terrain.setTile position tile) world
    if retainsFishing updated water player fishing then updated else world

/-- Preserve each used site independently when adding Life Crystals to an older world. -/
private def retrofitLifeCrystals (seed : UInt64) (world : World) (water : Water) (bodies : List Player)
    (homes fixedTiles : List TilePos) (sites : Array LifeCrystalSite) : World :=
  let baseline := generatePreLifeCrystalTerrain seed
  let rooms := homes.map (homeFootprint world)
  sites.foldl (fun updated site =>
    if pristineFootprint world baseline water bodies rooms fixedTiles site.footprint then
      updated.setTile site.position .lifeCrystal
    else updated) world

private def inFlightTerrainContacts (world : World) (projectiles : Array Projectile)
    (grapple : GrappleState) : List TilePos :=
  let hits := projectiles.toList.filterMap fun shot =>
    (terrainHit world shot.x shot.y shot.vx shot.vy).map TerrainHit.tile
  match grapple with
  | .flying hook =>
    let contact := if hook.kind == .basic then
        (terrainHit world hook.x hook.y hook.vx hook.vy).map TerrainHit.tile
      else if hook.returning then none else hook.dissonanceContact world
    contact.toList ++ (hook.held.map (·.tile)).toList ++ hits
  | _ => hits

private def aquaTerrainFrame (stream : AquaStream) (world : World) (magma : Water) : Option AquaStream :=
  (List.range (if stream.steam then 1 else 3)).foldl
    (fun active _ => active.bind (fun shot => shot.advance world magma)) (some stream)

private def handgunTerrainFrame (shot : Projectile) (world : World) (water magma : Water) : Option Projectile :=
  (List.range 2).foldl (fun active _ => active.bind fun shot =>
    shot.advanceHandgun world (water.submerged world shot.x shot.y || magma.submerged world shot.x shot.y)) (some shot)

private def expandProjectileBox (box : HitBox) (pixels : Int) : HitBox :=
  let margin := pixels * (tileUnits / 16)
  ⟨box.left - margin, box.top - margin, box.right + margin, box.bottom + margin⟩

private def intactUnderworldHouse (world baseline : World) (tower : UnderworldTower) : Bool := Id.run do
  for column in [tower.left:tower.right + 1] do
    for row in [tower.roof:tower.floor + 2] do
      if column == tower.left || column == tower.right || row == tower.roof || row ≥ tower.floor then
        let position : TilePos := ⟨column, row⟩
        if baseline.tile position == .obsidianBrick && world.tile position != .obsidianBrick then
          return false
  return true

/-- Keep the old east wall where a room, attachment or active collision depends on it. -/
private def preserveHighlandsBoundary (world : World) (water : Water) (homes : List TilePos)
    (bodies : List Player) (projectiles : Array Projectile) (grapple : GrappleState) : TilePos → Bool :=
  let rooms := homes.map (homeFootprint world)
  let hits := inFlightTerrainContacts world projectiles grapple
  let fixed := match grapple with
    | .latched anchor => anchor.tile :: hits
    | _ => hits
  fun position =>
    let above := { position with y := position.y - 1 }
    let west := { position with x := position.x - 1 }
    let supportsTorch := world.tile west == .torch && !(adjacentTiles west).any (fun neighbor =>
      neighbor != position && (world.tile? neighbor).any (fun tile => tile.solid && !tile.isDoor))
    fixed.contains position ||
      rooms.any (fun room => (position :: adjacentTiles position).any room.contains) ||
      bodies.any (fun body => retainsBodyTile body position .bedrock .air ||
        (body.x + playerWidth == position.x * tileUnits && body.overlapsTile west)) ||
      doorBase world above == some above || world.tile above == .rail || world.tile above == .homeBeacon ||
      supportsTorch || water.level world west > 0

/-- Preserve cell coordinates while inserting unknown or dry cells at the end of each row. -/
private def widenCells (cells : ByteArray) (before after : World) : ByteArray := Id.run do
  let mut widened := ByteArray.emptyWithCapacity (after.width * after.height)
  for y in [:before.height] do
    let start := y * before.width
    for x in [:before.width] do
      widened := widened.push cells[start + x]!
    for _ in [before.width:after.width] do
      widened := widened.push 0
  return widened

/-- Retrofit a whole site together when its terrain and active uses are undisturbed. -/
private def retrofitUndisturbedSite (world baseline : World) (water : Water) (bodies : List Player)
    (homes fixedTiles : List TilePos) (footprint : Array TilePos) (edits : Array (TilePos × Tile))
    (keepsActive : World → Bool) : World :=
  let rooms := homes.map (homeFootprint world)
  if !pristineFootprint world baseline water bodies rooms fixedTiles footprint then world
  else
    let updated := edits.foldl (fun terrain (position, tile) => terrain.setTile position tile) world
    if keepsActive updated then updated else world

private def retrofitClassicResources (seed : UInt64) (world : World) (water magma : Water)
    (bodies : List Player) (homes fixedTiles : List TilePos) : World :=
  let baseline := generatePreClassicTerrain seed
  let rooms := homes.map (homeFootprint world)
  let offsets : Array Int := #[-1, 0, 1]
  (classicResources seed).foldl (fun updated (position, tile) =>
    let footprint := offsets.flatMap fun dy =>
      offsets.map fun dx => (⟨position.x + dx, position.y + dy⟩ : TilePos)
    if pristineFootprint world baseline water bodies rooms fixedTiles footprint &&
        footprint.all (fun cell => magma.level world cell == 0) then
      updated.setTile position tile
    else updated) world

private def retrofitTemple (state : GameState) (saveVersion : Nat) (bodies : List Player) : GameState :=
  if saveVersion ≥ 72 || state.legacyEarly then state else
  let world := state.world
  let baseline := (generateWorld state.seed).1
  let available (position : TilePos) :=
    state.water.level world position == 0 && state.magma.level world position == 0 &&
      state.honey.level world position == 0 && !bodies.any (·.overlapsTile position) &&
      !state.tileHits.any (·.position == position) && !state.nightDrops.any (·.position == position)
  let temple := if saveVersion < 71 then
      TempleLayout.install world state.walls state.seed fun p =>
        world.tile p == baseline.tile p && (state.chart.tile? world p).isNone && available p
    else (TempleLayout.findLegacy world).map (·, world, state.walls)
  match temple with
  | some (layout, terrain, walls) =>
    let footprint := layout.footprint
    let canEdit (p : TilePos) :=
      let wall := walls[p.y.toNat * world.width + p.x.toNat]?.getD 0
      available p && (wall == 0 || wall == TempleLayout.wallCode) &&
        (terrain.tile p == baseline.tile p || (terrain.tile p == .lihzahrdBrick && footprint.contains p))
    let (terrain, walls) := (layout.installGolemArena terrain walls canEdit).getD (terrain, walls)
    let (world, templeWires) := TempleWiring.install layout terrain available
    let chests := if saveVersion < 71 then state.chests ++ initialTempleChests state.seed layout else state.chests
    { state with world, walls, chests, templeWires }
  | none => state

set_option maxRecDepth 2048 in
set_option maxHeartbeats 800000 in
/-- Validate a save before exposing it as a playable state. -/
def decode (text : String) : Except String GameState := do
  let json ← Json.parse text
  let saveVersion : Nat ← readField json "version"
  if saveVersion == 0 || saveVersion > version then
    throw s!"Unsupported save version {saveVersion}."
  let worldName ← if saveVersion < 67 then pure "" else readField json "worldName"
  let worldCreated ← if saveVersion < 67 then pure "" else readField json "worldCreated"
  let worldFavorite ← if saveVersion < 67 then pure false else readField json "worldFavorite"
  let legacyEarly ← if saveVersion < 25 then pure true else readField json "legacyEarly"
  let hasUnderworld := saveVersion ≥ 46 && !legacyEarly
  let width : Nat ← readField json "width"
  let height : Nat ← readField json "height"
  let expectedWidth := if saveVersion < 6 then legacyWorldWidth
    else if saveVersion < 19 then preHighlandsWorldWidth else worldWidth
  let expectedHeight := if hasUnderworld then underworldHeight else worldHeight
  if width != expectedWidth || height != expectedHeight then
    throw s!"Unsupported world dimensions {width} × {height}."
  let tileArray ← (← json.getObjVal? "tiles").getArr?
  let maxTileCode := if saveVersion == 1 then 6 else if saveVersion == 2 then 8
    else if saveVersion < 5 then 10 else if saveVersion == 5 then 15
    else if saveVersion == 6 then 16 else if saveVersion == 7 then 17
    else if saveVersion < 10 then Tile.geode.code.toNat
    else if saveVersion == 10 then Tile.homeBeacon.code.toNat
    else if saveVersion < 14 then Tile.openedCache.code.toNat
    else if saveVersion == 14 then Tile.sunleafSprout.code.toNat
    else if saveVersion == 15 then Tile.ruinBrick.code.toNat
    else if saveVersion < 18 then Tile.rail.code.toNat
    else if saveVersion == 18 then Tile.lifeCrystal.code.toNat
    else if saveVersion == 19 then Tile.highlandBerry.code.toNat
    else if saveVersion == 20 then Tile.mineMarker.code.toNat
    else if saveVersion == 21 then Tile.cavernAltar.code.toNat
    else if saveVersion < 24 then Tile.magmaMarker.code.toNat
    else if saveVersion == 24 then Tile.loom.code.toNat
    else if saveVersion == 25 then Tile.leadAnvil.code.toNat
    else if saveVersion == 26 then Tile.doorOpenMiddle.code.toNat
    else if saveVersion == 27 then Tile.crimsonGrass.code.toNat
    else if saveVersion == 28 then Tile.clayPot.code.toNat
    else if saveVersion < 31 then Tile.glassBlock.code.toNat
    else if saveVersion < 46 then Tile.dungeonOpenChest.code.toNat
    else if saveVersion < 49 then Tile.hellstone.code.toNat
    else if saveVersion < 50 then Tile.hellforge.code.toNat
    else if saveVersion < 51 then Tile.crimtaneBrick.code.toNat
    else if saveVersion < 52 then Tile.titaniumForge.code.toNat
    else if saveVersion < 57 then Tile.gelatinCrystal.code.toNat
    else if saveVersion < 58 then Tile.fishingCrate.code.toNat
    else if saveVersion < 61 then Tile.jungleGrass.code.toNat
    else if saveVersion < 70 then Tile.crispyHoneyBlock.code.toNat
    else if saveVersion < 71 then Tile.planteraBulb.code.toNat
    else if saveVersion < 72 then Tile.lihzahrdDoorLeftLeaf.code.toNat
    else Tile.lihzahrdAltarLowerRight.code.toNat
  let tiles ← decodeTileBytes tileArray (width * height) maxTileCode
  let world : World := { width, height, tiles }
  if saveVersion ≥ 52 then validateCavernCrystals world
  let hardmode : Bool ← if saveVersion < 50 then pure false else readField json "hardmode"
  let altarsSmashed : Nat ← if saveVersion < 51 then pure 0 else readField json "altarsSmashed"
  let hardmodeOreMask : Nat ← if saveVersion < 51 then pure 0 else readField json "hardmodeOreMask"
  if (altarsSmashed > 0 && !hardmode) || hardmodeOreMask ≥ 2 ^ min altarsSmashed 3 then
    throw "Hardmode ore choices do not match the altar progress."
  let equipment ← if saveVersion < 9 then pure {}
    else do decodeEquipment (← json.getObjVal? "equipment") saveVersion
  let wingedSlime ← if saveVersion < 65 then pure none
    else decodeWingedSlime (← json.getObjVal? "wingedSlime")
  let assassinDash ← if saveVersion < 64 then pure {}
    else decodeAssassinDash (← json.getObjVal? "assassinDash") equipment
  let weaponArmorStats ← if saveVersion < 64 then pure #[]
    else decodeWeaponArmorStats (← json.getObjVal? "weaponArmorStats")
  let tongued : Bool ← if saveVersion < 50 then pure false else readField json "tongued"
  let maxPlayerVx := if saveVersion < 34 then equipment.speed else max equipment.speed npcContactKnockbackX
  let maxPlayerVx := if equipment.crystalAssassinSet then max maxPlayerVx 270 else maxPlayerVx
  let maxPlayerVx := if wingedSlime.isSome then wingedSlimeMaxX else maxPlayerVx
  let player ← decodePlayer (← json.getObjVal? "player") world maxPlayerVx
    (allowTerrainOverlap := tongued) (maxVy := if wingedSlime.isSome then wingedSlimeMaxY else maxVerticalSpeed)
  let maxItemCode := if saveVersion == 1 then 6 else if saveVersion == 2 then 9
    else if saveVersion == 3 then 13 else if saveVersion == 4 then 14
    else if saveVersion == 5 then 17 else if saveVersion == 6 then 19
    else if saveVersion == 7 then 22 else if saveVersion == 8 then 25
    else if saveVersion == 9 then Item.raidCrest.code.toNat
    else if saveVersion < 12 then Item.recallCharm.code.toNat
    else if saveVersion == 12 then Item.hunterBow.code.toNat
    else if saveVersion == 13 then Item.caveEel.code.toNat
    else if saveVersion == 14 then Item.barkskinTonic.code.toNat
    else if saveVersion == 15 then Item.ruinHammer.code.toNat
    else if saveVersion == 16 then Item.minecart.code.toNat
    else if saveVersion == 17 then Item.grapplingHook.code.toNat
    else if saveVersion == 18 then Item.lifeCrystal.code.toNat
    else if saveVersion == 19 then Item.highlandBerry.code.toNat
    else if saveVersion == 20 then Item.highlandCrossbow.code.toNat
    else if saveVersion == 21 then Item.deepstoneAegis.code.toNat
    else if saveVersion < 24 then Item.obsidian.code.toNat
    else if saveVersion == 24 then Item.woodHammer.code.toNat
    else if saveVersion == 25 then Item.platinumAxe.code.toNat
    else if saveVersion == 26 then Item.zombieArm.code.toNat
    else if saveVersion == 27 then Item.heart.code.toNat
    else if saveVersion == 28 then Item.rottedFork.code.toNat
    else if saveVersion == 29 then Item.bottle.code.toNat
    else if saveVersion == 30 then Item.platinumCoin.code.toNat
    else if saveVersion < 36 then Item.bookOfSkulls.code.toNat
    else if saveVersion < 38 then Item.bone.code.toNat
    else if saveVersion < 39 then Item.nazar.code.toNat
    else if saveVersion < 43 then Item.aquaScepter.code.toNat
    else if saveVersion < 44 then Item.blueMoon.code.toNat
    else if saveVersion < 45 then Item.magicMissile.code.toNat
    else if saveVersion < 46 then Item.handgun.code.toNat
    else if saveVersion < 47 then Item.obsidianSkull.code.toNat
    else if saveVersion < 48 then Item.hellwingBow.code.toNat
    else if saveVersion < 49 then Item.batBat.code.toNat
    else if saveVersion < 50 then Item.goldenBugNet.code.toNat
    else if saveVersion < 51 then Item.wallOfFleshTrophy.code.toNat
    else if saveVersion < 52 then Item.trifoldMap.code.toNat
    else if saveVersion < 53 then Item.monsterMeat.code.toNat
    else if saveVersion < 54 then Item.spazmatismTrophy.code.toNat
    else if saveVersion < 57 then Item.goldWorm.code.toNat
    else if saveVersion < 58 then Item.manaPotion.code.toNat
    else if saveVersion < 59 then Item.mudBlock.code.toNat
    else if saveVersion < 60 then Item.theDuke.code.toNat
    else if saveVersion < 61 then Item.itsScragglinTime.code.toNat
    else if saveVersion < 62 then Item.crispyHoneyBlock.code.toNat
    else if saveVersion < 68 then Item.queenSlimeTrophy.code.toNat
    else if saveVersion < 69 then Item.destroyerTrophy.code.toNat
    else if saveVersion < 70 then Item.skeletronPrimeTrophy.code.toNat
    else if saveVersion < 71 then Item.planteraTrophy.code.toNat
    else if saveVersion < 72 then Item.titanPotion.code.toNat
    else if saveVersion < 73 then Item.golemTrophy.code.toNat
    else Item.lizardEgg.code.toNat
  let inventory ← decodeInventory json saveVersion maxItemCode
  let cursorStack ← if saveVersion < 30 then pure none
    else do decodeSlot maxItemCode (← json.getObjVal? "cursorStack")
  let selected : Nat ← readField json "selected"
  let hotbarCount := if saveVersion == 1 then 6 else if saveVersion < 30 then 8 else Inventory.hotbarCount
  if selected ≥ hotbarCount then
    throw s!"Selected slot {selected} is outside the hotbar."
  let seed : UInt64 ← readField json "seed"
  let savedDungeon : Bool ← if saveVersion < 31 then pure false else readField json "dungeonInstalled"
  if savedDungeon && legacyEarly then throw "The Dungeon requires a Classic world."
  let savedLowerDungeon : Bool ← if saveVersion < 39 then pure false else readField json "lowerDungeonInstalled"
  if savedLowerDungeon && !savedDungeon then throw "The lower Dungeon requires the original Dungeon."
  let savedBlueMoonDungeon : Bool ← if saveVersion < 43 then pure false else readField json "blueMoonDungeonInstalled"
  if savedBlueMoonDungeon && !savedLowerDungeon then throw "The Blue Moon Dungeon requires the lower Dungeon."
  let savedMagicMissileDungeon : Bool ← if saveVersion < 44 then pure false else readField json "magicMissileDungeonInstalled"
  if savedMagicMissileDungeon && !savedBlueMoonDungeon then
    throw "The Magic Missile Dungeon requires the Blue Moon Dungeon."
  let savedDeepDungeon : Bool ← if saveVersion < 45 then pure false else readField json "deepDungeonInstalled"
  if savedDeepDungeon && !savedMagicMissileDungeon then
    throw "The deep Dungeon requires the Magic Missile Dungeon."
  let dungeon := dungeonLayout seed
  let dungeonChests ← if saveVersion < 31 then pure #[]
    else
      dungeonChestSites world dungeon savedDungeon savedLowerDungeon savedBlueMoonDungeon
        savedMagicMissileDungeon savedDeepDungeon
  let shadowChests := if !hasUnderworld then #[]
    else if saveVersion < 47 then (underworldLayout seed).legacyShadowChests else (underworldLayout seed).shadowChests
  let classicEntrance := saveVersion ≥ 26 && !legacyEarly
  let pockets := supplyPockets seed
  let crystalSites := if saveVersion < 24 then originalLifeCrystalSites seed else lifeCrystalSites seed
  let berryPositions := if saveVersion < 19 then #[] else highlandBerryPositions seed
  let mine := deepsteelMine seed
  let cavernLayout := highlandCavern seed
  if saveVersion ≥ 11 then
    validateLandmarks world.tiles world.width pockets crystalSites berryPositions mine cavernLayout.altar
  if world.tile cavernLayout.altar == .cavernAltar && world.tile mine.marker != .mineMarker then
    throw "The Deepstone Cavern requires its mine entrance marker."
  if world.tile magmaMarker == .magmaMarker && world.tile cavernLayout.altar != .cavernAltar then
    throw "The Magma Gallery requires the Deepstone Cavern altar."
  let combat ← if saveVersion == 1 then pure (initialCombat world)
    else do decodeCombat (← json.getObjVal? "combat") world saveVersion
  let respawnTicks : Nat ← if saveVersion < 24 then pure 0 else readField json "respawnTicks"
  if respawnTicks > classicRespawnTicks || (combat.health == 0) != (respawnTicks > 0) then
    throw "The respawn timer must match a defeated player."
  let darknessTicks : Nat ← if saveVersion < 48 then pure 0 else readField json "darknessTicks"
  if darknessTicks > maxDarknessTicks || (respawnTicks > 0 && darknessTicks > 0) then
    throw "Invalid Darkness duration."
  let obsidianSkinTicks : Nat ← if saveVersion < 49 then pure 0 else readField json "obsidianSkinTicks"
  if obsidianSkinTicks > maxObsidianSkinTicks || (respawnTicks > 0 && obsidianSkinTicks > 0) then
    throw "Invalid Obsidian Skin duration."
  let netSwingTicks : Nat ← if saveVersion < 49 then pure 0 else readField json "netSwingTicks"
  if netSwingTicks > 25 || (respawnTicks > 0 && netSwingTicks > 0) then
    throw "Invalid Bug Net swing duration."
  let hotTileTicks : Nat ← if saveVersion < 46 then pure 0 else readField json "hotTileTicks"
  let burningTicks : Nat ← if saveVersion < 46 then pure 0 else readField json "burningTicks"
  if hotTileTicks ≥ 10 || (respawnTicks > 0 && hotTileTicks > 0) then throw "Invalid hot tile contact timer."
  if burningTicks > 20 || (respawnTicks > 0 && burningTicks > 0) then throw "Invalid Burning duration."
  if respawnTicks > 0 && (combat.invulnerable > 0 || combat.swing > 0 || combat.onFire > 0 ||
      combat.lavaCooldown > 0 || combat.regenTime > 0 || combat.regenProgress > 0) then
    throw "A defeated player cannot have active combat timers."
  let warden ← if saveVersion < 4 then pure .ready
    else do decodeWarden (← json.getObjVal? "warden") world
  if saveVersion ≥ 5 then validateDoors world
  let surface ← if saveVersion < 5 then pure {}
    else do decodeSurface (← json.getObjVal? "surface") world seed player saveVersion legacyEarly
  let weather ← if saveVersion < 56 then pure (if legacyEarly then {} else initialWeather seed)
    else do decodeWeather (← json.getObjVal? "weather") legacyEarly
  let chests ← if saveVersion < 5 then pure #[]
    else do
      decodeChests (← json.getObjVal? "chests") world saveVersion maxItemCode dungeonChests shadowChests hasUnderworld
  let initialChests := if savedDungeon then initialDungeonChests seed (shadowKey := hasUnderworld) else #[]
  let initialChests := if savedLowerDungeon then initialChests.push (initialLowerDungeonChest seed) else initialChests
  let initialChests := if savedBlueMoonDungeon then initialChests.push (initialBlueMoonDungeonChest seed) else initialChests
  let initialChests := if savedMagicMissileDungeon then initialChests.push (initialMagicMissileDungeonChest seed)
    else initialChests
  let initialChests := if savedDeepDungeon then initialChests ++ initialDeepDungeonChests seed else initialChests
  for initial in initialChests do
    if world.tile initial.position == .dungeonGoldChest &&
        chests.find? (·.position == initial.position) != some initial then
      throw "A locked dungeon chest must retain its generated contents."
  if hasUnderworld then
    let retainsShadowLoot := fun (expected : Array Chest) => expected.all fun initial =>
      world.tile initial.position != .shadowChest || chests.find? (·.position == initial.position) == some initial
    let historical := initialLegacyShadowChests seed
    let migrated := if saveVersion < 47 then historical else historical ++ initialAdditionalShadowChests seed
    -- Existing worlds retain the original pair. All still-locked chests must
    -- agree on one whole loot assignment, even when some sites are absent.
    let retainsPrimary := saveVersion ≥ 47 && retainsShadowLoot (initialPrimaryShadowChests seed)
    let retainsCurrent := saveVersion ≥ 49 && retainsShadowLoot (initialShadowChests seed)
    if !retainsShadowLoot migrated && !retainsPrimary && !retainsCurrent then
      throw "A locked Shadow Chest must retain its generated contents."
  let homeDoor ← if saveVersion < 5 then pure none
    else do decodeHomeDoor (← json.getObjVal? "homeDoor") world classicEntrance
  let oldWater ← if saveVersion < 6 then pure none
    else do pure (some (← decodeLiquid (← json.getObjVal? "water") world "water" (saveVersion < 23)))
  let oldQuarry ← if saveVersion < 6 then pure none
    else do pure (some (← decodeQuarry (← json.getObjVal? "quarry") world))
  let oldSentry ← if saveVersion < 7 then pure none
    else do pure (some (← decodeSentry (← json.getObjVal? "sentry") world))
  let storm ← if saveVersion < 7 then pure .ready
    else do decodeStorm (← json.getObjVal? "storm") world player
  let raid ← if saveVersion < 9 then pure .ready
    else do decodeRaid (← json.getObjVal? "raid") world seed player
  let ruin ← if saveVersion < 15 then pure .ready
    else do decodeRuin (← json.getObjVal? "ruin") world player
  let cavern ← if saveVersion < 21 then pure .ready
    else do decodeCavern (← json.getObjVal? "cavern") world player cavernLayout.altar
  let projectiles ← if saveVersion < 7 then pure #[]
    else do decodeProjectiles (← json.getObjVal? "projectiles") world storm raid ruin cavern saveVersion
  let shortsword ← if saveVersion < 42 then pure none
    else do decodeShortsword (← json.getObjVal? "shortsword") saveVersion
  if respawnTicks > 0 && shortsword.isSome then throw "A defeated player cannot have an active shortsword."
  let blueMoon ← if saveVersion < 43 then pure none
    else do decodeFlailAttack (← json.getObjVal? "blueMoon") saveVersion
  let aquaStreams ← if saveVersion < 39 then pure #[]
    else do decodeAquaStreams (← json.getObjVal? "aquaStreams") world saveVersion
  let aquaBurst ← if saveVersion < 39 then pure none else do
    match ← json.getObjVal? "aquaBurst" with
    | .null => pure none
    | data => do
      let target : WorldAim ← if saveVersion < 40 then WorldAim.atTile <$> decodePosition data
        else decodeWorldAim data
      if !WorldAim.valid world target then throw "Aqua Scepter burst target lies outside the world."
      pure (some target)
  let aquaAnimation : Nat ← if saveVersion < 39 then pure 0 else readField json "aquaAnimation"
  let aquaShotDelay : Nat ← if saveVersion < 39 then pure 0 else readField json "aquaShotDelay"
  let mana : Nat ← if saveVersion < 39 then pure baseMana else readField json "mana"
  let manaRegenDelay : Nat ← if saveVersion < 39 then pure 0 else readField json "manaRegenDelay"
  let manaRegenProgress : Nat ← if saveVersion < 39 then pure 0 else readField json "manaRegenProgress"
  if aquaAnimation > aquaMaxAnimation || aquaShotDelay > aquaMaxShotDelay ||
      (aquaAnimation > 0) != aquaBurst.isSome ||
      (aquaAnimation == 0 && aquaShotDelay > 0) then
    throw "Invalid Aqua Scepter burst state."
  if mana > baseMana || manaRegenDelay > aquaManaDelay || manaRegenProgress ≥ manaRegenUnit then
    throw "Invalid mana or regeneration state."
  if respawnTicks > 0 && (!aquaStreams.isEmpty || aquaBurst.isSome || manaRegenDelay > 0 || manaRegenProgress > 0) then
    throw "A defeated player cannot have active Aqua Scepter or mana regeneration state."
  let ecology ← if saveVersion < 12 then pure (initialCaveEcology seed)
    else do decodeCaveEcology (← json.getObjVal? "ecology") world seed saveVersion
  if world.tile mine.marker != .mineMarker && ecology.sites[3]? != some (.waiting 0) then
    throw "An active Oreback requires the original mine marker."
  let wardenBodies := match warden with
    | .fighting boss => [boss.body]
    | _ => []
  let stormBodies := match storm with
    | .fighting boss => [boss.body]
    | _ => []
  let raidBodies := match raid with
    | .active wave => wave.foes.toList.map (·.body)
    | _ => []
  let ruinBodies := match ruin with
    | .fighting boss => [boss.body]
    | _ => []
  let cavernBodies := match cavern with
    | .fighting boss => [boss.body]
    | _ => []
  let bossBodies := wardenBodies ++ stormBodies ++ raidBodies ++ ruinBodies ++ cavernBodies
  let raidActive := match raid with | .active _ => true | _ => false
  if respawnTicks > 0 && (raidActive || !bossBodies.isEmpty || !projectiles.isEmpty) then
    throw "A defeated player cannot have active battles or projectiles."
  let encounterBodies := [player] ++ combat.crawler.toList.map (·.body) ++
    surface.creature.toList.map (·.body) ++ surface.flying.toList.map (·.body) ++ bossBodies
  let historicalBodies := encounterBodies ++
    (oldQuarry.bind (·.creature)).toList.map (·.body) ++
    (oldSentry.bind (·.creature)).toList.map (·.body)
  let baseline := if saveVersion < 11 then generatePrePocketsTerrain seed else world
  let world := if saveVersion < 8 then
      migrateTerrain saveVersion seed world baseline historicalBodies homeDoor oldWater projectiles
    else world
  let water := match oldWater with
    | some water => water
    | none => Water.initialLegacy world
  let breath ← if saveVersion < 6 then pure maxBreath else readField json "breath"
  if breath > maxBreath then throw "Player breath exceeds its simulation bounds."
  let quarry := match oldQuarry with
    | some quarry => quarry
    | none => initialQuarry world
  let sentry := match oldSentry with
    | some sentry => sentry
    | none => initialSentry world
  let bowCooldown ← if saveVersion < 7 then pure 0 else readField json "bowCooldown"
  let maxBowCooldown := if saveVersion < 20 then bowCooldownTicks
    else if saveVersion < 28 then highlandCrossbowCooldown
    else if saveVersion < 44 then 32
    else if saveVersion < 47 then magicMissileSlowUseTime
    else if saveVersion < 62 then flamelashSlowUseTime else max flamelashSlowUseTime 45
  if bowCooldown > maxBowCooldown then throw "Bow cooldown exceeds its simulation bounds."
  let chart ← if saveVersion < 8 then pure ((Chart.empty world).reveal world player.centerTile)
    else do decodeChart (← json.getObjVal? "chart") world maxTileCode
  if saveVersion ≥ 11 then
    validateLandmarks chart.cells world.width pockets crystalSites berryPositions mine cavernLayout.altar 1
  if saveVersion ≥ 31 then
    validateDungeonChestCells chart.cells world.width dungeon savedDungeon savedLowerDungeon
      savedBlueMoonDungeon savedMagicMissileDungeon savedDeepDungeon 1
  if saveVersion ≥ 46 then validateShadowChestCells chart.cells world.width shadowChests hasUnderworld
  let chart := if (chart.tile? world player.centerTile).isNone then chart.reveal world player.centerTile else chart
  let merchantStock ← if saveVersion < 8 then pure (initialMerchantStockFor legacyEarly)
    else decodeMerchantStock json saveVersion legacyEarly
  let merchantDoor ← if saveVersion < 8 then pure none
    else do decodeHomeDoor (← json.getObjVal? "merchantDoor") world classicEntrance
  if merchantDoor.isSome && merchantDoor == homeDoor then
    throw "Two residents cannot share a registered door."
  let homeAnchor ← if saveVersion < 10 then pure none
    else do decodeHomeAnchor (← json.getObjVal? "homeAnchor") world
  let held := cursorStack.orElse fun _ => inventory[selected]?.join
  let fishing ← if saveVersion < 13 then pure .idle
    else do decodeFishing (← json.getObjVal? "fishing") world water player inventory held legacyEarly saveVersion
  let carts ← if saveVersion < 16 then pure #[] else do decodeCarts (← json.getObjVal? "carts") world
  let ride ← if saveVersion < 16 then pure none else do decodeRide (← json.getObjVal? "ride") world player carts
  let grapple ← if saveVersion < 17 then pure .idle
    else decodeGrapple (← json.getObjVal? "grapple") saveVersion
  if respawnTicks > 0 && (ride.isSome || grapple != .idle || fishing != .idle) then
    throw "A defeated player cannot ride, grapple or fish."
  let residentBodies := ecology.sites.toList.filterMap fun site => match site with
    | .alive resident => some resident.body
    | _ => none
  let bodies := encounterBodies ++ quarry.creature.toList.map (·.body) ++
    sentry.creature.toList.map (·.body) ++ residentBodies
  let homes := homeDoor.toList ++ merchantDoor.toList
  let cartTiles := carts.toList ++ ride.toList.flatMap (fun active => active.rail :: active.next.toList)
  let occupiedBodies := bodies ++ cartTiles.map cartBody
  let shots := projectiles.toList.map fun shot => (⟨shot.x / tileUnits, shot.y / tileUnits⟩ : TilePos)
  let wardenDrop := match warden with | .reward position => [position] | _ => []
  let stormDrop := match storm with | .reward position => [position] | _ => []
  let raidDrop := match raid with | .reward position => [position] | _ => []
  let ruinDrop := match ruin with | .reward position => [position] | _ => []
  let cavernDrop := match cavern with | .reward position => [position] | _ => []
  let residentDrops := ecology.sites.toList.filterMap fun site => match site with
    | .loot position => some position
    | _ => none
  let fixedTiles := shots ++ wardenDrop ++ stormDrop ++ raidDrop ++ ruinDrop ++ cavernDrop ++ residentDrops
  let hookTiles := match grapple with
    | .idle => []
    | .flying hook => ⟨hook.x / tileUnits, hook.y / tileUnits⟩ :: (hook.held.map (·.tile)).toList
    | .latched anchor => [anchor.tile]
  let fishingTiles := match fishing with
    | .idle => []
    | .waiting position _ | .biting position _ | .soaking position _ | .hooked position .. => [position]
    | .bobber motion _ => [motion.tile]
  let activeTiles := fixedTiles ++ hookTiles ++ fishingTiles ++ cartTiles
  let (world, water) := if saveVersion < 18 then
      let world := if saveVersion < 11 then
          retrofitSupplyPockets world baseline pockets water bodies homes fixedTiles
        else world
      let (world, water) := if saveVersion < 13 then retrofitPond seed world water bodies homes fixedTiles
        else (world, water)
      let world := if saveVersion < 14 then retrofitSunleaf seed world water bodies homes fixedTiles
        else world
      let world := if saveVersion < 15 then retrofitRuins seed world water bodies homes fixedTiles player fishing
        else world
      (retrofitLifeCrystals seed world water occupiedBodies homes activeTiles crystalSites, water)
    else (world, water)
  let crops ← if saveVersion < 14 then pure #[] else do decodeCrops (← json.getObjVal? "crops") world
  let gills : Nat ← if saveVersion < 14 then pure 0 else readField json "gills"
  let barkskin : Nat ← if saveVersion < 14 then pure 0 else readField json "barkskin"
  let cratePotion : Nat ← if saveVersion < 58 then pure 0 else readField json "cratePotion"
  let fishingPotion : Nat ← if saveVersion < 59 then pure 0 else readField json "fishingPotion"
  let sonarPotion : Nat ← if saveVersion < 59 then pure 0 else readField json "sonarPotion"
  let potionSickness : Nat ← if saveVersion < 27 then pure 0 else readField json "potionSickness"
  if gills > gillsTicks || barkskin > barkskinTicks || cratePotion > cratePotionTicks ||
      fishingPotion > fishingPotionTicks || sonarPotion > sonarPotionTicks then
    throw "An alchemy timer exceeds its bounds."
  let honeyBuff : Nat ← if saveVersion < 61 then pure 0 else readField json "honeyBuff"
  if honeyBuff > honeyBuffDuration then throw "Honey buff exceeds its simulation bounds."
  if potionSickness > potionSicknessTicks then throw "Potion Sickness exceeds its simulation bounds."
  if respawnTicks > 0 && (bowCooldown > 0 || gills > 0 || barkskin > 0 || cratePotion > 0 ||
      fishingPotion > 0 || sonarPotion > 0 || potionSickness > 0 || honeyBuff > 0) then
    throw "A defeated player cannot have active equipment or potion timers."
  let trash := inventory[Inventory.trashSlot]?.join
  let ownedCount (item : Item) :=
    let cursor := (cursorStack.filter (·.item == item)).map (·.count) |>.getD 0
    let discarded := (trash.filter (·.item == item)).map (·.count) |>.getD 0
    chests.foldl (fun total chest => total + Inventory.count chest.slots item)
      (Inventory.count inventory item + cursor + discarded)
  let shadowKeyRecovery : Bool ← if saveVersion < 46 then pure false else readField json "shadowKeyRecovery"
  if shadowKeyRecovery && (!hasUnderworld || ownedCount .shadowKey > 0) then
    throw "A pending Shadow Key requires an expanded world and no existing key."
  if grapple != .idle then
    if ride.isSome || (legacyEarly && fishing != .idle) || !GrappleState.valid world player grapple then
      throw "The grappling hook cannot be deployed in this state."
    let basic := match grapple with
      | .flying flight => flight.kind == .basic || flight.held.any (·.kind == .basic)
      | .latched anchor => anchor.kind == .basic
      | .idle => false
    if basic && equipment.hook != some .grapplingHook && ownedCount .grapplingHook == 0 then
      throw "A deployed grappling hook must belong to the player or be stored in a chest."
  let unmined := crystalSites.foldl (fun count site =>
    count + if world.tile site.position == .lifeCrystal then 1 else 0) (0 : Nat)
  let crateLifeCrystals : Nat ← if saveVersion < 57 then pure 0 else readField json "crateLifeCrystals"
  if unmined + ownedCount .lifeCrystal + combat.lifeCrystals > crystalSites.size + crateLifeCrystals then
    throw "The total Life Crystals exceed the number generated in this world."
  -- Version 30 has a trash slot. A collected reward may have been discarded.
  let invalidCollected (count : Nat) := count > 1 || (saveVersion < 30 && count == 0)
  let quantity := if saveVersion < 30 then "exactly one" else "at most one"
  let blades := ownedCount .frostbrand
  match warden with
  | .cleared =>
    if invalidCollected blades then throw s!"A cleared Warden encounter must have {quantity} Frostbrand."
  | _ =>
    if blades != 0 then throw "Frostbrand cannot be owned before collecting the Warden's reward."
  let bows := ownedCount .stormBow
  match storm with
  | .cleared =>
    if invalidCollected bows then throw s!"A cleared Storm Herald encounter must have {quantity} Storm Bow."
  | _ =>
    if bows != 0 then throw "Storm Bow cannot be owned before collecting the Herald's reward."
  let crests := ownedCount .raidCrest + if equipment.accessory == some .raidCrest then 1 else 0
  match raid with
  | .cleared =>
    if invalidCollected crests then throw s!"A cleared Amber Raid must have {quantity} Raid Crest."
  | _ =>
    if crests != 0 then throw "Raid Crest cannot be owned before collecting the Amber Raid reward."
  let hammers := ownedCount .ruinHammer
  match ruin with
  | .cleared =>
    if invalidCollected hammers then throw s!"A cleared Ruin Sentinel encounter must have {quantity} Ruin Hammer."
  | _ =>
    if hammers != 0 then throw "Ruin Hammer cannot be owned before collecting the Sentinel's reward."
  let aegis := ownedCount .deepstoneAegis + if equipment.armor == some .deepstoneAegis then 1 else 0
  match cavern with
  | .cleared =>
    if invalidCollected aegis then throw s!"A cleared Deepstone Colossus encounter must have {quantity} Deepstone Aegis."
  | _ =>
    if aegis != 0 then throw "Deepstone Aegis cannot be owned before collecting the Colossus's reward."
  let (world, water, chart) := if saveVersion < 19 then
      let keepBoundary := preserveHighlandsBoundary world water homes occupiedBodies
        projectiles grapple
      let widened := extendHighlands seed world keepBoundary
      (widened, { levels := widenCells water.levels world widened }, { cells := widenCells chart.cells world widened })
    else (world, water, chart)
  let (world, water, magma) ← if saveVersion < 22 then
      let contacts := inFlightTerrainContacts world projectiles grapple
      let keepsActive candidate := grapple.valid candidate player && retainsFishing candidate water player fishing &&
        carts.all (canParkCart candidate) && ride.all (fun active => active.valid candidate carts)
      let fixed := activeTiles ++ contacts
      let world := if saveVersion < 20 then
          retrofitUndisturbedSite world (generatePreDeepsteelTerrain seed) water occupiedBodies homes fixed
            mine.footprint mine.edits keepsActive
        else world
      let world := if saveVersion < 21 && world.tile mine.marker == .mineMarker then
          retrofitUndisturbedSite world (generatePreCavernTerrain seed) water occupiedBodies homes fixed
            cavernLayout.footprint cavernLayout.edits keepsActive
        else world
      let gallery := magmaGallery seed
      let candidate := if world.tile cavernLayout.altar == .cavernAltar then
          retrofitUndisturbedSite world (generatePreMagmaTerrain seed) water occupiedBodies homes fixed
            gallery.footprint gallery.edits keepsActive
        else world
      let dry := Water.empty world
      if candidate.tile magmaMarker == .magmaMarker then
        let filled := Magma.fillFromEighths water candidate gallery.water
        if retainsFishing candidate filled player fishing then
          pure (candidate, filled, Magma.fillFromEighths dry candidate gallery.magma)
        else pure (world, water, dry)
      else pure (world, water, dry)
    else do
      let magma ← decodeLiquid (← json.getObjVal? "magma") world "magma" (saveVersion < 23) (some water)
      if !hasUnderworld && world.tile magmaMarker != .magmaMarker && magma.levels.data.any (· != 0) then
        throw "Magma requires the original gallery marker."
      pure (world, water, magma)
  let world := if saveVersion < 24 then
      let fixed := activeTiles ++ inFlightTerrainContacts world projectiles grapple
      retrofitClassicResources seed world water magma occupiedBodies homes fixed
    else world
  let walls ← if saveVersion < 24 then pure (ByteArray.mk (Array.replicate (world.width * world.height) 0))
    else do decodeWalls (← json.getObjVal? "walls") world saveVersion
  let furniture ← if saveVersion < 24 then pure #[]
    else do decodeFurniture (← json.getObjVal? "furniture") world saveVersion
  if saveVersion ≥ 29 then validatePlacedBottles world
  let bedSpawnPoint ← if saveVersion < 24 then pure none
    else do decodeBedSpawnPoint (← json.getObjVal? "bedSpawnPoint") world walls furniture
  let guide := homeDoor.bind fun door =>
    if legacyEarly then (inspectHome world door).toOption
    else (inspectClassicHome world walls door).toOption.map (·.stand)
  let merchant := merchantDoor.bind fun door =>
    if legacyEarly then (inspectSeparateHome world door homeDoor).toOption
    else ((inspectClassicHome world walls door).toOption.map (·.stand)).filter (fun stand => some stand != guide)
  let townGuide ← if saveVersion < 26 then pure (if legacyEarly then none else initialGuide world)
    else do decodeTownGuide (← json.getObjVal? "townGuide") world legacyEarly
  let toolCooldown : Nat ← if saveVersion < 25 then pure 0 else readField json "toolCooldown"
  if toolCooldown > maxToolCooldown then throw "The tool cooldown exceeds its simulation bounds."
  let tileHits ← if saveVersion < 25 then pure #[] else do decodeTileHits (← json.getObjVal? "tileHits") world
  if respawnTicks > 0 && (toolCooldown > 0 || !tileHits.isEmpty) then
    throw "A defeated player cannot have active tool state."
  let gelDrops ← if saveVersion < 25 then pure #[] else do decodeGelDrops (← json.getObjVal? "gelDrops") world
  let nightDrops ← if saveVersion < 26 then pure #[]
    else do decodeNightDrops (← json.getObjVal? "nightDrops") world saveVersion
  let bombs ← if saveVersion < 28 then pure #[] else do decodeBombs (← json.getObjVal? "bombs") world saveVersion
  let eye ← if saveVersion < 27 then pure none else do decodeEye (← json.getObjVal? "eye") world saveVersion
  let eyeDefeated ← if saveVersion < 27 then pure false else readField json "eyeDefeated"
  if legacyEarly && (eye.isSome || eyeDefeated) then throw "The Eye of Cthulhu requires a Classic world."
  if (respawnTicks > 0 || !isNight surface.clock false) && eye.any (·.phase != .retreat) then
    throw "The Eye of Cthulhu can only retreat after dawn or during player respawn."
  let eater ← if saveVersion < 29 then pure none else do decodeEater (← json.getObjVal? "eater") world
  let brain ← if saveVersion < 29 then pure none else do decodeBrain (← json.getObjVal? "brain") world
  let evilEnemies ← if saveVersion < 29 then pure #[] else do decodeEvilFlyers (← json.getObjVal? "evilEnemies") world
  let originalAltars := if legacyEarly then #[] else eyeAltars seed
  let altar := altarTile seed
  if saveVersion == 27 then
    validateEyeAltarCells world.tiles world.width originalAltars altar
    validateEyeAltarCells chart.cells world.width originalAltars altar 1
    let _ ← countWholeSites world originalAltars altarCells altar "An evil altar"
  let retrofitAllowed := fun (world : World) (water magma : Water) (walls : ByteArray)
      (extraBoxes : List HitBox) (extraTiles : List TilePos) =>
    let bodies := occupiedBodies ++ townGuide.toList.map (·.body)
    let fixed := activeTiles ++ inFlightTerrainContacts world projectiles grapple ++ homes ++
      guide.toList ++ merchant.toList ++ (townGuide.bind (·.doorToClose)).toList ++
      gelDrops.toList.map (·.position) ++ nightDrops.toList.map (·.position) ++
      tileHits.toList.map (·.position) ++ chests.toList.map (·.position) ++ extraTiles
    let rooms := homes.map (homeFootprint world)
    let eyeBoxes := eye.toList.flatMap fun boss => boss.hitBox :: boss.servants.toList.map (·.hitBox)
    let eaterBoxes := eater.toList.flatMap fun boss =>
      boss.chains.toList.flatMap fun chain => chain.toList.map (·.hitBox)
    let brainBoxes := brain.toList.flatMap fun boss => boss.hitBox :: boss.creepers.toList.map (·.hitBox)
    let enemyBoxes := eyeBoxes ++ eaterBoxes ++ brainBoxes ++ evilEnemies.toList.map (·.hitBox) ++ extraBoxes
    fun (position : TilePos) =>
      let x := position.x * tileUnits
      let y := position.y * tileUnits
      water.level world position == 0 && magma.level world position == 0 && !nearBeacon world position &&
        !hasWoodWall world walls position && !rooms.any (·.contains position) && !fixed.contains position &&
        !bodies.any (·.overlapsTile position) &&
        !enemyBoxes.any (fun box => box.left < x + tileUnits && x < box.right &&
          box.top < y + tileUnits && y < box.bottom) &&
        !bombs.any (fun bomb => bomb.attached == some position ||
          ((bomb.position.x - position.x).natAbs ≤ 1 && (bomb.position.y - position.y).natAbs ≤ 1)) &&
        !furniture.any (fun piece => piece.cells.contains position || piece.supports.contains position)
  let acceptRetrofit := fun (previous candidate : World) (water : Water) (walls : ByteArray) =>
    let stand := fun entrance => entrance.bind fun position =>
      (inspectClassicHome candidate walls position).toOption.map (·.stand)
    let candidateGuide := stand homeDoor
    let candidateMerchant := (stand merchantDoor).filter (fun position => some position != candidateGuide)
    let keepsBed := bedSpawnPoint.all fun anchor =>
      furniture.any (fun piece => piece.position == anchor && (bedSpawn candidate walls piece).isSome)
    if candidate.tiles == previous.tiles || !(validateDoors candidate).isOk ||
        candidateGuide != guide || candidateMerchant != merchant || !keepsBed ||
        !grapple.valid candidate player || !retainsFishing candidate water player fishing ||
        !carts.all (canParkCart candidate) || !ride.all (fun active => active.valid candidate carts) ||
        !furniture.all (·.intact candidate) || !projectiles.all (·.valid candidate) then previous
    else candidate
  let protectClassicRooms := fun (world : World) (walls : ByteArray) (allowed : TilePos → Bool) =>
    let bedInterior := bedSpawnPoint.bind fun anchor =>
      (furniture.find? (·.position == anchor)).bind fun bed =>
        (bedSpawn world walls bed).map (·.centerTile)
    let interiors := guide.toList ++ merchant.toList ++ bedInterior.toList
    let classicRooms := interiors.filterMap fun start => (scanBackedRoom world walls start).toOption
    fun position => allowed position && !classicRooms.any (·.contains position)
  let (world, addedDungeon) := if saveVersion < 31 && !legacyEarly then
      let allowed := retrofitAllowed world water magma walls [] []
      let world := if saveVersion < 27 then retrofitEyeAltars seed world allowed else world
      let world := if saveVersion < 28 then acceptRetrofit world (retrofitEvilTerrain seed world allowed) water walls
        else world
      let world := if saveVersion < 29 then acceptRetrofit world (retrofitEvilBossResources seed world allowed) water walls
        else world
      if saveVersion < 26 then (world, false) else
        match retrofitDungeon seed world (protectClassicRooms world walls allowed) with
        | none => (world, false)
        | some candidate =>
          let accepted := acceptRetrofit world candidate water walls
          (accepted, accepted.tiles != world.tiles)
    else (world, false)
  let evilRelicsBroken : Nat ← if saveVersion < 28 then pure 0 else readField json "evilRelicsBroken"
  let installedEvil ← if saveVersion ≥ 28 then do
    let newAltars := if legacyEarly then #[] else evilAltars seed
    let relicSites := if legacyEarly then #[] else evilRelics seed
    let potSites := if legacyEarly then #[] else evilPots seed
    let relic := evilRelicTile seed
    let altars := originalAltars ++ newAltars
    validateEyeAltarCells world.tiles world.width altars altar
    validateEyeAltarCells chart.cells world.width altars altar 1
    validateEvilObjectCells world.tiles world.width relicSites potSites relic
    validateEvilObjectCells chart.cells world.width relicSites potSites relic 1
    let intactOriginalAltars ← countWholeSites world originalAltars altarCells altar "An evil altar"
    let installedAltars ← countWholeSites world newAltars altarCells altar "An evil altar"
    let intactRelics ← countWholeSites world relicSites evilRelicCells relic "An evil relic"
    let intactPots ← countWholeSites world potSites evilPotCells .clayPot "A clay pot"
    let missingAltars := altars.size - (intactOriginalAltars + installedAltars)
    if altarsSmashed > missingAltars then throw "Smashed altar progress exceeds missing altar sites."
    let installed := installedAltars > 0 ||
      (hardmode && saveVersion ≥ 51 && (intactRelics > 0 || intactPots > 0 || evilRelicsBroken > 0))
    if !installed then
      if intactRelics != 0 || intactPots != 0 || evilRelicsBroken != 0 then
        throw "Evil relic progress and pots require an installed evil biome."
    else if installedAltars + altarsSmashed < newAltars.size ||
        intactRelics + evilRelicsBroken != relicSites.size then
      throw "Evil relic progress does not match the remaining relics."
    pure installed
  else pure false
  let evilBossDefeated ← if saveVersion < 29 then pure false else readField json "evilBossDefeated"
  if eater.isSome || brain.isSome || evilBossDefeated || !evilEnemies.isEmpty then
    if legacyEarly || !installedEvil then
      throw "Evil biome encounters require an installed Classic evil biome."
  let corruption := evilRelicTile seed == .shadowOrb
  let expectedFlyer := if corruption then EvilFlyerKind.eaterOfSouls else .crimera
  if (eater.isSome && !corruption) || (brain.isSome && corruption) ||
      evilEnemies.any (·.kind != expectedFlyer) then
    throw "An evil biome encounter does not match this world's evil type."
  if respawnTicks > 0 then
    if !evilEnemies.isEmpty then throw "Evil biome enemies must be cleared during player respawn."
    if eater.any (fun boss => !boss.retreating) || brain.any (·.phase != .retreat) then
      throw "An evil biome boss can only retreat during player respawn."
  let dungeonInstalled := savedDungeon || addedDungeon
  let chests := if addedDungeon then chests ++ initialDungeonChests seed else chests
  let skeletron ← if saveVersion < 31 then pure none
    else do decodeSkeletron (← json.getObjVal? "skeletron") world player saveVersion
  let skeletronDefeated : Bool ← if saveVersion < 31 then pure false else readField json "skeletronDefeated"
  let oldManWaiting : Bool ← if saveVersion < 31 then pure addedDungeon else readField json "oldManWaiting"
  let dungeonGuardian ← if saveVersion < 31 then pure none else do
    match ← json.getObjVal? "dungeonGuardian" with
    | .null => pure none
    | data => pure (some (← decodeEyeMotion data))
  let dungeonGuardianHealth : Nat ← if saveVersion < 31 then pure 0 else readField json "dungeonGuardianHealth"
  let dungeonSlimes ← if saveVersion < 35 then pure #[]
    else do decodeDungeonSlimes (← json.getObjVal? "dungeonSlimes") world
  let darkCasters ← if saveVersion < 36 then pure #[]
    else do decodeDarkCasters (← json.getObjVal? "darkCasters") world
  let waterSpheres ← if saveVersion < 36 then pure #[]
    else do decodeWaterSpheres (← json.getObjVal? "waterSpheres") world
  let nextDarkCasterId : Nat ← if saveVersion < 36 then pure 1 else readField json "nextDarkCasterId"
  let nextWaterSphereId : Nat ← if saveVersion < 36 then pure 1 else readField json "nextWaterSphereId"
  let angryBones ← if saveVersion < 37 then pure #[]
    else do decodeAngryBones (← json.getObjVal? "angryBones") world
  let nextAngryBonesId : Nat ← if saveVersion < 37 then pure 1 else readField json "nextAngryBonesId"
  let cursedSkulls ← if saveVersion < 38 then pure #[]
    else do decodeCursedSkulls (← json.getObjVal? "cursedSkulls") world
  let nextCursedSkullId : Nat ← if saveVersion < 38 then pure 1 else readField json "nextCursedSkullId"
  let cursedTicks : Nat ← if saveVersion < 38 then pure 0 else readField json "cursedTicks"
  if !dungeonInstalled && (skeletron.isSome || skeletronDefeated || oldManWaiting || dungeonGuardian.isSome) then
    throw "A dungeon encounter requires an installed Dungeon."
  if !dungeonSlimes.isEmpty && (!dungeonInstalled || !skeletronDefeated) then
    throw "Dungeon Slimes require the installed Dungeon and Skeletron's defeat."
  if !dungeonInstalled && (!darkCasters.isEmpty || !waterSpheres.isEmpty) then
    throw "Dark Casters and Water Spheres require an installed Dungeon."
  if !dungeonInstalled && !angryBones.isEmpty then
    throw "Angry Bones require an installed Dungeon."
  if !dungeonInstalled && (!cursedSkulls.isEmpty || cursedTicks > 0) then
    throw "Cursed Skulls and the Cursed debuff require an installed Dungeon."
  if nextDarkCasterId == 0 || darkCasters.any (·.id ≥ nextDarkCasterId) then
    throw "The next Dark Caster ID must exceed all live Dark Caster IDs."
  if nextWaterSphereId == 0 || waterSpheres.any (·.id ≥ nextWaterSphereId) then
    throw "The next Water Sphere ID must exceed all live Water Sphere IDs."
  if nextAngryBonesId == 0 || angryBones.any (·.id ≥ nextAngryBonesId) then
    throw "The next Angry Bones ID must exceed all live Angry Bones IDs."
  if nextCursedSkullId == 0 || cursedSkulls.any (·.id ≥ nextCursedSkullId) then
    throw "The next Cursed Skull ID must exceed all live Cursed Skull IDs."
  if cursedTicks > maxCursedTicks ||
      (cursedTicks > 0 && (respawnTicks > 0 || equipment.accessory == some .nazar)) then
    throw "Invalid Cursed debuff timer."
  if skeletronDefeated && (skeletron.isSome || oldManWaiting || dungeonGuardian.isSome) then
    throw "The Old Man and his guardians are absent after defeating Skeletron."
  -- Dungeon AI sees the old clock and runs before the player's late damage checks.
  let firstRespawnFrame := saveVersion ≥ 34 && respawnTicks == classicRespawnTicks
  if respawnTicks > 0 && (dungeonGuardian.isSome ||
      (!firstRespawnFrame && skeletron.any (·.phase != .retreat))) then
    throw "Only a retreating Skeletron may remain during player respawn."
  let firstDawnFrame := saveVersion ≥ 34 && surface.clock == 0
  if !firstDawnFrame && !isNight surface.clock false &&
      skeletron.any (fun boss => boss.phase == .hover || boss.phase == .spin) then
    throw "Skeletron must enrage or retreat during daylight."
  if (dungeonGuardianHealth == 0) != dungeonGuardian.isNone || dungeonGuardianHealth > dungeonGuardianMaxHealth ||
      !dungeonGuardian.all (fun guardian => guardian.valid world dungeonGuardianSpeed) then
    throw "Invalid Dungeon Guardian motion or health."
  let dungeonBossBoxes := skeletron.toList.flatMap fun boss => boss.targets.toList.map (·.2)
  let dungeonBoxes := dungeonBossBoxes ++ dungeonGuardian.toList.map dungeonGuardianBox ++
    dungeonSlimes.toList.map (·.hitBox) ++ darkCasters.toList.map (·.hitBox) ++
    waterSpheres.toList.map (·.hitBox) ++ angryBones.toList.map (·.hitBox) ++ cursedSkulls.toList.map (·.hitBox)
  let originalChestTiles := (dungeonChestCells dungeon.keyChest ++ dungeonChestCells dungeon.goldChest).toList
  let dungeonFixed := originalChestTiles ++ darkCasters.toList.filterMap (·.destination)
  let (world, addedLowerDungeon) := if saveVersion < 39 && dungeonInstalled then
      let allowed := protectClassicRooms world walls (retrofitAllowed world water magma walls dungeonBoxes dungeonFixed)
      match retrofitLowerDungeon seed world allowed with
      | none => (world, false)
      | some candidate =>
        let accepted := acceptRetrofit world candidate water walls
        if accepted.tiles != world.tiles && (dungeonChestSites accepted dungeon true true false false false).isOk &&
            inFlightTerrainContacts world projectiles grapple == inFlightTerrainContacts accepted projectiles grapple then
          (accepted, true)
        else (world, false)
    else (world, false)
  let lowerDungeonInstalled := savedLowerDungeon || addedLowerDungeon
  let chests := if addedLowerDungeon then chests.push (initialLowerDungeonChest seed) else chests
  let extensionAllowed := fun (terrain : World) (water magma : Water) (walls : ByteArray)
      (extraBoxes : List HitBox) (extraTiles : List TilePos) =>
    let boxes := dungeonBoxes ++ aquaStreams.toList.map (·.hitBox) ++ extraBoxes
    let swordFrames := shortsword.toArray ++ (shortsword.bind (·.advance)).toArray ++
      (shortsword.bind (fun thrust => thrust.advance.bind (·.advance))).toArray
    let swordTiles := (swordFrames.flatMap (·.cutTiles player)).toList
    let fixed := dungeonFixed ++ (dungeonChestCells dungeon.lowerGoldChest).toList ++ swordTiles ++ extraTiles
    let allowed := protectClassicRooms terrain walls (retrofitAllowed terrain water magma walls boxes fixed)
    fun (position : TilePos) =>
      let x := position.x * tileUnits
      let y := position.y * tileUnits
      let box : HitBox := ⟨x, y, x + tileUnits, y + tileUnits⟩
      allowed position && !swordFrames.any (fun thrust => thrust.intersects player box)
  let keepsTerrainProjectiles := fun (previous candidate : World) (magma : Water) =>
    inFlightTerrainContacts previous projectiles grapple == inFlightTerrainContacts candidate projectiles grapple &&
      aquaStreams.all (fun stream => aquaTerrainFrame stream previous magma == aquaTerrainFrame stream candidate magma) &&
      bombs.all (fun bomb => advanceBomb previous bomb == advanceBomb candidate bomb)
  let keepsFlail := fun (attack : BlueMoonAttack) (previous candidate : World) =>
    [false, true].all fun held =>
      attack.flail.advance previous player attack.aim combat.facing held water magma ==
        attack.flail.advance candidate player attack.aim combat.facing held water magma
  let (world, addedBlueMoonDungeon) := if saveVersion < 43 && lowerDungeonInstalled then
      match retrofitBlueMoonDungeon seed world (extensionAllowed world water magma walls [] []) with
      | none => (world, false)
      | some candidate =>
        let accepted := acceptRetrofit world candidate water walls
        if accepted.tiles != world.tiles && (dungeonChestSites accepted dungeon true true true false false).isOk &&
            keepsTerrainProjectiles world accepted magma then
          (accepted, true)
        else (world, false)
    else (world, false)
  let blueMoonDungeonInstalled := savedBlueMoonDungeon || addedBlueMoonDungeon
  let chests := if addedBlueMoonDungeon then chests.push (initialBlueMoonDungeonChest seed) else chests
  let (world, addedMagicMissileDungeon) := if saveVersion < 44 && blueMoonDungeonInstalled then
      let flail := blueMoon.filter (fun attack => attack.flail.valid world && attack.aim.valid world)
      let boxes := flail.toList.map (·.flail.hitBox)
      let fixed := (dungeonChestCells dungeon.blueMoonChest).toList
      match retrofitMagicMissileDungeon seed world (extensionAllowed world water magma walls boxes fixed) with
      | none => (world, false)
      | some candidate =>
        let accepted := acceptRetrofit world candidate water walls
        if accepted.tiles != world.tiles && (dungeonChestSites accepted dungeon true true true true false).isOk &&
            keepsTerrainProjectiles world accepted magma && flail.all (fun attack => keepsFlail attack world accepted) then
          (accepted, true)
        else (world, false)
    else (world, false)
  let magicMissileDungeonInstalled := savedMagicMissileDungeon || addedMagicMissileDungeon
  let chests := if addedMagicMissileDungeon then chests.push (initialMagicMissileDungeonChest seed) else chests
  let magicMissiles ← if saveVersion < 44 then pure #[]
    else do decodeMagicMissiles (← json.getObjVal? "magicMissiles") saveVersion
  let (world, addedDeepDungeon) := if saveVersion < 45 && magicMissileDungeonInstalled then
      let flail := blueMoon.filter (fun attack => attack.flail.valid world && attack.aim.valid world)
      let missileBoxes : List HitBox := magicMissiles.toList.filterMap fun attack =>
        if !attack.missile.valid world then none else
          -- Cover a full 32-pixel update in any direction, including tile-cut rounding.
          some (expandProjectileBox attack.missile.blastBox 34)
      let boxes := flail.toList.map (·.flail.hitBox) ++ missileBoxes
      let imminentBlasts := (bombs.filter (·.fuse == 1)).flatMap (·.blastTiles world)
      let fixed := (dungeonChestCells dungeon.blueMoonChest ++ dungeonChestCells dungeon.magicMissileChest ++
        imminentBlasts).toList
      match retrofitDeepDungeon seed world (extensionAllowed world water magma walls boxes fixed) with
      | none => (world, false)
      | some candidate =>
        let accepted := acceptRetrofit world candidate water walls
        let keepsRanged := projectiles.all fun shot =>
          if shot.kind == .unholyArrow then advanceUnholyArrow world shot == advanceUnholyArrow accepted shot
          else advanceProjectileBoxes world shot #[] == advanceProjectileBoxes accepted shot #[]
        if accepted.tiles != world.tiles && (dungeonChestSites accepted dungeon true true true true true).isOk &&
            keepsTerrainProjectiles world accepted magma && keepsRanged &&
            flail.all (fun attack => keepsFlail attack world accepted) then
          (accepted, true)
        else (world, false)
    else (world, false)
  let deepDungeonInstalled := savedDeepDungeon || addedDeepDungeon
  let chests := if addedDeepDungeon then chests ++ initialDeepDungeonChests seed else chests
  let valor ← if saveVersion < 45 then pure none
    else do decodeValor (← json.getObjVal? "valor") saveVersion
  let sunfury ← if saveVersion < 46 then pure none
    else do decodeFlailAttack (← json.getObjVal? "sunfury") saveVersion
  let darkLance ← if saveVersion < 46 then pure none
    else do decodeDarkLance (← json.getObjVal? "darkLance") saveVersion
  let expandingUnderworld := saveVersion < 46 && !legacyEarly
  let (world, water, magma, walls, chart, chests) := if expandingUnderworld then
      let extended := extendUnderworld seed world
      let emptyRows := ByteArray.mk (Array.replicate (extended.width * (extended.height - world.height)) 0)
      (extended, { levels := water.levels ++ emptyRows }, Magma.seedUnderworld magma extended seed,
        walls ++ emptyRows, { cells := chart.cells ++ emptyRows }, chests ++ initialLegacyShadowChests seed)
    else (world, water, magma, walls, chart, chests)
  let (chests, inventory, cursorStack, shadowKeyRecovery) := if expandingUnderworld then
      -- A cursor item would replace the selected tool while its action is in progress.
      let cursorIdle := fishing == .idle && grapple == .idle && aquaBurst.isNone && shortsword.isNone &&
        blueMoon.isNone && valor.isNone && magicMissiles.all (fun attack => !attack.missile.guided)
      deliverMigratedShadowKey dungeon.goldChest chests inventory cursorStack cursorIdle
    else (chests, inventory, cursorStack, shadowKeyRecovery)
  let beforeShadowTowers := world
  let (world, chests, addedShadowTowers) := if saveVersion < 47 && !legacyEarly then
      let flails := (blueMoon.toList ++ sunfury.toList).filter (fun attack => attack.flail.valid world)
      let missiles := magicMissiles.toList.filter (fun attack => attack.missile.valid world)
      let yoyos := valor.toList.filter (fun attack => attack.yoyo.valid world)
      let lanceFrames := (darkLance.toList ++ (darkLance.bind (·.advance)).toList).filter (·.valid)
      let boxes := flails.map (fun attack => expandProjectileBox attack.flail.hitBox 80) ++
        missiles.map (fun attack => expandProjectileBox attack.missile.blastBox 34) ++
        yoyos.map (fun attack => expandProjectileBox attack.yoyo.hitBox 34) ++
        lanceFrames.flatMap (fun lance => lance.hitBox player :: (lance.extensionHitBox player).toList)
      let occupiedChests := chests.toList.flatMap fun chest =>
        let position := chest.position
        if (world.tile position).isFramedChest then
          (dungeonChestCells position).toList ++ [⟨position.x, position.y + 2⟩, ⟨position.x + 1, position.y + 2⟩]
        else [position, ⟨position.x, position.y + 1⟩]
      let imminentBlasts := (bombs.filter (·.fuse == 1)).flatMap (·.blastTiles world)
      let fixed := occupiedChests ++ imminentBlasts.toList ++ lanceFrames.flatMap (fun lance => (lance.cutTiles player).toList)
      let standingBodies := occupiedBodies ++ townGuide.toList.map (·.body)
      let keepsRanged := fun (previous candidate : World) => projectiles.all fun shot =>
        if shot.kind == .handgunBullet then
          handgunTerrainFrame shot previous water magma == handgunTerrainFrame shot candidate water magma
        else if shot.kind == .unholyArrow then advanceUnholyArrow previous shot == advanceUnholyArrow candidate shot
        else advanceProjectileBoxes previous shot #[] == advanceProjectileBoxes candidate shot #[]
      (initialAdditionalShadowChests seed).foldl (fun (terrain, stored, added) chest =>
        let clear := extensionAllowed terrain water magma walls boxes fixed
        let allowed := fun position => clear position &&
          !standingBodies.any (fun body => retainsBodyTile body position (terrain.tile position) .air) &&
          !(adjacentTiles position).any (fun nearby => terrain.tile nearby == .torch)
        match retrofitShadowChest seed terrain chest.position allowed with
        | none => (terrain, stored, added)
        | some candidate =>
          let accepted := acceptRetrofit terrain candidate water walls
          if accepted.tiles != terrain.tiles && keepsTerrainProjectiles terrain accepted magma &&
              keepsRanged terrain accepted then
            (accepted, stored.push chest, true)
          else (terrain, stored, added)) (world, chests, false)
    else (world, chests, false)
  let chart := if addedShadowTowers then
      let visible := chart.reveal world player.centerTile
      if visible != chart.reveal beforeShadowTowers player.centerTile then visible else chart
    else chart
  let status := if saveVersion < 3 then s!"Version {saveVersion} world loaded. Frost Cavern added."
    else if saveVersion == 3 then "Version 3 world loaded. The Warden's hall has opened."
    else if saveVersion == 4 then "Version 4 world loaded. A new day begins."
    else if saveVersion == 5 then "Version 5 world loaded. The quarry has opened."
    else if saveVersion == 6 then "Version 6 world loaded. The eastern outpost has opened."
    else if saveVersion == 7 then "Version 7 world loaded. New biomes and trade are available."
    else if saveVersion == 8 then "Version 8 world loaded. New gear and the Watchhorn are available."
    else if saveVersion == 9 then "Version 9 world loaded. Home Beacons and platforms are available."
    else if saveVersion == 10 then "Version 10 world loaded. Search untouched terrain for supply caches."
    else if saveVersion == 11 then "Version 11 world loaded. Cave habitats are active."
    else if saveVersion == 12 then "Version 12 world loaded. Fishing is available."
    else if saveVersion == 13 then "Version 13 world loaded. Sunleaf and alchemy are available."
    else if saveVersion == 14 then
      if world.tile ruinAltar == .ruinAltar then "Version 14 world loaded. The Ancient Ruins have opened."
      else "Version 14 world loaded. The existing terrain at the ruin site was preserved."
    else if saveVersion == 15 then "Version 15 world loaded. Rails and minecarts are available."
    else if saveVersion == 16 then "Version 16 world loaded. Grappling hooks are available."
    else if saveVersion == 17 then "Version 17 world loaded. Search untouched caves for Life Crystals."
    else if saveVersion == 18 then "Version 18 world loaded. The Eastern Highlands have opened."
    else if saveVersion == 19 then
      if world.tile mine.marker == .mineMarker then "Version 19 world loaded. The Deepsteel Mine has opened."
      else "Version 19 world loaded. The existing terrain at the mine site was preserved."
    else if saveVersion == 20 then
      if world.tile cavernLayout.altar == .cavernAltar then "Version 20 world loaded. The Deepstone Cavern has opened."
      else "Version 20 world loaded. The existing terrain at the cavern site was preserved."
    else if saveVersion == 21 then
      if world.tile magmaMarker == .magmaMarker then "Version 21 world loaded. The Magma Gallery has opened."
      else "Version 21 world loaded. The existing terrain at the gallery site was preserved."
    else if expandingUnderworld then "World loaded. The Underworld has opened."
    else if addedShadowTowers then "World loaded. More Underworld ruins can be explored."
    else if addedDungeon then "World loaded. The Dungeon has opened."
    else if addedLowerDungeon then "World loaded. The lower Dungeon has opened."
    else if addedBlueMoonDungeon then "World loaded. The Blue Moon Dungeon has opened."
    else if addedMagicMissileDungeon then "World loaded. The Magic Missile Dungeon has opened."
    else if addedDeepDungeon then "World loaded. The deep Dungeon has opened."
    else "World loaded."
  let npcImmunity ← if saveVersion < 41 then pure #[]
    else do decodeNpcImmunity (← json.getObjVal? "npcImmunity") saveVersion
  let npcBurns ← if saveVersion < 46 then pure #[]
    else do decodeNpcBurns (← json.getObjVal? "npcBurns") saveVersion
  let flowerFireballs ← if saveVersion < 47 then pure #[]
    else do decodeFlowerFireballs (← json.getObjVal? "flowerFireballs") saveVersion
  let hellwingShots ← if saveVersion < 47 then pure #[]
    else do decodeHellwingShots (← json.getObjVal? "hellwingShots") saveVersion
  let flamelashes ← if saveVersion < 47 then pure #[]
    else do decodeFlamelashes (← json.getObjVal? "flamelashes") saveVersion
  let lavaSlimes ← if saveVersion < 48 then pure #[] else do decodeLavaSlimes (← json.getObjVal? "lavaSlimes")
  let underworldFlyers ← if saveVersion < 48 then pure #[] else do decodeUnderworldFlyers (← json.getObjVal? "underworldFlyers")
  let fireImps ← if saveVersion < 48 then pure #[] else do decodeFireImps (← json.getObjVal? "fireImps")
  let burningSpheres ← if saveVersion < 48 then pure #[] else do decodeBurningSpheres (← json.getObjVal? "burningSpheres")
  let boneSerpent ← if saveVersion < 49 then pure none else do decodeBoneSerpent (← json.getObjVal? "boneSerpent")
  let underworldCritters ← if saveVersion < 49 then pure #[]
    else do decodeUnderworldCritters (← json.getObjVal? "underworldCritters")
  let wraiths ← if saveVersion < 51 then pure #[]
    else do decodeWraiths (← json.getObjVal? "wraiths")
  let nextWraithId : Nat ← if saveVersion < 51 then pure 1 else readField json "nextWraithId"
  let slowTicks : Nat ← if saveVersion < 51 then pure 0 else readField json "slowTicks"
  let hardmodeCavern ← if saveVersion < 52 then pure #[]
    else do decodeCavernEnemies (← json.getObjVal? "hardmodeCavern")
  let nextHardmodeCavernId : Nat ← if saveVersion < 52 then pure 1 else readField json "nextHardmodeCavernId"
  let cavernFlames ← if saveVersion < 52 then pure #[]
    else do decodeCavernFlames (← json.getObjVal? "cavernFlames") saveVersion
  let cursedInfernoTicks : Nat ← if saveVersion < 52 then pure 0 else readField json "cursedInfernoTicks"
  let cursedInfernoProgress : Nat ← if saveVersion < 52 then pure 0 else readField json "cursedInfernoProgress"
  let weakTicks : Nat ← if saveVersion < 52 then pure 0 else readField json "weakTicks"
  let retinazer ← if saveVersion < 53 then pure none else do decodeTwin (← json.getObjVal? "retinazer")
  let spazmatism ← if saveVersion < 53 then pure none else do decodeTwin (← json.getObjVal? "spazmatism")
  let twinsDefeated : Bool ← if saveVersion < 53 then pure false else readField json "twinsDefeated"
  let twinsPending : Bool ← if saveVersion < 53 then pure false else readField json "twinsPending"
  let mechanicalNights : Nat ← if saveVersion < 53 then pure 0 else readField json "mechanicalNights"
  let twinShots ← if saveVersion < 53 then pure #[]
    else do decodeTwinShots (← json.getObjVal? "twinShots") saveVersion
  let destroyer ← if saveVersion < 68 then pure none else do decodeDestroyer (← json.getObjVal? "destroyer")
  let destroyerPending : Bool ← if saveVersion < 68 then pure false else readField json "destroyerPending"
  let destroyerDefeated : Bool ← if saveVersion < 68 then pure false else readField json "destroyerDefeated"
  let destroyerProbes ← if saveVersion < 68 then pure #[] else decodeDestroyerProbes (← json.getObjVal? "destroyerProbes")
  let nextDestroyerProbeId : Nat ← if saveVersion < 68 then pure 1 else readField json "nextDestroyerProbeId"
  let skeletronPrime ← if saveVersion < 69 then pure none else do decodePrimeHead (← json.getObjVal? "skeletronPrime")
  let primeArms ← if saveVersion < 69 then pure #[] else do decodePrimeArms (← json.getObjVal? "primeArms")
  let primeShots ← if saveVersion < 69 then pure #[] else do decodePrimeShots (← json.getObjVal? "primeShots") saveVersion
  let primePending : Bool ← if saveVersion < 69 then pure false else readField json "primePending"
  let primeDefeated : Bool ← if saveVersion < 69 then pure false else readField json "primeDefeated"
  let queenSlime ← if saveVersion < 62 then pure none else do decodeQueenSlime (← json.getObjVal? "queenSlime")
  let queenMinions ← if saveVersion < 62 then pure #[] else do decodeQueenMinions (← json.getObjVal? "queenMinions")
  let queenShots ← if saveVersion < 62 then pure #[]
    else do decodeQueenShots (← json.getObjVal? "queenShots") saveVersion
  let queenSlimeDefeated : Bool ← if saveVersion < 62 then pure false else readField json "queenSlimeDefeated"
  let nextQueenMinionId : Nat ← if saveVersion < 62 then pure 1 else readField json "nextQueenMinionId"
  let dagger ← if saveVersion < 66 then pure none else decodeDagger (← json.getObjVal? "dagger") saveVersion
  let minionTarget ← if saveVersion < 66 then pure none else do
    let target ← json.getObjVal? "minionTarget"
    if target == .null then pure none else some <$> decodeNpcTarget target saveVersion
  let wallOfFlesh ← if saveVersion < 50 then pure none
    else do decodeWallOfFlesh (← json.getObjVal? "wallOfFlesh")
  let wallHungry ← if saveVersion < 50 then pure #[]
    else do decodeWallHungry (← json.getObjVal? "wallHungry")
  let wallLeeches ← if saveVersion < 50 then pure #[]
    else do decodeWallLeeches (← json.getObjVal? "wallLeeches")
  let wallLasers ← if saveVersion < 50 then pure #[]
    else do decodeWallLasers (← json.getObjVal? "wallLasers") saveVersion
  let horrified : Bool ← if saveVersion < 50 then pure false else readField json "horrified"
  let guideRespawnPending : Bool ← if saveVersion < 50 then pure false else readField json "guideRespawnPending"
  let voodooDolls ← if saveVersion < 50 then pure #[]
    else do decodeDroppedVoodooDolls (← json.getObjVal? "voodooDolls")
  let nextUnderworldNpcId : Nat ← if saveVersion < 48 then pure 1 else readField json "nextUnderworldNpcId"
  if magicMissiles.any (fun attack => attack.target.any fun target => match target with
      | .lavaSlime id | .underworldFlyer id | .fireImp id | .burningSphere id
      | .boneSerpent id _ | .underworldCritter id | .wallHungry id | .wallLeech id _ => id ≥ nextUnderworldNpcId
      | _ => false) then
    throw "A Magic Missile references an Underworld NPC ID that has not been allocated."
  if boneSerpent.any (fun serpent => magicMissiles.any fun attack => attack.target.any fun target => match target with
      | .boneSerpent id part => id == serpent.id && part ≥ serpent.segments.size
      | _ => false) then
    throw "A Magic Missile references a Bone Serpent segment that has not been allocated."
  if wallLeeches.any (fun leech => magicMissiles.any fun attack => attack.target.any fun target => match target with
      | .wallLeech id part => id == leech.id && part ≥ leech.segments.size
      | _ => false) then
    throw "A Magic Missile references a Wall leech segment that has not been allocated."
  if magicMissiles.any (fun attack => attack.target.any fun target => match target with
      | .wraith id => id ≥ nextWraithId
      | .hardmodeCavern id => id ≥ nextHardmodeCavernId
      | _ => false) then
    throw "A Magic Missile references a Hardmode NPC ID that has not been allocated."
  let demonSickles ← if saveVersion < 48 then pure #[]
    else do decodeDemonSickles (← json.getObjVal? "demonSickles") saveVersion
  let (world, furniture) := if saveVersion < 49 && !legacyEarly && world.height == underworldHeight then
      let actorBoxes := lavaSlimes.toList.map (·.hitBox) ++ underworldFlyers.toList.map (·.hitBox) ++
        fireImps.toList.map (·.hitBox) ++ burningSpheres.toList.map (·.hitBox)
      let flailBoxes := (blueMoon.toList ++ sunfury.toList).map (fun attack =>
        expandProjectileBox attack.flail.hitBox 80)
      let spellBoxes := magicMissiles.toList.map (·.missile.blastBox) ++
        flamelashes.toList.map (·.projectile.blastBox) ++ flowerFireballs.toList.map (·.hitBox) ++
        hellwingShots.toList.map (·.hitBox) ++ demonSickles.toList.map (·.hitBox)
      let shotBoxes := flailBoxes ++ spellBoxes.map (fun box => expandProjectileBox box 40) ++
        valor.toList.map (fun attack => expandProjectileBox attack.yoyo.hitBox 34) ++
        darkLance.toList.flatMap (fun lance => lance.hitBox player :: (lance.extensionHitBox player).toList)
      let fixed := fireImps.toList.filterMap (·.destination) ++
        (bombs.filter (·.fuse == 1)).toList.flatMap (fun bomb => (bomb.blastTiles world).toList)
      let clear := extensionAllowed world water magma walls (actorBoxes ++ shotBoxes) fixed
      let baseline := generateTerrain seed
      let pristine := fun position => clear position && baseline.tile position == .air &&
        !(adjacentTiles position).any (fun p => world.tile p == .torch)
      let uncharted := fun position => pristine position && (chart.tile? world position).isNone
      let (candidate, installed) := installHellforges seed world furniture uncharted
      let accepted := acceptRetrofit world candidate water walls
      if accepted.tiles != world.tiles && keepsTerrainProjectiles world accepted magma then
        (accepted, installed)
      else if furniture.any (·.kind == .hellforge) then (world, furniture)
      else
        -- A surveyed Underworld can still receive one forge when the remembered
        -- interior agrees with the original air and the house shell is intact.
        let layout := underworldLayout seed
        let intactHouses := (#[layout.westTower, layout.eastTower] ++ layout.extraTowers).filter
          (intactUnderworldHouse world baseline)
        let surveyed := fun (position : TilePos) => pristine position &&
          (chart.tile? world position).all (· == .air) &&
          intactHouses.any (fun tower => (tower.left : Int) < position.x && position.x < (tower.right : Int) &&
            (tower.roof : Int) < position.y && position.y < (tower.floor : Int))
        let (_, suggestions) := installHellforges seed world furniture surveyed
        match suggestions[furniture.size]? with
        | none => (world, furniture)
        | some forge =>
          let candidate := forge.cells.foldl (fun terrain cell => terrain.setTile cell .hellforge) world
          let accepted := acceptRetrofit world candidate water walls
          if accepted.tiles != world.tiles && keepsTerrainProjectiles world accepted magma then
            (accepted, furniture.push forge)
          else (world, furniture)
    else (world, furniture)
  let world := if saveVersion == 50 && hardmode && !legacyEarly && world.height == underworldHeight then
      -- The old format did not keep the victory tick. Use the same bands across its checkpoints.
      let converted := convertHardmodeTerrain world seed (hardmodeConversionEntropy seed 0)
      -- Preserve an in-progress tool hit on the exact material it was started on.
      tileHits.foldl (fun terrain hit => terrain.setTile hit.position hit.tile) converted
    else world
  let (world, honey) ← if saveVersion < 61 then
      if legacyEarly then pure (world, default)
      else
        let baseline := (generateWorld seed).1
        pure (installHoneyPool world water magma seed fun pos =>
          world.tile pos == baseline.tile pos && (chart.tile? world pos).isNone &&
          walls[pos.y.toNat * world.width + pos.x.toNat]?.getD 0 == 0 &&
          !occupiedBodies.any (·.overlapsTile pos))
    else do
      let data ← json.getObjVal? "honey"
      if (← data.getArr?).isEmpty then pure (world, default)
      else
        let honey ← decodeLiquid data world "honey" false (some ⟨Water.liquidBarrier water magma⟩)
        pure (world, honey)
  let world := if saveVersion < 70 && !legacyEarly then
      let baseline := (generateWorld seed).1
      installUndergroundJungle world seed fun position =>
        world.tile position == baseline.tile position && (chart.tile? world position).isNone &&
          walls[position.y.toNat * world.width + position.x.toNat]?.getD 0 == 0 &&
          water.level world position == 0 && magma.level world position == 0 && honey.level world position == 0 &&
          !occupiedBodies.any (·.overlapsTile position) && !tileHits.any (·.position == position)
    else world
  match fishing with
  | .soaking position _ | .hooked position .. =>
    if !(fishingLiquidAt world water magma player position honey).any (fun (surface, _) => surface == position) then
      throw "The Classic fishing float is outside a valid liquid surface."
  | _ => pure ()
  let angler : AnglerState ←
    if saveVersion < 59 then
      pure (if legacyEarly then {} else
        { questIndex := chooseAnglerQuest seed weather.day hardmode (altarTile seed == .crimsonAltar)
            (eyeDefeated || evilBossDefeated || skeletronDefeated || hardmode) (anglerCoastExists world seed)
            (!honey.levels.isEmpty) })
    else do
      let data ← json.getObjVal? "angler"
      let rescued : Bool ← readField data "rescued"
      let questIndex : Nat ← readField data "questIndex"
      let completed : Nat ← readField data "completed"
      let turnedIn : Bool ← readField data "turnedIn"
      let position ← match ← data.getObjVal? "position" with
        | .null => pure none
        | value => some <$> decodePosition value
      let home ← match ← data.getObjVal? "home" with
        | .null => pure none
        | value => some <$> decodePosition value
      if questIndex ≥ anglerQuests.size || completed > weather.day + 1 ||
          (turnedIn && (!rescued || completed == 0)) || (rescued && position.isNone) ||
          position.any (fun pos => !world.contains pos) ||
          home.any (fun pos => !rescued || !world.contains pos) ||
          (legacyEarly && (rescued || position.isSome || completed > 0 || questIndex != 0)) then
        throw "Invalid Angler quest or NPC state."
      pure { rescued, questIndex, completed, turnedIn, position, home }
  let state : GameState := {
    seed, worldName, worldCreated, worldFavorite, world, player, inventory, cursorStack, selected, combat, warden,
    surface, weather, crateLifeCrystals, angler
    chests, homeDoor, guide, townGuide, water, magma, honey, breath, quarry, sentry, storm, projectiles, shortsword, bowCooldown
    chart, merchantStock, merchantDoor, merchant, equipment, wingedSlime, assassinDash, weaponArmorStats, raid,
    homeAnchor, ecology, fishing
    crops, gills, barkskin, cratePotion, fishingPotion, sonarPotion, potionSickness, honeyBuff,
    ruin, carts, ride, grapple, cavern, walls, furniture, bedSpawnPoint
    legacyEarly, respawnTicks, hotTileTicks, burningTicks, toolCooldown, tileHits, gelDrops, nightDrops, eye, eyeDefeated
    evilRelicsBroken, bombs, eater, brain, evilBossDefeated, evilEnemies
    dungeonInstalled, lowerDungeonInstalled, blueMoonDungeonInstalled, magicMissileDungeonInstalled
    deepDungeonInstalled, shadowKeyRecovery
    skeletron, skeletronDefeated
    oldManWaiting, dungeonGuardian, dungeonGuardianHealth
    dungeonSlimes, darkCasters, waterSpheres, nextDarkCasterId, nextWaterSphereId
    angryBones, nextAngryBonesId, cursedSkulls, nextCursedSkullId, cursedTicks
    lavaSlimes, underworldFlyers, fireImps, burningSpheres, boneSerpent, underworldCritters
    nextUnderworldNpcId, demonSickles, darknessTicks, obsidianSkinTicks, netSwingTicks
    wallOfFlesh, wallHungry, wallLeeches, wallLasers, hardmode, altarsSmashed, hardmodeOreMask
    wraiths, nextWraithId, slowTicks
    hardmodeCavern, nextHardmodeCavernId, cavernFlames
    cursedInfernoTicks, cursedInfernoProgress, weakTicks
    retinazer, spazmatism, twinsDefeated, twinsPending, mechanicalNights, twinShots
    destroyer, destroyerDefeated, destroyerPending, destroyerProbes, nextDestroyerProbeId
    skeletronPrime, primeArms, primeShots, primePending, primeDefeated
    queenSlime, queenMinions, queenShots, queenSlimeDefeated, nextQueenMinionId, dagger, minionTarget
    horrified, tongued, guideRespawnPending, voodooDolls
    aquaStreams, aquaBurst, aquaAnimation, aquaShotDelay, mana, manaRegenDelay, manaRegenProgress, npcImmunity, blueMoon
    magicMissiles, valor, sunfury, darkLance, npcBurns, flowerFireballs, hellwingShots, flamelashes, status }
  let state ← restorePlantera state json saveVersion
  let state ← restoreTempleFight state json saveVersion
  validateTempleFight state
  if !state.validNpcImmunity then throw "NPC hit cooldown references an inactive target."
  if !state.validDagger then throw "Invalid Enchanted Dagger or minion target state."
  if !state.validBlueMoon then throw "Invalid Blue Moon attack motion, aim, slot or NPC cooldown."
  if !state.validMagicMissiles then throw "Invalid Magic Missile motion, aim, slot or target."
  if !state.validValor then throw "Invalid Valor motion, aim, slot or player state."
  if !state.validSunfury then throw "Invalid Sunfury motion, aim, slot or NPC cooldown."
  if !state.validDarkLance then throw "Invalid Dark Lance motion, facing, slot or player state."
  if !state.validNpcBurns then throw "NPC burn references an inactive or duplicate target."
  if !state.validUnderworldProjectiles then throw "Invalid Underworld projectile motion, aim, slot or cooldown."
  if !state.validUnderworldEnemies then throw "Invalid Underworld enemy, NPC ID or Demon Sickle state."
  if !state.validWallEncounter then throw "Invalid Wall of Flesh, minion, laser, debuff or Guide Voodoo Doll state."
  if !state.validHardmode then throw "Invalid Hardmode progress, Wraith or Slow state."
  if !state.validTwins then throw "Invalid Twins or mechanical projectile state."
  if !state.validDestroyer then throw "Invalid Destroyer encounter or Probe state."
  if !state.validSkeletronPrime then throw "Invalid Skeletron Prime encounter."
  if !state.validPlantera then throw "Invalid Plantera bulb or encounter state."
  if !state.validQueenSlime then throw "Invalid Queen Slime encounter or projectile state."
  let cavernIds := hardmodeCavern.map (·.id)
  if nextHardmodeCavernId == 0 || cavernIds.toList.eraseDups.length != cavernIds.size ||
      cavernIds.any (fun id => id == 0 || id ≥ nextHardmodeCavernId) ||
      (!hardmode && (!hardmodeCavern.isEmpty || !cavernFlames.isEmpty || nextHardmodeCavernId != 1)) ||
      !hardmodeCavern.all (·.valid world) ||
      !cavernFlames.all (fun flame => flame.slot.isSome && flame.valid world) then
    throw "Invalid Hardmode cavern enemy, NPC ID or Clinger flame."
  if cursedInfernoTicks > 180 || cursedInfernoProgress ≥ 5 ||
      (cursedInfernoTicks == 0 && cursedInfernoProgress != 0) || weakTicks > maxWeakTicks ||
      (weakTicks > 0 && equipment.accessory == some .vitamins) ||
      (!hardmode && weakTicks > 0) || (saveVersion < 53 && !hardmode && cursedInfernoTicks > 0) ||
      (respawnTicks > 0 && (cursedInfernoTicks > 0 || weakTicks > 0)) then
    throw "Invalid Cursed Inferno or Weak debuff."
  if !state.validProjectileSlots then throw "Projectile slots are duplicated in the shared pool."
  if !weaponArmorStats.isEmpty then
    let slots := state.armedProjectileSlots
    if weaponArmorStats.any (fun entry => !slots.contains entry.1) then
      throw "Projectile armor stats refer to a missing player projectile."
  if assassinDash != {} && (ride.isSome || grapple != .idle || tongued || respawnTicks > 0) then
    throw "A dashing player must be alive and free to move."
  if let some mount := wingedSlime then
    if ride.isSome || grapple != .idle || tongued || respawnTicks > 0 || combat.health == 0 ||
        assassinDash != {} || player.collides world wingedSlimeHeightBoost ||
        player.vx != (mount.vx * 16).toInt32.toInt || player.vy != (mount.vy * 16).toInt32.toInt then
      throw "Winged Slime mount does not match the player's movement state."
  return retrofitTemple state saveVersion occupiedBodies

private def openTemporaryFile (directory : System.FilePath) : IO (IO.FS.Handle × System.FilePath) := do
  let pid ← IO.Process.getPID
  let timestamp ← IO.monoNanosNow
  for attempt in [:16] do
    let path := directory / s!".lean4terria-save.{pid}.{timestamp}.{attempt}.tmp"
    try
      let handle ← IO.FS.Handle.mk path .writeNew
      return (handle, path)
    catch
      | .alreadyExists .. => pure ()
      | error => throw error
  throw (IO.userError "Could not create an unused temporary save file.")

/-- Flush a uniquely named file beside the destination before atomically replacing the save. -/
def write (path : System.FilePath) (state : GameState) : IO (Except String Unit) := do
  try
    let directory := path.parent.getD "."
    IO.FS.createDirAll directory
    let (handle, temporary) ← openTemporaryFile directory
    try
      handle.putStr (encode state)
      handle.flush
      IO.FS.rename temporary path
    catch error =>
      try IO.FS.removeFile temporary catch _ => pure ()
      throw error
    return .ok ()
  catch error =>
    return .error s!"Could not write save ({error})."

/-- Read a save without altering the original file, including when validation fails. -/
def read (path : System.FilePath) : IO (Except String GameState) := do
  try
    return decode (← IO.FS.readFile path)
  catch error =>
    return .error s!"Could not read save ({error})."

end Lean4Terria.Save
