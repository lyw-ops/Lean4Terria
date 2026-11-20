import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalBestiaryProgress.Provider
import Lean4Terria.OriginalBestiaryProgress.Database

/-!
# Bestiary completion (Terraria 1.4.5.8 desktop, single player)

Hand port of `Main.GetBestiaryProgressReport()` and `BestiaryUnlockProgressReport.CompletionPercent`:
every entry of `Main.BestiaryDB` (546 entries, copied from the real database into `Database.lean`) counts 1 when its
`UIInfoProvider.GetEntryUICollectionInfo().UnlockState > 0`; the percent is the Single division
`amount / total` (1 when there are no entries). Unlock states come from the world's bestiary section
(`BestiaryUnlocksTracker.Load`: kills `(string, int)` pairs, sightings, chats — each a length-prefixed list) and the six
provider kinds ported from their CIL, including the salamander / shelly / crawdad providers that depend on
`NPC.cavernMonsterType`, which `NPC.SetWorldSpecificMonstersByWorldID` derives from the world id.

Evidence: `.lake/original-bestiary-progress/`. The world id and the section bytes are inputs (read from the world file
header / section table by the caller).
-/

namespace Lean4Terria.OriginalBestiaryProgress

structure Tracker where
  /-- `NPCKillsTracker._killCountsByNpcId` (a later duplicate key overwrites) -/
  kills : Array (String × Int) := #[]
  sights : Array String := #[]
  chats : Array String := #[]
  deriving Repr, Inhabited

def Tracker.killCount (t : Tracker) (id : String) : Int :=
  match t.kills.findIdx? (·.1 == id) with
  | some i => t.kills[i]!.2
  | none => 0

/-- `BinaryReader` over the section bytes. -/
private structure Reader where
  data : ByteArray
  pos : Nat := 0

private abbrev Rd := StateT Reader (Except String)

private def readByte : Rd UInt8 := do
  let r ← get
  if h : r.pos < r.data.size then
    set { r with pos := r.pos + 1 }
    pure r.data[r.pos]
  else throw "unexpected end of stream"

/-- `ReadInt32`: little endian, two's complement. -/
private def readInt32 : Rd Int := do
  let mut v : Nat := 0
  for k in [0:4] do
    let b ← readByte
    v := v + b.toNat * 256 ^ k
  pure (if v ≥ 2 ^ 31 then (v : Int) - 2 ^ 32 else (v : Int))

/-- `ReadString`: 7-bit encoded byte length, then UTF-8. -/
private def readString : Rd String := do
  let mut len : Nat := 0
  let mut shift := 0
  for _ in [0:5] do
    let b ← readByte
    len := len + (b.toNat % 128) * 2 ^ shift
    shift := shift + 7
    if b.toNat < 128 then break
  let mut bytes := ByteArray.empty
  for _ in [0:len] do bytes := bytes.push (← readByte)
  match String.fromUTF8? bytes with
  | some s => pure s
  | none => throw "invalid UTF-8"

private def readCount : Rd Nat := do
  let n ← readInt32
  pure n.toNat

/-- `BestiaryUnlocksTracker.Load`: kills, sightings, chats, in that order. A negative count reads nothing. -/
def parseSection (data : ByteArray) : Except String Tracker :=
  let go : Rd Tracker := do
    let mut t : Tracker := {}
    let nk ← readCount
    for _ in [0:nk] do
      let name ← readString
      let kills ← readInt32
      match t.kills.findIdx? (·.1 == name) with
      | some i => t := { t with kills := t.kills.set! i (name, kills) }
      | none => t := { t with kills := t.kills.push (name, kills) }
    let ns ← readCount
    for _ in [0:ns] do
      let name ← readString
      if !t.sights.contains name then t := { t with sights := t.sights.push name }
    let nc ← readCount
    for _ in [0:nc] do
      let name ← readString
      if !t.chats.contains name then t := { t with chats := t.chats.push name }
    pure t
  (go.run { data }).map (·.1)

/-- `NPC.SetWorldSpecificMonstersByWorldID`: the 2×3 `NPC.cavernMonsterType` for a world id. -/
def cavernMonsterType (worldId : Int) : Array (Array Nat) := Id.run do
  let mut rng := UnifiedRandom.seed worldId
  let (a, r) := rng.next 0 3
  rng := r
  let (b, r) := rng.next 0 3
  rng := r
  let mut first := a
  let second := b
  while first == second do
    let (v, r) := rng.next 0 3
    rng := r
    first := v
  let mut out : Array (Array Nat) := #[]
  for i in [0:2] do
    let kind := if i == 1 then second else first
    let mut row : Array Nat := #[]
    for _ in [0:3] do
      let (lo, hi) : Int × Int := if kind == 0 then (494, 496) else if kind == 1 then (496, 498) else (498, 507)
      let (v, r) := rng.next lo hi
      rng := r
      row := row.push v.toNat
    out := out.push row
  return out

/-- `CommonEnemyUICollectionInfoProvider.GetUnlockStateByKillCount(kills, quick, needed)` (signed int division). -/
def unlockByKills (kills : Int) (quick : Bool) (needed : Int) : Nat :=
  let half := needed.tdiv 2
  let fifth := needed.tdiv 5
  if quick && kills > 0 then 4
  else if kills ≥ needed then 4
  else if kills ≥ half then 3
  else if kills ≥ fifth then 2
  else if kills ≥ 1 then 1
  else 0

private def creditOf (netId : Nat) : String :=
  match Database.cavernCreditIds.find? (·.1 == netId) with
  | some (_, id) => id
  | none => ""

/-- The kill-based state of a Salamander / Shelly / Crawdad entry (`GetEntryUICollectionInfo`). -/
private def salamanderState (t : Tracker) (cavern : Array (Array Nat)) (id : String) (needed : Int) : Nat :=
  let state := unlockByKills (t.killCount id) false needed
  let included := cavern.any fun row => row.any fun n => creditOf n == id
  if included then state
  else Id.run do
    let mut lowest := 4
    for row in cavern do
      for n in row do
        let st := unlockByKills (t.killCount (creditOf n)) false needed
        if lowest > st then lowest := st
    return lowest

private def critterState (t : Tracker) (id : String) : Nat := if t.sights.contains id then 4 else 0

/-- `Provider.GetEntryUICollectionInfo().UnlockState`. -/
partial def unlockState (t : Tracker) (cavern : Array (Array Nat)) : Provider → Nat
  | .common id quick needed => unlockByKills (t.killCount id) quick needed
  | .critter id => critterState t id
  | .town id => if t.chats.contains id then 4 else 0
  | .gold goldId normal =>
    let g := critterState t goldId
    let best := normal.foldl (fun acc id => if critterState t id > acc then critterState t id else acc) (if g > 0 then g else 0)
    if best == 0 then 0
    else if Database.goldCreditIds.any (fun id => critterState t id > 0) then best else 0
  | .highest main ps =>
    let first := if h : main < ps.size then unlockState t cavern ps[main] else 0
    ps.foldl (fun acc p => let st := unlockState t cavern p; if acc < st then st else acc) first
  | .salamander id needed => salamanderState t cavern id needed

structure Report where
  total : Nat
  amount : Nat
  deriving Repr, BEq

/-- `CompletionPercent` in Single precision: `amount / total`, or 1 with no entries. -/
def Report.percent (r : Report) : Float32 :=
  if r.total == 0 then 1 else Float32.ofNat r.amount / Float32.ofNat r.total

/-- `Main.GetBestiaryProgressReport()` for a tracker and a world id. -/
def progressReport (t : Tracker) (worldId : Int) : Report :=
  let cavern := cavernMonsterType worldId
  let amount := Database.entries.foldl (fun acc p => if unlockState t cavern p > 0 then acc + 1 else acc) 0
  { total := Database.entries.size, amount }

end Lean4Terria.OriginalBestiaryProgress
