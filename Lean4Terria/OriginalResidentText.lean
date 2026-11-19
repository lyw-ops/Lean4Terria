import Std.Data.HashMap
import Lean4Terria.OriginalForestGuideDialogue

/-!
# Resident dialogue text (Terraria 1.4.5.8 desktop, en-US): read in place from the player's own `Terraria.exe`

No dialogue text is bundled here. The caller hands over the bytes of the installed `Terraria.exe`; this module finds
its CLR manifest resources with a pure `ByteArray` reader (PE header → sections → CLI header → metadata root → `#~`
tables → `ManifestResource` rows → the CLI resource directory), parses the en-US localization JSON the way vanilla
loads it, and formats the Guide / Old Man / Help lines with the vanilla global substitutions. Checked head to head
against the vanilla assembly run in a private host (`.lake/original-resident-text/1.4.5.8`).

IL read (1.4.5.8): `LanguageManager.LoadLanguage` / `LoadFilesForCulture` / `GetLanguageFilesForCulture` /
`LoadLanguageFromFileTextJson` / `UpdateTextValue` / `ProcessCopyCommandsInTexts` (+ its match evaluator) /
`LoadFromContentSources` / `GetTextValue` / `Exists`, `Utils.ReadEmbeddedResource`, `LocalizedText.SetValue` /
`Value` / `GetValueIfConditionsMet`, `VariableText.TryCreate` (+ evaluator, `.cctor` regex) / `TryFormat` /
`CheckConditionsAndLoadArgs`, `Lang.dialog`, `Lang.InitGlobalSubstitutions` (all 65 lambdas),
`Lang.GetGlobalSubstitution`, `NPC.GetFirstNPCNameOrNull`, `PlayerInput.Initialize` / `Load` / `Reset` /
`GenerateInputTag_ForCurrentGamemode` / `GenerateRawInputList` / `LocalizeKey`, `Main..cctor` (`cInv`,
`FavoriteKey`), `NPC.GetChat` (the Old Man branch), `Main.HelpText` (the `GuideHelpText.Help_<n>` probe).

## The vanilla pipeline this follows

1. Files: the manifest resource names, in `ManifestResource` table order, that start with
   `"Terraria.Localization.Content." + culture` and end with `".json"` (en-US 1.4.5.8: `Items`, the bare `en-US.json`,
   `Town`, `Legacy`, `Projectiles`, `NPCs`, `Game`). Each is read with a default `StreamReader` (UTF-8, BOM dropped).
2. Each file is Newtonsoft `Dictionary<string, Dictionary<string, string>>` (comments and trailing commas accepted; a
   repeated property keeps the last value at the first position — 1.4.5.8 has three such keys). Key =
   `Category + "." + Key`; an existing key is overwritten (`SetValue`), a new one appended.
3. Resource packs (`LoadFromContentSources`) are not modelled: vanilla's own Content folder has no `Localization/`.
4. `ProcessCopyCommandsInTexts`: in insertion order, each text has `{$(\w+\.\w+)}` replaced by the referenced
   text's current unformatted value (or by the bare key when it does not exist), up to 100 rounds until stable.
5. `VariableText`: `{(\?!?)?([a-zA-Z][\w\.]*)}` — `{?X}` / `{?!X}` are conditions (removed from the format, the
   substitution must be the boolean `true` / `false`), `{X}` becomes `{i}` (first-seen order). `Value` runs
   `StringBuilder.AppendFormat` with the substitutions; if a condition fails or a variable is `null` the raw text
   (braces and all) is shown. A text without any match is shown as is.
6. `Language.GetTextValue(key)` returns the key itself when the key does not exist; `Lang.dialog(n)` is
   `GetTextValue("LegacyDialog." + n)`.

## Boundaries (`Boundary`), never guesses

* the image is not a PE32/PE32+ CLR assembly this reader understands, or a resource lies outside the resource
  directory (`image`, `resource`);
* a JSON construct the original files do not use (single quotes, unquoted names, non-string values, `$` variant keys,
  non-ASCII whitespace, a file shorter than two characters — vanilla stops loading the remaining files there) (`json`,
  `resource`);
* a `\w` test on a non-ASCII character inside a candidate placeholder (.NET's Unicode classes are not transcribed);
* a substitution vanilla registers but the caller's environment does not supply (`unknownSubstitution`) — e.g.
  `{Homeless}` without the talk NPC's state; gamepad input tags; a key-binding trigger vanilla does not know;
* a composite-format item other than `{n}` with `n` in range, or an unpaired brace (vanilla throws `FormatException`)
  (`format`);
* `missingKey` from the strict `text` (the vanilla-faithful `getTextValue` returns the key instead).
-/

namespace Lean4Terria.OriginalResidentText

open OriginalForestGuideDialogue (Line Rng)

/-- Why a text could not be produced exactly as vanilla would. -/
inductive Boundary where
  | image (what : String)
  | resource (name what : String)
  | json (file : String) (offset : Nat) (what : String)
  | missingKey (key : String)
  | unknownSubstitution (key name : String)
  | format (key what : String)
  | unsupported (what : String)
  deriving Repr, BEq, Inhabited

def Boundary.toString : Boundary → String
  | .image w => s!"image: {w}"
  | .resource n w => s!"resource {n}: {w}"
  | .json f o w => s!"json {f} at byte {o}: {w}"
  | .missingKey k => s!"no text for key {k}"
  | .unknownSubstitution k n => s!"{k}: substitution {n} is not supplied by the environment"
  | .format k w => s!"{k}: {w}"
  | .unsupported w => s!"unsupported: {w}"

instance : ToString Boundary := ⟨Boundary.toString⟩

abbrev R := Except Boundary

/-! ## Little-endian reads with bounds -/

namespace Bytes

def u8 (b : ByteArray) (o : Nat) : R Nat :=
  if h : o < b.size then .ok b[o].toNat else .error (.image s!"read past the end of the file at {o}")

def u16 (b : ByteArray) (o : Nat) : R Nat := do return (← u8 b o) + (← u8 b (o + 1)) * 0x100

def u32 (b : ByteArray) (o : Nat) : R Nat := do return (← u16 b o) + (← u16 b (o + 2)) * 0x10000

def u64 (b : ByteArray) (o : Nat) : R Nat := do return (← u32 b o) + (← u32 b (o + 4)) * 0x100000000

/-- A NUL-terminated UTF-8 string starting at `o` (at most `limit` bytes before the NUL). -/
def cstr (b : ByteArray) (o : Nat) (limit : Nat := 4096) : R (String × Nat) := do
  let mut q := o
  while (← u8 b q) != 0 do
    q := q + 1
    if q - o > limit then throw (.image s!"unterminated string at {o}")
  match String.fromUTF8? (b.extract o q) with
  | some s => return (s, q - o)
  | none => throw (.image s!"string at {o} is not UTF-8")

end Bytes

open Bytes

/-! ## PE / CLR metadata: `ManifestResource` rows and their bytes -/

structure Section where
  virtualAddress : Nat
  virtualSize : Nat
  rawPointer : Nat
  rawSize : Nat
  deriving Repr

/-- One `ManifestResource` (table 0x28) row, with the file range of its bytes when it is embedded. -/
structure Resource where
  name : String
  /-- The row's `Offset` column (into the CLI resource directory). -/
  offset : Nat
  flags : Nat
  /-- The `Implementation` coded index: 0 for a resource embedded in this file. -/
  implementation : Nat
  /-- File offset of the first byte (after the 4-byte length prefix). -/
  dataStart : Nat
  length : Nat
  deriving Repr

structure Image where
  bytes : ByteArray
  sections : Array Section
  resourcesRva : Nat
  resourcesSize : Nat
  /-- Every manifest resource, in table order (= `Assembly.GetManifestResourceNames()` order). -/
  resources : Array Resource

def rvaToOffset (secs : Array Section) (rva : Nat) : R Nat :=
  match secs.find? (fun s => s.virtualAddress ≤ rva && rva < s.virtualAddress + max s.virtualSize s.rawSize) with
  | some s =>
    if rva - s.virtualAddress < s.rawSize then .ok (s.rawPointer + (rva - s.virtualAddress))
    else .error (.image s!"RVA {rva} lies in the uninitialised tail of a section")
  | none => .error (.image s!"RVA {rva} is in no section")

/-- A metadata table column: fixed width, heap index, simple table index or coded index (tag bits, tables). -/
inductive Col where
  | fixed (n : Nat)
  | str | guid | blob
  | table (t : Nat)
  | coded (bits : Nat) (tables : List Nat)

private def typeDefOrRef := Col.coded 2 [0x02, 0x01, 0x1B]
private def hasConstant := Col.coded 2 [0x04, 0x08, 0x17]
private def hasCustomAttribute := Col.coded 5
  [0x06, 0x04, 0x01, 0x02, 0x08, 0x09, 0x0A, 0x00, 0x0E, 0x17, 0x14, 0x11, 0x1A, 0x1B, 0x20, 0x23, 0x26, 0x27, 0x28,
   0x2A, 0x2C, 0x2B]
private def hasFieldMarshal := Col.coded 1 [0x04, 0x08]
private def hasDeclSecurity := Col.coded 2 [0x02, 0x06, 0x20]
private def memberRefParent := Col.coded 3 [0x02, 0x01, 0x1A, 0x06, 0x1B]
private def hasSemantics := Col.coded 1 [0x14, 0x17]
private def methodDefOrRef := Col.coded 1 [0x06, 0x0A]
private def memberForwarded := Col.coded 1 [0x04, 0x06]
private def implementation := Col.coded 2 [0x26, 0x23, 0x27]
private def customAttributeType := Col.coded 3 [0x06, 0x0A]
private def resolutionScope := Col.coded 2 [0x00, 0x1A, 0x23, 0x01]

/-- ECMA-335 II.22 row layouts of tables 0x00 … 0x28 (`ManifestResource`), in table order. -/
private def schema : Array (List Col) := #[
  [.fixed 2, .str, .guid, .guid, .guid],                                  -- 0x00 Module
  [resolutionScope, .str, .str],                                          -- 0x01 TypeRef
  [.fixed 4, .str, .str, typeDefOrRef, .table 0x04, .table 0x06],         -- 0x02 TypeDef
  [.table 0x04],                                                          -- 0x03 FieldPtr
  [.fixed 2, .str, .blob],                                                -- 0x04 Field
  [.table 0x06],                                                          -- 0x05 MethodPtr
  [.fixed 4, .fixed 2, .fixed 2, .str, .blob, .table 0x08],               -- 0x06 MethodDef
  [.table 0x08],                                                          -- 0x07 ParamPtr
  [.fixed 2, .fixed 2, .str],                                             -- 0x08 Param
  [.table 0x02, typeDefOrRef],                                            -- 0x09 InterfaceImpl
  [memberRefParent, .str, .blob],                                         -- 0x0A MemberRef
  [.fixed 2, hasConstant, .blob],                                         -- 0x0B Constant
  [hasCustomAttribute, customAttributeType, .blob],                       -- 0x0C CustomAttribute
  [hasFieldMarshal, .blob],                                               -- 0x0D FieldMarshal
  [.fixed 2, hasDeclSecurity, .blob],                                     -- 0x0E DeclSecurity
  [.fixed 2, .fixed 4, .table 0x02],                                      -- 0x0F ClassLayout
  [.fixed 4, .table 0x04],                                                -- 0x10 FieldLayout
  [.blob],                                                                -- 0x11 StandAloneSig
  [.table 0x02, .table 0x14],                                             -- 0x12 EventMap
  [.table 0x14],                                                          -- 0x13 EventPtr
  [.fixed 2, .str, typeDefOrRef],                                         -- 0x14 Event
  [.table 0x02, .table 0x17],                                             -- 0x15 PropertyMap
  [.table 0x17],                                                          -- 0x16 PropertyPtr
  [.fixed 2, .str, .blob],                                                -- 0x17 Property
  [.fixed 2, .table 0x06, hasSemantics],                                  -- 0x18 MethodSemantics
  [.table 0x02, methodDefOrRef, methodDefOrRef],                          -- 0x19 MethodImpl
  [.str],                                                                 -- 0x1A ModuleRef
  [.blob],                                                                -- 0x1B TypeSpec
  [.fixed 2, memberForwarded, .str, .table 0x1A],                         -- 0x1C ImplMap
  [.fixed 4, .table 0x04],                                                -- 0x1D FieldRVA
  [.fixed 4, .fixed 4],                                                   -- 0x1E EncLog
  [.fixed 4],                                                             -- 0x1F EncMap
  [.fixed 4, .fixed 2, .fixed 2, .fixed 2, .fixed 2, .fixed 4, .blob, .str, .str],  -- 0x20 Assembly
  [.fixed 4],                                                             -- 0x21 AssemblyProcessor
  [.fixed 4, .fixed 4, .fixed 4],                                         -- 0x22 AssemblyOS
  [.fixed 2, .fixed 2, .fixed 2, .fixed 2, .fixed 4, .blob, .str, .str, .blob],     -- 0x23 AssemblyRef
  [.fixed 4, .table 0x23],                                                -- 0x24 AssemblyRefProcessor
  [.fixed 4, .fixed 4, .fixed 4, .table 0x23],                            -- 0x25 AssemblyRefOS
  [.fixed 4, .str, .blob],                                                -- 0x26 File
  [.fixed 4, .fixed 4, .str, .str, implementation],                       -- 0x27 ExportedType
  [.fixed 4, .fixed 4, .str, implementation]]                             -- 0x28 ManifestResource

/-- Parse the PE / CLI headers and every `ManifestResource` row of a .NET assembly image. -/
def Image.parse (b : ByteArray) : R Image := do
  unless (← u16 b 0) == 0x5A4D do throw (.image "no MZ header")
  let pe ← u32 b 0x3C
  unless (← u32 b pe) == 0x4550 do throw (.image "no PE signature")
  let nsec ← u16 b (pe + 6)
  let optSize ← u16 b (pe + 20)
  let opt := pe + 24
  let ddir ← match (← u16 b opt) with
    | 0x10B => pure (opt + 96)
    | 0x20B => pure (opt + 112)
    | m => throw (.image s!"optional header magic {m}")
  unless (← u32 b (ddir - 4)) > 14 do throw (.image "no CLI header data directory")
  let mut secs : Array Section := #[]
  for i in [0:nsec] do
    let s := opt + optSize + 40 * i
    secs := secs.push { virtualSize := ← u32 b (s + 8), virtualAddress := ← u32 b (s + 12),
                        rawSize := ← u32 b (s + 16), rawPointer := ← u32 b (s + 20) }
  let cli ← rvaToOffset secs (← u32 b (ddir + 14 * 8))
  let md ← rvaToOffset secs (← u32 b (cli + 8))
  let resRva ← u32 b (cli + 24)
  let resSize ← u32 b (cli + 28)
  unless (← u32 b md) == 0x424A5342 do throw (.image "no metadata root signature")
  let verLen ← u32 b (md + 12)
  let hdr := md + 16 + verLen
  let nStreams ← u16 b (hdr + 2)
  let mut q := hdr + 4
  let mut tablesAt : Option Nat := none
  let mut stringsAt : Option Nat := none
  for _ in [0:nStreams] do
    let off ← u32 b q
    let (name, len) ← cstr b (q + 8) 32
    if name == "#~" then tablesAt := some (md + off)
    if name == "#-" then throw (.image "uncompressed (#-) metadata tables")
    if name == "#Strings" then stringsAt := some (md + off)
    q := q + 8 + (len + 1 + 3) / 4 * 4
  let some t := tablesAt | throw (.image "no #~ stream")
  let some strings := stringsAt | throw (.image "no #Strings stream")
  let heap ← u8 b (t + 6)
  if heap &&& 0x40 != 0 then throw (.image "metadata tables with extra data")
  let valid ← u64 b (t + 8)
  let mut rows : Array Nat := Array.replicate 64 0
  let mut p := t + 24
  for tb in [0:64] do
    if (valid >>> tb) % 2 == 1 then
      rows := rows.set! tb (← u32 b p)
      p := p + 4
  let width : Col → Nat
    | .fixed n => n
    | .str => if heap &&& 1 != 0 then 4 else 2
    | .guid => if heap &&& 2 != 0 then 4 else 2
    | .blob => if heap &&& 4 != 0 then 4 else 2
    | .table x => if rows[x]! > 0xFFFF then 4 else 2
    | .coded bits tbs => if tbs.foldl (fun m x => max m rows[x]!) 0 ≥ 2 ^ (16 - bits) then 4 else 2
  let rowSize (tb : Nat) : Nat := (schema[tb]!.map width).foldl (· + ·) 0
  -- the ManifestResource table starts after every earlier table's rows
  let mrStart := (List.range 0x28).foldl (fun acc tb => acc + rowSize tb * rows[tb]!) p
  let rd (o w : Nat) : R Nat := if w == 4 then u32 b o else u16 b o
  let res ← rvaToOffset secs resRva
  let mut out : Array Resource := #[]
  let cols := schema[0x28]!.map width
  for i in [0:rows[0x28]!] do
    let r := mrStart + rowSize 0x28 * i
    let offset ← u32 b r
    let flags ← u32 b (r + 4)
    let nameIdx ← rd (r + 8) cols[2]!
    let impl ← rd (r + 8 + cols[2]!) cols[3]!
    let (name, _) ← cstr b (strings + nameIdx)
    let mut dataStart := 0
    let mut length := 0
    if impl == 0 then
      unless offset + 4 ≤ resSize do throw (.resource name s!"offset {offset} outside the {resSize}-byte resource directory")
      length ← u32 b (res + offset)
      dataStart := res + offset + 4
      unless offset + 4 + length ≤ resSize && dataStart + length ≤ b.size do
        throw (.resource name s!"{length} bytes at {offset} run past the {resSize}-byte resource directory")
    out := out.push { name, offset, flags, implementation := impl, dataStart, length }
  return { bytes := b, sections := secs, resourcesRva := resRva, resourcesSize := resSize, resources := out }

def Image.resourceBytes (img : Image) (r : Resource) : ByteArray :=
  img.bytes.extract r.dataStart (r.dataStart + r.length)

/-- `LanguageManager.GetLanguageFilesForCulture`: the manifest resources, in table order, named
`Terraria.Localization.Content.<culture>…​.json`. -/
def Image.languageFiles (img : Image) (culture : String := "en-US") : Array Resource :=
  img.resources.filter fun r => r.name.startsWith ("Terraria.Localization.Content." ++ culture) && r.name.endsWith ".json"

/-- The en-US files of the 1.4.5.8 `Terraria.exe` (and `TerrariaServer.exe`, byte-identical), in load order, with
their byte lengths: what a load reports so the caller can see it read the expected build. -/
def vanilla1458EnUS : List (String × Nat) :=
  [("Terraria.Localization.Content.en-US.Items.json", 445893), ("Terraria.Localization.Content.en-US.json", 135003),
   ("Terraria.Localization.Content.en-US.Town.json", 123843), ("Terraria.Localization.Content.en-US.Legacy.json", 56900),
   ("Terraria.Localization.Content.en-US.Projectiles.json", 35974),
   ("Terraria.Localization.Content.en-US.NPCs.json", 24129), ("Terraria.Localization.Content.en-US.Game.json", 164224)]

/-! ## Newtonsoft-compatible reading of `Dictionary<string, Dictionary<string, string>>` -/

namespace Json

private def isWs (c : UInt8) : Bool := c == 32 || c == 9 || c == 10 || c == 13

/-- Skip whitespace, `// …` and `/* … */` comments. -/
partial def skip (file : String) (b : ByteArray) (p : Nat) : R Nat :=
  if h : p < b.size then
    let c := b[p]
    if isWs c then skip file b (p + 1)
    else if c == 0x2F then
      if b[p + 1]? == some 0x2F then
        let rec eol (q : Nat) : Nat := if q < b.size && b[q]! != 10 && b[q]! != 13 then eol (q + 1) else q
        skip file b (eol (p + 2))
      else if b[p + 1]? == some 0x2A then
        let rec close (q : Nat) : R Nat :=
          if q + 1 ≥ b.size then .error (.json file p "unterminated /* comment")
          else if b[q]! == 0x2A && b[q + 1]! == 0x2F then .ok (q + 2) else close (q + 1)
        do skip file b (← close (p + 2))
      else .error (.json file p "unexpected '/'")
    else if c ≥ 0x80 || c == 0x0B || c == 0x0C then .error (.json file p "non-ASCII or vertical whitespace outside a string")
    else .ok p
  else .ok p

private def hexVal (c : UInt8) : Option Nat :=
  if 0x30 ≤ c && c ≤ 0x39 then some (c.toNat - 0x30)
  else if 0x61 ≤ c && c ≤ 0x66 then some (c.toNat - 0x57)
  else if 0x41 ≤ c && c ≤ 0x46 then some (c.toNat - 0x37) else none

private def pushUtf8 (out : ByteArray) (cp : Nat) : ByteArray :=
  if cp < 0x80 then out.push cp.toUInt8
  else if cp < 0x800 then (out.push (0xC0 + cp / 0x40).toUInt8).push (0x80 + cp % 0x40).toUInt8
  else if cp < 0x10000 then
    ((out.push (0xE0 + cp / 0x1000).toUInt8).push (0x80 + cp / 0x40 % 0x40).toUInt8).push (0x80 + cp % 0x40).toUInt8
  else
    (((out.push (0xF0 + cp / 0x40000).toUInt8).push (0x80 + cp / 0x1000 % 0x40).toUInt8).push
      (0x80 + cp / 0x40 % 0x40).toUInt8).push (0x80 + cp % 0x40).toUInt8

private def hex4 (file : String) (b : ByteArray) (p : Nat) : R Nat := do
  let mut v := 0
  for i in [0:4] do
    match b[p + i]? >>= hexVal with
    | some d => v := v * 16 + d
    | none => throw (.json file p "bad \\u escape")
  return v

/-- A `"…"` string at `p` (Newtonsoft escapes: `\" \\ \/ \' \b \f \n \r \t \uXXXX`, raw characters kept). -/
partial def str (file : String) (b : ByteArray) (p : Nat) : R (String × Nat) := do
  unless b[p]? == some 0x22 do throw (.json file p "expected a double-quoted string")
  let rec go (q : Nat) (out : ByteArray) : R (ByteArray × Nat) := do
    let some c := b[q]? | throw (.json file p "unterminated string")
    if c == 0x22 then return (out, q + 1)
    if c == 0 then throw (.json file q "NUL inside a string")
    if c != 0x5C then return ← go (q + 1) (out.push c)
    let some e := b[q + 1]? | throw (.json file q "unterminated escape")
    match e with
    | 0x22 => go (q + 2) (out.push 0x22) | 0x5C => go (q + 2) (out.push 0x5C) | 0x2F => go (q + 2) (out.push 0x2F)
    | 0x27 => go (q + 2) (out.push 0x27) | 0x62 => go (q + 2) (out.push 8) | 0x66 => go (q + 2) (out.push 12)
    | 0x6E => go (q + 2) (out.push 10) | 0x72 => go (q + 2) (out.push 13) | 0x74 => go (q + 2) (out.push 9)
    | 0x75 =>
      let u ← hex4 file b (q + 2)
      if 0xD800 ≤ u && u < 0xDC00 then
        unless b[q + 6]? == some 0x5C && b[q + 7]? == some 0x75 do throw (.json file q "lone high surrogate")
        let lo ← hex4 file b (q + 8)
        unless 0xDC00 ≤ lo && lo < 0xE000 do throw (.json file q "lone high surrogate")
        go (q + 12) (pushUtf8 out (0x10000 + (u - 0xD800) * 0x400 + (lo - 0xDC00)))
      else if 0xDC00 ≤ u && u < 0xE000 then throw (.json file q "lone low surrogate")
      else go (q + 6) (pushUtf8 out u)
    | _ => throw (.json file q "bad escape")
  let (bytes, q) ← go (p + 1) .empty
  match String.fromUTF8? bytes with
  | some s => return (s, q)
  | none => throw (.json file p "string is not UTF-8")

/-- Append `(k, v)` the way `dictionary[k] = v` does: a repeated key keeps its first position, takes the last value. -/
def assign {α} (entries : Array (String × α)) (index : Std.HashMap String Nat) (k : String) (v : α) :
    Array (String × α) × Std.HashMap String Nat :=
  match index[k]? with
  | some i => (entries.set! i (k, v), index)
  | none => (entries.push (k, v), index.insert k entries.size)

/-- `{ name: value, … }` with a trailing comma allowed, values read by `value`. -/
partial def object {α} (file : String) (b : ByteArray) (p : Nat) (value : Nat → R (α × Nat)) :
    R (Array (String × α) × Nat) := do
  unless b[p]? == some 0x7B do throw (.json file p "expected an object")
  let mut q ← skip file b (p + 1)
  let mut entries : Array (String × α) := #[]
  let mut index : Std.HashMap String Nat := {}
  repeat
    if b[q]? == some 0x7D then return (entries, q + 1)
    if b[q]? != some 0x22 then throw (.json file q "expected a double-quoted property name or '}'")
    let (k, q1) ← str file b q
    let q2 ← skip file b q1
    unless b[q2]? == some 0x3A do throw (.json file q2 "expected ':'")
    let (v, q3) ← value (← skip file b (q2 + 1))
    (entries, index) := assign entries index k v
    let q4 ← skip file b q3
    match b[q4]? with
    | some 0x2C => q ← skip file b (q4 + 1)
    | some 0x7D => return (entries, q4 + 1)
    | _ => throw (.json file q4 "expected ',' or '}'")
  throw (.json file p "unreachable")

/-- A whole localization file: categories of string texts. -/
def file (name : String) (b : ByteArray) : R (Array (String × Array (String × String))) := do
  let inner (p : Nat) : R (Array (String × String) × Nat) := do
    if b[p]? != some 0x7B then throw (.json name p "category value is not an object")
    object name b p fun q => do
      if b[q]? != some 0x22 then throw (.json name q "text value is not a string")
      str name b q
  let (cats, q) ← object name b (← skip name b 0) inner
  let e ← skip name b q
  unless e == b.size do throw (.json name e "content after the top-level object")
  return cats

end Json

/-- `StreamReader.ReadToEnd` with the default UTF-8 encoding and BOM detection, as bytes without the BOM. -/
def decodeResource (name : String) (b : ByteArray) : R ByteArray := do
  let b0 := b[0]?; let b1 := b[1]?
  if (b0 == some 0xFE && b1 == some 0xFF) || (b0 == some 0xFF && b1 == some 0xFE) ||
     (b0 == some 0 && b1 == some 0 && b[2]? == some 0xFE && b[3]? == some 0xFF) then
    throw (.resource name "UTF-16 / UTF-32 byte order mark")
  let body := if b0 == some 0xEF && b1 == some 0xBB && b[2]? == some 0xBF then b.extract 3 b.size else b
  match String.fromUTF8? body with
  | none => throw (.resource name "not valid UTF-8")
  | some s =>
    -- `text.Length < 2` (UTF-16 code units) → FormatException, and vanilla loads no further file
    if (s.toList.foldl (fun n c => n + if c.toNat ≥ 0x10000 then 2 else 1) 0) < 2 then
      throw (.resource name "shorter than two characters (vanilla stops loading the language here)")
    return body

/-! ## The loaded dictionary -/

/-- `LanguageManager._localizedTexts` after `LoadLanguage`: every key's current unformatted value, in insertion
order, plus the files it came from. -/
structure Dict where
  order : Array String := #[]
  texts : Std.HashMap String String := {}
  /-- `(resource name, byte length)` of each file loaded, in order. -/
  files : Array (String × Nat) := #[]

/-- `UpdateTextValue` then, for a new key, `_localizedTexts.Add`. -/
def Dict.update (d : Dict) (file key value : String) : R Dict := do
  if key.contains '$' then throw (.json file 0 s!"variant key {key} ($): AddVariant is not transcribed")
  if d.texts.contains key then return { d with texts := d.texts.insert key value }
  return { d with order := d.order.push key, texts := d.texts.insert key value }

/-! ### `\w` and the two placeholder regexes -/

/-- .NET `\w` on ASCII; a non-ASCII character is a boundary (its Unicode category is not transcribed). -/
private def wordChar (key : String) (c : Char) : R Bool :=
  if c.toNat < 0x80 then .ok (c.isAlphanum || c == '_')
  else .error (.format key s!"\\w on non-ASCII character U+{String.ofList (Nat.toDigits 16 c.toNat)} inside a placeholder")

/-- `{\$(\w+\.\w+)}` at `i`: the group text and the index after the match. -/
private def copyMatchAt (key : String) (cs : Array Char) (i : Nat) : R (Option (String × Nat)) := do
  unless cs[i]? == some '{' && cs[i + 1]? == some '$' do return none
  let mut j := i + 2
  let mut g : String := ""
  let mut part := 0     -- 0: first \w+, 1: second \w+
  let mut runLen := 0
  repeat
    let some c := cs[j]? | return none
    if ← wordChar key c then
      g := g.push c; runLen := runLen + 1; j := j + 1
    else if c == '.' && part == 0 && runLen > 0 then
      g := g.push c; part := 1; runLen := 0; j := j + 1
    else if c == '}' && part == 1 && runLen > 0 then
      return some (g, j + 1)
    else return none
  return none

/-- One `Regex.Replace` of `ProcessCopyCommandsInTexts` over `s`, looking keys up in `texts`. -/
def replaceCopyCommands (key s : String) (texts : Std.HashMap String String) : R String := do
  let cs := s.toList.toArray
  let mut out := ""
  let mut i := 0
  while i < cs.size do
    match ← copyMatchAt key cs i with
    | some (g, j) => out := out ++ texts.getD g g; i := j
    | none => out := out.push cs[i]!; i := i + 1
  return out

/-- `ProcessCopyCommandsInTexts`: every text in insertion order, re-replaced until stable (at most 100 rounds). -/
def Dict.processCopyCommands (d : Dict) : R Dict := do
  let mut texts := d.texts
  for key in d.order do
    let mut cur := texts.getD key ""
    for _ in [0:100] do
      let next ← replaceCopyCommands key cur texts
      if next == cur then break
      texts := texts.insert key next
      cur := next
  return { d with texts }

/-- `LanguageManager.LoadLanguage` for one culture on an already parsed image (no resource packs). -/
def Dict.ofImage (img : Image) (culture : String := "en-US") : R Dict := do
  let mut d : Dict := {}
  for r in img.languageFiles culture do
    if r.implementation != 0 then throw (.resource r.name "not embedded in this file")
    let body ← decodeResource r.name (img.resourceBytes r)
    for (cat, kvs) in ← Json.file r.name body do
      for (k, v) in kvs do
        d ← d.update r.name (cat ++ "." ++ k) v
    d := { d with files := d.files.push (r.name, r.length) }
  d.processCopyCommands

/-- Read the language straight from the bytes of the player's `Terraria.exe`. -/
def load (exe : ByteArray) (culture : String := "en-US") : R Dict := do
  Dict.ofImage (← Image.parse exe) culture

/-- `LocalizedText.UnformattedValue` (after the copy commands). -/
def Dict.raw? (d : Dict) (key : String) : Option String := d.texts[key]?

/-- `Language.Exists`. -/
def Dict.exists (d : Dict) (key : String) : Bool := d.texts.contains key

/-- Whether the loaded files are the 1.4.5.8 en-US set (names, order and lengths). -/
def Dict.isVanilla1458EnUS (d : Dict) : Bool := d.files.toList == vanilla1458EnUS

/-! ## `VariableText` -/

/-- `VariableText.TryCreate`: the composite format, the conditions `(name, required)` and the variable names. -/
structure VarText where
  format : String
  conditions : Array (String × Bool)
  variables : Array String
  deriving Repr, BEq

/-- `{(\?!?)?([a-zA-Z][\w\.]*)}` at `i`: `(prefix, name, index after)`. -/
private def varMatchAt (key : String) (cs : Array Char) (i : Nat) : R (Option (String × String × Nat)) := do
  unless cs[i]? == some '{' do return none
  let mut j := i + 1
  let mut pre := ""
  if cs[j]? == some '?' then
    pre := "?"; j := j + 1
    if cs[j]? == some '!' then pre := "?!"; j := j + 1
  let some c0 := cs[j]? | return none
  unless ('a' ≤ c0 && c0 ≤ 'z') || ('A' ≤ c0 && c0 ≤ 'Z') do return none
  let mut name := String.singleton c0
  j := j + 1
  repeat
    let some c := cs[j]? | return none
    if c == '}' then return some (pre, name, j + 1)
    if c == '.' || (← wordChar key c) then name := name.push c; j := j + 1
    else return none
  return none

/-- `VariableText.TryCreate(text)`: `none` when the substitution regex does not match (a plain text). -/
def VarText.create (key raw : String) : R (Option VarText) := do
  let cs := raw.toList.toArray
  let mut fmt := ""
  let mut conds : Array (String × Bool) := #[]
  let mut vars : Array String := #[]
  let mut any := false
  let mut i := 0
  while i < cs.size do
    match ← varMatchAt key cs i with
    | some (pre, name, j) =>
      any := true
      if pre != "" then conds := conds.push (name, pre == "?")
      else
        let idx ← match vars.findIdx? (· == name) with
          | some k => pure k
          | none => do vars := vars.push name; pure (vars.size - 1)
        fmt := fmt ++ "{" ++ toString idx ++ "}"
      i := j
    | none => fmt := fmt.push cs[i]!; i := i + 1
  return if any then some { format := fmt, conditions := conds, variables := vars } else none

/-- `StringBuilder.AppendFormat(format, args)` for the items the texts use: `{{`, `}}`, `{n}`. Anything else is the
vanilla `FormatException` or a format feature not transcribed: a boundary. -/
def appendFormat (key fmt : String) (args : Array String) : R String := do
  let cs := fmt.toList.toArray
  let mut out := ""
  let mut i := 0
  while i < cs.size do
    let c := cs[i]!
    if c == '}' then
      if cs[i + 1]? == some '}' then out := out.push '}'; i := i + 2
      else throw (.format key "unpaired '}' in the composite format (FormatException)")
    else if c == '{' then
      if cs[i + 1]? == some '{' then out := out.push '{'; i := i + 2
      else
        let mut j := i + 1
        let mut n := 0
        while (cs[j]?.map Char.isDigit).getD false do
          n := n * 10 + (cs[j]!.toNat - 48); j := j + 1
        unless j > i + 1 && cs[j]? == some '}' do
          throw (.format key s!"composite format item at {i} other than {"{"}n{"}"} (alignment, format string or FormatException)")
        unless n < args.size do throw (.format key s!"format item {n} with {args.size} arguments (FormatException)")
        out := out ++ args[n]!
        i := j + 1
    else out := out.push c; i := i + 1
  return out

/-! ## Substitutions (`Lang.InitGlobalSubstitutions`) -/

/-- A substitution's boxed value: a string, a `bool`, an `int`, or `null`. -/
inductive Value where
  | str (s : String)
  | bool (b : Bool)
  | int (i : Int)
  | null
  deriving Repr, BEq, Inhabited

/-- `object.ToString()` as `AppendFormat` writes it (invariant digits, `True` / `False`). -/
def Value.render : Value → String
  | .str s => s
  | .bool b => if b then "True" else "False"
  | .int i => toString i
  | .null => ""

/-- The substitution environment: `none` = the caller did not supply this name. -/
abbrev Env := String → Option Value

/-- Every name `Lang.InitGlobalSubstitutions` registers (1.4.5.8). A name outside this list is `null` in vanilla. -/
def globalNames : List String :=
  ["Nurse", "Merchant", "ArmsDealer", "Dryad", "Demolitionist", "Clothier", "Guide", "Wizard", "GoblinTinkerer",
   "Mechanic", "Truffle", "Steampunker", "DyeTrader", "PartyGirl", "Cyborg", "Painter", "WitchDoctor", "Pirate",
   "Stylist", "TravelingMerchant", "Angler", "Bartender", "WorldName", "Day", "BloodMoon", "Eclipse",
   "MoonLordDefeated", "GolemDefeated", "DukeFishronDefeated", "FrostLegionDefeated", "MartiansDefeated",
   "PumpkingDefeated", "IceQueenDefeated", "HardMode", "Homeless", "InventoryKey", "PlayerName", "GolfGuy",
   "TaxCollector", "Rain", "Graveyard", "AnglerCompletedQuestsCount", "TotalDeathsCount", "WorldEvilStone",
   "InputTriggerUI_BuildFromInventory", "InputTriggerUI_TrashEnabled", "InputTriggerUI_Trash",
   "InputTriggerUI_FavoriteItem", "InputTrigger_QuickEquip", "InputTrigger_Grapple", "InputTrigger_LockOn",
   "InputTrigger_RadialQuickbar", "InputTrigger_RadialHotbar", "InputTrigger_SmartCursor", "InputTrigger_UseOrAttack",
   "InputTrigger_Jump", "InputTrigger_InteractWithTile", "InputTrigger_InteractWithTileUI", "InputTrigger_ToggleOrOpen",
   "InputTrigger_SmartSelect", "InputTrigger_Mount", "InputTrigger_Healing", "InputTrigger_ManaHealing",
   "InputTrigger_QuickBuff", "ToggleArmorSetBonusKey"]

/-- `Lang.GetGlobalSubstitution` through the environment. -/
def resolve (env : Env) (key name : String) : R Value :=
  match env name with
  | some v => .ok v
  | none => if globalNames.contains name then .error (.unknownSubstitution key name) else .ok .null

/-- `VariableText.TryFormat` with the substitutions: `none` when a condition fails or a variable is `null`. -/
def VarText.tryFormat (vt : VarText) (env : Env) (key : String) : R (Option String) := do
  for (name, required) in vt.conditions do
    let b := match ← resolve env key name with | .bool b => b | _ => false
    if b != required then return none
  let mut args : Array String := #[]
  for name in vt.variables do
    match ← resolve env key name with
    | .null => return none
    | v => args := args.push v.render
  return some (← appendFormat key vt.format args)

/-- `LocalizedText.Value` of a raw (unformatted) text. -/
def formatValue (env : Env) (key raw : String) : R String := do
  match ← VarText.create key raw with
  | none => return raw
  | some vt => return (← vt.tryFormat env key).getD raw

/-- `Language.GetTextValue(key)`, vanilla-faithful: a missing key yields the key itself. -/
def Dict.getTextValue (d : Dict) (env : Env) (key : String) : R String :=
  match d.texts[key]? with
  | none => .ok key
  | some raw => formatValue env key raw

/-- `Language.GetTextValue(key)` for a key that must exist (`missingKey` otherwise). -/
def Dict.text (d : Dict) (env : Env) (key : String) : R String :=
  match d.texts[key]? with
  | none => .error (.missingKey key)
  | some raw => formatValue env key raw

/-- `Language.Exists(key) && GetText(key).GetValueIfConditionsMet(out text)`: the text when every condition holds and
every variable is non-null, else `none`. -/
def Dict.textIfConditionsMet (d : Dict) (env : Env) (key : String) : R (Option String) := do
  let some raw := d.texts[key]? | return none
  match ← VarText.create key raw with
  | none => return some raw
  | some vt => vt.tryFormat env key

/-- `Lang.dialog(n)`. -/
def Dict.dialog (d : Dict) (env : Env) (n : Nat) : R String := d.getTextValue env s!"LegacyDialog.{n}"

/-- The text of a dialogue `Line` (`Line.toKey` looked up with `Language.GetTextValue`); `Line.none` is the empty
chat text. A key missing from the language is a boundary here (vanilla would show the key). -/
def Dict.lineText (d : Dict) (env : Env) : Line → R String
  | .none => .ok ""
  | l => d.text env l.toKey

/-- A text with no placeholder at all (what `Language.GetTextValue` returns without consulting substitutions), or a
missing key (returned as is); `none` for a text that would need the substitutions. -/
def Dict.plainValue (d : Dict) (key : String) : Option String :=
  match d.texts[key]? with
  | none => some key
  | some raw => match VarText.create key raw with
    | .ok none => some raw
    | _ => none

/-! ## The environment from the game: world, player, keyboard and mouse -/

/-- Every trigger `PlayerInput.KnownTriggers` lists (1.4.5.8): a `KeyStatus` lookup of anything else throws. -/
def knownTriggers : List String :=
  ["MouseLeft", "MouseRight", "Up", "Down", "Left", "Right", "Jump", "Throw", "Inventory", "Grapple", "SmartSelect",
   "SmartCursor", "QuickMount", "QuickHeal", "QuickMana", "QuickBuff", "MapZoomIn", "MapZoomOut", "MapAlphaUp",
   "MapAlphaDown", "MapFull", "MapStyle", "Hotbar1", "Hotbar2", "Hotbar3", "Hotbar4", "Hotbar5", "Hotbar6", "Hotbar7",
   "Hotbar8", "Hotbar9", "Hotbar10", "HotbarMinus", "HotbarPlus", "DpadRadial1", "DpadRadial2", "DpadRadial3",
   "DpadRadial4", "RadialHotbar", "RadialQuickbar", "DpadSnap1", "DpadSnap2", "DpadSnap3", "DpadSnap4", "MenuUp",
   "MenuDown", "MenuLeft", "MenuRight", "LockOn", "ViewZoomIn", "ViewZoomOut", "Loadout1", "Loadout2", "Loadout3",
   "PreviousLoadout", "NextLoadout", "ToggleCameraMode", "ToggleCreativeMenu", "ArmorSetAbility", "Dash"]

/-- The default keyboard-and-mouse bindings: with no saved input profile `PlayerInput.Initialize` selects "Custom",
initialised with preset 1 (Redigit), whose `InputMode.Keyboard` block of `PlayerInput.Reset` (`IL_0034` …
`IL_0410`) is this list (each trigger one key; triggers absent here have none). -/
def defaultBindings : List (String × List String) :=
  [("MouseLeft", ["Mouse1"]), ("MouseRight", ["Mouse2"]), ("Up", ["W"]), ("Down", ["S"]), ("Left", ["A"]),
   ("Right", ["D"]), ("Jump", ["Space"]), ("Inventory", ["Escape"]), ("Grapple", ["E"]), ("SmartSelect", ["LeftShift"]),
   ("SmartCursor", ["LeftControl"]), ("QuickMount", ["R"]), ("QuickHeal", ["H"]), ("QuickMana", ["J"]),
   ("QuickBuff", ["B"]), ("MapStyle", ["Tab"]), ("MapFull", ["M"]), ("MapZoomIn", ["Add"]), ("MapZoomOut", ["Subtract"]),
   ("MapAlphaUp", ["PageUp"]), ("MapAlphaDown", ["PageDown"]), ("Hotbar1", ["D1"]), ("Hotbar2", ["D2"]),
   ("Hotbar3", ["D3"]), ("Hotbar4", ["D4"]), ("Hotbar5", ["D5"]), ("Hotbar6", ["D6"]), ("Hotbar7", ["D7"]),
   ("Hotbar8", ["D8"]), ("Hotbar9", ["D9"]), ("Hotbar10", ["D0"]), ("ViewZoomOut", ["OemMinus"]),
   ("ViewZoomIn", ["OemPlus"]), ("ToggleCreativeMenu", ["C"]), ("Loadout1", ["F1"]), ("Loadout2", ["F2"]),
   ("Loadout3", ["F3"]), ("ToggleCameraMode", ["F4"])]

/-- Keyboard-and-mouse input state the input-trigger substitutions read (`PlayerInput.UsingGamepad` false). -/
structure Keyboard where
  /-- `CurrentProfile.InputModes[Keyboard].KeyStatus`: the keys of each trigger, in order. -/
  bindings : List (String × List String) := defaultBindings
  /-- `Main.cInv` (`Main..cctor`: "Escape"). -/
  inventoryKey : String := "Escape"
  /-- `Main.FavoriteKey.ToString()` (`Main..cctor`: `Keys.LeftAlt` = 164). -/
  favoriteKey : String := "LeftAlt"
  disableQuickTrash : Bool := false
  disableLeftShiftTrashCan : Bool := false
  /-- `Main.ReversedUpDownArmorSetBonuses`. -/
  reversedUpDownArmorSetBonuses : Bool := false
  deriving Repr, Inhabited

/-- World and player state the substitutions read. Optional fields left `none` make texts that need them a
`unknownSubstitution` boundary. -/
structure World where
  /-- `Main.player[Main.myPlayer].name`. -/
  playerName : String
  /-- `Main.worldName`. -/
  worldName : String
  dayTime : Bool := true
  bloodMoon : Bool := false
  eclipse : Bool := false
  hardMode : Bool := false
  /-- `Main.raining`. -/
  raining : Bool := false
  /-- `WorldGen.crimson` (`{WorldEvilStone}`). -/
  crimson : Bool := false
  downedMoonlord : Bool := false
  downedGolemBoss : Bool := false
  downedFishron : Bool := false
  downedFrost : Bool := false
  downedMartians : Bool := false
  downedHalloweenKing : Bool := false
  downedChristmasIceQueen : Bool := false
  /-- The active NPCs in `Main.npc` slot order, `(type, GivenOrTypeName)`: `NPC.GetFirstNPCNameOrNull` takes the
  first of a type; a town NPC that is absent is `null` (vanilla then shows the raw text). -/
  npcs : Array (Nat × String) := #[]
  /-- `{Homeless}`: the talk NPC's `homeless` (false when not talking). -/
  homeless : Option Bool := none
  /-- `{Graveyard}`: `LocalPlayer.ZoneGraveyard`. -/
  zoneGraveyard : Option Bool := none
  /-- `{AnglerCompletedQuestsCount}`: `LocalPlayer.anglerQuestsFinished`. -/
  anglerQuestsFinished : Option Int := none
  /-- `{TotalDeathsCount}`: `LocalPlayer.numberOfDeathsPVE`. -/
  deathsPVE : Option Int := none
  deriving Repr, Inhabited

/-- The NPC type each town-NPC name substitution looks up. -/
def townNpcTypes : List (String × Nat) :=
  [("Nurse", 18), ("Merchant", 17), ("ArmsDealer", 19), ("Dryad", 20), ("Demolitionist", 38), ("Clothier", 54),
   ("Guide", 22), ("Wizard", 108), ("GoblinTinkerer", 107), ("Mechanic", 124), ("Truffle", 160), ("Steampunker", 178),
   ("DyeTrader", 207), ("PartyGirl", 208), ("Cyborg", 209), ("Painter", 227), ("WitchDoctor", 228), ("Pirate", 229),
   ("Stylist", 353), ("TravelingMerchant", 368), ("Angler", 369), ("Bartender", 550), ("GolfGuy", 588),
   ("TaxCollector", 441)]

/-- `PlayerInput.LocalizeKey`: `Mouse1` / `Mouse2` are the language's `Controls.LeftClick` / `RightClick`. -/
def localizeKey (d : Dict) (k : String) : Option String :=
  if k == "Mouse1" then d.plainValue "Controls.LeftClick"
  else if k == "Mouse2" then d.plainValue "Controls.RightClick"
  else some k

/-- `GenerateInputTag_ForCurrentGamemode(_, trigger)` on keyboard: `GenerateRawInputList` of the trigger's keys
("" for none, `/`-joined). `none` for a trigger vanilla does not know (it would throw). -/
def inputTag (d : Dict) (kb : Keyboard) (trigger : String) : Option String := do
  unless knownTriggers.contains trigger do none
  let keys := (kb.bindings.lookup trigger).getD []
  let parts ← keys.mapM (localizeKey d)
  return "/".intercalate parts

/-- The global substitutions for the given world, player and keyboard state, as `Lang.InitGlobalSubstitutions`
registers them. A value the state cannot give is `none` (a boundary when a text needs it). -/
def env (d : Dict) (w : World) (kb : Keyboard := {}) : Env := fun name =>
  let lang (k : String) : Option Value := (d.plainValue k).map .str
  let tag (t : String) : Option Value := (inputTag d kb t).map .str
  match townNpcTypes.lookup name with
  | some t => some (match w.npcs.find? (·.1 == t) with | some (_, n) => .str n | none => .null)
  | none =>
  match name with
  | "WorldName" => some (.str w.worldName)
  | "PlayerName" => some (.str w.playerName)
  | "Day" => some (.bool w.dayTime)
  | "BloodMoon" => some (.bool w.bloodMoon)
  | "Eclipse" => some (.bool w.eclipse)
  | "MoonLordDefeated" => some (.bool w.downedMoonlord)
  | "GolemDefeated" => some (.bool w.downedGolemBoss)
  | "DukeFishronDefeated" => some (.bool w.downedFishron)
  | "FrostLegionDefeated" => some (.bool w.downedFrost)
  | "MartiansDefeated" => some (.bool w.downedMartians)
  | "PumpkingDefeated" => some (.bool w.downedHalloweenKing)
  | "IceQueenDefeated" => some (.bool w.downedChristmasIceQueen)
  | "HardMode" => some (.bool w.hardMode)
  | "Rain" => some (.bool w.raining)
  | "Homeless" => w.homeless.map .bool
  | "Graveyard" => w.zoneGraveyard.map .bool
  | "AnglerCompletedQuestsCount" => w.anglerQuestsFinished.map .int
  | "TotalDeathsCount" => w.deathsPVE.map .int
  | "WorldEvilStone" => lang (if w.crimson then "Misc.Crimstone" else "Misc.Ebonstone")
  | "InventoryKey" => some (.str kb.inventoryKey)
  | "InputTriggerUI_BuildFromInventory" => tag "QuickMount"
  | "InputTriggerUI_TrashEnabled" => some (.bool !kb.disableQuickTrash)
  | "InputTriggerUI_Trash" => lang (if kb.disableLeftShiftTrashCan then "Controls.Control" else "Controls.Shift")
  | "InputTriggerUI_FavoriteItem" => some (.str kb.favoriteKey)
  | "InputTrigger_QuickEquip" => tag "Grapple"
  | "InputTrigger_Grapple" => tag "Grapple"
  | "InputTrigger_LockOn" => tag "LockOn"
  | "InputTrigger_RadialQuickbar" => tag "HotbarMinus"
  | "InputTrigger_RadialHotbar" => tag "HotbarPlus"
  | "InputTrigger_SmartCursor" => tag "SmartCursor"
  | "InputTrigger_UseOrAttack" => tag "MouseLeft"
  | "InputTrigger_Jump" => tag "Jump"
  | "InputTrigger_InteractWithTile" => tag "MouseRight"
  | "InputTrigger_InteractWithTileUI" => tag "MouseRight"
  | "InputTrigger_ToggleOrOpen" => tag "MouseRight"
  | "InputTrigger_SmartSelect" => tag "SmartSelect"
  | "InputTrigger_Mount" => tag "QuickMount"
  | "InputTrigger_Healing" => tag "QuickHeal"
  | "InputTrigger_ManaHealing" => tag "QuickMana"
  | "InputTrigger_QuickBuff" => tag "QuickBuff"
  | "ToggleArmorSetBonusKey" => lang (if kb.reversedUpDownArmorSetBonuses then "Key.UP" else "Key.DOWN")
  | _ => none

/-! ## Chat lines -/

/-- `NPC.GetChat()` for the Old Man (`type == 37`): day (and not a remix world) `Next(3)` → 82 … 84; otherwise
`Next(4)` → 85 … 88 while `statLifeMax < 300 || statDefense < 10`, else 89 … 92. A pending conditional dialogue
(`nextDialogue`) is outside this transcription. -/
def oldManChat (dayTime remixWorld : Bool) (statLifeMax statDefense : Int) (rng : Rng)
    (pendingDialogue : Bool := false) : R (Line × Rng) := do
  if pendingDialogue then throw (.unsupported "GetChat: ConditionalDialogue.GetChatAndClearCondition")
  if dayTime && !remixWorld then
    let (r, rng) := rng.next 3
    return (.legacy (82 + r), rng)
  let (r, rng) := rng.next 4
  return (.legacy ((if statLifeMax < 300 || statDefense < 10 then 85 else 89) + r), rng)

/-- `Main.HelpText`'s language probe for `OriginalForestGuideDialogue.helpText`: `GuideHelpText.Help_<h>` exists and
`GetValueIfConditionsMet` succeeds. Computed once for every `GuideHelpText.Help_<n>` key of the loaded language. -/
def Dict.helpLangKey (d : Dict) (env : Env) : R (Int → Bool) := do
  let pre := "GuideHelpText.Help_"
  let mut ok : List Int := []
  for k in d.order do
    if k.startsWith pre then
      if let some n := (String.ofList (k.toList.drop pre.length)).toInt? then
        if s!"{pre}{n}" == k && (← d.textIfConditionsMet env k).isSome then ok := n :: ok
  return fun h => ok.contains h

end Lean4Terria.OriginalResidentText
