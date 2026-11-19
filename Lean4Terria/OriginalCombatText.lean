import Init.Data.Vector.Basic
import Lean4Terria.UnifiedRandom
import Lean4Terria.OriginalForestChatLayout

/-! Terraria 1.4.5.8 desktop's fixed CombatText pool, allocation and update.
The caller owns the shared world phase and supplies real font measurement.
Original-method evidence is in `.lake/original-combat-text/`. -/
namespace Lean4Terria.OriginalCombatText

open OriginalForestChatLayout (Color Rect Font Sprite)

private def single (n : Int) : Float32 := (Int32.ofInt n).toFloat32
private def fmax (a b : Float32) : Float32 := if a > b then a else b

structure Vec2 where
  x : Float32 := 0
  y : Float32 := 0
  deriving BEq, Repr, Inhabited

/-- All fields survive while inactive. In particular, pooled ordinary NewText
does not overwrite velocity.x left by a previous dramatic text. -/
structure Entry where
  position : Vec2 := {}
  velocity : Vec2 := {}
  alpha : Float32 := 0
  alphaDir : Int := 1
  text : String := ""
  scale : Float32 := 1
  rotation : Float32 := 0
  color : Color := ⟨0,0,0,0⟩
  active : Bool := false
  lifeTime : Int := 0
  crit : Bool := false
  dot : Bool := false
  deriving Repr, Inhabited

def capacity : Nat := 100

structure State where
  slots : Vector Entry capacity := Vector.replicate capacity {}
  deriving Repr, Inhabited

def State.firstFreeSlot (state : State) : Nat :=
  (state.slots.toArray.findIdx? (fun entry => !entry.active)).getD capacity

def State.hasFreeSlot (state : State) : Bool := state.firstFreeSlot < capacity

structure Context where
  netMode : Int := 0
  noPooling : Bool := false
  gravity : Float32 := 1
  deriving Repr, Inhabited

structure NewTextInput where
  rect : Rect
  color : Color
  text : String
  dramatic : Bool := false
  dot : Bool := false
  deriving Repr

/-- Font 0 for ordinary text, 1 for dramatic text; called only after a free slot
is found. Results must be the original font's MeasureString Float32 dimensions. -/
abbrev Measure := Bool → String → Except String Vec2

structure Created where
  slot : Nat
  entry : Entry
  deriving Repr

structure NewTextResult where
  state : State
  slot : Nat
  created : Option Created
  random : UnifiedRandom

/-- Complete ordinary NewText(string) allocation. Server/full-pool returns100
without font access or RNG. A successful allocation draws x/y, plus x velocity
for dramatic text. Rectangle widths/heights are nonnegative Int32 values. -/
def newText (state : State) (context : Context) (input : NewTextInput)
    (measure : Measure) (random : UnifiedRandom) : Except String NewTextResult := do
  let slot := state.firstFreeSlot
  if context.netMode == 2 || slot ≥ capacity then
    return ⟨state,capacity,none,random⟩
  let size ← measure input.dramatic input.text
  let old := if context.noPooling then ({} : Entry) else state.slots.getD slot {}
  let halfW := input.rect.w.tdiv 2
  let halfH := input.rect.h.tdiv 2
  let (xRoll,random) := random.next (-halfW) (halfW+1)
  let (yRoll,random) := random.next (-halfH) (halfH+1)
  let x := single input.rect.x + single input.rect.w * 0.5 - size.x * 0.5
  let y := single input.rect.y + single input.rect.h * 0.25 - size.y * 0.5
  let position : Vec2 := ⟨x + single xRoll,y + single yRoll⟩
  let (position,vy) := if context.gravity == -1 then
      ({position with y := single input.rect.y + single input.rect.h * 0.75 + size.y * 0.5},(7 : Float32))
    else (position,(-7 : Float32))
  let mut entry : Entry := {old with
    position,velocity := {old.velocity with y := vy},alpha:=1,alphaDir:= -1,
    active:=true,scale:=0,rotation:=0,color:=input.color,text:=input.text,
    lifeTime:=60,crit:=input.dramatic,dot:=input.dot}
  let mut random := random
  if input.dramatic then
    let (roll,next) := random.next (-25) 26
    random := next
    let vx := single roll * 0.05
    let rotation : Float32 := single (120 / 2 : Int) * 0.002
    entry := {entry with
      lifeTime := 120
      velocity := ⟨vx,entry.velocity.y * 2⟩
      rotation := if vx < 0 then rotation * (-1) else rotation}
  if input.dot then entry := {entry with velocity := {entry.velocity with y := -4},lifeTime:=40}
  return ⟨{slots:=state.slots.set! slot entry},slot,some ⟨slot,entry⟩,random⟩

/-- The real int overload delegates after Int32.ToString; default decimal signs
and digits match the ordinary desktop culture. -/
def newNumber (state : State) (context : Context) (input : NewTextInput)
    (amount : Int32) (measure : Measure) (random : UnifiedRandom) : Except String NewTextResult :=
  newText state context {input with text := toString amount.toInt} measure random

/-- Complete CombatText.Update, including the post-deactivation crit scale
write. TargetScale is exactly1 in this desktop version. No random samples. -/
def Entry.advance (entry : Entry) : Entry := Id.run do
  if !entry.active then return entry
  let mut entry : Entry := {entry with alpha := entry.alpha + single entry.alphaDir * 0.05}
  if entry.alpha.toFloat ≤ 0.6 then entry := {entry with alphaDir:=1}
  if entry.alpha ≥ 1 then entry := {entry with alpha:=1,alphaDir:= -1}
  let vy := if entry.dot then entry.velocity.y + 0.15
    else if entry.crit then entry.velocity.y * 0.92 * 0.92
    else entry.velocity.y * 0.92
  let velocity : Vec2 := ⟨entry.velocity.x * 0.93,vy⟩
  entry := {entry with
    velocity
    position := ⟨entry.position.x+velocity.x,entry.position.y+velocity.y⟩
    lifeTime := (Int32.ofInt (entry.lifeTime-1)).toInt}
  if entry.lifeTime ≤ 0 then
    entry := {entry with scale:=entry.scale-0.1}
    if entry.scale.toFloat < 0.1 then entry := {entry with active:=false}
    entry := {entry with lifeTime:=0}
    if entry.crit then entry := {entry with alphaDir:= -1,scale:=entry.scale+0.07}
    return entry
  if entry.crit then
    entry := {entry with rotation:=if entry.velocity.x < 0 then entry.rotation+0.001 else entry.rotation-0.001}
  if entry.dot then
    entry := {entry with scale:=entry.scale+0.5}
    if entry.scale.toFloat > 0.8 then entry := {entry with scale:=0.8}
  else
    if entry.scale < 1 then entry := {entry with scale:=entry.scale+0.1}
    if entry.scale > 1 then entry := {entry with scale:=1}
  return entry

/-- UpdateCombatText scans all100 slots in order. Its caller, rather than this
method, supplies the world/server/pause gate. -/
def State.advance (state : State) : State := ⟨state.slots.map Entry.advance⟩

/-- clearAll changes only active; pooled velocities and all other fields stay. -/
def State.clearAll (state : State) : State :=
  ⟨state.slots.map fun entry => {entry with active:=false}⟩

def damagedFriendly : Color := ⟨255,80,90,255⟩
def damagedFriendlyCrit : Color := ⟨255,100,30,255⟩
def damagedHostile : Color := ⟨255,160,80,255⟩
def damagedHostileCrit : Color := ⟨255,100,30,255⟩
def othersDamagedHostile : Color := ⟨102,64,32,102⟩
def othersDamagedHostileCrit : Color := ⟨102,40,12,102⟩
def healLife : Color := ⟨100,255,100,255⟩
def healMana : Color := ⟨100,100,255,255⟩
def lifeRegen : Color := ⟨255,60,70,255⟩
def lifeRegenNegative : Color := ⟨255,140,40,255⟩

/-- The two real FontAssets.CombatText fonts, supplied by the resource owner. -/
structure Fonts where
  normal : Font
  critical : Font

private def codeUnits (text : String) : Array Nat :=
  text.toList.foldl (init := #[]) fun units c =>
    let n := c.toNat
    if n < 65536 then units.push n
    else (units.push (55296+(n-65536)/1024)).push (56320+(n-65536)%1024)

private def glyph (font : Font) (unit : Nat) : Except String OriginalForestChatLayout.Glyph := do
  if let some data := font.glyphs[unit]? then return data
  if font.ranges.any (fun (lo,hi) => lo ≤ unit && unit ≤ hi) then
    throw s!"{font.name}: declared glyph {unit} has no metrics"
  match font.glyphs[font.default.toNat]? with
  | some data => return data
  | none => throw s!"{font.name}: default glyph missing"

/-- Original DynamicSpriteFont.MeasureString's UTF16 iteration. Valid Unicode
outside the BMP is two default glyphs when its surrogate units are unsupported. -/
def measureText (font : Font) (text : String) : Except String Vec2 := do
  if text.isEmpty then return {}
  let mut x : Float32 := 0
  let mut y := single font.lineSpacing
  let mut widest : Float32 := 0
  let mut lines : Nat := 0
  let mut right : Float32 := 0
  let mut first := true
  for unit in codeUnits text do
    if unit == 10 then
      widest := fmax (x+fmax right 0) widest
      right := 0
      x := 0
      y := single font.lineSpacing
      first := true
      lines := lines+1
    else if unit != 13 then
      let g ← glyph font unit
      let kx := if first then fmax g.kx 0 else g.kx
      if !first then x := x+(font.spacing+right)
      x := x+(kx+g.ky)
      right := g.kz
      y := fmax y (single g.padding.h)
      first := false
  return ⟨fmax (x+fmax right 0) widest,y+single (lines*font.lineSpacing : Int)⟩

def Fonts.measure (fonts : Fonts) : Measure := fun critical text =>
  measureText (if critical then fonts.critical else fonts.normal) text

private def drawText (font : Font) (text : String) (position : Vec2) (color : Color)
    (rotation : Float32) (origin : Vec2) (scale : Float32) : Except String (Array Sprite) := do
  let mut out := #[]
  let mut first := true
  if rotation == 0 then
    -- Complete InternalDrawFast branch (effects/character arrays are absent).
    let mut x := position.x-origin.x*scale
    let mut y := position.y-origin.y*scale
    let lineStart := x
    for unit in codeUnits text do
      if unit == 10 then
        x := lineStart
        y := y+single font.lineSpacing*scale
        first := true
      else if unit != 13 then
        let g ← glyph font unit
        let kx := if first then fmax g.kx 0 else g.kx
        if !first then x := x+font.spacing*scale
        x := x+kx*scale
        let dx := x+single g.padding.x*scale
        let dy := y+single g.padding.y*scale
        let (tw,th) := font.pages[g.page]!
        out := out.push (OriginalForestChatLayout.draw s!"{font.name}.page{g.page}"
          tw th dx dy (some g.glyph) color 0 0 0 scale scale)
        x := x+(g.ky+g.kz)*scale
        first := false
  else
    -- InternalDraw: translation(-origin*scale) * rotationZ; then Transform.
    let sin := (Float.sin rotation.toFloat).toFloat32
    let cos := (Float.cos rotation.toFloat).toFloat32
    let tx := -origin.x*scale
    let ty := -origin.y*scale
    let m41 := ((tx*cos+ty*(-sin))+0)+0
    let m42 := ((tx*sin+ty*cos)+0)+0
    let mut x : Float32 := 0
    let mut y : Float32 := 0
    for unit in codeUnits text do
      if unit == 10 then
        x := 0
        y := y+single font.lineSpacing*scale
        first := true
      else if unit != 13 then
        let g ← glyph font unit
        let kx := if first then fmax g.kx 0 else g.kx
        if !first then x := x+font.spacing*scale
        x := x+kx*scale
        let gx := x+single g.padding.x*scale
        let gy := y+single g.padding.y*scale
        let dx := (gx*cos+gy*(-sin))+m41
        let dy := (gx*sin+gy*cos)+m42
        let (tw,th) := font.pages[g.page]!
        out := out.push (OriginalForestChatLayout.draw s!"{font.name}.page{g.page}"
          tw th (dx+position.x) (dy+position.y) (some g.glyph) color rotation 0 0 scale scale)
        x := x+(g.ky+g.kz)*scale
        first := false
  return out

structure DrawContext where
  camera : Vec2
  screenHeight : Nat
  gravity : Float32 := 1

/-- The complete CombatText loop is inline in Main.DoDraw, IL160b..19ef.
It reads all100 slots in order, draws four colored outlines then foreground,
and leaves the pool/RNG unchanged. The caller owns hideUI and the world zoom
SpriteBatch matrix. Outputs are raw FNA sprites, before that matrix/GPU stage. -/
def draw (state : State) (fonts : Fonts) (context : DrawContext) : Except String (Array Sprite) := do
  let mut out := #[]
  for entry in state.slots.toArray do
    if entry.active then
      let font := if entry.crit then fonts.critical else fonts.normal
      let size ← measureText font entry.text
      let origin : Vec2 := ⟨size.x*0.5,size.y*0.5⟩
      let ratio := entry.scale / 1
      let outlineFactor := ratio*entry.alpha*0.3
      let outlineAlpha := ratio*entry.alpha
      let channel := fun (n : Nat) (factor : Float32) => (single n*factor).toInt64.toInt
      let outline := Color.mk' (channel entry.color.r outlineFactor)
        (channel entry.color.g outlineFactor) (channel entry.color.b outlineFactor)
        (channel entry.color.a outlineAlpha)
      let foregroundChannel := fun (n : Nat) => (single n*ratio*entry.alpha).toInt64.toInt
      let foreground := Color.mk' (foregroundChannel entry.color.r) (foregroundChannel entry.color.g)
        (foregroundChannel entry.color.b) (foregroundChannel entry.color.a)
      for (dx,dy,color) in #[ ((-1 : Float32),(0 : Float32),outline), (1,0,outline),
          (0,-1,outline), (0,1,outline), (0,0,foreground) ] do
        let px := ((entry.position.x-context.camera.x)+dx)+origin.x
        let py := if context.gravity == -1 then
            ((single context.screenHeight-(entry.position.y-context.camera.y))+dy)+origin.y
          else ((entry.position.y-context.camera.y)+dy)+origin.y
        out := out ++ (← drawText font entry.text ⟨px,py⟩ color entry.rotation origin entry.scale)
  return out

end Lean4Terria.OriginalCombatText
