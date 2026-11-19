namespace Lean4Terria.Desktop

@[extern "terria_open"] opaque openWindow (width height : UInt32) (title : @& String) : IO Unit
@[extern "terria_close"] opaque closeWindow : IO Unit
@[extern "terria_should_close"] opaque shouldClose : IO Bool
@[extern "terria_frame_time"] opaque frameTime : IO Float
@[extern "terria_input"] opaque input : IO UInt64
@[extern "terria_read_character"] opaque readCharacter : IO UInt32
@[extern "terria_dash_tap"] opaque dashTap : IO UInt32
@[extern "terria_mouse_x"] opaque mouseX : IO UInt32
@[extern "terria_mouse_y"] opaque mouseY : IO UInt32
@[extern "terria_begin"] opaque beginDrawing (color : UInt32) : IO Unit
@[extern "terria_end"] opaque endDrawing : IO Unit
@[extern "terria_premultiplied_alpha"] opaque premultipliedAlpha (enabled : Bool) : IO Unit
@[extern "terria_rect"] opaque rectRaw (x y width height color : UInt32) : IO Unit
@[extern "terria_gradient"] opaque gradient (width height top bottom : UInt32) : IO Unit
@[extern "terria_clip_begin"] opaque beginClip (x y width height : UInt32) : IO Unit
@[extern "terria_clip_end"] opaque endClip : IO Unit
@[extern "terria_map_input"] opaque mapInput : IO UInt64
@[extern "terria_clipboard"] opaque setClipboard (value : @& String) : IO Unit
@[extern "terria_read_clipboard"] opaque readClipboard : IO String
@[extern "terria_text_width"] opaque textWidthRaw (label : @& String) (size : UInt32) : IO UInt32
@[extern "terria_text"] opaque textRaw (label : @& String) (x y size color : UInt32) : IO Unit
@[extern "terria_sound"] opaque sound (kind : UInt32) : IO Unit
@[extern "terria_wall_music"] opaque wallMusic (active : Bool) : IO Unit
@[extern "terria_twins_music"] opaque twinsMusic (active : Bool) : IO Unit
@[extern "terria_destroyer_music"] opaque destroyerMusic (active : Bool) : IO Unit
@[extern "terria_prime_music"] opaque primeMusic (active : Bool) : IO Unit
@[extern "terria_plantera_music"] opaque planteraMusic (active : Bool) : IO Unit
@[extern "terria_golem_music"] opaque golemMusic (active : Bool) : IO Unit
@[extern "terria_temple_music"] opaque templeMusic (active : Bool) : IO Unit
@[extern "terria_magic_missile_trail"] opaque magicMissileTrail (points : @& FloatArray)
  (clock opacity : Float) : IO Bool
@[extern "terria_flamelash_trail"] opaque flamelashTrail (points : @& FloatArray)
  (clock opacity empowerment : Float) : IO Bool
@[extern "terria_original_sprite"] opaque originalSpriteRaw (asset : @& String)
  (sourceX sourceY sourceWidth sourceHeight x y width height color flags : UInt32)
  (rotation : Float) : IO Bool

def originalSprite (asset : String) (x y : Int) (width height : Nat)
    (sourceX sourceY sourceWidth sourceHeight := 0) (color : UInt32 := 0xffffffff)
    (fit mirror := false) (rotation : Float := 0) (additive queenSlime queenSlimeMount := false) : IO Bool :=
  let flags := (if fit then 1 else 0) + (if mirror then 2 else 0) + (if additive then 4 else 0) +
    (if queenSlime then 8 else 0) + (if queenSlimeMount then 16 else 0)
  originalSpriteRaw asset sourceX.toUInt32 sourceY.toUInt32 sourceWidth.toUInt32 sourceHeight.toUInt32
    (UInt32.ofInt x) (UInt32.ofInt y) width.toUInt32 height.toUInt32 color flags rotation

def rect (x y : Int) (width height : Nat) (color : UInt32) : IO Unit :=
  rectRaw (UInt32.ofInt x) (UInt32.ofInt y) width.toUInt32 height.toUInt32 color

def text (label : String) (x y : Int) (size : Nat) (color : UInt32) : IO Unit :=
  textRaw label (UInt32.ofInt x) (UInt32.ofInt y) size.toUInt32 color

def textWidth (label : String) (size : Nat) : IO Nat :=
  UInt32.toNat <$> textWidthRaw label size.toUInt32

def withClip (x y width height : Nat) (draw : IO Unit) : IO Unit := do
  beginClip x.toUInt32 y.toUInt32 width.toUInt32 height.toUInt32
  try draw finally endClip

end Lean4Terria.Desktop
