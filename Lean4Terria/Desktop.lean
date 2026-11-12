namespace Lean4Terria.Desktop

@[extern "terria_open"] opaque openWindow (width height : UInt32) (title : @& String) : IO Unit
@[extern "terria_close"] opaque closeWindow : IO Unit
@[extern "terria_should_close"] opaque shouldClose : IO Bool
@[extern "terria_frame_time"] opaque frameTime : IO Float
@[extern "terria_input"] opaque input : IO UInt32
@[extern "terria_mouse_x"] opaque mouseX : IO UInt32
@[extern "terria_mouse_y"] opaque mouseY : IO UInt32
@[extern "terria_begin"] opaque beginDrawing (color : UInt32) : IO Unit
@[extern "terria_end"] opaque endDrawing : IO Unit
@[extern "terria_rect"] opaque rectRaw (x y width height color : UInt32) : IO Unit
@[extern "terria_gradient"] opaque gradient (width height top bottom : UInt32) : IO Unit
@[extern "terria_text"] opaque textRaw (label : @& String) (x y size color : UInt32) : IO Unit
@[extern "terria_sound"] opaque sound (kind : UInt32) : IO Unit

def rect (x y : Int) (width height : Nat) (color : UInt32) : IO Unit :=
  rectRaw (UInt32.ofInt x) (UInt32.ofInt y) width.toUInt32 height.toUInt32 color

def text (label : String) (x y : Int) (size : Nat) (color : UInt32) : IO Unit :=
  textRaw label (UInt32.ofInt x) (UInt32.ofInt y) size.toUInt32 color

end Lean4Terria.Desktop
