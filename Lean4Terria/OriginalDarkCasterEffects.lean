import Lean4Terria.OriginalDarkCaster

namespace Lean4Terria.OriginalDarkCaster

open OriginalCollision (Vector single tileIndex)
open OriginalForestGuideArrow (DustParticle Viewport DustFrame)

structure DustLight where
  x : Int
  y : Int
  r : Float32
  g : Float32
  b : Float32
  deriving Repr

structure DustStep where
  particle : DustParticle
  active : Bool
  light : DustLight

/-- The ordinary type 172 and 6 branches of desktop `Dust.UpdateDust`.
These naturally generated particles have zero fadeIn and no customData.
The caller advances particles in slot order and applies the returned light. -/
def advanceDust (dust : DustParticle) (view : Viewport) : DustStep :=
  let position := dust.position + dust.velocity
  let blue := dust.request.typeId == 172
  let intensity := if blue then min 1 (dust.scale * 0.5) else min 0.6 (dust.scale * 1.4)
  let light : DustLight := {
    x := tileIndex position.x, y := tileIndex position.y
    r := if blue then 0 else intensity
    g := if blue then intensity * (intensity * 0.25) else intensity * 0.65
    b := if blue then intensity * intensity else intensity * 0.4 }
  let velocity : Vector := ⟨dust.velocity.x * 0.99,
    dust.velocity.y + if dust.noGravity then 0 else if blue then 0.1 else 0.05⟩
  let rotation := dust.rotation + velocity.x * 0.5
  let scale := dust.scale - 0.01
  let velocity := if dust.noGravity then ⟨velocity.x * 0.92, velocity.y * 0.92⟩ else velocity
  let scale := if dust.noGravity then scale - 0.04 else scale
  { particle := { dust with position, velocity, rotation, scale }
    active := scale ≥ 0.1 && scale ≤ 10 && position.y ≤ view.position.y + single view.height
    light }

/-- Desktop `Dust.GetAlpha` on ordinary caster particles. Blue particles
are full-intensity translucent. Type 6 retains the sampled light's RGB. -/
def dustAlpha (dust : DustParticle) (light : UInt32) : UInt32 :=
  if dust.request.typeId == 172 then 0xfafafa96 else (light &&& 0xffffff00) ||| 25

structure NpcSprite where
  source : DustFrame
  position : Vector
  origin : Vector
  rgba : UInt32
  rotation : Float32
  scale : Float32
  flip : Bool

/-- Ordinary `DrawNPCDirect` with the loaded NPC texture dimensions.
The supplied light is sampled at the NPC's center by the desktop renderer. -/
def npcSprite (enemy : Enemy) (screen : Vector) (textureWidth frameHeight : Nat)
    (light : UInt32) : NpcSprite :=
  let n := enemy.body
  let ox := single textureWidth / 2
  let oy := single frameHeight / 2
  let x := n.position.x - screen.x + single (n.width / 2)
  let y := n.position.y - screen.y + single n.height - single frameHeight * n.scale +
    oy * n.scale + 4 + n.gfxOffY
  { source := ⟨0, n.frameY * frameHeight, textureWidth, frameHeight⟩
    position := ⟨x, y⟩, origin := ⟨ox, oy⟩
    rgba := if enemy.kind == .waterSphere then 0 else light
    rotation := n.rotation, scale := n.scale, flip := n.spriteDirection == 1 }

end Lean4Terria.OriginalDarkCaster
