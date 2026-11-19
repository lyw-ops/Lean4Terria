import Lean4Terria.OriginalBanks
import Lean4Terria.OriginalBankProjectiles.Chester

/-!
# Summoned personal-storage projectiles

Handwritten from vanilla 1.4.5.8 for the Money Trough (item 3213, projectile 525),
the Void Bag in both forms (items 4131 and 5325, projectile 734) and the Eye Bone
(item 5098, projectile 960, Chester).

* `Player.ItemCheck_Shoot` gives the aim and the spawn.
* `Projectile.Update` gives the per-frame order: AI, `HandleMovement`,
  `AutomaticallyChangesDirection`, the `active` return, then `timeLeft`.
* `AI_154_VoidLens` and the `aiStyle == 97` branch of `Projectile.AI` give the motion.
* `Main.TryInteractingWithVoidLens` and `Main.TryInteractingWithMoneyTrough` (the
  latter also serves 960) give hover and right-click. They are called from
  `DrawProjDirect`, so they run on the draw side of the frame.
* `Player.HandleBeingInChestRange` decides whether the storage stays open.
* `Recipe.CollectItemsFromChests` and `NearbyChests.GetBanksInRangeOf` decide
  crafting visibility.

All arithmetic is `Float32`, done in the order of the IL. The local player owns
every projectile here (`owner == Main.myPlayer`).

Not modelled:
* wet, lava and shimmer movement (`Projectile.UpdatePosition` uses the pre-AI
  velocity in water);
* `gfxOffY` and mount rotation in `RotatedRelativePoint`;
* dust and light;
* outside `Chester`, which carries the pet AI (`aiStyle 26`) and buff 331:
  wet movement, the sub-stepped `HandleMovement` path and the petting states.

`updateAll` keeps its signature and still moves only 525 and 734.
`updateWorld` also moves Chesters. It takes the collision grid, the owner and
the open storage.
-/

namespace Lean4Terria.OriginalBankProjectiles

open OriginalCollision (Vector single tileIndex)
open OriginalItems (Inventory)
open OriginalMining (Cell inToolRange)
open OriginalPlayerCollision (Body)

inductive Kind where
  | moneyTrough | voidLens | chester
  deriving BEq, Repr, Inhabited

def Kind.projectileId : Kind → Nat
  | .moneyTrough => 525
  | .voidLens => 734
  | .chester => 960

def Kind.aiStyle : Kind → Nat
  | .moneyTrough => 97
  | .voidLens => 154
  | .chester => 26

/-- `Item.shoot` of the summoning items. Both Void Bag forms summon 734. -/
def Kind.ofItem : UInt16 → Option Kind
  | 3213 => some .moneyTrough
  | 4131 | 5325 => some .voidLens
  | 5098 => some .chester
  | _ => none

/-- `Player.cursorItemIconID` while the mouse is over the projectile. -/
def Kind.cursorItem : Kind → UInt16
  | .moneyTrough => 3213
  | .voidLens => 4131
  | .chester => 5098

/-- `Player.noThrow` written on hover. -/
def Kind.noThrow : Kind → Nat
  | .voidLens => 2
  | _ => 4

/-- `Projectile.TryGetContainerIndex`, the `player.chest` value. -/
def Kind.chestIndex : Kind → Int
  | .voidLens => -5
  | _ => -2

def Kind.bank : Kind → OriginalBanks.Kind
  | .voidLens => .voidVault
  | _ => .piggyBank

def Kind.width : Kind → Nat
  | .chester => 20
  | _ => 30

def Kind.height : Kind → Nat
  | .chester => 20
  | _ => 24

/-- `Main.projFrames`. -/
def Kind.frames : Kind → Nat
  | .moneyTrough => 5
  | .voidLens => 8
  | .chester => 20

/-- `timeLeft` from `SetDefaults`. -/
def Kind.lifetime : Kind → Nat
  | .chester => 18000
  | _ => 10800

/-- Projectile 734 starts transparent and fades in. -/
def Kind.spawnAlpha : Kind → Nat
  | .voidLens => 255
  | _ => 0

def Kind.shootSpeed : Kind → Float32
  | .chester => 0
  | _ => 4

/-- `useTime == useAnimation` for every item here; none auto-reuses. -/
def Kind.useTime : Kind → Nat
  | .chester => 20
  | _ => 28

/-- Eye Bone `buffType`. -/
def chesterBuff : Nat := 331

inductive Sound where
  /-- `SoundID.Item<n>`, `LegacySoundStyle(2, n)`. -/
  | item (style : Nat)
  /-- `SoundID.ChesterOpen`, `LegacySoundStyle(42, 251)`, 2 variations. -/
  | chesterOpen
  /-- `SoundID.ChesterClose`, `LegacySoundStyle(42, 253)`, 2 variations. -/
  | chesterClose
  deriving BEq, Repr

/-- `Item.UseSound`. -/
def Kind.useSound : Kind → Sound
  | .moneyTrough => .item 59
  | .voidLens => .item 130
  | .chester => .item 2

/-- The Void Lens plays Item130 directly. `PlayInteractiveProjectileOpenCloseSound`
plays Item59 for 525 both ways and Chester open/close for 960. -/
def Kind.toggleSound (kind : Kind) (opening : Bool) : Sound :=
  match kind with
  | .voidLens => .item 130
  | .moneyTrough => .item 59
  | .chester => if opening then .chesterOpen else .chesterClose

structure Projectile where
  kind : Kind
  /-- Identity behind `TrackedProjectileReference`. A new projectile gets a new one. -/
  serial : Nat
  active : Bool := true
  position : Vector
  velocity : Vector
  alpha : Nat
  timeLeft : Nat
  frame : Nat := 0
  frameCounter : Nat := 0
  direction : Int := 1
  spriteDirection : Int := 1
  ai0 : Float32 := 0
  ai1 : Float32 := 0
  /-- Pet-only state (960). The other kinds leave these at their defaults. -/
  localAI0 : Float32 := 0
  localAI1 : Float32 := 0
  oldPosition : Vector := ⟨0, 0⟩
  rotation : Float32 := 0
  tileCollide : Bool := true
  gfxOffY : Float32 := 0
  stepSpeed : Float32 := 0
  deriving BEq, Repr

/-- `Main.projectile`, indexed by `whoAmI`. `none` is an empty slot. -/
abbrev Slots := Array (Option Projectile)

def maxProjectiles : Nat := 1000

/-- `Entity.Center` uses integer halves of the size. -/
def entityCenter (position : Vector) (width height : Nat) : Vector :=
  ⟨position.x + single (width / 2 : Nat), position.y + single (height / 2 : Nat)⟩

def bodyCenter (body : Body) : Vector := entityCenter body.position body.width body.height

def Projectile.center (p : Projectile) : Vector := entityCenter p.position p.kind.width p.kind.height

/-! ## Use and spawn -/

/-- The four items swing once per press because they lack autoReuse.
`releaseUseItem` is true after a frame with the button up. -/
structure Use where
  itemAnimation : Nat := 0
  itemTime : Nat := 0
  releaseUseItem : Bool := true
  deriving BEq, Repr, Inhabited

/-- One `ItemCheck`. The shot fires on the frame the swing starts:
`itemAnimation = useAnimation - 1`, then `itemTime = useTime`. -/
def Use.step (use : Use) (kind : Kind) (held : Bool) : Use × Bool :=
  if held && use.releaseUseItem && use.itemAnimation == 0 then
    ({ itemAnimation := kind.useTime - 1, itemTime := kind.useTime, releaseUseItem := false }, true)
  else
    ({ itemAnimation := use.itemAnimation - 1, itemTime := use.itemTime - 1,
       releaseUseItem := !held }, false)

/-- `RotatedRelativePoint(MountedCenter)`, standing, no `gfxOffY`. -/
def shootOrigin (body : Body) : Vector := bodyCenter body

/-- `Main.mouseX + Main.screenPosition.X`, all in Float32. -/
def mouseWorld (mouseX mouseY : Int) (screen : Vector) : Vector :=
  ⟨single mouseX + screen.x, single mouseY + screen.y⟩

/-- `ItemCheck_Shoot`. A `NaN` pair or a zero pair aims along `direction`. -/
def aimVelocity (origin mouse : Vector) (direction : Int) (speed : Float32) : Vector :=
  let dx := mouse.x - origin.x
  let dy := mouse.y - origin.y
  let length := Float32.sqrt (dx * dx + dy * dy)
  if (dx.isNaN && dy.isNaN) || (dx == 0 && dy == 0) then
    ⟨single direction * speed, 0 * speed⟩
  else
    let scale := speed / length
    ⟨dx * scale, dy * scale⟩

/-- `Projectile.NewProjectile` centres the hitbox on the origin. -/
def spawn (kind : Kind) (serial : Nat) (origin velocity : Vector) : Projectile :=
  { kind, serial, velocity, alpha := kind.spawnAlpha, timeLeft := kind.lifetime
    position := ⟨origin.x - single kind.width * 0.5, origin.y - single kind.height * 0.5⟩ }

/-- `NewProjectile` takes the first inactive slot. With all 1000 busy, nothing spawns. -/
def Slots.insert (slots : Slots) (p : Projectile) : Slots × Option Nat :=
  match slots.findIdx? (fun slot => !(slot.any (·.active))) with
  | some i => (slots.set! i (some p), some i)
  | none => if slots.size < maxProjectiles then (slots.push (some p), some slots.size)
            else (slots, none)

def Projectile.kill (p : Projectile) : Projectile := { p with active := false, timeLeft := 0 }

/-- `ItemCheck_Shoot`. 960 is a pet (`Main.projPet`), so the local player's
Chesters die before `NewProjectile`, and the new one usually reuses the old
slot. 525 and 734 spawn beside the old one, and the AI loop removes the older. -/
def summon (slots : Slots) (p : Projectile) : Slots × Option Nat :=
  let slots := if p.kind == .chester then
      slots.map (·.map fun q => if q.active && q.kind == .chester then q.kill else q)
    else slots
  Slots.insert slots p

/-! ## Per-frame update -/

def opacity (alpha : Nat) : Float32 := 1 - alpha.toFloat32 / 255

/-- `set_Opacity`: `(int)MathHelper.Clamp((1 - v) * 255, 0, 255)`. -/
def alphaOf (value : Float32) : Nat :=
  let raw := (1 - value) * 255
  let clamped := if raw > 255 then 255 else if raw < 0 then 0 else raw
  clamped.toInt32.toInt.toNat

def scaleVector (v : Vector) (k : Float32) : Vector := ⟨v.x * k, v.y * k⟩

/-- The Void Lens AI after the replacement loop. -/
def voidLensAI (p : Projectile) : Projectile := Id.run do
  let mut p := { p with velocity := scaleVector p.velocity 0.94, direction := 1 }
  let counter := p.frameCounter + 1
  p := if counter ≥ 5 then
      let frame := p.frame + 1
      { p with frameCounter := 0, frame := if frame ≥ p.kind.frames then 0 else frame }
    else { p with frameCounter := counter }
  if opacity p.alpha < 1 then
    p := { p with alpha := alphaOf (opacity p.alpha + 0.03) }
    if opacity p.alpha > 1 then p := { p with alpha := alphaOf 1 }
  return p

/-- The `aiStyle == 97` frame cycle 0,1,2,3,4,3,2,1 on a 4-tick step. -/
def moneyTroughFrame (p : Projectile) : Projectile :=
  let counter := p.frameCounter + 1
  let c := counter.toFloat32
  let frames : List Nat := [0, 1, 2, 3, 4, 3, 2, 1]
  match (List.range 8).find? (fun k => c < 4 * (k + 1 : Nat).toFloat32) with
  | some k => { p with frameCounter := counter, frame := frames[k]! }
  | none => { p with frameCounter := 0, frame := 0 }

/-- The `aiStyle == 97` motion after the replacement loop: drift, then hover. -/
def moneyTroughMotion (p : Projectile) (ownerCenterX : Float32) : Projectile :=
  if p.ai0 == 0 then
    let v := p.velocity
    if (Float32.sqrt (v.x * v.x + v.y * v.y)).toFloat < 0.1 then
      { p with velocity := ⟨0, 0⟩, ai0 := 1, ai1 := 45 }
    else
      let velocity := scaleVector v 0.94
      let direction : Int := if velocity.x < 0 then -1 else 1
      { p with velocity, direction, spriteDirection := direction }
  else
    let direction : Int := if ownerCenterX < p.center.x then -1 else 1
    let ai1 := p.ai1 + 1
    let vy := if ai1 > 0 then p.velocity.y - 0.005 else p.velocity.y + 0.005
    let ai1 := if ai1 ≥ 90 then ai1 * -1 else ai1
    { p with direction, spriteDirection := direction, ai1, velocity := ⟨p.velocity.x, vy⟩ }

/-- The replacement loop shared by both AIs. The projectile with more `timeLeft`
survives, and a tie keeps the one running its AI. Slots before `i` already
updated this frame. -/
def replaceOlder (slots : Slots) (i : Nat) : Slots × Bool := Id.run do
  let some (some me) := slots[i]? | return (slots, false)
  let mut slots := slots
  let mut me := me
  let mut selfKilled := false
  for j in [0:slots.size] do
    if j == i then continue
    let some (some other) := slots[j]? | continue
    if !other.active || other.kind != me.kind then continue
    if me.timeLeft ≥ other.timeLeft then
      slots := slots.set! j (some other.kill)
    else
      me := me.kill
      selfKilled := true
      slots := slots.set! i (some me)
      -- AI_154 returns at once. The aiStyle 97 loop keeps scanning with timeLeft 0.
      if me.kind == .voidLens then return (slots, true)
  return (slots, selfKilled)

/-- One `Projectile.Update` of slot `i`, dry and without tile collision. -/
def updateSlot (slots : Slots) (i : Nat) (ownerCenterX : Float32) : Slots := Id.run do
  let some (some start) := slots[i]? | return slots
  if !start.active || start.kind == .chester then return slots
  let slots0 := if start.kind == .moneyTrough then slots.set! i (some (moneyTroughFrame start)) else slots
  let (slots1, selfKilled) := replaceOlder slots0 i
  let some (some p) := slots1[i]? | return slots1
  let p := match p.kind with
    | .voidLens => if selfKilled then p else voidLensAI p
    | _ => moneyTroughMotion p ownerCenterX
  -- HandleMovement (tileCollide false): position += velocity after the AI
  let p := { p with position := p.position + p.velocity }
  -- AutomaticallyChangesDirection holds for aiStyle 97 and 154
  let p := { p with direction := if p.velocity.x < 0 then -1 else 1 }
  if !p.active then return slots1.set! i (some p)
  let p := { p with timeLeft := p.timeLeft - 1 }
  let p := if p.timeLeft == 0 then p.kill else p
  return slots1.set! i (some p)

/-- `Main.UpdateWorld_Projectiles` over the slots in index order. -/
def updateAll (slots : Slots) (ownerCenterX : Float32) : Slots := Id.run do
  let mut slots := slots
  for i in [0:slots.size] do
    slots := updateSlot slots i ownerCenterX
  return slots

/-! ## Chester (960) -/

def Projectile.toPet (p : Projectile) : Chester.Pet :=
  { active := p.active, position := p.position, velocity := p.velocity, oldPosition := p.oldPosition,
    timeLeft := p.timeLeft, ai0 := p.ai0, ai1 := p.ai1, localAI0 := p.localAI0, localAI1 := p.localAI1,
    frame := p.frame, frameCounter := p.frameCounter, direction := p.direction,
    spriteDirection := p.spriteDirection, rotation := p.rotation, tileCollide := p.tileCollide,
    gfxOffY := p.gfxOffY, stepSpeed := p.stepSpeed }

def Projectile.withPet (p : Projectile) (q : Chester.Pet) : Projectile :=
  { p with active := q.active, position := q.position, velocity := q.velocity, oldPosition := q.oldPosition,
           timeLeft := q.timeLeft, ai0 := q.ai0, ai1 := q.ai1, localAI0 := q.localAI0, localAI1 := q.localAI1,
           frame := q.frame, frameCounter := q.frameCounter, direction := q.direction,
           spriteDirection := q.spriteDirection, rotation := q.rotation, tileCollide := q.tileCollide,
           gfxOffY := q.gfxOffY, stepSpeed := q.stepSpeed }

/-- `Main.UpdateWorld_Projectiles` with Chesters. `tracked` names the projectile
whose storage is open (`Storage.tracker`), which drives the opening animation. -/
def updateWorld (grid : OriginalCollision.Grid) (owner : Chester.Owner) (slots : Slots)
    (tracked : Option (Kind × Nat) := none) : Slots := Id.run do
  let mut slots := slots
  for i in [0:slots.size] do
    match slots[i]? with
    | some (some p) =>
      if p.kind == .chester then
        if p.active then
          let isTracked := tracked == some (.chester, p.serial)
          slots := slots.set! i (some (p.withPet (Chester.update grid owner p.toPet isTracked)))
      else slots := updateSlot slots i owner.center.x
    | _ => pure ()
  return slots

def ownedChesters (slots : Slots) : Nat :=
  (slots.filter (·.any fun p => p.active && p.kind == .chester)).size

/-- `Player.UpdateBuffs` for buff 331, run inside `Player.Update` before the
item use. Returns the new buff, `petFlagChesterPet` and the slots, with a
Chester spawned at the player centre if none is owned. -/
def chesterBuffStep (buff : Chester.Buff) (body : Body) (slots : Slots) (serial : Nat) :
    Chester.Buff × Bool × Slots × Bool := Id.run do
  let (buff, flag, needSpawn) := buff.update (ownedChesters slots)
  if !needSpawn then return (buff, flag, slots, false)
  let (slots, _) := Slots.insert slots (spawn .chester serial (bodyCenter body) ⟨0, 0⟩)
  return (buff, flag, slots, true)

/-! ## Hover, right-click and staying open -/

/-- `Entity.Hitbox`: `(int)position`, size. -/
def Projectile.hitbox (p : Projectile) : Int × Int × Nat × Nat :=
  (p.position.x.toInt32.toInt, p.position.y.toInt32.toInt, p.kind.width, p.kind.height)

/-- `Utils.ClosestPointInRect`. It clamps to the float `Left/Right/Top/Bottom`,
and `Right = X + Width`. -/
def closestPoint (hitbox : Int × Int × Nat × Nat) (point : Vector) : Vector :=
  let (x, y, w, h) := hitbox
  let cx := if point.x < single x then single x else point.x
  let cx := if cx > single (x + w) then single (x + w) else cx
  let cy := if point.y < single y then single y else point.y
  let cy := if cy > single (y + h) then single (y + h) else cy
  ⟨cx, cy⟩

def toTile (v : Vector) : Int × Int := (tileIndex v.x, tileIndex v.y)

/-- `IsInTileInteractionRange(x, y, Simple, 0)`. -/
def inTileInteractionRange (body : Body) (tile : Int × Int) : Bool :=
  tile.1 ≥ 0 && tile.2 ≥ 0 && inToolRange body ⟨tile.1.toNat, tile.2.toNat⟩ 5 3 0

/-- `IsProjectileInteractableAndInInteractionRange`. `IsInteractable` holds for
all three types. -/
def inInteractionRange (body : Body) (p : Projectile) : Bool :=
  p.active && inTileInteractionRange body (toTile (closestPoint p.hitbox (bodyCenter body)))

/-- `Rectangle.Contains(mouse.ToPoint())`. -/
def mouseOver (p : Projectile) (mouse : Vector) : Bool :=
  let (x, y, w, h) := p.hitbox
  let mx := mouse.x.toInt32.toInt
  let my := mouse.y.toInt32.toInt
  x ≤ mx && mx < x + w && y ≤ my && my < y + h

/-- What `player.chest`, `chestX/Y`, `Main.playerInventory` and the tracked
reference (`voidLensChest` for 734, `piggyBankProjTracker` for 525 and 960) hold. -/
structure Storage where
  chest : Int := -1
  tracker : Option (Kind × Nat) := none
  chestX : Int := 0
  chestY : Int := 0
  playerInventory : Bool := false
  deriving BEq, Repr, Inhabited

structure Click where
  storage : Storage
  /-- `HasInteractableObjectThatIsNotATile`: the tile right-click is skipped this frame. -/
  blocksTiles : Bool := true
  /-- The cursor shows `kind.cursorItem` and `noThrow := kind.noThrow`. -/
  cursorIcon : Bool := true
  /-- `Main.mouseRightRelease := false`, `releaseUseTile := false`. -/
  consumed : Bool := false
  /-- `Main.stackSplit := 600` on opening. -/
  stackSplit : Option Nat := none
  sound : Option Sound := none
  deriving BEq, Repr

/-- `TryInteractingWith{VoidLens,MoneyTrough}` with mouse aim, not smart cursor.
`none` means the projectile does not take the cursor this frame.
`blockInteraction` is `Player.BlockInteractionWithProjectiles`; it is 3 at
start-up and after `ToggleInv`, and counts down on frames with the right
button up and released. -/
def interact (storage : Storage) (body : Body) (p : Projectile) (mouse : Vector)
    (mouseRight mouseRightRelease : Bool) (blockInteraction : Nat)
    (lastMouseInterface : Bool := false) (disableVoidBag : Int := -1) : Option Click := do
  guard (inInteractionRange body p && mouseOver p mouse && !lastMouseInterface)
  if !(mouseRight && mouseRightRelease && blockInteraction == 0) then return { storage }
  let index := p.kind.chestIndex
  if storage.chest == index then
    return { storage := { storage with chest := -1 }, consumed := true,
             sound := some (p.kind.toggleSound false) }
  if p.kind == .voidLens && disableVoidBag ≥ 0 then return { storage, consumed := true }
  let (tx, ty) := toTile p.center
  return { storage := { storage with chest := index, tracker := some (p.kind, p.serial),
                                     chestX := tx, chestY := ty, playerInventory := true },
           consumed := true, stackSplit := some 600, sound := some (p.kind.toggleSound true) }

def findTracked (slots : Slots) (kind : Kind) (serial : Nat) : Option Projectile :=
  slots.findSome? fun slot => slot.bind fun p =>
    if p.active && p.kind == kind && p.serial == serial then some p else none

/-- The tracked branches of `HandleBeingInChestRange`, run inside `Player.Update`.
A reference whose chest index no longer equals `player.chest` is dropped first.
A missing projectile, or one out of reach of its hitbox closest point, closes
the storage with the close sound. `tracked = false` leaves the tile rules
(`OriginalBanks.inOpenRange`) in charge. Pass the body from before this frame's
movement: `HandleBeingInChestRange` runs before `Player.Update` moves the player,
so walking away closes the storage one frame after the position leaves range. -/
def keepOpen (storage : Storage) (body : Body) (slots : Slots) : Storage × Option Sound × Bool := Id.run do
  let storage := match storage.tracker with
    | some (kind, _) => if storage.chest == kind.chestIndex then storage else { storage with tracker := none }
    | none => storage
  let some (kind, serial) := storage.tracker | return (storage, none, false)
  let some p := findTracked slots kind serial
    | return ({ storage with chest := -1 }, some (kind.toggleSound false), true)
  let near := closestPoint p.hitbox (bodyCenter body)
  let storage := { storage with chestX := near.x.toInt32.toInt.tdiv 16, chestY := near.y.toInt32.toInt.tdiv 16 }
  if inTileInteractionRange body (storage.chestX, storage.chestY) then return (storage, none, true)
  return ({ storage with chest := -1 }, some (kind.toggleSound false), true)

/-! ## Crafting visibility -/

/-- `Player.useVoidBag`: an open bag with stack > 0 in slots 0..57. -/
def useVoidBag (inventory : Inventory) : Bool :=
  (inventory.extract 0 58).any (·.any fun s => s.item == OriginalBanks.openVoidBag && s.count > 0)

/-- `ContainerIndexToPlayerBank(-5)` fails if slots 0..57 hold a closed bag with stack > 0. -/
def voidVaultUsable (inventory : Inventory) : Bool :=
  !(inventory.extract 0 58).any (·.any fun s => s.item == OriginalBanks.closedVoidBag && s.count > 0)

/-- The projectile pass of `GetBanksInRangeOf(player, 600)`: slots in index order.
The square is 79 tiles around the player centre tile, since `(int)(600/16 + 2) = 39`. -/
def portalBanks (body : Body) (inventory : Inventory) (slots : Slots) : Array OriginalBanks.Kind := Id.run do
  let (cx, cy) := toTile (bodyCenter body)
  let mut found := #[]
  for slot in slots do
    let some p := slot | continue
    if !p.active then continue
    let (tx, ty) := toTile (closestPoint p.hitbox (bodyCenter body))
    if tx < cx - 39 || tx > cx + 39 || ty < cy - 39 || ty > cy + 39 then continue
    if p.kind == .voidLens && !voidVaultUsable inventory then continue
    if !found.contains p.kind.bank then found := found.push p.kind.bank
  return found

def Storage.bankKind (storage : Storage) : Option OriginalBanks.Kind :=
  match storage.chest with
  | -2 => some .piggyBank
  | -3 => some .safe
  | -4 => some .defendersForge
  | -5 => some .voidVault
  | _ => none

/-- The personal stores in `Recipe._recipeChests`, in order:
1. the open container;
2. the Void Vault if `useVoidBag` and it is not open;
3. the projectiles in range;
4. the tile scan, `OriginalBanks.nearby` (`tileBanks`).

World chests come after these. -/
def craftingBanks (storage : Storage) (body : Body) (inventory : Inventory) (slots : Slots)
    (tileBanks : Array OriginalBanks.Kind) (craftFromNearbyChests : Bool := true) :
    Array OriginalBanks.Kind := Id.run do
  let mut out := #[]
  if let some kind := storage.bankKind then out := out.push kind
  if useVoidBag inventory && storage.chest != -5 then out := out.push .voidVault
  if craftFromNearbyChests then
    for kind in portalBanks body inventory slots ++ tileBanks do
      if !out.contains kind then out := out.push kind
  return out

end Lean4Terria.OriginalBankProjectiles
