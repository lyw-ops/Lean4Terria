import Lean4Terria.OriginalCollision

namespace Lean4Terria.OriginalTileRules

open OriginalCollision (TileRules)

/-! Tile flags read from the initialized Terraria 1.4.5.8 runtime. These
are the loaded world defaults, before temporary changes made by simulation. -/

private def tileCount : Nat := 754

private def flags (ids : Array Nat) : Array Bool :=
  ids.foldl (fun table id => table.set! id true) (Array.replicate tileCount false)

private def solid : Array Bool := flags
  #[0, 1, 2, 6, 7, 8, 9, 10, 19, 22, 23, 25, 30, 37, 38, 39, 40, 41, 43, 44, 45, 46, 47, 48, 53, 54,
    56, 57, 58, 59, 60, 63, 64, 65, 66, 67, 68, 70, 75, 76, 107, 108, 109, 111, 112, 116, 117, 118,
    119, 120, 121, 122, 123, 127, 130, 137, 138, 140, 145, 146, 147, 148, 150, 151, 152, 153, 154,
    155, 156, 157, 158, 159, 160, 161, 162, 163, 164, 166, 167, 168, 169, 170, 175, 176, 177, 179,
    180, 181, 182, 183, 188, 189, 190, 191, 192, 193, 194, 195, 196, 197, 198, 199, 200, 202, 203,
    204, 206, 208, 211, 221, 222, 223, 224, 225, 226, 229, 230, 232, 234, 235, 239, 248, 249, 250,
    251, 252, 253, 255, 256, 257, 258, 259, 260, 261, 262, 263, 264, 265, 266, 267, 268, 272, 273,
    274, 284, 311, 312, 313, 315, 321, 322, 325, 326, 327, 328, 329, 345, 346, 347, 348, 350, 357,
    367, 368, 369, 370, 371, 379, 380, 381, 383, 384, 385, 387, 388, 396, 397, 398, 399, 400, 401,
    402, 403, 404, 407, 408, 409, 415, 416, 417, 418, 421, 422, 426, 427, 430, 431, 432, 433, 434,
    435, 436, 437, 438, 439, 446, 447, 448, 458, 459, 460, 472, 473, 474, 476, 477, 478, 479, 481,
    482, 483, 484, 492, 495, 496, 498, 500, 501, 502, 503, 507, 508, 512, 513, 514, 515, 516, 517,
    534, 535, 536, 537, 539, 540, 541, 546, 557, 562, 563, 566, 618, 625, 626, 627, 628, 633, 635,
    641, 659, 661, 662, 664, 666, 667, 668, 669, 670, 671, 672, 673, 674, 675, 676, 677, 678, 679,
    680, 681, 682, 683, 684, 685, 686, 687, 688, 689, 690, 691, 692, 708, 711, 712, 713, 714, 715,
    716, 717, 718, 719, 722, 726, 727, 728, 729, 730, 731, 732, 734, 735, 736, 737, 738, 739, 740,
    741, 742, 743, 744, 745, 746, 747, 748, 749, 750]

private def solidTop : Array Bool := flags
  #[14, 16, 18, 19, 87, 88, 101, 114, 134, 239, 275, 276, 277, 278, 279, 280, 281, 285, 286, 296,
    297, 298, 299, 309, 310, 339, 358, 359, 361, 362, 363, 364, 376, 380, 391, 392, 393, 394, 405,
    413, 414, 427, 435, 436, 437, 438, 439, 469, 532, 533, 538, 542, 544, 550, 551, 553, 554, 555,
    556, 558, 559, 582, 599, 600, 601, 602, 603, 604, 605, 606, 607, 608, 609, 610, 611, 612, 619,
    629, 632, 640, 643, 644, 645, 710]

private def platforms : Array Bool := flags
  #[19, 427, 435, 436, 437, 438, 439]

private def cuttable : Array Bool := flags
  #[3, 24, 28, 32, 51, 52, 61, 62, 69, 71, 73, 74, 82, 83, 84, 110, 113, 115,
    184, 201, 205, 231, 236, 254, 352, 382, 444, 454, 484, 485, 518, 519, 528, 529,
    549, 636, 637, 638, 654, 655, 711]

def isCuttable (id : UInt16) : Bool :=
  cuttable[id.toNat]?.getD false

def breakableInObjectPlacement (id : UInt16) : Bool :=
  (isCuttable id && id != 484 && id != 654) ||
    (#[324, 186, 187, 185, 165, 530, 233, 227, 485, 81, 624] : Array UInt16).contains id

private def tables : Array Bool := flags
  #[14, 18, 19, 87, 88, 101, 114, 275, 276, 277, 278, 279, 280, 281, 285, 286, 296, 297, 298, 299,
    309, 310, 339, 358, 359, 361, 362, 363, 364, 376, 380, 391, 392, 393, 394, 405, 413, 414, 427,
    435, 436, 437, 438, 439, 469, 532, 533, 538, 542, 544, 550, 551, 553, 554, 555, 556, 558, 559,
    582, 599, 600, 601, 602, 603, 604, 605, 606, 607, 608, 609, 610, 611, 612, 619, 629, 632, 640,
    643, 644, 645, 710]

def isTable (id : UInt16) : Bool :=
  tables[id.toNat]?.getD false

private def noAttach : Array Bool := flags
  #[3, 4, 10, 13, 14, 15, 16, 17, 18, 19, 20, 21, 27, 50, 86, 87, 88, 89, 90, 91, 92, 93, 94,
    95, 96, 97, 98, 99, 101, 102, 110, 114, 134, 387, 388, 390, 427, 435, 436, 437, 438, 439, 441, 467,
    468, 469, 486, 487, 488, 489, 490, 497, 564, 565, 568, 569, 570, 572, 580, 590, 593, 594, 595,
    615, 620, 704, 707]

def preventsSolidAnchor (id : UInt16) : Bool :=
  noAttach[id.toNat]?.getD false

private def closedDoors : Array Bool := flags
  #[10, 388]

private def spawnHazards : Array Bool := flags
  #[32, 37, 48, 53, 58, 69, 76, 80, 112, 116, 123, 224, 230, 232, 234, 352, 484, 655, 684, 750]

def collision (id : UInt16) : TileRules :=
  let i := id.toNat
  { solid := solid[i]?.getD false
    solidTop := solidTop[i]?.getD false
    platform := platforms[i]?.getD false
    closedDoor := closedDoors[i]?.getD false }

def isRope (id : UInt16) : Bool :=
  (#[213, 214, 353, 365, 366, 449, 450, 451, 504] : Array UInt16).contains id

def isTreeTrunk (id : UInt16) : Bool :=
  (#[5, 72, 583, 584, 585, 586, 587, 588, 589, 596, 616, 634] : Array UInt16).contains id

def supportsBlockPlacement (id : UInt16) : Bool :=
  (collision id).solid || isRope id ||
    (#[124, 314, 561, 574, 575, 576, 577, 578] : Array UInt16).contains id

/-- Whether the tile is unsafe under the spawn clearance check, which
uses no player-specific immunities. -/
def canHurtAtSpawn (id : UInt16) (getGoodWorld dontStarveWorld : Bool := false) : Bool :=
  (id != 230 || getGoodWorld) && (id != 80 || dontStarveWorld) && spawnHazards[id.toNat]?.getD false

end Lean4Terria.OriginalTileRules
