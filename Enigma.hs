module Enigma where

import Data.Bits
import Data.Foldable (foldl')
import Data.List
import Data.Word

---

{-

(0) ID GEAR
1 -> 1
0 -> 0

(1) FLIP GEAR
1 -> 0
0 -> 1

For now, assume we're handling `Word64` in all cases.

-}

-- | Encrypt/decrypt a data block and advance the gear.
crypt :: Word64 -> Word64 -> Word64
crypt k v = foldl' xor v . realign $ gears k

-- | The gear states necessary to encrypt all 64 bits of the input data.
gears :: Word64 -> [Word64]
gears g = take 64 [g, g+1 ..]

-- | Realign the gears bit-by-bit to allow for efficient XOR usage later.
--
-- Example. The three columnar gearsets:
--
-- @
-- [ 0 ] [ 0 ] [ 0 ]
-- [ 1 ] [ 0 ] [ 0 ]
-- [ 0 ] [ 1 ] [ 0 ]
-- @
--
-- are regrouped into rows:
--
-- @
-- [ 0 0 0 ]
-- [ 1 0 0 ]
-- [ 0 1 0 ]
-- @
realign :: [Word64] -> [Word64]
realign = map i2b . transpose . map b2i

-- | Unfold a `Word64` into its constituent bits.
b2i :: Word64 -> [Bool]
b2i g = map (testBit g) [0 .. 63]

-- | Collapse a group of bits into a `Word64`.
i2b :: [Bool] -> Word64
i2b = foldl' f zeroBits . zip [0..]
  where f acc (n, True) = setBit acc n
        f acc _ = acc
