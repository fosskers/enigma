-- |
-- Module    : Enigma
-- Copyright : (c) Colin Woodbury, 2017
-- License   : BSD3
-- Maintainer: Colin Woodbury <colingw@gmail.com>
--
-- This library provides a symmetric key encryption algorithm inspired by
-- the famous /Enigma Machine/ used by Nazi Germany in World War 2.
--
-- The original machine had two main hardware deficiencies that allowed
-- the Allies to eventually crack it:
--
-- 1. To keep the machine small, it only had 3 (or 4, depending on the model)
-- mechanical /Rotors/ which each mapped an input value to some output. Wired
-- together, these Rotors would map input letters pressed on a keyboard to
-- output letters. With each input the Rotors would advance, changing the
-- mapping. However, with so few Rotors, the amount of obfuscation steps that
-- it could perform were limited.
--
-- 2. To address the above problem, the designers of the Enigma decided to
-- have the output of the last Rotor feed back into itself, winding its way
-- back through the machine. This artificially extends the Rotor count, but
-- also proved to be the critical flaw.
--
-- More detailed explanations of the original machine can be easily found on
-- the internet.
--
-- This library takes the following approach:
--
-- 1. It works on bits, not characters.
-- 2. The 26-letter Rotors are reduced to 2 possible bit transformations, "id" and "flip".
-- 3. We represent a single rotor as a single bit. 0 is "id", 1 is "flip".
-- 4. The entire machine (group of rotors) is represented by a series of bits.
-- 5. Like the original Enigma, our secret key is the initial state of the rotors,
--    which is just a series of bits. It can thus be arbitrarily long to offer
--    many many obfuscation steps.
-- 6. Like the original Enigma, each input (bit) is encrypted with a different key (rotor set).
-- 7. Like the original Enigma, the ciphering algorithm both encrypts and decrypts.
-- 8. The ciphering algorithm doesn't feed back into itself.

module Enigma
  ( -- * Encryption / Decryption
    Key(..)
  , cipher
    -- * Re-exports
  , runState, execState, evalState
  ) where

import           Control.Monad.Trans.State
import           Data.Bits
import qualified Data.Vector.Unboxed as U
import           Data.Word

---

-- | A secret key. Represents the state of all the Rotors.
newtype Key = Key { _key :: Word64 } deriving (Eq, Show)

-- | Encrypt/decrypt a data block. The `State` value is the last rotorset used.
--
-- > λ evalState (cipher (evalState (cipher v) k)) k == v
-- > True
--
-- ==== __Examples__
--
-- Encrypt a `Word64`:
--
-- > λ evalState (cipher 1) $ Key 1
-- > 6513794518609451620
--
-- Decrypt a `Word64`:
--
-- > λ evalState (cipher 6513794518609451620) $ Key 1
-- > 1
--
-- Chain encryptions, carrying over the rotor state between operations:
--
-- > λ evalState ((,) <$> cipher 1 <*> cipher 2) $ Key 1
-- > (6513794518609451620,7321263536672712088)
--
-- Encrypt a `Foldable`:
--
-- > λ evalState (traverse cipher [1,2,3]) $ Key 1
-- > [6513794518609451620,7321263536672712088,11932949555100099993]
--
-- Encrypt a message:
--
-- > λ evalState (traverse (cipher . fromIntegral . ord) "Hint") $ Key 1
-- > [6513794518609451565,7321263536672712179,11932949555100100084,6513794518609451537]
cipher :: Word64 -> State Key Word64
cipher v = xor v . b2w . U.map (odd . popCount) <$> rotors

-- | The rotor states necessary to encrypt all 64 bits of the input data.
rotors :: State Key (U.Vector Word64)
rotors = do
  r <- _key <$> get
  let rs = U.enumFromN (r+1) 64
  put . Key $ U.last rs
  pure rs

-- | Pack a list of "bits" back into a `Word64`.
b2w :: U.Vector Bool -> Word64
b2w = U.foldl' f zeroBits . U.zip (U.enumFromN 0 64)
  where f acc (n, True) = setBit acc n
        f acc _ = acc
