Enigma
======

[![Build Status](https://travis-ci.org/fosskers/enigma.svg?branch=master)](https://travis-ci.org/fosskers/enigma)

This library provides a symmetric key encryption algorithm inspired by the
famous [*Enigma Machine*](https://en.wikipedia.org/wiki/Enigma_machine) used
by Nazi Germany in World War 2. The original machine had two main hardware
deficiencies that allowed the Allies to eventually crack it:

1. To keep the machine small, it only had 3 (or 4, depending on the model)
mechanical *Rotors* which each mapped an input value to some output. Wired
together, these Rotors would map input letters pressed on a keyboard to
output letters. With each input the Rotors would advance, changing the
mapping. However, with so few Rotors, the amount of obfuscation steps that
it could perform were limited.

2. To address the above problem, the designers of the Enigma decided to have
the output of the last Rotor feed back into itself, winding its way back
through the machine. This artificially extends the Rotor count, but also
proved to be the critical flaw.

More detailed explanations of the original machine can be easily found on
the internet.

This library takes the following approach:

1. It works on bits, not characters.
2. The 26-letter Rotors are reduced to 2 possible bit transformations, "id" and "flip".
3. We represent a single rotor as a single bit. 0 is "id", 1 is "flip".
4. The entire machine (group of rotors) is represented by a series of bits.
5. Like the original Enigma, our secret key is the initial state of the rotors,
   which is just a series of bits. It can thus be arbitrarily long to offer
   many many obfuscation steps.
6. Like the original Enigma, each input (bit) is encrypted with a different key (rotor set).
7. Like the original Enigma, the ciphering algorithm both encrypts and decrypts.
8. The ciphering algorithm doesn't feed back into itself.

**Note:** This algorithm is brand new, so its level of security is unclear.

Examples
--------

Encrypt a `Word64`:
```haskell
λ evalState (cipher 1) $ Key 1
6513794518609451620
```

Decrypt a `Word64`:
```haskell
λ evalState (cipher 6513794518609451620) $ Key 1
1
```

Chain encryptions, carrying over the rotor state between operations:
```haskell
λ evalState ((,) <$> cipher 1 <*> cipher 2) $ Key 1
(6513794518609451620,7321263536672712088)
```

Encrypt a `Foldable`:
```haskell
λ evalState (traverse cipher [1,2,3]) $ Key 1
[6513794518609451620,7321263536672712088,11932949555100099993]
```

Encrypt a message:
```haskell
λ evalState (traverse (cipher . fromIntegral . ord) "Hint") $ Key 1
[6513794518609451565,7321263536672712179,11932949555100100084,6513794518609451537]
```

Performance
-----------

(More benchmarks needed)

Machine: ThinkPad X1 Carbon w/ Intel COREi7

Encrypting... | Average Time
------------- | ------------
Single `Word64` | 1.9 μs
10k `Word64` | 19.5 ms
1mil `Word64` (~7.5mb) | 1.95 s
