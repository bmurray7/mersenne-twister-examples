{-# LANGUAGE ParallelListComp #-}

import Data.Bits
import Data.Word

(w, n, m, r) = (32, 624, 397, 31)
a            = 0x9908B0DF
(u, d)       = (11, 0xFFFFFFFF)
(s, b)       = (7, 0x9D2C5680)
(t, c)       = (15, 0xEFC60000)
l            = 18

-- | Initialize the MT state of 624 32-bit values from a 32-bit seed.
--
-- Usage example:
--
-- > st0 = mtInit 1
-- > z0  = mtNextValue st0
-- > st1 = mtNextState st0
-- > z1  = mtNextValue st1
-- > st2 = mtNextState st1
--
-- That initializes the state from the value 1, and then computes two
-- random values @z0@ and @z1@; the next random value would be
-- @mtNextValue st2@.
--
-- Note that the first 624 state values are not used to produce random
-- numbers, so the initial state we produce comprises the @x@ values
-- @[x_1,...,x_n]@.
mtInit :: Word32 -> [Word32]
mtInit seed = mtNextState st0
  where
    st0 = [seed] ++
          [ f * (x `xor` (x `shiftR` (w - 2))) + i
          | x <- st0
          | i <- [1 .. n - 1] ]
    f = 1812433253

-- | Compute the next MT state from the current state.
--
-- I.e., if the input state is @[x_i,...,x_{i+n-1}]@ then the output
-- state is @[x_{i+1},...,x_{i+n}]@.
mtNextState :: [Word32] -> [Word32]
mtNextState st = tail st ++ [nextStateValue]
  where
    nextStateValue = (st !! m) `xor`
                     multA (upper (st !! 0) .|. lower (st !! 1))
    multA x = if (x .&. 1) == 0
              then (x `shiftR` 1)
              else (x `shiftR` 1) `xor` a
    upper x = (x `shiftR` r)       `shiftL` r
    lower x = (x `shiftL` (w - r)) `shiftR` (w - r)

-- | Compute the next MT random value @z@ from the last value
-- @x_{i+n-1}@ of the current state @[x_i,...,x_{i+n-1}]@.
mtNextValue :: [Word32] -> Word32
mtNextValue st = z
  where
    x  = last st
    y0 = x  `xor` ((x  `shiftR` u) .&. d)
    y1 = y0 `xor` ((y0 `shiftL` s) .&. b)
    y2 = y1 `xor` ((y1 `shiftL` t) .&. c)
    z  = y2 `xor` (y2  `shiftR` l) 

-- | Compute an infinite stream of MT random values from a seed.
mtStream :: Word32 -> [Word32]
mtStream = map mtNextValue . iterate mtNextState . mtInit
