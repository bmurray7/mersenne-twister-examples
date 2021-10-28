#    Copyright (c)  2021 JOAN SEBASTIAN BETANCOURT
#    Permission is granted to copy, distribute and/or modify this document
#    under the terms of the GNU Free Documentation License, Version 1.3
#    or any later version published by the Free Software Foundation;
#    with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
#    A copy of the license is included in the section entitled "GNU
#    Free Documentation License".

# R implementation of a Mersenne Twister with a state size of 19937 bits

MT19937.seed <- 5489 # default initial seed used by Nishimura and Matsumoto in their 2015 C implementation

# generates n random deviates from a uniform distribution
MT19937.runif <- function (n, min = 0, max = 1) {
  # R has not built-in bitwise operators for values greater than 2^31, so they must be reimplemented
  # (These are probably not the most efficient solutions)
  shiftL <- function(x, n) x * 2^n
  shiftR <- function(x, n) x %/% 2^n
  xor <- function(x, y) .bitOp(x, y, bitwXor)
  and <- function(x, y) .bitOp(x, y, bitwAnd)
  bitSlice <- function(x, from, length) shiftR(x %% 2^(from+length), from)
  .bitOp <- function(x, y, bitwOp) (bitwOp(bitSlice(x, 0, 23),bitSlice(y, 0, 23)) + shiftL(bitwOp(bitSlice(x, 23, 23), bitSlice(y, 23, 23)), 23) + shiftL(bitwOp(bitSlice(x, 46, 23), bitSlice(y, 46, 23)), 46))
  
  .int32 <- function(x) bitSlice(x, 0, 32)
  
  # a modified modulo for 1-based array indexing
  modulo <- function(a, m) if(a %% m == 0) m else a %% m
  
  # initialization
  mt <- vector(mode = "numeric", length = 624)
  mt[1] <- MT19937.seed
  for(i in 2:624) mt[i] <- .int32(1812433253 * xor(mt[i-1],shiftR(mt[i-1], 30)) + i - 1)
  
  twist <- function(){
    for(i in 1:624) {
      # Get the most significant bit and add it to the less significant bits of the next number
      x <- and(mt[i], 0x80000000) + and(mt[modulo(i, 624)], 0x7fffffff)
      xA <- shiftR(x, 1)
      if (x %% 2 != 0) xA <- xor(xA, 0x9908b0df)
      mt[i] <<- xor(mt[modulo(i+396,624)], xA)
    }
  }
  
  # extract n numbers
  results <- vector(mode = "numeric", length = n)
  for(i in 1:n){
    # internal state index
    index <- modulo(i, 624)
    if (index == 1) twist()
    y <- mt[index]
    # Right shift by 11 bits
    y <- xor(y, shiftR(y, 11))
    # Shift y left by 7 and take the bitwise and of 2636928640
    y <- xor(y, and(shiftL(y, 7), 2636928640))
    # Shift y left by 15 and take the bitwise and of y and 4022730752
    y <- xor(y, and(shiftL(y, 7), 4022730752))
    # Right shift by 18 bits
    y <- xor(y, shiftR(y, 18))
    # project the number to a (0, 1] interval and then to the [a, b] desired interval
    results[i] <- (.int32(y) / 2^32 * (max - min)) + min
  }
  # replace the seed with the last value generated
  MT19937.seed <<- results[n] 
  return(results)
}