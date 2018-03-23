module Random

%access public export

RandomIntStream : Type
RandomIntStream = Stream Int

RandomDoubleStream : Type
RandomDoubleStream = Stream Double

randoms : Int -> RandomIntStream
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

splitRandom : RandomIntStream -> (RandomIntStream, RandomIntStream)
splitRandom (s1 :: s2 :: ss) = (randoms s1, randoms s2)

transformStream :  (Double, Double)
                -> RandomIntStream
                -> RandomDoubleStream
transformStream (from, to) (r :: rs) = r' :: transformStream (from, to) rs
  where
    precision : Int
    precision = 10000

    -- cut to range
    rLimit : Int
    rLimit = mod r precision

    -- normalize to 0..1
    rNorm : Double
    rNorm = cast rLimit / cast precision

    r' : Double
    r' = from + ((to - from) * rNorm)