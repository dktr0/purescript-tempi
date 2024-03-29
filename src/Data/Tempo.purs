module Data.Tempo where

-- A PureScript library/module closely tracking the Haskell tempi library
-- (and maintained by the same person.)


import Prelude (bind, div, identity, mod, negate, one, otherwise, pure, zero, ($), (*), (+), (-), (/), (==))
import Data.DateTime (DateTime, adjust, diff)
import Data.DateTime.Instant (toDateTime, instant)
import Data.Time.Duration (Milliseconds(..), Seconds(..))
import Data.Rational (Rational, denominator, fromBigInt, fromInt, numerator, toNumber, (%))
import Data.Maybe (fromJust, maybe)
import Data.Newtype (unwrap)
import Data.Int (floor)
import Effect (Effect)
import Effect.Now (nowDateTime)
import Partial.Unsafe (unsafePartial)
import JS.BigInt (BigInt)

-- | Musical tempo is represented as a data structure with three orthogonal components.

type Tempo = {
  freq :: Rational, -- frequency of cycles/beats, ie. cycles/beats per second
  time :: DateTime, -- a time at which a number of "elapsed" cycles/beats will be indicated
  count :: Rational -- the number of "elapsed" cycles/beats at the time indicated
  }


-- | Create a new Tempo record with a given frequency, with count 0 anchored in the present moment.

newTempo :: Rational -> Effect Tempo
newTempo freq = do
  time  <- nowDateTime
  pure { freq, time, count: fromInt 0 }


-- | The 'origin' of a Tempo is the time at which the number of elapsed cycles/beats
-- would have been 0. (Note that unlike the Haskell version of this function, there is potential
-- silent failure/error built-in in cases where adjust does not return a valid DateTime - in such
-- hopefully very rare, liminal cases, this function will simply return the time anchor from the
-- Tempo record as a kind of "default" value.)

origin :: Tempo -> DateTime
origin x = maybe x.time identity $ adjust (Seconds $ toNumber increment) x.time
  where increment = x.count * (fromInt (-1)) / x.freq


-- | Given a Tempo and a clock time (DateTime), timeToCount tells us how many cycles/beats
-- have elapsed at that time.

timeToCount :: Tempo -> DateTime -> Rational
timeToCount x t = d' * x.freq + x.count
  where
    d = unwrap (diff t x.time :: Milliseconds)
    d' = floor d % 1000

-- | I think there might be numerical precision issues with timeToCount and PureScript's Rational type
-- so am also experimenting with timeToCountNumber as well (it might have different precision issues...)

timeToCountNumber :: Tempo -> DateTime -> Number
timeToCountNumber x t = df + toNumber x.count
  where
    timeDiff = unwrap (diff t x.time :: Milliseconds)
    df = timeDiff * toNumber x.freq / 1000.0


-- | Given a Tempo and a count of elapsed cycles/beats, countToTime tells us when that "beat"
-- will (or would have) take(n) place. (note: countToTime has the same caveat about a liminal potential
-- silent failure as origin above.)

countToTime :: Tempo -> Rational -> DateTime
countToTime x c = maybe x.time identity $ adjust (Seconds $ toNumber (c / x.freq)) (origin x)


-- | ForeignTempo and fromForeignTempo are provided to facilitate constructing a Tempo
-- with high precision numbers (Rationals) from basic JavaScript types. (For example, this
-- should be useful in receiving Tempi that are "originally" expressed/produced by the
-- Haskell tempi library when compiled with GHCJS and interacting with PureScript code.)

type ForeignTempo = {
  freqNumerator :: BigInt,
  freqDenominator :: BigInt,
  time :: Number, -- POSIX/epoch-1970 time, in seconds
  countNumerator :: BigInt,
  countDenominator :: BigInt
 }

fromForeignTempo :: ForeignTempo -> Tempo
fromForeignTempo x = { freq, time, count }
  where
    freq = x.freqNumerator % x.freqDenominator
    time = toDateTime $ unsafePartial $ fromJust $ instant $ Milliseconds $ x.time * 1000.0
    count = x.countNumerator % x.countDenominator


-- given a metre (eg. 4 beats) an offset (eg. every 2 beats after the 4 beat metre) and a value in those same units
-- return the next moment in those units that matches the provided metre and offset. if the provide value is such
-- a moment, then it is returned.
nextBeat :: Rational -> Rational -> Rational -> Rational
nextBeat metre offset x = (ceilingRational ((x-offset)/metre)) * metre + offset

-- like nextBeat except that if the provided value matches the metre and offset it is not returned - the next moment
-- that matches the specification is returned instead.
nextBeatExclusive :: Rational -> Rational -> Rational -> Rational
nextBeatExclusive metre offset x = (floorRational ((x-offset)/metre) + one) * metre + offset

-- used by nextBeat (above)
ceilingRational :: Rational -> Rational
ceilingRational x
  | mod (numerator x) (denominator x) == zero = x
  | otherwise = fromBigInt (div (numerator x) (denominator x) + one)

-- used by nextBeatExclusive (above)
floorRational :: Rational -> Rational
floorRational x
  | mod (numerator x) (denominator x) == zero = x
  | otherwise = fromBigInt (div (numerator x) (denominator x))
