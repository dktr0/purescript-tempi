module Data.Tempo where

-- A PureScript library/module closely tracking the Haskell tempi library
-- (and maintained by the same person.)


import Prelude
import Data.DateTime
import Data.DateTime.Instant
import Data.Time.Duration
import Data.Rational
import Data.Maybe
import Data.Newtype
import Data.Int (floor)
import Effect (Effect)
import Effect.Now (nowDateTime)
import Partial.Unsafe

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
origin x = maybe x.time identity $ adjust (Milliseconds $ toNumber increment) x.time
  where increment = x.count * (fromInt (-1000)) / x.freq


-- | Given a Tempo and a clock time (DateTime), timeToCount tells us how many cycles/beats
-- have elapsed at that time.

timeToCount :: Tempo -> DateTime -> Rational
timeToCount x t = d' * x.freq + x.count
  where
    d = unwrap (diff t x.time :: Milliseconds) -- difference as a Number in milliseconds
    d' = floor d % 1000 -- difference as a Rational in seconds

-- | I think there might be numerical precision issues with timeToCount and PureScript's Rational type
-- so am also experimenting with timeToCountNumber as well (it might have different precision issues...)

timeToCountNumber :: Tempo -> DateTime -> Number
timeToCountNumber x t = df + toNumber x.count
  where
    timeDiff = unwrap (diff t x.time :: Milliseconds) -- difference as a Number in milliseconds
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
  freqNumerator :: Int,
  freqDenominator :: Int,
  time :: Number, -- POSIX/epoch-1970 time, in milliseconds
  countNumerator :: Int,
  countDenominator :: Int
 }

fromForeignTempo :: ForeignTempo -> Tempo
fromForeignTempo x = { freq, time, count }
  where
    freq = x.freqNumerator % x.freqDenominator
    time = toDateTime $ unsafePartial $ fromJust $ instant $ Milliseconds x.time
    count = x.countNumerator % x.countDenominator
