{-# LANGUAGE TemplateHaskell #-}
module NewTTRS.Law where

import Control.Lens
import Data.Array.Unboxed
import Data.List
import Data.Monoid
import Statistics.Distribution (complCumulative, cumulative)
import Statistics.Distribution.Normal (normalDistr)

import NewTTRS.Outcome

data Law = Law { lawRaw :: !(UArray Int Double)
               , lawMean, lawStddev :: !Double }

data LawUpdate = LawUpdate
  { playerLaw , opponentLaw :: Law
  , updateOutcome :: Outcome
  }

-- | Law assigned to unrated players
defaultLaw :: Law
defaultLaw = normalLaw 1800 450

-- | The list of discrete scores characterized by a law
omega :: [Int]
omega = [0,10..3600]

-- | The parameter used to characterize upset probability.
alpha :: Double
alpha = 0.0148540595817432

-- | Generate a normalized law from a list of probabilities
-- which correspond to the elements of 'omega'.
lawFromList :: [Double] -> Law
lawFromList xs = Law (listArray (0,360) normalized) mean (sqrt variance)
  where
  normalized = fmap (/ sum xs) xs
  mean     = sum [ fromIntegral p       * x | (p,x) <- zip omega normalized ]
  variance = sum [ fromIntegral (p * p) * x | (p,x) <- zip omega normalized ]
           - mean * mean

-- | Find the probability that the score is in the range of
-- [i-5,i+5] given a law.
lawAt :: Law -> Int -> Double
lawAt law i = lawRaw law ! (i `div` 10)
{-# INLINE lawAt #-}

-- | Probability of an upset given the difference in two ratings.
upsetProbability :: Int -> Double
upsetProbability d = recip (1 + exp (alpha * fromIntegral d))

-- | Perform a bayesian inference given a player's law, the opponent's law
-- and the outcome of the matches between the two.
lawUpdate ::
  Law {- ^ Player's law to be updated -} ->
  Law {- ^ Opponent's law -} ->
  Outcome {- ^ Player's outcome against opponent -} ->
  Law
lawUpdate lp lq outcome
    = lawFromList
      [ sum [ upsetProbability (q - p) ^ w
            * upsetProbability (p - q) ^ l
            * lawAt lq q
            | q <- omega ]
      * lawAt lp p
      | p <- omega
      ]
  where
  w = view outcomeWins outcome
  l = view outcomeLosses outcome

-- | Perform the inverse computation of 'lawUpdate'
lawUnupdate ::
  Law {- ^ Player's law to be updated -} ->
  Law {- ^ Opponent's law -} ->
  Outcome {- ^ Player's outcome against opponent -} ->
  Law
lawUnupdate lp lq outcome
    = lawFromList
      [ lawAt lp p
      / sum [ upsetProbability (q - p) ^ w
            * upsetProbability (p - q) ^ l
            * lawAt lq q
            | q <- omega ]
      | p <- omega
      ]
  where
  w = view outcomeWins outcome
  l = view outcomeLosses outcome

-- | Chance that a player with the first law will defeat a player with the second law
chanceToWin :: Law -> Law -> Double
chanceToWin lp lq =
  sum [ upsetProbability (q - p)
      * lawAt lp p
      * lawAt lq q
      | q <- omega
      , p <- omega
      ]

-- | Generate a discretized normal law given a mean and standard deviation.
normalLaw ::
   Double {- ^ mean -} ->
   Double {- ^ standard deviation -} ->
   Law
normalLaw mean stddev = lawFromList $ mkNormal 0 3600 mean stddev

-- | Generate a discrete approximation of a normal distribution.
mkNormal ::
  Int    {- ^ lower bound -} ->
  Int    {- ^ upper bound -} ->
  Double {- ^ mean -} ->
  Double {- ^ standard deviation -} ->
  [Double]
mkNormal lo hi mean stddev =
    [ c (lo + 5)  ]
 ++ [ c (p+10) - c p | p <- [lo+5, lo+15 .. hi - 15]]
 ++ [ c' (hi - 5) ]
  where
  distr = normalDistr mean stddev
  c     = cumulative      distr . fromIntegral
  c'    = complCumulative distr . fromIntegral

-- | Degrade a law given a certain number of days. When days is
-- less than 1, no degrading is done.
timeEffect :: Int -> Law -> Law
timeEffect days law
  | days <= 0 = law
  | otherwise = lawFromList
              $ sum [ lawAt law x * timeAt y | x <- omega, y <- [-3600,-3590.. -x]]
              : [ sum [ lawAt law x * timeAt (r - x) | x <- omega ]
                | r <- omega \\ [0,3600] ]
             ++ [ sum [lawAt law x * timeAt y | x <- omega, y <- [3600-x,3610-x..3600]] ]
  where
  timeArray :: UArray Int Double
  timeArray = listArray (-360,360) $ mkNormal (-3600) 3600 0 (70 * sqrt (fromIntegral days / 365))
  timeAt y = timeArray ! (y `div` 10)

-- | Compute a metric of the law used to order players
lawScore :: Law -> Double
lawScore law = lawMean law - 2 * lawStddev law

-- | Return the raw law discrete approximations
lawElems :: Law -> [Double]
lawElems = elems . lawRaw
