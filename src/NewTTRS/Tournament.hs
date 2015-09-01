{-# LANGUAGE TemplateHaskell #-}
module NewTTRS.Tournament where

import Control.Lens
import Data.List (foldl')
import Data.Map (Map)
import Data.Maybe (fromMaybe)
import Data.Monoid (mempty)
import Data.Time.Calendar (Day, diffDays)
import qualified Data.Map as Map

import NewTTRS.Law
import NewTTRS.Match
import NewTTRS.Outcome

data PlayerSummary name = PlayerSummary
  { _summaryInitialLaw, _summaryFinalLaw :: Law
  , _summaryMatches :: Map name MatchSummary
  }

data MatchSummary = MatchSummary
  { _summaryAdjustedLaw  :: Law
  , _summaryMeanChange, _summaryStddevChange :: Double
  , _summaryOutcome      :: Outcome
  }

makeLenses ''PlayerSummary
makeLenses ''MatchSummary

-- | Compute a map of outcomes where the first key
-- is the player's name, the second key is the opponent's
-- name and the outcome is from the point of view of the
-- player.
matchOutcomes :: Ord name => [Match name] -> Map name (Map name Outcome)
matchOutcomes = foldl' addMatch Map.empty
  where
  look :: (Functor f, Ord name) => name -> name -> LensLike' f (Map name (Map name Outcome)) Outcome
  look w l = at w . non' _Empty . at l . non' _Empty

  addMatch outcomes match = updateWinner match . updateLoser match $ outcomes
  updateWinner match = look (view matchWinner match) (view matchLoser  match) . outcomeWins   +~ 1
  updateLoser  match = look (view matchLoser  match) (view matchWinner match) . outcomeLosses +~ 1

-- | Compute a final law and match summary set for a player
-- given the tournament information, initial laws, and player\'s
-- outcomes.
updatePlayer ::
  Ord name =>
  Map name Law     {- ^ Nearly adjusted laws -} ->
  Map name Law     {- ^ Initial laws going into the tournament -} ->
  name             {- ^ Name of player to update -} ->
  Map name Outcome {- ^ outcomes for games played by this player -} ->
  PlayerSummary name
updatePlayer nearlyAdjustedLaws laws playerName opponents
  = PlayerSummary
      { _summaryInitialLaw      = initial
      , _summaryFinalLaw        = final
      , _summaryMatches         = matchSummaries
      }
  where
  initial = getLaw playerName laws

  (final, matchSummaries) = imapAccumL computeMatchSummary initial opponents

  computeMatchSummary opponentName accLaw outcome =
    ( finalLaw
    , MatchSummary
        { _summaryAdjustedLaw    = opponentAdjustedLaw
        , _summaryMeanChange     = lawMean   finalLaw - lawMean   accLaw
        , _summaryStddevChange   = lawStddev finalLaw - lawStddev accLaw
        , _summaryOutcome        = outcome
        })
    where
    opponentNearlyAdjustedLaw = fromMaybe (error "missing nearly adjusted law")
                              $ Map.lookup opponentName nearlyAdjustedLaws
    opponentAdjustedLaw = lawUnupdate opponentNearlyAdjustedLaw initial (flipOutcome outcome)
    finalLaw = lawUpdate accLaw opponentAdjustedLaw outcome

degradeLaw ::
  Day {- ^ Today -} ->
  Day  {- ^ Last update -} ->
  Law ->
  Law
degradeLaw today lastUpdate law = timeEffect days law
  where
  days = fromIntegral $ diffDays today lastUpdate

evaluateTournament :: Ord name => [Match name] -> Map name Law -> Map name (PlayerSummary name)
evaluateTournament tournament laws
  = imap (updatePlayer firstPassLaws laws) outcomes
  where
  outcomes = matchOutcomes tournament
  firstPassLaws = imap (firstPass laws) outcomes

firstPass :: Ord name => Map name Law -> name -> Map name Outcome -> Law
firstPass initialLaws name = ifoldl aux (getLaw name initialLaws)
  where
  aux otherPlayer accLaw outcome =
    lawUpdate accLaw (getLaw otherPlayer initialLaws) outcome

getLaw :: Ord name => name -> Map name Law -> Law
getLaw n m = fromMaybe defaultLaw $ Map.lookup n m

traverseKeys :: Ord b => Traversal (Map a v) (Map b v) a b
traverseKeys f = fmap Map.fromList . (traverse . _1) f . Map.toList

tournamentNameTraversal :: Ord b =>
  Traversal (Map a (PlayerSummary a)) (Map b (PlayerSummary b)) a b
tournamentNameTraversal f = fmap Map.fromList . traverse fixEntry . Map.toList
  where
  fixEntry = beside id (summaryMatches . traverseKeys) f
