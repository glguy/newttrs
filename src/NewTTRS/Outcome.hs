{-# LANGUAGE TemplateHaskell #-}

module NewTTRS.Outcome where

import Control.Lens
import Data.Monoid

data Outcome = Outcome { _outcomeWins, _outcomeLosses :: !Int }
  deriving (Eq, Show)

makeLenses ''Outcome

instance Monoid Outcome where
  mempty = Outcome 0 0

instance Semigroup Outcome where
  Outcome w1 l1 <> Outcome w2 l2 = Outcome (w1+w2) (l1+l2)

-- | Turn wins into losses and losses into wins.
flipOutcome :: Outcome -> Outcome
flipOutcome (Outcome a b) = Outcome b a

instance AsEmpty Outcome
