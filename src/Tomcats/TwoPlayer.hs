module Tomcats.TwoPlayer (
	Player(..),
	otherPlayer,
	wonValuation, lostValuation, drawnValuation,
	) where

-- | An explicit representation of the collection of players. The 'O' player
-- should like valuations of @0@, and the 'I' player should like valuations of
-- @1@. (Cute, right?)
data Player = O | I deriving (Eq, Ord, Read, Show, Bounded, Enum)

otherPlayer :: Player -> Player
otherPlayer O = I
otherPlayer I = O

-- | @wonValuation p@ is a suitable valuation for a game that's finished and
-- won by @p@.
wonValuation :: Player -> Double
wonValuation O = 0
wonValuation I = 1

-- | @lostValuation p@ is a suitable valuation for a game that's finished and
-- lost by @p@. This is equivalent to combining 'wonValuation' and
-- 'otherPlayer'.
lostValuation :: Player -> Double
lostValuation = wonValuation . otherPlayer

-- | A suitable valuation for a game that's finished and neither player won.
drawnValuation :: Double
drawnValuation = 0.5
