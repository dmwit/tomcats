{-# Language FlexibleContexts #-}
{-# Language GADTs #-}
{-# Language MultiParamTypeClasses #-}

module Tomcats.AlphaZero.Shared (
	RNG(..),
	StatefulGen,
	createSystemRandom,
	) where

import System.Random.MWC
import System.Random.Stateful

-- | An RNG, but which kind it is is abstracted away so I can change it later
-- without you knowing. (It's mwc-random right now.)
data RNG where RNG :: StatefulGen g IO => g -> RNG

instance StatefulGen RNG IO where
	uniformWord32R r (RNG g) = uniformWord32R r g
	uniformWord64R r (RNG g) = uniformWord64R r g
	uniformWord8 (RNG g) = uniformWord8 g
	uniformWord16 (RNG g) = uniformWord16 g
	uniformWord32 (RNG g) = uniformWord32 g
	uniformWord64 (RNG g) = uniformWord64 g
	uniformShortByteString n (RNG g) = uniformShortByteString n g
