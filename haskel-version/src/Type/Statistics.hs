{-# LANGUAGE NoImplicitPrelude #-}
module Type.Statistics
    ( Statistics(..)
    )
  where

import Prelude (Float)

import Data.Int
import Data.Eq
import Text.Show


data Statistics = Statistics
    { totalCost :: Int
    , internalCallTime :: Int
    , internalCallCost :: Float
    , externalCallTime :: Int
    , externalCallCost :: Float
    , internalSMSAmount :: Int
    , internalSMSCost :: Float
    , externalSMSAmount :: Int
    , externalSMSCost :: Float
    , calls :: Int
    }
  deriving (Eq, Show)
