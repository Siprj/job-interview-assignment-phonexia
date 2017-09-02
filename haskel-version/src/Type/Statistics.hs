{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
module Type.Statistics
    ( Statistics(..)
    , calls
    , externalCallCost
    , externalCallTime
    , externalSMSAmount
    , externalSMSCost
    , internalCallCost
    , internalCallTime
    , internalSMSAmount
    , internalSMSCost
    , totalCost
    )
  where

import Prelude (Float)

import Control.Lens
import Data.Int
import Data.Eq
import Text.Show


data Statistics = Statistics
    { _calls :: Int
    , _externalCallCost :: Float
    , _externalCallTime :: Int
    , _externalSMSAmount :: Int
    , _externalSMSCost :: Float
    , _internalCallCost :: Float
    , _internalCallTime :: Int
    , _internalSMSAmount :: Int
    , _internalSMSCost :: Float
    , _totalCost :: Float
    }
  deriving (Eq, Show)

makeLenses ''Statistics
