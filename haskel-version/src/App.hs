{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE WildcardRecords #-}
module App
    ( parse
    )
  where

import Prelude (fromIntegral, fromEnum)

import Control.Applicative ((<$>))
import Data.ByteString.Lazy (readFile)
import Data.Csv
import Data.Either
import Data.Function
import Data.Functor
import Data.String
import Data.Tuple
import Data.Vector
import System.IO (IO, FilePath)

import Type.Constants
import Type.SessionEntry
import Type.Statistics

parse :: FilePath -> IO (Either String (Vector SessionEntry))
parse path = (fmap snd . decodeByNameWith options)
    <$> readFile path
  where
    options = DecodeOptions . fromIntegral $ fromEnum ';'

gatherStatistics :: Vector SessionEntry -> Statistics
gatherStatistics = foldl go def
  where
    def = Statistics
        { totalCost = monthlyFee
        , internalCallTime = 0
        , internalCallCost = 0
        , externalCallTime = 0
        , externalCallCost = 0
        , internalSMSAmount = 0
        , internalSMSCost = 0
        , externalSMSAmount = 0
        , externalSMSCost = 0
        , calls = 0
        }

    go =
