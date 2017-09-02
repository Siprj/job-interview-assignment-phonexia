{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RecordWildCards #-}
module App
    ( gatherStatistics
    , parse
    , prettyPrint
    )
  where

import Prelude (Float, (+), (-), (*), (/), fromIntegral, fromEnum)

import Control.Applicative ((<$>))
import Control.Lens
import Data.Bool
import Data.ByteString.Lazy (readFile)
import Data.Csv
import Data.Either
import Data.Foldable
import Data.Function
import Data.Functor
import Data.Int (Int)
import Data.Monoid ((<>))
import Data.Ord
import Data.String
import Data.Tuple
import Data.Vector (Vector)
import System.IO (IO, FilePath)
import Text.Show (show)

import Constants
import Type.SessionEntry
import Type.Statistics

parse :: FilePath -> IO (Either String (Vector SessionEntry))
parse path = (fmap snd . decodeByNameWith options)
    <$> readFile path
  where
    options = DecodeOptions . fromIntegral $ fromEnum ';'

gatherStatistics :: Vector SessionEntry -> Statistics
gatherStatistics = view _1 . foldr go (def, freeMinutes*60, freeSmss)
  where
    def :: Statistics
    def = Statistics
        { _totalCost = monthlyFee
        , _internalCallTime = 0
        , _internalCallCost = 0
        , _externalCallTime = 0
        , _externalCallCost = 0
        , _internalSMSAmount = 0
        , _internalSMSCost = 0
        , _externalSMSAmount = 0
        , _externalSMSCost = 0
        , _calls = 0
        }

    go :: SessionEntry -> (Statistics, Int, Int) -> (Statistics, Int, Int)
    go (SessionEntry number Call{..}) (stats, freeSeconds, freeSmss') =
        ( incrementCalls . addTimeAndCost $ case subType of
            Internal ->
                ( internalCallTime
                , internalCallCost
                , nonFreeTime duration freeSeconds freeNumber * feeInternalCall
                )
            External ->
                ( externalCallTime
                , externalCallCost
                , nonFreeTime duration freeSeconds freeNumber * feeExternalCall
                )
        , if freeSeconds <= 0
            then freeSeconds
            else freeSeconds - duration
        , freeSmss'
        )
      where
        freeNumber = isFreeNumber number
        nonFreeTime = magic rampSeconds

        addTimeAndCost (callTimeLense, callCostLense, cost) =
            over callCostLense (+ cost)
            . over totalCost (+ cost)
            $ over callTimeLense (+ duration) stats

    go (SessionEntry number SMS{..}) (stats, freeSeconds, freeSmss') =
        ( addAmountAndCost $ case subType of
            Internal ->
                ( internalSMSAmount
                , internalSMSCost
                , nonFreeSms freeSmss' freeNumber * feeInternalSms
                )
            External ->
                ( externalSMSAmount
                , externalSMSCost
                , nonFreeSms freeSmss' freeNumber * feeExternalSms
                )
        , freeSeconds
        , if freeSmss' <= 0
            then freeSmss'
            else freeSmss' - 1
        )
      where
        freeNumber = isFreeNumber number
        addAmountAndCost (smsAmoutLense, smsCostLense, cost) =
            over smsCostLense (+ cost)
            . over totalCost (+ cost)
            $ over smsAmoutLense (+ 1) stats
        nonFreeSms = magic id 1

    rampSeconds v = if v < 60
        then 60
        else v

    isFreeNumber a = elem a freeNumbers
    incrementCalls stats = over calls (+ 1) stats

    magic f callLength freeTime pred = fromIntegral $
        if freeTime <= 0 && pred
            then callLength
            else if freeTime - callLength > 0
                then 0
                else f $ callLength - freeTime

prettyPrint :: Statistics -> String
prettyPrint Statistics{..} = unlines
    [ "Total price [-]: " <> show _totalCost
    , "Calls [-]: " <> show _calls
    , "Monthly fee [-]: " <> show monthlyFee
    , "Internal call time [min]: " <> show (toMinutes _internalCallTime)
    , "Internal call price [-]: " <> show _internalCallCost
    , "Internal sms amount [-]: " <> show _internalSMSAmount
    , "Internal sms price [-]: " <> show _internalSMSCost
    , "External call time [min]: " <> show (toMinutes _externalCallTime)
    , "External call price [-]: " <> show _externalCallCost
    , "External sms amount [-]: " <> show _externalSMSAmount
    , "External sms price [-]: " <> show _externalSMSCost
    ]
  where
    toMinutes :: Int -> Float
    toMinutes v = fromIntegral v / 60
