{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Constants
  where

import Prelude ((/), Float)

import Data.Int
import Data.Text

feeExternalCall, feeExternalSms, feeInternalCall, feeInternalSms, monthlyFee
    :: Float
freeMinutes, freeSmss :: Int
freeNumbers :: [Text]

feeExternalCall = (3.5 / 60.0)
feeExternalSms = 2.0
feeInternalCall = (1.5 / 60.0)
feeInternalSms = 1.0
freeMinutes = 100
freeNumbers = ["+420732563345", "+420707325673"]
freeSmss = 10
monthlyFee = 900
