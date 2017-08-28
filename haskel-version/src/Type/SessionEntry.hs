{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Type.SessionEntry
    ( SessionEntry(..)
    , EntryType(..)
    , SubType(..)
    )
  where

import Control.Applicative ((<$>), (<*>), pure)
import Control.Monad ((>>=), mzero)
import Data.Csv
import Data.Eq
import Data.Function
import Data.Int
import Data.Text
import Text.Show


data SessionEntry = SessionEntry
    { number :: Text
    , entry :: EntryType
    }
  deriving (Eq, Show)

data EntryType =
    Call
        { duration :: Int
        , subType :: SubType
        }
    | SMS
        { subType :: SubType
        }
  deriving (Eq, Show)

data SubType = Internal | External
  deriving (Eq, Show)


instance FromNamedRecord SessionEntry where
    parseNamedRecord m = SessionEntry
        <$> m .: "number"
        <*> entryType
      where
        entryType :: Parser EntryType
        entryType = m .: "type" >>= \case
            ("VS" :: Text) -> Call <$> m .: "duration" <*> pure Internal
            "VM" -> Call <$> m .: "duration" <*> pure External
            "SS" -> pure $ SMS Internal
            "SM" -> pure $ SMS External
            _ -> mzero
