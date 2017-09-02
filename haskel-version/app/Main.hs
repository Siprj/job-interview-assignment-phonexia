{-# LANGUAGE NoImplicitPrelude #-}
module Main where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Function
import Data.Monoid
import Data.String
import Options.Applicative
import System.Exit
import System.IO

import App


opt :: Parser String
opt = strOption
    ( long "file"
    <> metavar "TARGET"
    <> short 'f'
    <> help "Target for the greeting"
    )

main :: IO ()
main = execParser opts >>= parse >>= either (\v -> print v >> exitFailure)
    (putStrLn . prettyPrint . gatherStatistics)
  where
    opts = info (opt <**> helper)
        ( fullDesc
        <> progDesc "Compute tariff information from data file"
        )

