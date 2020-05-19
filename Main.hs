{-#language OverloadedStrings#-}

module Main where

import Data.Text
import Data.Time.Clock
import Data.Time.Format
import Text.Pretty.Simple

main :: IO ()
main = pPrint example

instance Read NominalDiffTime where
    readsPrec _ = readSTime False defaultTimeLocale "%h:%m"

data Entry = Entry
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Ticket = Issue | PullRequest deriving (Show, Read)

example :: [Entry]
example =
    [ Entry
        { description = "Planting"
        , tickets = [Issue, PullRequest]
        , time = 6300
        , isDone = True
        }
    , Entry
        { description = "Gathering"
        , tickets = [Issue, Issue]
        , time = 7500
        , isDone = True
        }
    ]