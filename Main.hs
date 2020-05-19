{-#language OverloadedStrings#-}

module Main where

import Data.Text
import Data.Time.Clock
import Data.Time.Format
import Text.Pretty.Simple
import Text.Read

main :: IO ()
main = do
    example <- readFile "example"
    pPrint (read example :: [Entry])

instance Read NominalDiffTime where
    readPrec = do
        String s <- lexP
        parseTimeM False defaultTimeLocale "%h:%m" s

data Entry = Entry
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Ticket = Issue | PullRequest deriving (Show, Read)
