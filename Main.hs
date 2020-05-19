{-#language OverloadedStrings#-}

module Main where

import Text.LaTeX
import Data.Text (Text)
import qualified Data.Text.IO as Text
import Data.Time.Clock
import Data.Time.Format
import Text.Pretty.Simple
import Text.Read

main :: IO ()
main = do
    example <- getContents
    let entries = read example :: [Entry]
    Text.putStrLn $ (render :: LaTeX -> Text) $ execLaTeXM $ do
        documentclass [] article
        document $ entriesToTable entries

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

entriesToTable :: [Entry] -> LaTeXM ()
entriesToTable xs = tabular Nothing [LeftColumn, LeftColumn] $ do
    tex & tex