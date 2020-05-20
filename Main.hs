{-#language OverloadedStrings#-}
{-#language RecordWildCards#-}

module Main where

import Text.LaTeX
import Text.LaTeX.Packages.TabularX
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
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
        usepackage [] tabularxp
        document $ entriesToTable entries

instance Read NominalDiffTime where
    readPrec = do
        String s <- lexP
        parseTimeM False defaultTimeLocale "%h:%M" s

instance Texy NominalDiffTime where
    texy = texy . Text.pack . formatTime defaultTimeLocale "%h:%M"

data Entry = Entry
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Ticket = Issue | PullRequest deriving (Show, Read)

instance Texy Ticket where texy _ = "Not implemented."

entriesToTable :: [Entry] -> LaTeXM ()
entriesToTable xs = tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", LeftColumn, LeftColumn, LeftColumn] $ do
    hline
    "meow" & "kusau" & "kotau" & "putasau" >> lnbk
    hline
    sequence_ $ fmap entryToRow xs
    hline

entryToRow :: Entry -> LaTeXM ()
entryToRow Entry {..} = texy description & texy isDone & texy time >> lnbk
