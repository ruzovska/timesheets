{-#language OverloadedStrings#-}
{-#language RecordWildCards#-}

module Main where

import Text.LaTeX
import Text.LaTeX.Packages.TabularX
import Data.Text (Text)
import qualified Data.Text.IO as Text
import qualified Data.Text as Text
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import Text.Pretty.Simple
import Text.Read

main :: IO ()
main = do
    example <- getContents
    let Log {..} = read example :: Log
    Text.putStrLn $ (render :: LaTeX -> Text) $ execLaTeXM $ do
        documentclass [] article
        usepackage [] tabularxp
        document $ do
            makeHeader start end
            entriesToTable entries

defaultTimeFormat = "%h:%0M"

instance Read NominalDiffTime where
    readPrec = do
        String s <- lexP
        parseTimeM False defaultTimeLocale defaultTimeFormat s

instance Texy NominalDiffTime where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultTimeFormat

defaultDayFormat = "%d of %B %Y"

instance Texy Day where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultDayFormat

data Log = Log
    { start, end :: Day
    , entries :: [Entry]
    } deriving (Show, Read)

data Entry = Entry
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Ticket = Issue | PullRequest deriving (Show, Read)

instance Texy Ticket where texy _ = "Not implemented."

makeHeader :: Day -> Day -> LaTeXM ()
makeHeader start end = do
    textbf $ do
        "Time Sheet for "
        texy start
        "---"
        texy end
        lnbkspc (Ex 2)

entriesToTable :: [Entry] -> LaTeXM ()
entriesToTable xs = tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", LeftColumn, RightColumn] $ do
    hline
    "Description" & "See also" & "Time" >> lnbk
    hline
    sequence_ $ fmap entryToRow xs
    hline
    "Total" & "" & texy (sum (fmap time xs)) >> lnbk
    hline

entryToRow :: Entry -> LaTeXM ()
entryToRow Entry {..} = texy description & "" & texy time >> lnbk
