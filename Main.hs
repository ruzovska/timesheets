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

defaultTimeFormat = "%h:%0M"

instance Read NominalDiffTime where
    readPrec = do
        String s <- lexP
        parseTimeM False defaultTimeLocale defaultTimeFormat s

instance Texy NominalDiffTime where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultTimeFormat

data Entry = Entry
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Ticket = Issue | PullRequest deriving (Show, Read)

instance Texy Ticket where texy _ = "Not implemented."

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
