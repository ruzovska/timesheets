{-#language OverloadedStrings#-}
{-#language RecordWildCards#-}

module Main where

import Text.LaTeX
import Text.LaTeX.Packages.Hyperref
import Text.LaTeX.Packages.TabularX
import qualified Data.List as List
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
    input <- getContents
    let periods = read input :: [Period]
    Text.putStrLn $ (render :: LaTeX -> Text) $ execLaTeXM $ do
        documentclass [] article
        usepackage ["colorlinks=true"] hyperref
        usepackage [] tabularxp
        usepackage ["left=2cm, right=2cm, top=2cm"] "geometry"
        document $ do
            (sequence_ . fmap periodToTable) periods

defaultTimeFormat = "%h:%0M"

instance Texy NominalDiffTime where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultTimeFormat

defaultDayFormat = "%d of %B %Y"

instance Texy Day where
    texy = texy . Text.pack . formatTime defaultTimeLocale defaultDayFormat

data Period = Period
    { start, end :: Day
    , tasks :: [Task]
    , services :: [Service]
    } deriving (Show, Read)

data Task = Task
    { description :: Text
    , tickets :: [Ticket]
    , time :: NominalDiffTime
    , isDone :: Bool
    } deriving (Show, Read)

data Service = Service
    { weight :: Double
    , name :: Text
    , price :: Double
    } deriving (Show, Read)

data Ticket = Issue String Int | PullRequest String Int deriving (Show, Read)

instance Texy Ticket where
    texy (Issue repository n) =
        href [] (createURL ("https://github.com/" <> repository <> "/issues/" <> show n)) ("#" <> texy n)
    texy (PullRequest repository n) =
        href [] (createURL ("https://github.com/" <> repository <> "/pull/" <> show n)) ("#" <> texy n)

makeHeader :: Day -> Day -> LaTeXM ()
makeHeader start end = do
    (textbf . center) $ do
        "Time Sheet for "
        texy start
        "---"
        texy end
        lnbkspc (Ex 2)

periodToTable :: Period -> LaTeXM ()
periodToTable Period {..} = do
    makeHeader start end
    tabularx (CustomMeasure textwidth) Nothing [NameColumn "X", CenterColumn, NameColumn "X", RightColumn] $ do
        hline
        "Description" & "Status" & "See also" & "Time" >> lnbk
        hline
        sequence_ $ fmap taskToRow tasks
        "Total" & "" & "" & texy (sum (fmap time tasks)) >> lnbk
        hline >> lnbkspc (Ex 2)

taskToRow :: Task -> LaTeXM ()
taskToRow Task {..} = do
    texy description & (if isDone then "completed" else "") & sequence_ (List.intersperse newline (fmap (mbox . texy) tickets)) & texy time >> lnbk
    hline