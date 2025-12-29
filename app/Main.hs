{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Lens (Getter, foldOf, sumOf, to, view, (.=), (^.), (^..), (^?), _2)
import Control.Monad (forM)
import Control.Monad.State (evalState, gets, modify)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Foldable (forM_)
import Data.Function (on)
import Data.List (nub, sortOn)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Text (Text, pack)
import Data.Text qualified as Text
import Data.Text.IO (hPutStrLn, writeFile)
import Data.Time (Day, toGregorian)
import Data.Time.Calendar (DayOfMonth, MonthOfYear, Year)
import Data.Time.Calendar.Month (Month)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy (def, layout_title, layoutlr_title, line, plot, plotLeft, plotRight)
import Options.Applicative as OptParse
import System.FilePath ((</>))
import System.IO (IOMode (WriteMode), stderr, withFile)
import Universum.Document (Document, documentMarks)
import Universum.Mark (Mark (MarkComment, MarkPhoto, _comment), MarkMetadata, comment, grade, markDate, markMetadata, _MarkRate, _photo)
import Prelude hiding (hPutStrLn, writeFile)

data Options = Options
  { optCommand :: Command,
    optInputFile :: FilePath
  }

data Command
  = PlotWordCount Int FilePath
  | PlotGrade Int FilePath
  | ConvertToMarkdown FilePath

commandParser :: OptParse.Parser Command
commandParser =
  OptParse.subparser
    ( OptParse.command
        "plot-word-count"
        (info plotWordCountCommand (progDesc "Plot the word count over time"))
        <> OptParse.command "plot-mood" (info plotGradeCommand (progDesc "Plot the mood over time"))
        <> OptParse.command "convert-to-markdown" (info convertToMarkdownCommand (progDesc "Convert the whole (extracted) diary into a markdown file"))
    )
  where
    plotWordCountCommand = PlotWordCount <$> OptParse.option OptParse.auto (OptParse.long "average-days" <> OptParse.showDefault <> OptParse.value 1) <*> OptParse.strOption (OptParse.long "output-file")
    plotGradeCommand = PlotGrade <$> OptParse.option OptParse.auto (OptParse.long "average-days" <> OptParse.showDefault <> OptParse.value 1) <*> OptParse.strOption (OptParse.long "output-file")
    convertToMarkdownCommand = ConvertToMarkdown <$> OptParse.strOption (OptParse.long "output-dir")

optionsParser :: OptParse.Parser Options
optionsParser =
  Options
    <$> commandParser
    <*> OptParse.strOption (OptParse.long "input-file" <> OptParse.help "data.pr input file")

movingAverage :: Int -> ([a] -> f) -> [a] -> [f]
movingAverage n f xs = evalState (forM xs $ \x -> modify ((x :) . take (n - 1)) >> gets f) []

calcAverageDays :: [(Day, Int)] -> (Day, Float)
calcAverageDays xs =
  let totalCount = sum (snd <$> xs)
   in (fst (head xs), fromIntegral totalCount / fromIntegral (length xs))

plotWordCount :: Document -> Int -> FilePath -> IO ()
plotWordCount document averageDays outputFile =
  let dayGroups :: [NonEmpty.NonEmpty Mark]
      dayGroups = NonEmpty.groupBy ((==) `on` (view (markMetadata . markDate))) (document ^. documentMarks)
      wordCount :: Text -> Int
      wordCount = length . Text.words
      sumDay :: NonEmpty.NonEmpty Mark -> (Day, Int)
      sumDay marks = ((NonEmpty.head marks) ^. markMetadata . markDate, wordCount $ foldOf (traverse . comment) marks)
      dayToWordCount :: [(Day, Int)]
      dayToWordCount = sumDay <$> dayGroups
      averageDayToWordCount :: [(Day, Float)]
      averageDayToWordCount = movingAverage averageDays calcAverageDays dayToWordCount
   in toFile def outputFile $ do
        layoutlr_title .= "Word count per day"
        plotLeft (line "Word count" [dayToWordCount])
        plotRight (line "Averaged word count" [averageDayToWordCount])

monthToText :: Day -> Text
monthToText = pack . formatTime defaultTimeLocale "%B"

convertToMarkdown :: Document -> FilePath -> IO ()
convertToMarkdown document outputDirectory = do
  let markYear :: Getter MarkMetadata Year
      markYear = markDate . to ((\(year, _month, _day) -> year) . toGregorian)
      markMonth :: Getter MarkMetadata MonthOfYear
      markMonth = markDate . to ((\(_year, month, _day) -> month) . toGregorian)
      markDay :: Getter MarkMetadata DayOfMonth
      markDay = markDate . to ((\(_year, _month, day) -> day) . toGregorian)
      byYear :: [NonEmpty.NonEmpty Mark]
      byYear = NonEmpty.groupAllWith (view (markMetadata . markYear)) (document ^. documentMarks)
  forM_ byYear \marksThisYear -> do
    let year :: Year
        year = NonEmpty.head marksThisYear ^. markMetadata . markYear
    withFile (outputDirectory </> (show year <> ".md")) WriteMode \handle -> do
      let appendLine :: Text -> IO ()
          appendLine = hPutStrLn handle
      -- in case you're using emojis, and because noto is a nice standard font
      -- source: https://tex.stackexchange.com/questions/722966/automatically-use-fallback-font-for-emoji-when-generating-beamer-slides-with-pan
      appendLine "---"
      appendLine "mainfont: NotoSans"
      appendLine "mainfontfallback: "
      appendLine "  - \"NotoColorEmoji:mode=harf\""
      appendLine "---"
      appendLine ""
      appendLine ("# " <> pack (show year))
      appendLine ""
      let byMonth :: [NonEmpty.NonEmpty Mark]
          byMonth = NonEmpty.groupAllWith (view (markMetadata . markMonth)) (NonEmpty.toList marksThisYear)
      forM_ byMonth \marksThisMonth -> do
        appendLine ("## " <> monthToText (NonEmpty.head marksThisMonth ^. markMetadata . markDate))
        appendLine ""

        let byDay :: [NonEmpty.NonEmpty Mark]
            byDay = NonEmpty.groupAllWith (view (markMetadata . markDay)) (NonEmpty.toList marksThisMonth)

        forM_ byDay \marksForDay -> do
          let dayRating =
                case marksForDay ^? traverse . _MarkRate . _2 of
                  Nothing -> ""
                  Just rating -> " (" <> Text.replicate rating "⭐" <> ")"
          appendLine ("### " <> pack (show (NonEmpty.head marksForDay ^. markMetadata . markDay)) <> ". " <> dayRating)
          appendLine ""

          forM_ marksForDay \mark -> do
            case mark of
              m@(MarkComment {_comment}) -> do
                appendLine _comment
                appendLine ""
              m@(MarkPhoto {_photo, _comment}) -> do
                appendLine ("![" <> _comment <> "](" <> pack _photo <> " \"" <> _comment <> "\")")
                appendLine ""
              _ -> pure ()

-- let years :: [Year]
--     years = nub (document ^.. documentMarks . traverse . markMetadata . markDate . to ((\(year, _month, _day) -> year) . toGregorian))
-- forM_ years \year -> do
--   let marksThisYear =
--   pure ()

plotMood :: Document -> Int -> FilePath -> IO ()
plotMood document averageDays outputFile =
  let dayGroups :: [NonEmpty.NonEmpty Mark]
      dayGroups = NonEmpty.groupBy ((==) `on` (view (markMetadata . markDate))) (document ^. documentMarks)
      sumDay :: NonEmpty.NonEmpty Mark -> (Day, Int)
      sumDay marks = ((NonEmpty.head marks) ^. markMetadata . markDate, sumOf (traverse . grade) marks)
      dayToGrade :: [(Day, Int)]
      dayToGrade = sumDay <$> dayGroups
      averageDayToGrade :: [(Day, Float)]
      averageDayToGrade = movingAverage averageDays calcAverageDays dayToGrade
   in toFile def outputFile $ do
        layout_title .= "Grade per day"
        plot (line "Averaged grade" [averageDayToGrade])

main' :: Options -> IO ()
main' o = do
  parsedInputFile' <- eitherDecodeFileStrict (optInputFile o)
  case parsedInputFile' of
    Left e -> hPutStrLn stderr $ pack ("error reading input file " ++ optInputFile o ++ ": " ++ e)
    Right parsedInputFile ->
      case optCommand o of
        PlotWordCount averageDays outputFile -> plotWordCount parsedInputFile averageDays outputFile
        PlotGrade averageDays outputFile -> plotMood parsedInputFile averageDays outputFile
        ConvertToMarkdown outputFile -> convertToMarkdown parsedInputFile outputFile

main :: IO ()
main = main' =<< OptParse.execParser opts
  where
    opts =
      OptParse.info
        (optionsParser <**> OptParse.helper)
        ( OptParse.fullDesc
            <> OptParse.progDesc "Command-line utility for various Universum diary tasks"
        )
