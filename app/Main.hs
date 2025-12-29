module Main (main) where

import Control.Lens (foldOf, sumOf, view, (.=), (^.))
import Control.Monad (forM)
import Control.Monad.State (evalState, gets, modify)
import Data.Aeson (eitherDecodeFileStrict)
import Data.Function (on)
import qualified Data.List.NonEmpty as NonEmpty
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Time (Day)
import Graphics.Rendering.Chart.Backend.Diagrams (toFile)
import Graphics.Rendering.Chart.Easy (def, layout_title, layoutlr_title, line, plot, plotLeft, plotRight)
import Options.Applicative as OptParse
import System.IO (hPutStrLn, stderr)
import Universum.Document (Document, documentMarks)
import Universum.Mark (Mark, comment, grade, markDate, markMetadata)

data Options = Options
  { optCommand :: Command,
    optInputFile :: FilePath
  }

data Command
  = PlotWordCount Int FilePath
  | PlotGrade Int FilePath

commandParser :: OptParse.Parser Command
commandParser = OptParse.subparser (OptParse.command "plot-word-count" (info plotWordCountCommand (progDesc "Plot the word count over time")) <> OptParse.command "plot-mood" (info plotGradeCommand (progDesc "Plot the mood over time")))
  where
    plotWordCountCommand = PlotWordCount <$> OptParse.option OptParse.auto (OptParse.long "average-days" <> OptParse.showDefault <> OptParse.value 1) <*> OptParse.strOption (OptParse.long "output-file")
    plotGradeCommand = PlotGrade <$> OptParse.option OptParse.auto (OptParse.long "average-days" <> OptParse.showDefault <> OptParse.value 1) <*> OptParse.strOption (OptParse.long "output-file")

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
    Left e -> hPutStrLn stderr $ "error reading input file " ++ optInputFile o ++ ": " ++ e
    Right parsedInputFile ->
      case optCommand o of
        PlotWordCount averageDays outputFile -> plotWordCount parsedInputFile averageDays outputFile
        PlotGrade averageDays outputFile -> plotMood parsedInputFile averageDays outputFile

main :: IO ()
main = main' =<< OptParse.execParser opts
  where
    opts =
      OptParse.info
        (optionsParser <**> OptParse.helper)
        ( OptParse.fullDesc
            <> OptParse.progDesc "Command-line utility for various Universum diary tasks"
        )
