module Main where

import Data.Function(on)
import qualified Data.List.NonEmpty as NonEmpty
import Options.Applicative as OptParse
import Graphics.Rendering.Chart.Backend.Diagrams(toFile)
import Graphics.Rendering.Chart.Easy(plot, def, layout_title, line)
import Data.Time (Day)
import qualified Data.Text as Text
import Data.Text(Text)
import Data.Aeson(eitherDecodeFileStrict)
import System.IO(stderr, hPutStrLn)
import Control.Lens((.=), (^.), sumOf, view, foldOf)
import Universum.Document(Document, documentMarks)
import Universum.Mark(Mark, markDate, markMetadata, comment)

data Options = Options
  { optCommand :: Command,
    optInputFile :: FilePath
  }

data Command
  = PlotWordCount Int FilePath
  | PlotMood

commandParser :: OptParse.Parser Command
commandParser = OptParse.subparser (OptParse.command "plot-word-count" (info plotWordCountCommand (progDesc "Plot the word count over time")))
  where
    plotWordCountCommand = PlotWordCount <$> OptParse.option OptParse.auto (OptParse.long "average-days" <> OptParse.showDefault <> OptParse.value 1) <*> OptParse.strOption (OptParse.long "output-file")

optionsParser :: OptParse.Parser Options
optionsParser =
  Options
    <$> commandParser <*> OptParse.strOption (OptParse.long "input-file" <> OptParse.help "data.pr input file")

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
  in
    toFile def outputFile $ do
      layout_title .= "Word count per day"
      plot (line "Averaged word count" [ dayToWordCount ])

main' :: Options -> IO ()
main' o = do
  parsedInputFile' <- eitherDecodeFileStrict (optInputFile o)
  case parsedInputFile' of
    Left e -> hPutStrLn stderr $ "error reading input file " ++ optInputFile o ++ ": " ++ e
    Right parsedInputFile ->
      case optCommand o of
        PlotWordCount averageDays outputFile -> plotWordCount parsedInputFile averageDays outputFile
        PlotMood -> putStrLn "mood plot not supported yet"

main :: IO ()
main = main' =<< OptParse.execParser opts
  where
    opts =
      OptParse.info
        (optionsParser <**> OptParse.helper)
        ( OptParse.fullDesc
            <> OptParse.progDesc "Command-line utility for various Universum diary tasks"
        )
