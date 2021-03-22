module Muhsic.Option (Options(..), arguments) where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
    { optBpm  :: Float
    , optVol  :: Float
    , optOut :: FilePath
    , optIn :: FilePath}

bpm :: Parser Float
bpm = option auto
  (  long "bpm"
  <> short 'b'
  <> metavar "BPM"
  <> help "Beats per minute"
  <> showDefault
  <> value 120)

volume :: Parser Float
volume = option auto
  (  long "volume"
  <> short 'v'
  <> metavar "VOLUME"
  <> help "Specify base volume"
  <> showDefault
  <> value 0.2)

filePath :: Parser FilePath
filePath = option str
  (  long "output"
  <> short 'o'
  <> metavar "NAME"
  <> help "Name of the output"
  <> showDefault
  <> value "output.bin" )

inputFile :: Parser FilePath
inputFile = strOption
  (  long "input"
  <> short 'i'
  <> metavar "PATH"
  <> help "Input file"
  )

options :: Parser Options
options = Options <$> bpm <*> volume <*> filePath <*> inputFile

opts :: ParserInfo Options
opts = info (options <**> helper)
    ( fullDesc
    <> progDesc "Takes a file and play the corresponding music"
    <> header "muhsic - make muhsic with Hakell" )

arguments :: IO Options
arguments = execParser opts
