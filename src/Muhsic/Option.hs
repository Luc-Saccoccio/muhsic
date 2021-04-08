module Muhsic.Option
    (Options(..)
    , arguments)
    where

import           Data.Semigroup      ((<>))
import           Options.Applicative

data Options = Options
    { optContent :: Bool
    , optBpm  :: Float
    , optVol  :: Float
    , optOut :: FilePath
    , optIn :: FilePath}

versionNumber :: String
versionNumber = "v0.1.0.1"

content :: Parser Bool
content = switch
  (  long "content"
  <> short 'c'
  <> help "Display information about the file"
  )

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
  <> short 'V'
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

version :: Parser (a -> a)
version = infoOption versionNumber
  (  long "version"
  <> short 'v'
  <> help "Show program version")

options :: Parser Options
options = Options <$> content <*> bpm <*> volume <*> filePath <*> inputFile

opts :: ParserInfo Options
opts = info (helper <*> version <*> options)
    ( fullDesc
    <> progDesc "Takes a file and play the corresponding music"
    <> header "muhsic - make muhsic with Hakell" )

arguments :: IO Options
arguments = execParser opts
