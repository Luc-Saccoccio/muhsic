module Muhsic.Run where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Data.List.Split         (wordsBy)
import           Muhsic.Option
import           Muhsic.Wave
import           System.Process
import           Text.Printf


parseFile :: String -> [[[String]]]
parseFile = map (wordsBy (=="---")) . wordsBy (=="===") . filter goodLine . lines
    where
        noComment :: String -> Bool
        noComment (c:_) = c /= '#'
        noComment _     = False

        goodLine :: String -> Bool
        goodLine str = str /= "" && noComment str

printInfos :: Options -> [[[String]]] -> IO ()
printInfos (Options _ bpm volume outFile file) partition = putStrLn $ printf "Input file: %s\nOutput file: %s\nBeats per minute: %f\nVolume: %f\nNumber of mesure: %d" file outFile bpm volume nbMesures
    where
        nbMesures :: Int
        nbMesures = length partition

-- | Save the wave to a binary file
save :: FilePath -> [Pulse] -> IO ()
save filePath = B.writeFile filePath . B.toLazyByteString . foldMap B.floatLE

-- | Play the binary file
play :: FilePath -> [Pulse] -> IO ()
play outputFilePath music = do
  save outputFilePath music
  _ <- runCommand $ printf "ffplay -loglevel quiet -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

parseArguments :: Options -> IO ()
parseArguments opts@(Options True _ _ _ file) = printInfos opts . parseFile =<< readFile file
parseArguments (Options False bpm volume outFile file) = play outFile . wave volume beatDuration . parseFile =<< readFile file
    where
        beatDuration :: Seconds
        beatDuration = 60.0 / bpm

-- parseArguments opts@(Options c bpm volume outFile file) =
--     if c then
--          printInfos opts fileContent >> play outFile (wave volume beatDuration fileContent)
--     else
--          play outFile $ wave volume beatDuration fileContent
--     where
--         beatDuration :: Seconds
--         beatDuration = 60.0 / bpm
--
--         fileContent :: [[[String]]]
--         fileContent = readFile file >>= parseFile

-- | Main function
main :: IO ()
-- main = getArgs >>= parse >>= play . wave
main = parseArguments =<< arguments
