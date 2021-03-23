module Muhsic.Run where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Muhsic.Option
import           Muhsic.Wave
import           System.Process
import           Text.Printf


-- | Save the wave to a binary file
save :: FilePath -> [Pulse] -> IO ()
save filePath = B.writeFile filePath . B.toLazyByteString . foldMap B.floatLE

-- | Play the binary file
play :: FilePath -> [Pulse] -> IO ()
play outputFilePath music = do
  save outputFilePath music
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

parseArguments :: Options -> IO ()
-- parseArguments (Options _ _ _ _ True) = do
--     putStrLn versionNumber
--     return ()
parseArguments (Options bpm volume outFile file) = do
    fileContent <- readFile file
    play outFile . wave volume beatDuration $ fileContent
    return ()
    where
        beatDuration :: Seconds
        beatDuration = 60.0 / bpm

-- | Main function
main :: IO ()
-- main = getArgs >>= parse >>= play . wave
main = parseArguments =<< arguments
