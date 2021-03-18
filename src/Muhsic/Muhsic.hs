module Muhsic where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           Data.List.Split         (wordsBy)
import           System.Environment
import           System.Exit
import           System.Process
import           Text.Printf

-- | Pulse
type Pulse = Float
-- | Seconds
type Seconds = Float
-- | Samples
type Samples = Float
-- | Hertz (Frequency)
type Hz = Float
-- | Semitones
type Semitones = Float
-- | Beats
type Beats = Float

-- | outputFile name
outputFilePath :: FilePath
outputFilePath = "output.bin"

-- | Default volume
volume :: Float
volume = 0.2

-- | Sample rate (48 kHz)
sampleRate :: Samples
sampleRate = 48000.0

-- | La 440 / A440
pitchStandard :: Hz
pitchStandard = 440.0

-- | Beats per minute
bpm :: Beats
bpm = 120.0

-- | Duration of a beat
beatDuration :: Seconds
beatDuration = 60.0 / bpm

{- | NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
    0.5 for a pause (no sound)
    Note to frequency
-}
f :: Semitones -> Hz
f 0.5 = 0
f n   = pitchStandard * (2 ** (1.0 / 12.0)) ** n

{- | Takes a tuple with the notes and the duration, return
    the Pulse list (wave for this group of note)
-}
notes :: ([Semitones], Beats) -> [Pulse]
notes (ns, beats) = foldl1 sumFreq $ map (\n -> freq (f n) (beats * beatDuration)) ns

-- | Sum the frequencies, TODO : rework for different length of lists.
-- Assume the list have the same length
sumFreq :: [Pulse] -> [Pulse] -> [Pulse]
sumFreq [] []         = []
sumFreq xs []         = xs
sumFreq [] ys         = ys
sumFreq (x:xs) (y:ys) = (x+y):sumFreq xs ys

-- | Takes a frequency and a duration, and returns the wave (with the attack/release)
freq :: Hz -> Seconds -> [Pulse]
freq hz duration =
  map (* volume) $ zipWith3 (\x y z -> x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0,0.001 ..]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output :: [Pulse]
    output = map (sin .(* step)) [0.0 .. sampleRate * duration]

-- | Take the file, returns the final wave
wave :: String -> [Pulse]
wave = concatMap mesure . wordsBy (=="===") . filter goodLine . lines
    where
        noComment :: String -> Bool
        noComment (c:_) = c /= '#'
        noComment _     = False

        goodLine :: String -> Bool
        goodLine str = str /= "" && noComment str

-- | Process a "hand"
hand :: [String] -> [Pulse]
hand = concatMap (notes . readNotes . filter (/= "|") . words)

-- | Process a mesure
mesure :: [String] -> [Pulse]
mesure = noteSum . map hand . wordsBy (=="---")

-- | Sum the notes of a mesure
noteSum :: [[Pulse]] -> [Pulse]
noteSum [] = [0]
noteSum (x:xs) = noteSum' x $ noteSum xs
    where
        noteSum' :: Num a => [a] -> [a] -> [a]
        noteSum' [] []         = []
        noteSum' a []          = a
        noteSum' [] b          = b
        noteSum' (a:as) (b:bs) = (a+b):noteSum' as bs

-- | Save the wave to a binary file
save :: FilePath -> [Pulse] -> IO ()
save filePath = B.writeFile filePath . B.toLazyByteString . foldMap B.floatLE

-- | Read the notes and duration in a line
-- All are notes, except the last being the duration
readNotes :: [String] -> ([Semitones], Seconds)
readNotes list = aux list []
    where aux [x] accu    = (accu, read x::Seconds)
          aux (x:xs) accu = aux xs ((read x::Semitones):accu)

-- | Play the binary file
play :: [Pulse] -> IO ()
play music = do
  save outputFilePath music
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

-- | Parse the arguments
-- TODO : rework
parse :: [String] -> IO String
parse ["-h"] = usage   >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse []     = usage   >> exitSuccess
parse fs     = concat `fmap` mapM readFile fs

-- | Print the usage of the program
usage :: IO ()
usage   = putStrLn "Usage: muhsic [-vh] [file ..]"

-- | Print the version of the program
version :: IO ()
version = putStrLn "muhsic 0.2"

-- | Main function
main :: IO ()
main = getArgs >>= parse >>= play . wave

