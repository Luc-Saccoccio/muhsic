module Muhsic where

import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy    as B
import           System.Environment
import           System.Exit
import           System.Process
import           Text.Printf

type Pulse = Float
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float

outputFilePath :: FilePath
outputFilePath = "output.bin"

volume :: Float
volume = 0.2

sampleRate :: Samples
sampleRate = 48000.0

pitchStandard :: Hz
pitchStandard = 440.0

bpm :: Beats
bpm = 120.0

beatDuration :: Seconds
beatDuration = 60.0 / bpm

-- NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
f :: Semitones -> Hz
f 0.5 = 0
f n   = pitchStandard * (2 ** (1.0 / 12.0)) ** n

note :: (Semitones, Beats) -> [Pulse]
note (n, beats) = freq (f n) (beats * beatDuration)

notes :: ([Semitones], Beats) -> [Pulse]
notes (ns, beats) = foldl1 sumFreq $ map (\n -> freq (f n) (beats * beatDuration)) ns

-- Assume the list have the same length
sumFreq :: [Pulse] -> [Pulse] -> [Pulse]
sumFreq [] [] = []
sumFreq (x:xs) (y:ys) = (x+y):sumFreq xs ys

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

wave :: String -> [Pulse]
wave = concatMap (notes . readNotes . filter (/= "|") . words) . filter noComment . filter (/= "") . lines

noComment :: String -> Bool
noComment (c:_) = c /= '#'
noComment _ = False

save :: FilePath -> [Pulse] -> IO ()
save filePath = B.writeFile filePath . B.toLazyByteString . foldMap B.floatLE

readNotes :: [String] -> ([Semitones], Seconds)
readNotes list = aux list []
    where aux [x] accu = (accu, read x::Seconds)
          aux (x:xs) accu = aux xs ((read x::Semitones):accu)

play :: [Pulse] -> IO ()
play music = do
  save outputFilePath music
  _ <- runCommand $ printf "ffplay -showmode 1 -f f32le -ar %f %s" sampleRate outputFilePath
  return ()

parse :: [String] -> IO String
parse ["-h"] = usage   >> exitSuccess
parse ["-v"] = version >> exitSuccess
parse []     = getContents
parse fs     = concat `fmap` mapM readFile fs

usage :: IO ()
usage   = putStrLn "Usage: muhsic [-vh] [file ..]"

version :: IO ()
version = putStrLn "muhsic 0.2"

main :: IO ()
main = getArgs >>= parse >>= play . wave

