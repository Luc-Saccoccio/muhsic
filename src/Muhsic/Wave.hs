module Muhsic.Wave
    (Pulse
    , Seconds
    , sampleRate
    , wave
    ) where


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

-- | Sample rate (48 kHz)
sampleRate :: Samples
sampleRate = 48000.0

-- | La 440 / A440
pitchStandard :: Hz
pitchStandard = 440.0

{- | NOTE: the formula is taken from https://pages.mtu.edu/~suits/NoteFreqCalcs.html
    0.5 for a pause (no sound)
    Note to frequency
-}
f :: Semitones -> Hz
f 0.5 = 0
f n   = pitchStandard * (2 ** (1.0 / 12.0)) ** n

-- | Sum the frequencies
-- Assume the list have the same length
sumFreq :: [Pulse] -> [Pulse] -> [Pulse]
sumFreq [] []         = []
sumFreq xs []         = xs
sumFreq [] ys         = ys
sumFreq (x:xs) (y:ys) = (x+y):sumFreq xs ys

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

-- | Read the notes and duration in a line
-- All are notes, except the last being the duration
readNotes :: [String] -> ([Semitones], Seconds)
readNotes list = aux list []
    where aux [x] accu    = (accu, read x::Seconds)
          aux (x:xs) accu = aux xs ((read x::Semitones):accu)

-- | Takes a frequency and a duration, and returns the wave (with the attack/release)
mkFreq :: Float -> Hz -> Seconds -> [Pulse]
mkFreq volume hz duration =
  zipWith3 (\x y z -> volume * x * y * z) release attack output
  where
    step = (hz * 2 * pi) / sampleRate

    attack :: [Pulse]
    attack = map (min 1.0) [0.0,0.001 ..]

    release :: [Pulse]
    release = reverse $ take (length output) attack

    output :: [Pulse]
    output = map (sin .(* step)) [0.0 .. sampleRate * duration]

-- | Take the file, returns the final wave
wave :: Float -> Float -> [[[String]]] -> [Pulse]
wave volume beatDuration = concatMap mesure
    where
        -- | Process a mesure
        mesure :: [[String]] -> [Pulse]
        mesure = noteSum . map hand

        -- | Process a "hand"
        hand :: [String] -> [Pulse]
        hand = concatMap (uncurry notes . readNotes . filter (/= "|") . words)

        {- | Takes the notes and the duration, return
            the Pulse list (wave for this group of note)
        -}
        notes :: [Semitones] -> Beats -> [Pulse]
        notes ns beats = foldl1 sumFreq $ map (\n -> freq (f n) (beats * beatDuration)) ns

        freq :: Hz -> Seconds -> [Pulse]
        freq = mkFreq volume


