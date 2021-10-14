{-# LANGUAGE TypeApplications #-}

module Types where

import Data.Int (Int32)

type Pulse = [Float]
type Seconds = Float
type Samples = Float
type Hz = Float
type Semitones = Float
type Beats = Float
type Ringtone = String

data Tone = C | CSharp | D | DSharp | E | F | FSharp | G | GSharp | A | ASharp | B deriving (Enum, Eq, Show)
data Octave = Zero | One | Two | Three | Four | Five | Six | Seven | Eight deriving (Enum, Eq, Show)
data Duration = Full | Half | Quarter | Eighth | Sixteenth | Thirtysecond | Dotted Duration deriving (Eq, Show)
data Note = Pause Duration | Note Tone Octave Duration deriving (Eq, Show)

-- TODO: Schrijf en documenteer zipWithL, die zo ver mogelijk zipt;
-- als na het zippen, nog waarden in de eerste lijst zitten, plakt het deze er achteraan;
-- als na het zippen, nog waarden in de tweede lijst zitten, worden die weggegooid.

{-|Deze functie neemt het eerste element van lijst 1 en het eerste element van lijst 2 waar a
waarde uitkomt. deze waarde wordt in een nieuwe lijst geplaatst. Dit gaat door tot een van de lijsten
heeft geen waarden meer
als een van de lijsten leeg is en de eerste lijst bevat nog een waarde, dan wordt deze waarde erachter geplakt,
anders worden alle waarden weggegooid.
en de lijst met toegevoegde waarde geretourneerd -} 


zipWithL :: (a -> b -> a) -> [a] -> [b] -> [a]
zipWithL _ []_ = []
zipWithL _ a [] = a
zipWithL z (x:xs) (y:ys) = z x y : zipWithL z xs ys

-- TODO: Schrijf en documenteer zipWithR, die zo ver mogelijk zipt;
-- als na het zippen, nog waarden in de eerste lijst zitten, worden die weggegooid;
-- als na het zippen, nog waarden in de tweede lijst zitten, plakt het deze er achteraan.
{-|
Deze functie neemt het eerste element van lijst 1 en het eerste element van lijst 2 waar a
waarde uitkomt. deze waarde wordt in een nieuwe lijst geplaatst. Dit gaat door een van de lijsten
heeft geen waarden meer
als een van de lijsten leeg is en in de tweede lijst staat een andere waarde, dan wordt dit woord erachter geplakt,
anders worden alle waarden weggegooid.
en de lijst met toegevoegde waarde geretourneerd
-}
zipWithR :: (a -> b -> b) -> [a] -> [b] -> [b]
zipWithR _ _ [] = []
zipWithR _ [] a = a
zipWithR z (x:xs) (y:ys) = z x y : zipWithR z xs ys


data Sound = FloatFrames [Float]
  deriving Show

floatSound :: [Float] -> Sound
floatSound = FloatFrames  

instance Eq Sound where
  (FloatFrames xs) == (FloatFrames ys) = (all ((<  0.001) . abs) $ zipWith (-) xs ys) && (length xs == length ys)

-- TODO: Schrijf de instance-declaraties van Semigroup en Monoid voor Sound.
-- Semigroup is een typeclass met een operator (<>), die twee waarden combineert;
-- in deze context betekent dat "twee geluiden na elkaar afspelen".
-- Monoid bouwt voort op Semigroup, maar heeft een mempty; een lege waarde.

{-|
Deze instantie zorgt ervoor dat lijsten 'a' en 'b', zolang ze van het type [float] zijn, worden gecombineerd en vervolgens naar Sound.
-}
instance Semigroup Sound where
  (FloatFrames a) <> (FloatFrames b) = FloatFrames   $ a ++ b
 




instance Monoid Sound where
  mempty = FloatFrames[]

-- TODO: Schrijf en documenteer de operator `(<+>)` die twee `Sound`s  tot een enkel `Sound` combineert.
-- Combineren betekent hier: de geluiden tegelijkertijd afspelen. 
-- Als de lijsten niet even lang zijn, moet wat er overblijft achteraan worden toegevoegd!
{-|
Deze operator kijkt eerst naar de lengtes van zowel FloatFrames x als y. Als de eerste langer is dan de tweede wordt
de zipMetL-functie toegepast. Als de tweede langer is dan de eerste, wordt de functie zipWithR toegepast.
-}
(<+>) :: Sound -> Sound -> Sound
(FloatFrames x) <+> (FloatFrames y) 
 | length x > length y = FloatFrames $ zipWithL (+) x y 
 | otherwise = FloatFrames $ zipWithR (+) x y
x <+> y =  x <+>  y



floatToInt32 :: Float -> Int32
floatToInt32 x = fromIntegral $ round x

getAsInts :: Sound -> [Int32]
getAsInts (FloatFrames fs) = map (floatToInt32 . \x -> x * fromIntegral (div (maxBound @Int32 ) 2 )) fs

type Track = (Instrument, [Note])

newtype Instrument = Instrument (Hz -> Seconds -> Pulse)

instrument :: (Hz -> Seconds -> Pulse) -> Instrument
instrument = Instrument

newtype Modifier = Modifier (Pulse -> Pulse)

modifier :: (Pulse -> Pulse) -> Modifier
modifier = Modifier

instance Semigroup Modifier where
  (Modifier m1) <> (Modifier m2) = Modifier $ m1 . m2

-- TODO: Schrijf en documenteer de functie modifyInstrument, die een Modifier met een Instrument combineert. 
-- TIPS: Kijk goed naar de types! Gebruik een lambda om een functie te maken, die je verpakt in een Instrument.
{-|
converteer een instrument naar modifier en terug naar instrument
de functie neemt de hz en seconden van een instrument en geeft ze door aan "i" en zet het om in een puls
deze puls wordt doorgegeven aan m water maakt het modieuzer (wat ook een puls is)
deze modificatiepuls wordt geretourneerd als een puls van het instrument
-}

modifyInstrument :: Instrument -> Modifier -> Instrument
modifyInstrument (Instrument i) (Modifier m) = instrument $ \x y -> m $ i x y

-- TODO: Schrijf en documenteer de functie arrange die de functie in het meegegeven Instrument toepast op de frequentie en duur. 
-- TIPS: Kijk goed naar de types!

{-|
een instrument omzetten in een geluid
pakt de instrumentfunctie en geeft deze door in hz en seconden
hoer er komt een puls uit en deze puls wordt omgezet in een geluid (floatSound)
-}

arrange :: Instrument -> Hz -> Seconds -> Sound
arrange (Instrument i) h s = floatSound $ i h s
