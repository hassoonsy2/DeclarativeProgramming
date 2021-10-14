module Parsers where

import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Control.Applicative
import Control.Monad.Trans.State (StateT(..), evalStateT, put, get)
import Data.Maybe (isJust, fromMaybe)
import Data.Char (toUpper)
import Control.Monad(mzero, mplus)
import Data.List (uncons)
import Types
import Instruments

type Parser = (StateT String Maybe)
type CharSet = [Char]

pCharSet :: CharSet -> Parser Char
pCharSet cs = do input <- uncons <$> get
                 case input of
                   Just (h, t) -> if h `elem` cs then put t >> return h else mzero
                   Nothing -> mzero

-- TODO: Schrijf en documenteer de Parser `pComplementCharSet` die een lijst van karakters meekrijgt,
-- en het eerste karakter parset wanneer dit niet in de meegegeven set karakters zit.

-- | Creëert een Parser die het eerste teken parseert als het niet in de gegeven CharSet staat. Een andere manier om te worden is: het omgekeerde van pCharSet
pComplementCharSet :: CharSet -> Parser Char
pComplementCharSet cs = do input <- uncons <$> get
                           case input of
                             Just (h, t) -> if h `notElem` cs then put t >> return h else mzero
                             Nothing -> mzero


-- TODO: Schrijf en documenteer de Parser `pString` die een gegeven String probeert te parsen. 
-- TIPS: Parse een enkele letter met `pCharSet` en parse de rest recursief met `pString`; 
--       combineer beide waarden weer voordat je deze returnt. 
--       Vergeet niet een geval voor een lege String te schrijven.
pString :: String -> Parser String
pString [] = return mzero
pString (x:xs) = do first <- pCharSet [x]
                    rest  <- pString xs
                    return $ first : rest


pOptional :: Parser a -> Parser (Maybe a)
pOptional p = Just <$> p <|> return Nothing 

pRepeatSepBy :: Parser a -> Parser b -> Parser [b]
pRepeatSepBy sep p = (:) <$> p <*> mplus (sep *> pRepeatSepBy sep p) (return [])

pEmpty :: Parser ()
pEmpty = return ()

-- TODO: Schrijf en documenteer de Parser `pRepeat` die een enkele Parser herhaaldelijk toepast.
-- TIPS: Maak gebruik van `pRepeatSepBy` en `pEmpty`.
-- | Herhaalt de parser omdat deze wordt herhaald met een scheidingsteken van een lege tuple

pRepeat :: Parser a -> Parser [a]
pRepeat a = pEmpty `pRepeatSepBy` a


numbers :: CharSet
numbers = "0123456789"


-- TODO: Schrijf en documenteer de Parser `pNumber`, die een geheel getal parset.
-- TIPS: Combineer de voorgaande Parsers en de Prelude-functie read. 

-- | Creëert een Parser die ontleedt zolang het karakter een getal is, dan converteert het het naar een geheel getal

pNumber :: Parser Int
pNumber = fmap read $ pRepeat $ pCharSet numbers

pTone :: Parser Tone
pTone = do tone <- tRead . toUpper <$> pCharSet "abcdefg"
           sharp <- pOptional (pCharSet "#")
           if isJust sharp && tone `elem` [C,D,F,G,A]
             then return (succ tone)
             else return tone
  where tRead 'C' = C
        tRead 'D' = D
        tRead 'E' = E
        tRead 'F' = F
        tRead 'G' = G
        tRead 'A' = A
        tRead 'B' = B
        tRead _   = error "Invalid note"

-- TODO: Schrijf en documenteer de Parser` `pOctave`, die een getal parset naar de bijbehorende octaaf.
-- TIPS: Kijk in Types.hs naar het type Octave voor je begint te schrijven.
pOctave :: Parser Octave
pOctave =  fmap toEnum pNumber


pDuration :: Parser Duration
pDuration = do number <- pNumber
               case number of
                 1 -> return Full
                 2 -> return Half
                 4 -> return Quarter
                 8 -> return Eighth
                 16 -> return Sixteenth
                 32 -> return Thirtysecond
                 _ -> mzero

pPause :: Duration -> Parser Note
pPause d = do duration <- fromMaybe d <$> pOptional pDuration
              _ <- pCharSet "pP"
              return $ Pause duration

pNote :: Duration -> Octave -> Parser Note
pNote d o = do duration <- fromMaybe d <$> pOptional pDuration
               tone <- pTone
               dot <- pOptional (pCharSet ".")
               octave <- fromMaybe o <$> pOptional pOctave
               return $ Note tone octave (if isJust dot then Dotted duration else duration)

pComma :: Parser ()
pComma = () <$ do _ <- pCharSet ","
                  pOptional (pCharSet " ")

-- TODO: Schrijf en documenteer de Parser `pHeader`, die de start van de RTTL-string parset.
-- TIPS: We hebben je de naam van het bestand, en het converteren van bpm met fromIntegral vast gegeven.
--       Het stuk dat je rest om te parsen zit tussen de twee dubbele punten!
pHeader :: Parser (String, Duration, Octave, Beats)
pHeader = do name <- pRepeat (pComplementCharSet ":")
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             _ <- pOptional (pCharSet " ")
             -- Your code here!
             _ <- pCharSet ":"
             _ <- pOptional (pCharSet " ")
             return (name, Full, Zero, fromIntegral 0) -- Pas deze regel ook aan; maak van 0 de waarde van bpm!

pSeparator :: Parser ()
pSeparator = () <$ foldl1 mplus [pString " ", pString ", ", pString ","]

pRTTL :: Parser (String, [Note], Beats)
pRTTL = do (t, d, o, b) <- pHeader
           notes <- pRepeatSepBy pSeparator $ mplus (pNote d o) (pPause d)
           return (t, notes, b)

parse :: String -> Maybe (String, [Note], Beats)
parse x = evalStateT pRTTL x