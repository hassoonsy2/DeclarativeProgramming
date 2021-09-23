{-|
    Module      : Lib
    Description : Checkpoint voor V2DeP: cellulaire automata
    Copyright   : (c) Brian van der Bijl & Nick Roumimper, 2020
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum gaan we aan de slag met 1D cellulaire automata.
    Een cellulair automaton is een rekensysteem dat, gegeven een startsituatie en regels,
    in tijdstappen bepaalt of diens cellen (vakjes) "aan" of "uit" zijn ("levend" of "dood"). 
    Denk aan Conway's Game of Life [<https://playgameoflife.com/>], maar dan in één dimensie (op een lijn).
    Als we de tijdstappen verticaal onder elkaar plotten, krijgen we piramides met soms verrassend complexe patronen erin.
    LET OP: lees sowieso de informatie op [<https://mathworld.wolfram.com/Rule30.html>] en de aangesloten pagina's!
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib where

import Data.Maybe (catMaybes) -- Niet gebruikt, maar deze kan van pas komen...
import Data.List (unfoldr)
import Data.Tuple (swap)


-- ..:: Sectie 1: Basisoperaties op de FocusList ::..

-- Om de state van een cellulair automaton bij te houden bouwen we eerst een set functies rond een `FocusList` type. 
-- Dit type representeert een (1-dimensionale) lijst, met een enkel element dat "in focus" is. 
-- Het is hierdoor mogelijk snel en makkelijk een enkele cel en de cellen eromheen te bereiken.
-- Een voorbeeld van zo'n "gefocuste" lijst: 
--     [0, 1, 2, <3>, 4, 5]
-- waarbij 3 het getal is dat in focus is.
-- In Haskell representeren we de bovenstaande lijst als:
--     FocusList [3,4,5] [2,1,0]
-- waarbij het eerste element van de eerste lijst in focus is.
-- De elementen die normaal vóór de focus staan, staan in omgekeerde volgorde in de tweede lijst.
-- Hierdoor kunnen we makkelijk (lees: zonder veel iteraties te hoeven doen) bij de elementen rondom de focus,
-- en de focus makkelijk één plaats opschuiven.

data FocusList a = FocusList { forward :: [a]
                             , backward :: [a]
                             }
  deriving (Show, Eq)

-- De instance-declaraties mag je voor nu negeren.
instance Functor FocusList where
  fmap = mapFocusList

-- Enkele voorbeelden om je functies mee te testen:
intVoorbeeld :: FocusList Int
intVoorbeeld = FocusList [3,4,5] [2,1,0]

stringVoorbeeld :: FocusList String
stringVoorbeeld = FocusList ["3","4","5"] ["2","1","0"]

-- TODO: Schrijf en documenteer de functie toList, die een focus-list omzet in een gewone lijst. 
-- Het resultaat bevat geen focus-informatie meer, maar moet wel op de juiste volgorde staan.
-- Voorbeeld: toList intVoorbeeld ~> [0,1,2,3,4,5] 

-- | toList is een fuctie die een FocusList omzet naar een normale List; Reverse de Backwards List en plak de Forward list er achter
toList :: FocusList a -> [a]
toList (FocusList x z) = reverse z ++ x


-- TODO: Schrijf en documenteer de functie fromList, die een gewone lijst omzet in een focus-list. 
-- Omdat een gewone lijst geen focus heeft moeten we deze kiezen; dit is altijd het eerste element.

-- | fromList is een functie die een normale lijst omzet naar een focus list; Om het eerste element in focus te zetten doen we de hele lijst in de voorwaartse lijst en een lege lijst meegeven voor de achterwaartse lijst
fromList :: [a] -> FocusList a
fromList l = FocusList l []

-- Deze functie, die je van ons cadeau krijgt, schuift de focus één naar links op.
-- Voorbeeld: goLeft $ FocusList [3,4,5] [2,1,0] ~> FocusList [2,3,4,5] [1,0]
-- | Verplaats de focus één naar links
goLeft :: FocusList a -> FocusList a
goLeft (FocusList fw (f:bw)) = FocusList (f:fw) bw

-- TODO: Schrijf en documenteer de functie goRight, die de focuslist een plaats naar rechts opschuift.
-- | goRight verplaatst de focus van het element 1 naar rechts toe. De head van de Forwards list verplaatst je naar de 

goRight :: FocusList a -> FocusList a
goRight (FocusList (f:fw) b) = FocusList fw (f:b)

-- TODO: Schrijf en documenteer de functie leftMost, die de focus helemaal naar links opschuift.
-- | leftMost is een fucntie die de focus geheel naar links opschuift. De hele focusList wordt terug gezet naar een normale list en dan wordt een lege list in de backwards gezet en zo wordt de hele focus naar links verplaatst
leftMost :: FocusList a -> FocusList a
leftMost (FocusList f b) = FocusList (reverse b ++ f) [] 

-- TODO: Schrijf en documenteer de functie rightMost, die de focus helemaal naar rechts opschuift.
-- | rightMost is een fucntie die de focus geheel naar rechts opschuift. Eerst wordt de focus list omgezet naar een normale list, dan word in de forward het laatste element gezet in de backwards de voorgaande elementen in reverse mbv de init van de list
rightMost :: FocusList a -> FocusList a
rightMost fl = FocusList [last list] $ reverse $ init list
  where list = toList fl

-- Onze functies goLeft en goRight gaan er impliciet van uit dat er links respectievelijk rechts een waarde gedefinieerd is. 
-- De aanroep `goLeft $ fromList [1,2,3]` zal echter crashen, omdat er in een lege lijst gezocht wordt: er is niets verder naar links. 
-- Dit is voor onze toepassing niet handig, omdat we vaak de cellen direct links en rechts van de focus 
-- nodig hebben, ook als die (nog) niet bestaan.

-- Schrijf de functies totalLeft en totalRight die de focus naar links respectievelijk rechts opschuift; 
-- als er links/rechts geen vakje meer is, dan wordt een lege (dode) cel teruggeven. 
-- Hiervoor gebruik je de waarde `mempty`, waar we met een later college nog op in zullen gaan. 
-- Kort gezegd zorgt dit ervoor dat de FocusList ook op andere types blijft werken - 
-- je kan dit testen door totalLeft/totalRight herhaaldelijk op de `voorbeeldString` aan te roepen, 
-- waar een leeg vakje een lege string zal zijn.
-- NOTE: deze functie werkt niet met intVoorbeeld! (Omdat mempty niet bestaat voor Ints - maar daar komen we nog op terug!)

-- Grafisch voorbeeld: [⟨░⟩, ▓, ▓, ▓, ▓, ░]  ⤚totalLeft→ [⟨░⟩, ░, ▓, ▓, ▓, ▓, ░]

-- TODO: Schrijf en documenteer de functie totalLeft, zoals hierboven beschreven.
-- | Verplaatst de focus één naar links, maar als er geen element is, wordt het aangevuld met mempty
totalLeft :: (Eq a, Monoid a) => FocusList a -> FocusList a
totalLeft focusl = if length (backward focusl) == 0
  then FocusList (mempty : (toList focusl)) []
  else goLeft focusl


-- TODO: Schrijf en documenteer de functie totalRight, zoals hierboven beschreven.
-- | Verplaatst de focus één naar Rechts, maar als er geen element is, wordt het aangevuld met mempty
totalRight :: (Eq a, Monoid a) => FocusList a -> FocusList a
totalRight focusl = if length (forward focusl) == 1
  then FocusList [mempty] $ head (forward focusl) : backward focusl 
  else goRight focusl



-- ..:: Sectie 2 - Hogere-ordefuncties voor de FocusList ::..

-- In de colleges hebben we kennis gemaakt met een aantal hogere-orde functies zoals `map`, `zipWith` en `fold[r/l]`. 
-- Hier stellen we equivalente functies voor de FocusList op.

-- TODO: Schrijf en documenteer de functie mapFocusList.
-- Deze werkt zoals je zou verwachten: de functie wordt op ieder element toegepast, voor, op en na de focus. 
-- Je mag hier gewoon map voor gebruiken.
-- | Brengt een functie in kaart op een focuslijst. Het doet dit door de functie alleen toe te passen op de forward  en de backward
mapFocusList :: (a -> b) -> FocusList a -> FocusList b
mapFocusList x (FocusList forward  backward) = FocusList (map x forward) $ map x backward
-- TODO: Schrijf en documenteer de functie zipFocusListWith.
-- Deze functie zorgt ervoor dat ieder paar elementen uit de FocusLists als volgt met elkaar gecombineerd wordt:

-- [1, 2, ⟨3⟩,  4, 5]        invoer 1
-- [  -1, ⟨1⟩, -1, 1, -1]    invoer 2
--------------------------- (*)
-- [  -2, ⟨3⟩, -4, 5    ]    resultaat

-- of, in code: zipFocusListWith (*) (FocusList [3,4,5] [2,1]) (FocusList [1,-1,1,-1] [-1]) ~> FocusList [3,-4,5] [-2]
-- Oftewel: de meegegeven functie wordt aangeroepen op de twee focus-elementen, met als resultaat het nieuwe focus-element. 
-- Daarnaast wordt de functie paarsgewijs naar links/rechts doorgevoerd, waarbij gestopt wordt zodra een van beide uiteinden leeg is. 
-- Dit laatste is net als bij de gewone zipWith, die je hier ook voor mag gebruiken.

-- | Zipt twee focuslijsten met een bepaalde functie. Het doet dit door de twee forwards en twee backwards te ritsen met de functie
zipFocusListWith :: (a -> b -> c) -> FocusList a -> FocusList b -> FocusList c
zipFocusListWith f (FocusList for1 bac1) (FocusList for2 bac2) = 
  let
    for_length = min (length for1) (length for2)
    bac_length = min (length bac1) (length bac2)
  in FocusList 
  (zipWith f (take for_length for1) (take for_length for2))
  (zipWith f (take bac_length bac1) (take bac_length bac2))


-- TODO: Schrijf en documenteer de functie foldFocusList.
-- Het folden van een FocusList vergt de meeste toelichting: waar we met een normale lijst met een left fold en een 
-- right fold te maken hebben, folden we hier vanuit de focus naar buiten.
-- Vanuit de focus worden de elementen van rechts steeds gecombineerd tot een nieuw element, 
-- vanuit het element voor de focus gebeurt hetzelfde vanuit links. 
-- De twee resultaten van beide sublijsten (begin tot aan focus, focus tot en met eind) worden vervolgens nog een keer met de meegegeven functie gecombineerd. 
-- Hieronder een paar voorbeelden:

-- foldFocusList (*) [0, 1, 2, ⟨3⟩, 4, 5] = (0 * (1 * 2)) * ((3 * 4) * 5)
--                                       = (0 * 2) * (12 * 5)
--                                       = 0 * 60
--                                       = 0

-- foldFocusList (-) [0, 1, 2, ⟨3⟩, 4, 5] = (0 - (1 - 2)) - ((3 - 4) - 5)
--                                       = (0 - (-1)) - ((-1) - 5)
--                                       = 1 - (-6)
--                                       = 7

-- Je kunt, buiten de testsuite, `testFold` uitvoeren in "stack ghci" om je functie te testen.
-- Let op: de tweede lijst van de FocusList kan leeg zijn! (De eerste technisch gezien ook, maar dan heb je geen geldige FocusList.)
-- | Het Folding van een focuslijst met een bepaalde functie. Het doet dit door de forward en de backward  te vouwen en vervolgens de functie toe te passen om die twee te combineren
foldFocusList :: (a -> a -> a) -> FocusList a -> a
foldFocusList f (FocusList fw bw) =  (foldr1 f (reverse bw)) `f` (foldl1 f fw)



-- Testfunctie voor foldFocusList (geeft True als alles klopt, False als er één of meer niet kloppen)
testFold :: Bool
testFold = and [ foldFocusList (+) intVoorbeeld     == 15
               , foldFocusList (-) intVoorbeeld     == 7
               , foldFocusList (++) stringVoorbeeld == "012345"
               ]


-- ..:: Sectie 2.5: Types voor cellulaire automata ::..

-- Nu we een redelijk complete FocusList hebben, kunnen we deze gaan gebruiken om cellulaire automata in te ontwikkelen.
-- In deze sectie hoef je niets aan te passen, maar je moet deze wel even doornemen voor de volgende opgaven.

-- Een cel kan ofwel levend, ofwel dood zijn.
data Cell = Alive | Dead deriving (Show, Eq)

-- De onderstaande instance-declaraties mag je in dit practicum negeren.
instance Semigroup Cell where
  Dead <> x = x
  Alive <> x = Alive

instance Monoid Cell where
  mempty = Dead

-- De huidige status van ons cellulair automaton beschrijven we als een FocusList van Cells.
-- Dit type noemen we Automaton.
type Automaton = FocusList Cell

-- De standaard starttoestand bestaat uit één levende cel in focus.
start :: Automaton
start = FocusList [Alive] []

-- De context van een cel is de waarde van een cel, samen met de linker- en rechterwaarde, op volgorde.
-- Voorbeeld: de context van 4 in [1,2,3,4,5,6] is [3,4,5].
-- In de praktijk bestaat de context altijd uit drie cellen, maar dat zie je niet terug in dit type.
type Context = [Cell]

-- Een regel is een mapping van elke mogelijke context naar de "volgende state" - levend of dood.
-- Omdat er 2^3 = 8 mogelijke contexts zijn, zijn er 2^8 = 256 geldige regels.
-- Zie voor een voorbeeld [<https://mathworld.wolfram.com/Rule30.html>].
type Rule = Context -> Cell


-- ..:: Sectie 3: Rule30 en helperfuncties ::..
-- | een functie safeHead die het eerste item van een lijst geeft; als de lijst leeg is wordt een meegegeven default values teruggegeven.
-- TODO: Schrijf en documenteer de functie safeHead, die het eerste item van een lijst geeft; 
-- als de lijst leeg is, wordt een meegegeven defaultwaarde teruggegeven.
safeHead :: a        -- ^ Defaultwaarde
         -> [a]      -- ^ Bronlijst
         -> a
safeHead dv [] = dv
safeHead dv x = head(x)


-- TODO: Schrijf en documenteer de functie takeAtLeast, die werkt als `take`, maar met een extra argument. 
-- Als de lijst lang genoeg is werkt de functie hetzelfde als `take` en worden de eerste `n` elementen teruggegeven.
-- Zo niet, dan worden zoveel mogelijk elementen teruggegeven, en wordt daarna tot aangevuld met de meegegeven defaultwaarde.
-- Voorbeelden: takeAtLeast 4 0 [1,2,3,4,5] ~> [1,2,3,4]
--              takeAtLeast 4 0 [1,2]       ~> [1,2,0,0]

-- | Geeft de eerste n elementen van de opgegeven lijst, als er niet genoeg zijn, wordt deze aangevuld met de opgegeven standaardwaarde
takeAtLeast :: Int   -- ^ Aantal items om te pakken
            -> a     -- ^ Defaultwaarde
            -> [a]   -- ^ Bronlijst
            -> [a]
takeAtLeast n d s = take n s ++ replicate (n - length s) d

-- TODO: Schrijf en documenteer de functie context, die met behulp van takeAtLeast de context van de focus-cel in een Automaton teruggeeft. 
-- Niet-gedefinieerde cellen zijn per definitie Dead.

-- | pak minimaal 1 dode cell van de bw list van de focus list, plak daarna minimaal 2 dode cellen van de Fw list van de focus list
context :: Automaton -> Context
context (FocusList fw bw)= (takeAtLeast 1 Dead bw)++(takeAtLeast 2 Dead fw)

-- TODO: Schrijf en documenteer de functie expand die een Automaton uitbreidt met een dode cel aan beide uiteindes. 
-- We doen voor deze simulatie de aanname dat de "known universe" iedere ronde met 1 uitbreidt naar zowel links als rechts.

-- | bij elke Automaton die binnenkomt aan de uiteindes van de fw en bw van de focusList een [Dead] cell toevoegen, rekening houdent met de reverse list
expand :: Automaton -> Automaton
expand (FocusList fw bw) = (FocusList ( fw++[Dead]) (bw ++ [Dead]))

-- TODO: Vul de functie rule30 aan met de andere 7 gevallen. 
-- Voor bonuspunten: doe dit in zo min mogelijk regels code. De underscore _ is je vriend.

-- | Hard gecodeerde rule30 door gebruik te maken van pattern matching
rule30 :: Rule
rule30 [Dead, Dead, Dead] = Dead
rule30 [Dead, Dead, Alive] = Alive
rule30 [Dead, Alive, Dead] = Alive
rule30 [Dead, Alive, Alive] = Alive
rule30 [Alive, Dead, Dead] = Alive
rule30 [Alive, Dead, Alive] = Dead
rule30 [Alive, Alive, Dead] = Dead
rule30 [Alive, Alive, Alive] = Dead

-- ...

-- Je kan je rule-30 functie in GHCi (voer `stack ghci` uit) testen met het volgende commando:
-- putStrLn . showPyramid . iterateRule rule30 15 $ start
-- (Lees sectie 3.5 voor uitleg over deze functies - en hoe het afdrukken nou precies werkt!)

-- De verwachte uitvoer is dan:
{-             ▓
              ▓▓▓
             ▓▓░░▓
            ▓▓░▓▓▓▓
           ▓▓░░▓░░░▓
          ▓▓░▓▓▓▓░▓▓▓
         ▓▓░░▓░░░░▓░░▓
        ▓▓░▓▓▓▓░░▓▓▓▓▓▓
       ▓▓░░▓░░░▓▓▓░░░░░▓
      ▓▓░▓▓▓▓░▓▓░░▓░░░▓▓▓
     ▓▓░░▓░░░░▓░▓▓▓▓░▓▓░░▓
    ▓▓░▓▓▓▓░░▓▓░▓░░░░▓░▓▓▓▓
   ▓▓░░▓░░░▓▓▓░░▓▓░░▓▓░▓░░░▓
  ▓▓░▓▓▓▓░▓▓░░▓▓▓░▓▓▓░░▓▓░▓▓▓
 ▓▓░░▓░░░░▓░▓▓▓░░░▓░░▓▓▓░░▓░░▓
▓▓░▓▓▓▓░░▓▓░▓░░▓░▓▓▓▓▓░░▓▓▓▓▓▓▓ -}


-- ..:: Sectie 3.5: Het herhaaldelijk uitvoeren en tonen van een cellulair automaton ::..
-- In deze sectie hoef je niets aan te passen, maar je moet deze wel even doornemen voor de volgende opgaven.

-- Een reeks van Automaton-states achtereen noemen we een TimeSeries. Effectief dus gewoon een lijst van Automatons over tijd.
type TimeSeries = [Automaton]

-- De functie iterateRule voert een regel n keer uit, gegeven een starttoestand (Automaton). 
-- Het resultaat is een reeks van Automaton-states.
-- | Herhaal een gegeven regel @n@ keer, gegeven een startstatus. Het resultaat is een reeks toestanden van start tot @n@.
iterateRule :: Rule          -- ^ The rule to apply
            -> Int           -- ^ How many times to apply the rule
            -> Automaton     -- ^ The initial state
            -> TimeSeries
iterateRule r 0 s = [s] -- wanneer alle iteraties zijn voltooid, retourneert u de eindautomaat
iterateRule r n s = s : iterateRule r (pred n) (fromList $ applyRule $ leftMost $ expand s) -- pas recursie toe met dezelfde regel, nog één iteratie minder te gaan, en verwerk deze automaat: (vouw de automaat uit, selecteer de eerste cel, pas de regel toe op de automaat)
  where applyRule :: Automaton -> Context -- de functie die de regel herhaalt over elke cel
        applyRule (FocusList [] bw) = [] -- wanneer het aan het einde van de automaat is, retourneer een lege lijst
        applyRule z = r (context z) : applyRule (goRight z) -- pas de regel toe op de context rond het geselecteerde en voeg dat toe aan de andere resultaten van rechts

-- De functie showPyramid zet een reeks van Automaton-states om in een String die kan worden afgedrukt.
-- | Converteer een tijdreeks van Automaton-statussen naar een afdrukbare tekenreeks.
showPyramid :: TimeSeries -> String
showPyramid zs = unlines $ zipWith showFocusList zs $ reverse [0..div (pred w) 2]
  where w = length $ toList $ last zs :: Int
        showFocusList :: Automaton -> Int -> String
        showFocusList z p = replicate p ' ' <> concatMap showCell (toList z)
        showCell :: Cell -> String
        showCell Dead  = "░"
        showCell Alive = "▓"


-- ..:: Sectie 4: Cellulaire automata voor alle mogelijke regels ::..

-- Er bestaan 256 regels, die we niet allemaal met de hand gaan uitprogrammeren op bovenstaande manier. 
-- Zoals op de voorgenoemde pagina te zien is, heeft het nummer te maken met binaire codering. 
-- De toestand van een cel hangt af van de toestand van 3 cellen in de vorige ronde: de cel zelf en diens beide buren (de context). 
-- Afhankelijk van het nummer dat een regel heeft, mapt iedere combinatie naar een levende of dode cel.

-- TODO: Definieer de constante `inputs` die alle 8 mogelijke contexts weergeeft: [Alive,Alive,Alive], [Alive,Alive,Dead], etc.
-- Je mag dit met de hand uitschrijven, maar voor meer punten kun je ook een lijst-comprehensie of andere slimme functie verzinnen.
-- | Alle acht binaire mogelijkheden voor 3 bits gegeven in Alive of Dead
inputs :: [Context]
inputs = [ [a,b,c] | a <- [Dead,Alive],
                    b <- [Dead,Alive],
                    c <- [Dead,Alive]]


-- Deze helperfunctie evalueert met de functie (p) de waarde (x); als dit True teruggeeft, is het resultaat Just x, anders Nothing. 
-- | Als het gegeven predikaat van toepassing is op de gegeven waarde, retourneert u Alleen de opgegeven waarde; in alle andere gevallen, retourneer Niets.
guard :: (a -> Bool) -> a -> Maybe a
guard p x | p x = Just x
          | otherwise = Nothing

-- TODO: Voorzie de functie `binary` van uitgebreid commentaar.
-- Leg in dit commentaar uit: 
-- 1) Wat de functie precies doet;
-- 2) Stap voor stap, hoe de functie dat doel bereikt.
-- Tips: - Zoek de definitie van `unfoldr` op met Hoogle. 
--       - `toEnum` converteert een Int naar een ander type, in dit geval 0 -> False en 1 -> True voor Bool. 
-- | Converteert een geheel getal naar de overeenkomstige binaire waarde in True en False. Waarden hoger dan 255 zullen overlopen, dus binair 256 geeft hetzelfde als binair 0
binary :: Int -> [Bool]
binary = map toEnum . reverse . take 8 . (++ repeat 0)
       . unfoldr (guard (/= (0,0)) . swap . flip divMod 2)

-- TODO: Schrijf en documenteer de functie mask, die gegeven een lijst Booleans en een lijst elementen alleen de elementen laat staan 
-- die (qua positie) overeenkomen met een True.
-- Je kunt hiervoor zipWith en Maybe gebruiken (check `catMaybes` in Data.Maybe) of de recursie met de hand uitvoeren.
-- | Filtert een lijst met elementen op de overeenkomstige booleans op dezelfde index. Het doet dit door elk element dat bij False to Nothing hoort te maken en dat vervolgens uit te filteren met catMaybes
mask :: [Bool] -> [a] -> [a]
mask bList eList = catMaybes $ zipWith (\b e -> if b then Just e else Nothing) bList eList

-- TODO: Schrijf en documenteer de functie rule, die elk getal kan omzetten naar de bijbehorende regel. 
-- De Int staat hierbij voor het nummer van de regel; de Context `input` is waarnaar je kijkt om te zien of het resultaat
-- met de gevraagde regel Dead or Alive is. 
-- Tips: - Denk eraan dat het type Rule een shorthand is voor een functie-type, dus dat je met 2 argumenten te maken hebt. 
--       - Definieer met `where` een subset van `inputs` die tot een levende danwel dode cel leiden.

-- | Retourneert een bruikbare regel op basis van het opgegeven regelnummer. Het maskeren van binaire n en invoer resulteert in een lijst met patronen die Alive moeten retourneren, dus dan is het alleen maar controleren of de gegeven invoer in die lijst staat, zo ja: Alive, zo niet: Dead
rule :: Int -> Rule
rule n input
  | elem input (mask (binary n) inputs) = Alive
  | otherwise = Dead

{- Je kunt je rule-functie in GHCi testen met variaties op het volgende commando:

   putStrLn . showPyramid . iterateRule (rule 18) 15 $ start

                  ▓
                 ▓░▓
                ▓░░░▓
               ▓░▓░▓░▓
              ▓░░░░░░░▓
             ▓░▓░░░░░▓░▓
            ▓░░░▓░░░▓░░░▓
           ▓░▓░▓░▓░▓░▓░▓░▓
          ▓░░░░░░░░░░░░░░░▓
         ▓░▓░░░░░░░░░░░░░▓░▓
        ▓░░░▓░░░░░░░░░░░▓░░░▓
       ▓░▓░▓░▓░░░░░░░░░▓░▓░▓░▓
      ▓░░░░░░░▓░░░░░░░▓░░░░░░░▓
     ▓░▓░░░░░▓░▓░░░░░▓░▓░░░░░▓░▓
    ▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓░░░▓
   ▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓░▓

   Als het goed is zal `stack run` nu ook werken met de optie (d) uit het menu; 
   experimenteer met verschillende parameters en zie of dit werkt.
-}
