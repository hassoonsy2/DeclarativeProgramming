{-|
    Module      : Lib
    Description : Checkpoint voor V2DEP: recursie en lijsten
    Copyright   : (c) Brian van de Bijl, 2020
    License     : BSD3
    Maintainer  : nick.roumimper@hu.nl

    In dit practicum oefenen we met het schrijven van simpele functies in Haskell.
    Specifiek leren we hoe je recursie en pattern matching kunt gebruiken om een functie op te bouwen.
    LET OP: Hoewel al deze functies makkelijker kunnen worden geschreven met hogere-orde functies,
    is het hier nog niet de bedoeling om die te gebruiken.
    Hogere-orde functies behandelen we verderop in het vak; voor alle volgende practica mag je deze
    wel gebruiken.
    In het onderstaande commentaar betekent het symbool ~> "geeft resultaat terug";
    bijvoorbeeld, 3 + 2 ~> 5 betekent "het uitvoeren van 3 + 2 geeft het resultaat 5 terug".
-}

module Lib
    ( ex1, ex2, ex3, ex4, ex5, ex6, ex7
    ) where

-- TODO: Schrijf en documenteer de functie ex1, die de som van een lijst getallen berekent.
-- Voorbeeld: ex1 [3,1,4,1,5] ~> 14
ex1 :: [Int] -> Int
ex1(x:xs) = x + ex1 xs
ex1 [] = 0 

-- TODO: Schrijf en documenteer de functie ex2, die alle elementen van een lijst met 1 ophoogt.
-- Voorbeeld: ex2 [3,1,4,1,5] ~> [4,2,5,2,6]
ex2 :: [Int] -> [Int]
ex2 [z] = [z + 1 ] 
ex2(x:xs) = ex2 [x] !! 0 : ex2 xs


-- TODO: Schrijf en documenteer de functie ex3, die alle elementen van een lijst met -1 vermenigvuldigt.
-- Voorbeeld: ex3 [3,1,4,1,5] ~> [-3,-1,-4,-1,-5]
ex3 :: [Int] -> [Int]
ex3 [z] = [-z] 
ex3 (x:xs) = ex3 [x] !! 0 : ex3 xs 

-- TODO: Schrijf en documenteer de functie ex4, die twee lijsten aan elkaar plakt.
-- Voorbeeld: ex4 [3,1,4] [1,5] ~> [3,1,4,1,5]
-- Maak hierbij geen gebruik van de standaard-functies, maar los het probleem zelf met (expliciete) recursie op. 
-- Hint: je hoeft maar door een van beide lijsten heen te lopen met recursie.
ex4 :: [Int] -> [Int] -> [Int]
ex4 [] z = z 
ex4 (head : tail) z = head : ex4 tail z

-- TODO: Schrijf en documenteer een functie, ex5, die twee lijsten van gelijke lengte paarsgewijs bij elkaar optelt.
-- Voorbeeld: ex5 [3,1,4] [1,5,9] ~> [4,6,13]
ex5 :: [Int] -> [Int] -> [Int]
ex5 z [] = z
ex5 (head_first : tail_first) (head_second : tail_second) = [head_first + head_second] ++ ex5 tail_first tail_second 

-- TODO: Schrijf en documenteer een functie, ex6, die twee lijsten van gelijke lengte paarsgewijs met elkaar vermenigvuldigt.
-- Voorbeeld: ex6 [3,1,4] [1,5,9] ~> [3,5,36] 
ex6 :: [Int] -> [Int] -> [Int]
ex6 z [] = z
ex6 (head_first : tail_first) (head_second : tail_second) = [head_first * head_second] ++ ex6 tail_first tail_second

-- TODO: Schrijf en documenteer een functie, ex7, die de functies ex1 en ex6 combineert tot een functie die het inwendig product uitrekent.
-- Voorbeeld: ex7 [3,1,4] [1,5,9] geeft 3*1 + 1*5 + 4*9 = 44 terug als resultaat.
ex7 :: [Int] -> [Int] -> Int
ex7 first second = ex1 (ex6 first second)
