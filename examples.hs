module Examples where

import HRAS
import P
import Lexicon
import World hiding (w1, w2, w3, w4, w5)

-- copy and paste these worlds into World.hs
w1 = World { propositions = [
 "goldilocks admires herself",
 "alice kicks every dwarf with every sword",
 "you defeat every princess",
 "some dagger gives alice every wizard",
 "every sword gives some giant dorothy",
 "they kick themselves with every dagger",
 "every dwarf gives some sword alice",
 "alice sells every wizard littlemook",
 "alice takes some princess from herself",
 "every giant gives alice you"
 ] }
w2 = World { propositions = [
 "snowwhite sells every dagger itself",
 "alice kicks every dwarf with every sword",
 "some dagger gives atreyu some princess",
 "every sword kicks some giant with you",
 "alice kicks every princess with every wizard",
 "some dagger gives alice every wizard",
 "some dwarf sells dorothy goldilocks",
 "goldilocks kicks alice with some dagger",
 "every wizard gives snowwhite herself",
 "alice sells every wizard littlemook"
 ] }
w3 = World { propositions = [
 "some sword gives some giant to some wizard",
 "atreyu loves me",
 "snowwhite sells every dagger itself",
 "every princess gives alice herself",
 "every dwarf gives some sword alice",
 "some dwarf sells dorothy goldilocks",
 "alice takes some princess from herself",
 "you defeat every princess",
 "they kick themselves with every dagger",
 "some dagger gives alice every wizard"
 ] }
w4 = World { propositions = [
 "some dagger gives atreyu some princess",
 "atreyu defeats every giant",
 "every wizard gives snowwhite herself",
 "every dagger kicks every princess with some sword",
 "every giant gives alice you",
 "every sword kicks some giant with you",
 "atreyu defeats littlemook",
 "some dwarf admires some wizard",
 "alice kicks every dwarf with every sword",
 "goldilocks kicks alice with some dagger"
 ] }
w5 = World { propositions = [
 "you defeat every princess",
 "some dagger sells some giant me",
 "alice kicks every dwarf with every sword",
 "every sword kicks some giant with you",
 "atreyu defeats every giant",
 "alice takes some princess from herself",
 "snowwhite gives goldilocks to every dwarf",
 "some dagger gives atreyu some princess",
 "they kick themselves with every dagger",
 "goldilocks kicks alice with some dagger"
 ] }

-- examples
ex1 = isValid (TProp P "alice has_kicked every princess with every wizard")
ex2 = isSatisfied (TProp H "every sword kicks some giant with you") w1
ex3 = isSatisfiable (TProp F "every giant has_given alice you")
ex4 = isValid (TProp F "some dagger gave alice every wizard")
ex5 = isSatisfied (TProp H "some dagger will_give atreyu some princess") w4
ex6 = isSatisfiable (TProp H "some dwarf admires some wizard")
ex7 = isValid (TProp F "snowwhite sells every dagger itself")
ex8 = isSatisfied (TProp F "alice has_kicked every dwarf with every sword") w5
ex9 = isSatisfiable (TProp P "every dagger has_given me snowwhite")
ex10 = isSatisfied (TProp P "some dagger sold some giant me") w2
ex11 = isSatisfiable (TProp P "snowwhite will_give goldilocks to every dwarf")
ex12 = isSatisfied (TProp F "every sword has_given some giant dorothy") w3
ex13 = isSatisfied (TProp H "alice has_sold every wizard littlemook") w3
ex14 = isValid (TProp P "atreyu will_love me")
ex15 = isSatisfied (TProp H "dorothy loves littlemook") w2
ex16 = isSatisfied (TProp F "some dwarf has_sold dorothy goldilocks") w2
ex17 = isValid (TProp G "you have_defeated every princess")
ex18 = isSatisfied (TProp H "every dwarf will_give you atreyu") w3
ex19 = isSatisfiable (TProp P "some wizard took me from them")
ex20 = isSatisfied (TProp P "goldilocks has_admired herself") w2
ex21 = isSatisfied (TProp G "goldilocks has_kicked alice with some dagger") w3
ex22 = isSatisfied (TProp H "atreyu will_defeat every giant") w4
ex23 = isSatisfied (TProp G "some sword gives some giant to some wizard") w2
ex24 = isSatisfied (TProp P "every dwarf will_give some sword alice") w4
ex25 = isSatisfied (TProp G "every dagger kicks every princess with some sword") w3
ex26 = isSatisfied (TProp H "every princess has_given alice herself") w4
ex27 = isSatisfied (TProp F "atreyu defeated littlemook") w4
ex28 = isSatisfied (TProp F "they kick themselves with every dagger") w4
ex29 = isSatisfied (TProp P "every wizard gave snowwhite herself") w3
ex30 = isSatisfied (TProp F "alice will_take some princess from herself") w1

exs = [ex1,  ex2,  ex3,  ex4,  ex5,  ex6,  ex7,  ex8,  ex9,  ex10,
       ex11, ex12, ex13, ex14, ex15, ex16, ex17, ex18, ex19, ex20,
       ex21, ex22, ex23, ex24, ex25, ex26, ex27, ex28, ex29, ex30]

zipped = zip [1..30] exs
answers = [(1,False),  (2,True),   (3,True),   (4,True),   (5,True),
           (6,True),   (7,False),  (8,True),   (9,False),  (10,False),
           (11,True),  (12,True),  (13,False), (14,False), (15,False),
           (16,True),  (17,False), (18,False), (19,False), (20,False),
           (21,True),  (22,True),  (23,False), (24,True),  (25,False),
           (26,False), (27,True),  (28,True),  (29,False), (30,True)]
-- score out of 30
score = length . filter (==True) $ zipWith (==) zipped answers
-- examples the program missed
wrong = map fst $ filter (\(x,y) -> y == False) . zip [1..30] $ zipWith (==) zipped answers
