module World where

import HRAS
import P
import Lexicon

type Prop = String

data TemporalOperator = H | P | F | G deriving (Show, Eq)

data TProp = TProp { tempOp :: TemporalOperator,
                     prop ::  Prop } deriving (Show, Eq)

data World = World { propositions :: [Prop] } deriving (Show, Eq)

-- w1 = World { propositions = ["snowwhite smiles", "you love alice", "i give many swords to the wizard","i love alice","goldilocks sells dorothy a sword","a wizard sells a sword to himself","i shudder","he shudders","every wizard kicks every wizard"] }
-- w2 = World { propositions = ["i love alice","alice cheers","snowwhite smiles","every wizard kicks every wizard","a wizard sells a sword to himself","i shudder","he shudders","every wizard kicks every wizard","atreyu cheers","i cheer"] }
-- w3 = World { propositions = ["dorothy helps herself","i love alice","dorothy defeats the wizard", "i give many swords to the wizard","dorothy sells a sword to a wizard","every wizard kicks every wizard","atreyu cheers","i cheer", "i kick those wizards"] }
-- w4 = World { propositions = ["goldilocks gives many swords to every wizard", "dorothy sells a sword to a wizard","i love alice","i kick those wizards", "no person kicks atreyu","all persons kick the wizard","i defeat the wizard","i defeat the wizard"] }
-- w5 = World { propositions = ["littlemook kicks atreyu","you love alice", "i give no swords to the wizard", "i love alice","a wizard sells a sword to himself","i kick those wizards","no person kicks atreyu","all persons kick the wizard","no wizard loves a person"] }
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
model = [w1,w2,w3,w4,w5]
invalidProps =  [[p | p <- propositions x, (parses p) == []] | x <- model]
--Verb gives the set of forms for a single meaing
data Verb = Verb {forms :: [String]} deriving (Show, Eq)
smile   = Verb ["smiled", "will_smile", "smile","smiles","have_smiled","has_smiled"]
cheer   = Verb ["cheer","cheers","have_cheered","cheered","will_cheer","has_cheered"]
shudder = Verb ["shuddered","will_shudder","shudder","shudders","have_shuddered","has_shuddered"]
love    = Verb ["loved","will_love","love","have_loved","has_loved","loves"]
admire  = Verb ["admired","will_admire","admire","admires","has_admired","have-admired"]
help    = Verb ["helped","will_help","help","helps","have_helped","has_helped"]
defeat  = Verb ["defeated","will_defeat","defeat","defeats","has_defeated","have_defeated"]
give    = Verb ["gave","will_give","give","gives","has_given","have_given"]
sell    = Verb ["sold","will_sell","sell","sells","has_sold","have_sold"]
kick    = Verb ["kicked","will_kick","kick","kicks","have_kicked","has_kicked"]
takeV   = Verb ["took","will_take","take","takes","has_taken","have_taken"]
verbs = [smile,cheer,shudder,love,admire,help,defeat,give,sell,kick,takeV]


-- 3 layers of postioning: tense, temporal operator and function
-- Steps: 1. determine world(s) to start at   (function)
--        2. what worlds are gone to        (temporal operator)
--        3. when will it have to be true   (tense) --> past : present : future
findIndex :: World -> Int
findIndex w1 = [x | x <- [0..length model], w1 == (model !! x)] !! 0 -- finds index of a given world

getWorlds :: TemporalOperator -> World -> [World] --gets either future or past worlds
getWorlds t w
  | t == H || t == P =  (take ((findIndex w)) model)
  | t == F || t == G =  (drop ((findIndex w)) model)

getTense :: Prop -> Feat
getTense p = head (last [fs c | c <- cats, phon c /= "_"])
  where
    parse = head (parses p)
    cats = [t2c t | t <-subtrees parse, catLabel (t2c t) == "VP"]

compareV :: String -> String -> Bool --checks if two verbs are different forms of the same meaning
compareV v1 v2 = any (\v-> v1 `elem`forms v && v2 `elem` forms v) verbs

--checks if two phrases have the same meaning at different times
comparePM :: String -> String -> Bool
comparePM p1 p2 = (fc p1== fc p2) && (compareV (vc p1) (vc p2))
  where
    fc = (\p -> [t2c t | t <-subtrees (head (parses p)), catLabel (t2c t) /= "VP"])
    vc = (\p -> [phon (t2c t) | t <-subtrees (parses p !! 0), catLabel (t2c t) == "VP" && phon (t2c t) /= "_"] !! 0)

fWorldsT :: Feat -> World -> [World] --finds what worlds the present form must be true in
fWorldsT te w
  | te == Fut = drop (wIndex + 1) model
  | te == Pres = [w]
  | te == Past || te == Perf = take wIndex model
  where
    wIndex = findIndex w



checkValidW :: String -> World -> Bool
checkValidW pro w = any (comparePM pro) (propositions w) --checks a world

checkTenseWorlds :: String -> Feat -> World -> Bool
checkTenseWorlds pro te w = any (checkValidW pro) (fWorldsT te w)


-- NOTE The algorithm is not super efficient and some of the helper methods
-- likely could be refactored

isSatisfied :: TProp -> World -> Bool
isSatisfied tprop w
  | to == H || to == G = all (checkTenseWorlds pro tense) timeworlds
  | to == P || to == F = any (checkTenseWorlds pro tense) timeworlds
  where
    to = tempOp tprop
    pro = prop tprop
    timeworlds = getWorlds to w
    tense = getTense pro


isSatisfiable :: TProp -> Bool
isSatisfiable tprop = any (isSatisfied tprop) model


isValid :: TProp -> Bool
isValid tprop = all (isSatisfied tprop) model

--The different function just change the set of worlds that step 1 is applied over
--satifiable just checks if any world is the TProp satisfied for that world
--isValid checks for all worlds 
