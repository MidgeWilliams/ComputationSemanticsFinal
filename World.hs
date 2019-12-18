module World where

import HRAS
import P
import Lexicon

type Prop = String

data PathOperator =  E | A | N deriving (Show, Eq)

data BranchOperator = X | W | F | G deriving (Show, Eq)
-- Tree Structure
data WorldNode = WorldNode { worldD :: World, branches :: [WorldNode]} deriving (Show, Eq)

data CTLProp = CTLProp { pathOp :: PathOperator, branchOp :: BranchOperator,
              prop ::  [Prop] } deriving (Show, Eq)

data World = World { propositions :: [Prop] } deriving (Show, Eq)

w1 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves daggers",
 "i love alice",
 "the dwarf sells the dagger to a boy",
 "i take several things",
 "many princesses admire the wizards",
 "Goldilocks helps herself",
 "the women love the men",
 "the dwarves laugh",
 "the boys admire me"
]}

w2 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves daggers",
 "i love alice",
 "the dwarf sells the dagger to a boy",
 "you love giants",
 "many girls admire these women",
 "atreyu smiles",
 "the women love the men",
 "the dwarves laugh",
 "the boys admire me"
]}

w3 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves daggers",
 "i love alice",
 "the dwarf sells the dagger to a boy",
 "i take several things",
 "many girls admire these women",
 "Goldilocks helps herself",
 "the women love the men",
 "the dwarves laugh",
 "the boys admire me"
]}

w4 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves daggers",
 "the wizard laughs",
 "i love alice",
 "some boys kick most dwarves",
 "you love giants",
 "atreyu smiles",
 "the women love the men",
 "the girls love the dwarves",
 "he kicks himself"
]}

w5 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the wizard laughs",
 "The men cheer",
 "the dwarf sells the dagger to a boy",
 "many princesses admire the wizards",
 "many girls admire these women",
 "Goldilocks helps herself",
 "the women love the men",
 "the girls love the dwarves",
 "he kicks himself"
]}

w6 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the giants help the princess",
 "the wizard laughs",
 "some boys kick most dwarves",
 "i take several things",
 "many girls admire these women",
 "Goldilocks helps herself",
 "the women love the men",
 "the girls love the dwarves",
 "he kicks himself"
]}

w7 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves daggers",
 "The men cheer",
 "alice loves the boy",
 "many girls admire these women",
 "the dagger shudders",
 "atreyu smiles",
 "the boys love the girls",
 "the girls love the dwarves",
 "the boys admire me"
]}

w8 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "Alice defeats the princesses",
 "some boys kick most dwarves",
 "many girls admire these women",
 "the dagger shudders",
 "the girls sell the boys to the dwarves",
 "Goldilocks helps herself",
 "the boys love the girls",
 "the girls love the dwarves",
 "the boys admire me"
]}

w9 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the wizard laughs",
 "Alice defeats the princesses",
 "i take several things",
 "many girls admire these women",
 "the dagger shudders",
 "atreyu smiles",
 "the boys love the girls",
 "the girls love the dwarves",
 "the boys admire me"
]}

w10 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "Alice defeats the princesses",
 "many princesses admire the wizards",
 "many girls admire these women",
 "the dagger shudders",
 "the girls sell the boys to the dwarves",
 "Goldilocks helps herself",
 "the boys love the girls",
 "the dwarves laugh",
 "he kicks himself"
]}

w11 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves daggers",
 "Alice defeats the princesses",
 "she cheers",
 "many princesses admire the wizards",
 "many girls admire these women",
 "atreyu smiles",
 "the boys love the girls",
 "the dwarves laugh",
 "he kicks himself"
]}

w12 = World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the wizard laughs",
 "Alice defeats the princesses",
 "she cheers",
 "many girls admire these women",
 "the dagger shudders",
 "Goldilocks helps herself",
 "the boys love the girls",
 "the dwarves laugh",
 "he kicks himself"
]}

model = [(w1, w2), (w2, w3), (w3, w4), (w3, w5), (w3, w6), (w4, w7), (w5, w8), (w6, w9), (w6, w10), (w7, w11), (w7, w12)]

mn12 = WorldNode{worldD=w12,branches = []}
mn11 = WorldNode{worldD=w11,branches = []}
mn10 = WorldNode{worldD=w10,branches = []}
mn9 = WorldNode{worldD=w9,branches = []}
mn8 = WorldNode{worldD=w8,branches = []}
mn7 = WorldNode{worldD=w7,branches = [mn11,mn12]}
mn6 = WorldNode{worldD=w6,branches = [mn9,mn10]}
mn5 = WorldNode{worldD=w5,branches = [mn8]}
mn4 = WorldNode{worldD=w4,branches = [mn7]}
mn3 = WorldNode{worldD=w7,branches = [mn4,mn5,mn6]}
mn2 = WorldNode{worldD=w2,branches = [mn3]}
mn1 = WorldNode{worldD=w1,branches = [mn2]}

-- model = [(w1,[w2]), (w2,[w3]), (w3, [w4,w5,w6]), (w4,[w7]), (w5, w8), (w6, [w9, w10]) ]
-- TODO: imporve complexity of form matching
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
--

-- checks over all paths
testWorlds w ctl
  | patho == E = (any (checkWorld ctl) (branches w))
  | patho == A = (all (checkWorld ctl) (branches w))
  | otherwise = (any (checkWorld ctl) (branches w))
  where
    patho = (pathOp ctl)


checkWorld ctl world
  | brancho == G = (checkValidW (head (prop ctl)) (worldD world)) && all (checkWorld ctl) (branches world)
  | brancho == F = final_w world ctl (branches world)
  | otherwise = undefined
  where
    brancho = branchOp ctl

final_w world ctl [] = (head (prop ctl)) `elem` (propositions (worldD world))
final_w _ ctl xs = any (checkWorld ctl) xs



-- getTense :: Prop -> Feat
-- getTense p = head (last [fs c | c <- cats, phon c /= "_"])
--   where
--     parse = head (parses p)
--     cats = [t2c t | t <-subtrees parse, catLabel (t2c t) == "VP"]
--
-- TODO:improve complexity of finding tense
compareV :: String -> String -> Bool --checks if two verbs are different forms of the same meaning
compareV v1 v2 = any (\v-> v1 `elem`forms v && v2 `elem` forms v) verbs

-- checks if two phrases have the same meaning at different times
comparePM :: String -> String -> Bool
-- comparePM s1 s2 = s1 == s2
-- comparePM s1 s2 = True
comparePM p1 p2 = (fc p1== fc p2) && (compareV (vc p1) (vc p2))
  where
    fc = (\p -> [t2c t | t <-subtrees (head (parses p)), catLabel (t2c t) /= "VP"])
    vc = (\p -> [phon (t2c t) | t <-subtrees (parses p !! 0), catLabel (t2c t) == "VP" && phon (t2c t) /= "_"] !! 0)
--
-- fWorldsT :: Feat -> World -> [World] --finds what worlds the present form must be true in
-- fWorldsT te w = [w]
--
--
--
checkValidW :: String -> World -> Bool
checkValidW pro w = any (comparePM pro) (propositions w) --checks a world
--
-- checkTenseWorlds :: String -> Feat -> World -> Bool
-- checkTenseWorlds pro te w = any (checkValidW pro) (fWorldsT te w)


-- NOTE The algorithm is not super efficient and some of the helper methods
-- likely could be refactored

isSatisfied :: CTLProp -> WorldNode -> Bool
isSatisfied ctlProp w = testWorlds w ctlProp


isSatisfiable :: CTLProp -> Bool
isSatisfiable ctlProp = undefined


isValid :: CTLProp -> Bool
isValid ctlProp = undefined

--The different function just change the set of worlds that step 1 is applied over
--satifiable just checks if any world is the CTLProp satisfied for that world
--isValid checks for all worlds 
