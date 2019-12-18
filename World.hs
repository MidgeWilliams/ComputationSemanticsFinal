module World where

import HRAS
import P
import Lexicon

type Prop = String

data PathOperator =  E | A | N deriving (Show, Eq)

data BranchOperator = X | W | F | G deriving (Show, Eq)
-- Tree Structure
data WorldNode = WorldNode { worldD :: World, branches :: [WorldNode], parent :: Maybe WorldNode} deriving (Show, Eq)

data CTLProp = CTLProp { pathOp :: PathOperator, branchOp :: BranchOperator,
              prop ::  [Prop] } deriving (Show, Eq)

data World = World { propositions :: [Prop] } deriving (Show, Eq)

w1 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves the daggers",
 "i love alice",
 "the dwarf sells the dagger to a boy",
 "i take several things",
 "many princesses admire the wizards",
 "Goldilocks helps herself",
 "the women love the men",
 "the dwarves laugh",
 "the boys admire me",
 "alice defeats alice"
]}

w2 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves the daggers",
 "i love alice",
 "the dwarf sells the dagger to a boy",
 "you love the giants",
 "many girls admire these women",
 "atreyu smiles",
 "the women love the men",
 "the dwarves laugh",
 "the boys admire me"
]}

w3 =  World { propositions = [
 "some dwarf sells dorothy goldilocks",
 "the girl loves the daggers",
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
 "the girl loves the daggers",
 "the wizard laughs",
 "i love alice",
 "some boys kick most dwarves",
 "you love the giants",
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
 "the girl loves the daggers",
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
 "Alice defeats Alice",
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
 "the girl loves the daggers",
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

-- model = [(w1, w2), (w2, w3), (w3, w4), (w3, w5), (w3, w6), (w4, w7), (w5, w8), (w6, w9), (w6, w10), (w7, w11), (w7, w12)]

mn12 = WorldNode{worldD=w12,branches = [], parent = Just mn7}
mn11 = WorldNode{worldD=w11,branches = [], parent = Just mn7}
mn10 = WorldNode{worldD=w10,branches = [], parent = Just mn6}
mn9 = WorldNode{worldD=w9,branches = [], parent = Just mn6}
mn8 = WorldNode{worldD=w8,branches = [], parent = Just mn5}
mn7 = WorldNode{worldD=w7,branches = [mn11,mn12], parent = Just mn4}
mn6 = WorldNode{worldD=w6,branches = [mn9,mn10], parent = Just mn3}
mn5 = WorldNode{worldD=w5,branches = [mn8], parent = Just mn3}
mn4 = WorldNode{worldD=w4,branches = [mn7], parent = Just mn3}
mn3 = WorldNode{worldD=w7,branches = [mn4,mn5,mn6], parent = Just mn2}
mn2 = WorldNode{worldD=w2,branches = [mn3], parent = Just mn1}
mn1 = WorldNode{worldD=w1,branches = [mn2], parent = Nothing}

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
laugh   = Verb ["laugh","laughs","laughed","will_laugh","has_laughed","have_laughed"]
verbs = [smile,cheer,shudder,love,admire,help,defeat,give,sell,kick,takeV,laugh]
model = [w1,w2,w3,w4,w5,w6,w7,w8,w9,w10,w11,w12]
invalidProps =  [[p | p <- propositions x, (parses p) == []] | x <- model]

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

-- NOTE:to affect how tenses are handled modify checkValidW
checkWorld ctl world
  | brancho == G = (checkTenseValidW (head (prop ctl)) world) && all (checkWorld ctl) (branches world)
  | brancho == F = final_w world ctl (branches world)
  | brancho == X = (checkValidW (head (prop ctl))) (worldD world)
  | brancho == W = ((eval_p (head (prop ctl))) && any (checkWorld ctl) (branches world)) || (eval_p (last (prop ctl)))
  | otherwise = undefined
  where
    brancho = branchOp ctl
    eval_p  = (\p -> (checkValidW (p) (worldD world)))

final_w world ctl [] = (head (prop ctl)) `elem` (propositions (worldD world))
final_w _ ctl xs = any (checkWorld ctl) xs



getTense :: Prop -> Feat
getTense p = head (last [fs c | c <- cats, phon c /= "_"])
  where
    parse = head (parses p)
    cats = [t2c t | t <-subtrees parse, catLabel (t2c t) == "VP"]
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

-- fWorldsT :: Prop -> WorldNode -> [World] --finds what worlds the present form must be true in
fWorldsT pro w
  | te == Fut = undefined
  | te == Pres = [worldD w]
  | te == Past || te == Perf = getPast (parent w)
  where
    te = getTense pro

getPast Nothing = []
getPast (Just par) = [worldD par] ++ (getPast (parent par))

checkValidW :: String -> World -> Bool
checkValidW pro w = any (comparePM pro) (propositions w) --checks a world
--
checkTenseValidW pro wn = any (checkValidW pro) (fWorldsT pro wn)

isSatisfied :: CTLProp -> WorldNode -> Bool
isSatisfied ctlProp w = testWorlds w ctlProp


isSatisfiable :: CTLProp -> Bool
isSatisfiable ctlProp = undefined


isValid :: CTLProp -> Bool
isValid ctlProp = undefined

--The different function just change the set of worlds that step 1 is applied over
--satifiable just checks if any world is the CTLProp satisfied for that world
--isValid checks for all worlds 
e1 = isSatisfied (CTLProp E X ["the giants help the princess"]) mn3
e2 = isSatisfied (CTLProp A X ["the wizard laughs"]) mn3
e3 = isSatisfied (CTLProp E X ["the wizard laughs"]) mn5
e4 = isSatisfied (CTLProp A X ["the giants help the princess"]) mn3

e5 = isSatisfied (CTLProp E F ["she cheers"]) mn2
e6 = isSatisfied (CTLProp E F ["the men cheer"]) mn5
e7 = isSatisfied (CTLProp E W ["i love alice","alice loves the boy"]) mn3
e8 = isSatisfied (CTLProp A G ["alice defeated alice"]) mn10
