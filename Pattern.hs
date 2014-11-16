module Pattern where
import Utilities


-------------------------------------------------------
-- Match and substitute
--------------------------------------------------------

-- Replaces a wildcard in a list with the list given as the third argument
substitute :: Eq a => a -> [a] -> [a] -> [a]
substitute _ [] _ = []
substitute p (x:xs) s
   | p==x = s ++ (substitute p xs s)
   | otherwise = [x] ++(substitute p xs s)


-- Tries to match two lists. If they match, the result consists of the sublist
-- bound to the wildcard in the pattern list.
match :: Eq a => a -> [a] -> [a] -> Maybe [a]
match _ [] [] = Just []
match _ [] _ = Nothing
match _ _ [] = Nothing
match p (x1:xs1) (x2:xs2)
   | x1==p = orElse (singleWildcardMatch (x1:xs1) (x2:xs2)) (longerWildcardMatch [x2] (x1:xs1) (xs2))
   | x1==x2 = match p xs1 xs2
   | otherwise = Nothing

-- Helper function to match
singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch [] [] = Just []
singleWildcardMatch _ [] = Nothing
singleWildcardMatch (wc:ps) (x:xs) 
   | match wc ps xs == Nothing = Nothing
   | otherwise = Just [x]

longerWildcardMatch :: Eq a => [a] -> [a] -> [a] -> Maybe [a]
longerWildcardMatch _ _ [] = Nothing
longerWildcardMatch (j:js) (wc:ps) (x:xs) 
   | singleWildcardMatch (wc:ps) (x:xs) == Nothing  = longerWildcardMatch (j:js ++ [x]) (wc:ps) xs
   | otherwise = Just (j:js ++ [x])




-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply _ _ [] _ = Just []
transformationApply _ _ _ ([],[]) = Nothing
transformationApply p i ta tr 
   | match p (fst tr) ta == Nothing = Nothing
   | otherwise = mmap (substitute p (snd tr)) (match p (fst tr) ta)

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply _ _ _ [] = Just []
transformationsApply _ _ [] _ = Nothing
transformationsApply p i (t:tr) ta
   | match p (fst t) ta == Nothing = transformationsApply p i tr ta
   | otherwise = mmap (substitute p (snd t)) (match p (fst t) ta)

--Test cases
frenchPresentation = ("My name is *", "Je m'appelle *")
transformationTest = transformationApply '*' id "My name is Zacharias" frenchPresentation
transformationCheck = transformationTest == Just "Je m'appelle Zacharias"

frenchPresentationList = [("Test1 *", "FrenchTest1 *"),("Test2 *", "FrenchTest2 *"),("My name is *", "Je m'appelle *")]
transformationListTest = transformationsApply '*' id frenchPresentationList "Test2 works"
transformationListCheck = transformationListTest == Just "FrenchTest2 works"