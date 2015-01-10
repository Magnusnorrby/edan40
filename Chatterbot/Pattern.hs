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
   | x1==p = orElse (singleWildcardMatch (x1:xs1) (x2:xs2)) $ longerWildcardMatch (x1:xs1) (x2:xs2)
   | x1==x2 = match p xs1 xs2
   | otherwise = Nothing

-- Helper function to match
singleWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
singleWildcardMatch (wc:ps) (x:xs) = match wc ps xs >> return [x]


longerWildcardMatch :: Eq a => [a] -> [a] -> Maybe [a]
longerWildcardMatch (wc:ps) (x:xs) = mmap (x:) $ match wc (wc:ps) xs




-- Test cases --------------------

testPattern =  "a=*;"
testSubstitutions = "32"
testString = "a=32;"

substituteTest = substitute '*' testPattern testSubstitutions
substituteCheck = substituteTest == testString

matchTest = match '*' testPattern testString
matchCheck = matchTest == Just testSubstitutions

matchTest2 = match '*' testPattern "a=3;"



-------------------------------------------------------
-- Applying patterns
--------------------------------------------------------

-- Applying a single pattern
transformationApply :: Eq a => a -> ([a] -> [a]) -> [a] -> ([a], [a]) -> Maybe [a]
transformationApply p f ta (tr1, tr2) = mmap ((substitute p tr2) . f) $ match p tr1 ta

-- Applying a list of patterns until one succeeds
transformationsApply :: Eq a => a -> ([a] -> [a]) -> [([a], [a])] -> [a] -> Maybe [a]
transformationsApply p f tr ta = foldr1 orElse $ map (transformationApply p f ta) tr


--Test cases
frenchPresentation = ("My name is *", "Je m'appelle *")
transformationTest = transformationApply '*' id "My name is Zacharias" frenchPresentation
transformationCheck = transformationTest == Just "Je m'appelle Zacharias"

frenchPresentationList = [("Test1 *", "FrenchTest1 *"),("Test2 *", "FrenchTest2 *"),("My name is *", "Je m'appelle *")]
transformationListTest = transformationsApply '*' id frenchPresentationList "Test2 works"
transformationListCheck = transformationListTest == Just "FrenchTest2 works"