scoreMatch = 0
scoreMismatch = -1
scoreSpace = -1

type AlignmentType = (String,String)


attachHeads :: a -> a -> [([a],[a])] -> [([a],[a])]
attachHeads h1 h2 aList = [(h1:xs,h2:ys) | (xs,ys) <- aList]

attachTails :: a -> a -> [([a],[a])] -> [([a],[a])]
attachTails h1 h2 aList = [(xs++[h1],ys++[h2]) | (xs,ys) <- aList]

similarityScore :: (String,String) -> Int
similarityScore (s1,s2) = lookUp (length s1) (length s2)
         where 
	   lookUp i j = table!!i!!j
	   table = [[entry i j | j<-[0..]] | i<-[0..]]

	   entry :: Int -> Int -> Int
	   entry 0 0 = 0
	   entry i 0 = i * scoreSpace
	   entry 0 j = j * scoreSpace
	   entry i j = maximum [evalScore x y  + lookUp (i-1) (j-1), 
		  	   	evalScore x '_'  + lookUp   i   (j-1), 
			        evalScore '_' y  + lookUp (i-1)   j  ]
 	     where 
	       x = s1!!(i-1)
	       y = s2!!(j-1)

optAlignments :: String -> String -> [AlignmentType]
optAlignments s1 s2 = snd (lookUp (length s1) (length s2))
	 where 
   	   lookUp i j = table!!i!!j
	   table = [[entry i j | j<-[0..]] | i<-[0..]]

	   entry :: Int -> Int -> (Int, [AlignmentType])
	   entry 0 0 = (0, [([],[])])
	   entry i 0 = (ss + scoreSpace , attachTails   x  '-'  str)
             where 
	       (ss,str) = lookUp (i-1)  0
	       x        = s1!!(i-1)

	   entry 0 j = (ss + scoreSpace , attachTails  '-'  y  str)
	     where 
	       (ss,str) = lookUp   0  (j-1) 
	       y        = s2!!(j-1)

	   entry i j = (max ,concatMap snd optAlign)
	     where 
	       x = s1!!(i-1)
	       y = s2!!(j-1)
	       ss1 = (fst (lookUp (i-1) (j-1)) + (evalScore x y), 
	       	     attachTails   x  y  (snd (lookUp (i-1) (j-1) ))) 

	       ss2 = (fst (lookUp   i   (j-1)) +   scoreSpace, 
	       	     attachTails  '-' y  (snd (lookUp   i   (j-1) ))) 

	       ss3 = (fst (lookUp (i-1)   j  ) +   scoreSpace, 
	       	     attachTails   x '-' (snd (lookUp  (i-1)  j   )))

	       list = [ss1,ss2,ss3]
	       optAlign = maximaBy fst list
	       max = maximum $ map fst list

evalScore :: Char -> Char -> Int
evalScore c1 c2  
   | c1 == '_' = scoreSpace
   | c2 == '_' = scoreSpace
   | c1 == c2  = scoreMatch
   | otherwise = scoreMismatch

maximaBy :: Ord b => (a -> b) -> [a] -> [a]
maximaBy valueFcn xs = getMax valueFcn xs max
	 where max = maximum $ map valueFcn xs 


getMax :: Eq b => (a -> b) -> [a] -> b -> [a]
getMax _   []   _ = []
getMax f (x:xs) m 
   | f x == m = [x] ++ getMax f xs m
   | otherwise = getMax f xs m

 

optAlignmentsOld :: String -> String -> [AlignmentType]
optAlignmentsOld string1 string2 = maximaBy similarityScore $generateAlignment string1 string2


generateAlignment :: String -> String  -> [AlignmentType]
generateAlignment   []     []   = [([],[])]
generateAlignment (x:xs)   []   = attachHeads  x '-' $generateAlignment   xs    []  
generateAlignment   []   (y:ys) = attachHeads '-' y  $generateAlignment   []    ys 
generateAlignment (x:xs) (y:ys) = attachHeads  x  y  (generateAlignment   xs    ys   ) 
		  	      ++ (attachHeads '-' y  (generateAlignment (x:xs)  ys   )) 
			      ++ (attachHeads  x '-' (generateAlignment   xs  (y:ys) ))



outputOptAlignments :: String -> String -> IO ()
outputOptAlignments string1 string2 = do
   putStrLn ("The " ++ show (length alignments) ++ " optimal alignments are:")
   printAll alignments
		    where alignments = optAlignments string1 string2		         

printAll :: [AlignmentType] -> IO ()
printAll        []    = putStr "\n"
printAll ((s1,s2):ss) = do 
   putStr "\n"
   putStrLn s1
   putStrLn s2
   putStr "\n"
   printAll ss 




