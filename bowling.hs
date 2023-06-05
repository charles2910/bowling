-- Program to calculate bowling match score
--
--
-- Ideia roda o vetor de entrada fazendo take 2 e avaliando se é normal, spare ou strike aí coloca na lista de frames e retira da entrada
--
-- Bowl = switch 
-- 	  | lenght frame == 9
-- 	  	cur = take 3 plays
-- 	  	frame = frame:[(cur !! 0, cur !! 1, cur !! 2)]
--        | head take 2 plays == 10
--        	frame = frame:[(10, 0, 0)]
--        	pop 2 plays
--        | else
--        	cur = take 2 plays
--        	frame = frame:[(cur !! 0, cur !! 1, 0)]
--
main = do
        -- putStrLn "Enter the plays:"
        plays_string <- getLine
        let plays = take 21 (words plays_string)
        let plays_int = [read x :: Integer | x <- plays]
        -- putStrLn (show plays)
        -- putStrLn (show plays_int)
        let frames = checkEndFrame (loadFrames plays_int)
        -- putStrLn (show frames)
        putStrLn (printFrames frames ++ show (calcPoints frames))

data Play = Normal | Strike | Spare
        deriving (Show, Eq)

data Frame = Frame {
    plays :: (Integer, Integer),
    playType :: Play,
    bonus :: Integer,
    bonusPlay :: Bool,
    bonusPlayScore :: Integer
} deriving Show

loadFrames :: [Integer] -> [Frame]
loadFrames [] = []
loadFrames (h:t)
        | h == 10 = Frame { plays = (10, 0), playType = Strike, bonus = 1, bonusPlay = False, bonusPlayScore = 0 } : loadFrames t
        | h + (head t) == 10 && length (tail t) == 1 = Frame { plays = (h, head t), playType = Spare, bonus = 1, bonusPlay = True, bonusPlayScore = head (tail t) } : []
        | h + (head t) == 10 = Frame { plays = (h, head t), playType = Spare, bonus = 1, bonusPlay = False, bonusPlayScore = 0 } : loadFrames (tail t)
        | otherwise = Frame { plays = (h, head t), playType = Normal, bonus = 0, bonusPlay = False, bonusPlayScore = 0 } : loadFrames (tail t)

checkEndFrame :: [Frame] -> [Frame]
checkEndFrame frames
        | length frames == 12 = (take 9 frames) ++ [Frame { plays = (fst (plays (frames !! 9)), fst (plays (frames !! 10))), playType = playType (frames !! 9), bonus = 1, bonusPlay = True, bonusPlayScore = fst (plays (frames !! 11)) }]
        | length frames == 11 = (take 9 frames) ++ [Frame { plays = (fst (plays (frames !! 9)), fst (plays (frames !! 10))), playType = playType (frames !! 9), bonus = 1, bonusPlay = True, bonusPlayScore = snd (plays (frames !! 10)) }]
        | otherwise = frames

printFrames :: [Frame] -> [Char]
printFrames [] = []
printFrames (h:t)
        | bonusPlay h == True  = printPlay (plays h) ++ " " ++ printBonusPlay (fst $ plays h, snd $ plays h, bonusPlayScore h) ++ " | " ++ printFrames t
        | bonusPlay h == False = printPlay (plays h)  ++ " | " ++ printFrames t

printPlay :: (Integer, Integer) -> [Char]
printPlay (p1, p2)
        | p1 == 10 && p2 == 0 = "X _"
        | p1 + p2 == 10 = show p1 ++ " /"
        | p1 + p2 == 20 = "X X"
        | p1 + p2 > 10 = "X " ++ show p2
        | otherwise = show p1 ++ " " ++ show p2

printBonusPlay :: (Integer, Integer, Integer) -> [Char]
printBonusPlay (p1, p2, bonus) 
        | p2 == 10 && bonus == 10 = "X"
        | p1 == 10 && p2 + bonus == 10 = "/"
        | otherwise = show bonus

calcPoints :: [Frame] -> Integer
calcPoints [] = 0
calcPoints (h:t)
        | playType h == Normal = sumPlayScore (plays h) + bonusPlayScore h + calcPoints t
        | playType h == Spare && bonusPlay h == False = sumPlayScore (plays h)  + fst (plays (head t)) +  calcPoints t
        | playType h == Spare && bonusPlay h == True = sumPlayScore (plays h)  + bonusPlayScore h +  calcPoints t
        | playType h == Strike = 10 + sumPlayScore (getNextTwoScores (h:t)) + calcPoints t

sumPlayScore :: (Integer, Integer) -> Integer
sumPlayScore (p1, p2)
        | p1 == 10 = 10
        | p1 + p2 == 10 = 10
        | otherwise = p1 + p2

getNextTwoScores :: [Frame] -> (Integer, Integer)
getNextTwoScores [] = (0, 0)
getNextTwoScores (h:t)
        | playType h == Strike && bonusPlay h == True = (snd (plays h), bonusPlayScore h)
        | playType (head t) == Normal || playType (head t) == Spare = plays (head t)
        | playType (head t) == Strike && bonusPlay (head t) == True = plays (head t)
        | playType (head t) == Strike = (10, fst (plays (head (tail t))))
