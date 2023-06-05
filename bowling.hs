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
        putStrLn "Enter the plays:"
        plays_string <- getLine
        let plays = take 21 (words plays_string)
        let plays_int = [read x :: Integer | x <- plays]
        putStrLn (show plays)
        putStrLn (show plays_int)
        --putStrLn (show (loadFrames plays_int))
        putStrLn ((printFrames (loadFrames plays_int)))

data Play = Normal | Strike | Spare
        deriving Show

data Frame = Frame {
    plays :: (Integer, Integer),
    playType :: Play,
    bonus :: Integer,
    bonusPlay :: Integer
} deriving Show

loadFrames :: [Integer] -> [Frame]
loadFrames [] = []
loadFrames (h:t)
        | h == 10 = Frame { plays = (10, 0), playType = Strike, bonus = 1, bonusPlay = 0 } : loadFrames t
        | h + (head t) == 10 && length (tail t) == 1 = Frame { plays = (h, head t), playType = Spare, bonus = 1, bonusPlay = head (tail t) } : []
        | h + (head t) == 10 = Frame { plays = (h, head t), playType = Spare, bonus = 1, bonusPlay = 0 } : loadFrames (tail t)
        | otherwise = Frame { plays = (h, head t), playType = Normal, bonus = 0, bonusPlay = 0 } : loadFrames (tail t)

printFrames :: [Frame] -> [Char]
printFrames [] = []
printFrames (h:t) = case (playType h) of
        Normal -> show (fst $ plays h) ++ " " ++ show (snd $ plays h) ++ " | " ++ printFrames t
        Spare -> show (fst $ plays h) ++ " " ++ "/" ++ " | " ++ printFrames t
        Strike -> "X" ++ " " ++ "_" ++ " | " ++ printFrames t


