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
        plays <- getLine
        putStrLn plays