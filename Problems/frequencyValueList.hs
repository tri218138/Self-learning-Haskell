-- input: [1,2,0,0,1,4,3,9]
--output: 
-- **
-- ** **    *
-- ==========
-- 0123456789

frequency :: [Int] -> [Int] 
frequency ns = [length [ns !! j | j <- [0.. length ns - 1], ns !! j == i] | i <- [0..9]]

histogram :: [Int] -> String
histogram ns = deQui (frequency ns) (maximum (frequency ns)) ""
    where	deQui :: [Int] -> Int -> String -> String
        	deQui [] 0 "" = "==========\n0123456789\n"
		deQui ns 0 "" = "==========\n0123456789\n"
        	deQui ns 0 current = current ++ "==========\n0123456789\n"
        	deQui ns h current
            	    | h == 0 = current ++ deQui ns 0 current
            	    | otherwise 	= deQui ns (h - 1) (current ++ [if (ns !! i) >= h then '*' else ' ' | i <- [0.. length ns - 1]] ++ "\n")
