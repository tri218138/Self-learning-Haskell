split :: String -> Char -> [String]
split "" '\0' = [] -- empty list
split xs '\0' = [xs]
split str ch = deQui str ch ""
    where deQui :: String -> Char -> String -> [String]
          deQui "" '\0' "" = []
          deQui "" ch "" = []
          deQui "" ch ans = [ans]
          deQui (x:xs) ch ans
              | x == ch = ans : deQui xs ch ""
              | otherwise            = deQui xs ch (ans ++ [x]) -- if no seperator, just continue building up the current word
