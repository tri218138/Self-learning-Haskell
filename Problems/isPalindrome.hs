-- isPalindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome ns = and [(reverse ns !! i) == (ns !! i)| i <- [0.. length ns - 1]]
