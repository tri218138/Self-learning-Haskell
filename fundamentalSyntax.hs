-- get k start elements
head k ns

-- get list without k start elements
drop k ns

-- get list without last element
init ns

-- get last element
-- solution1
ns !! (length ns - 1)
-- solution2
last x = head (reverse x) x

-- create cycle list with k-first element(s)
cycleList = take k (cycle([1..n]))

-- remove element at index i of list ns
removeIndex i ns = take i ns ++ drop (1 + i) ns
