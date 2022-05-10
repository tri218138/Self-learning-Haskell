-- remove element at index i of list ns
removeIndex i ns = take i ns ++ drop (1 + i) ns
