main = print res

isGraphClosed :: Eq a => a -> (a -> [a]) -> (a -> Bool) -> Bool
isGraphClosed initial neighbours isOk = dfs initial [] neighbours isOk

dfs :: Eq a => a -> [a] -> (a -> [a]) -> (a -> Bool) -> Bool -- to samo co isGraphClosed, tylko dostaje jako argument dodatkowo visited, czyli listę odwiedzonych wierzchołków 

dfs initial visited neighbours isOk = 
    if (isOk initial) 
    then 
        andList 
            (mapList (\x -> dfs x (initial:visited) neighbours isOk) (filter_not_visited visited (neighbours initial))) 
    else False
    
filter_not_visited :: Eq a => [a] -> [a] -> [a]
filter_not_visited visited neighbour_list = filterList (\x -> not(elemList x visited)) neighbour_list
    
-- sprawdzenie grafu
check_vertex1 :: Integer -> Bool
check_vertex1 x = (mod x 3 == 0)
check_vertex2 :: Integer -> Bool
check_vertex2 x = True

neighbours :: Integer -> [Integer]
neighbours 1 = [2, 3]
neighbours 2 = [3, 1]
neighbours 3 = [1, 2]
neighbours x = []

res :: Bool
res = isGraphClosed 1 neighbours check_vertex2

-- funkcje pomocnicze z listami 


foldList :: (a -> b -> b) -> b -> [a] -> b
foldList fun acc [] = acc
foldList fun acc (h:t) = foldList fun (fun h acc) t


allList :: (a-> Bool) -> [a] -> Bool
allList isOk list = foldList fun True list 
    where fun element acc = ((isOk element) && acc)


andList :: [Bool] -> Bool
andList list = foldList (&&) True list


mapList :: (a -> b) -> [a] -> [b]
mapList fun lista = foldList (perform_and_append fun) [] lista

perform_and_append :: (a -> b) -> a -> [b] -> [b]
perform_and_append function element acc = (acc ++ [(function element)])

nth :: [a] -> Integer -> a
nth (h:t) 0 = h
nth (h:t) n = nth t (n-1)


filterList :: (a -> Bool) -> [a] -> [a]
filterList is_ok lista = foldList (add_if_ok is_ok) [] lista

add_if_ok :: (a -> Bool) -> a -> [a] -> [a]
add_if_ok is_ok element acc = 
    if (is_ok element) 
    then acc ++ [element]
    else acc
    

listLength :: [a] -> Integer
listLength [] = 0
listLength (h:t) = (listLength t) + 1

appendList :: [a] -> [a] -> [a]
appendList l1 [] = l1
appendList l1 (h2:t2) = appendList (l1 ++ [h2]) t2

elemList :: Eq a => a -> [a] -> Bool
elemList x lista = foldList (true_if_equal x) False lista 

true_if_equal :: Eq a => a -> a -> Bool -> Bool 
true_if_equal x y acc = (acc || (x == y))
