{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# HLINT ignore "Eta reduce" #-}
module OrientationMapper where
import Matriz ( Matrix , Matrix(M), set, get, empty, build, mmap, search, calculateLimit, dimensions, near, getMatrix)
import Ast (DataType' (Node), Mapper)

import Utils (format)
import GHC.TopHandler (runIO)

-- data MNode = MNode DataType' deriving (Show, Eq)



insert :: DataType' -> Mapper -> Mapper
insert n@(Node id r c) m = set (r, c) n m

process :: Mapper -> [String]
process m = iterRows m m 0

iterRows :: Mapper -> Mapper -> Int -> [String]
iterRows m (M []) _             = []
iterRows m (M (l:rest)) counter = iterCols m l counter 0 ++ iterRows m (M rest) (counter+1)

iterCols :: Mapper -> [DataType'] -> Int -> Int -> [String]
iterCols m [] r c       = []
iterCols m (h:rest) r c = toOrientations m h : iterCols m rest r (c+1)

toOrientations :: Mapper -> DataType' -> String
toOrientations m node = getOrientation node neighbor
                    where
                        neighbors2 = near node m
                        m_sin_node = set (x, y) (Node "" 0 0) neighbors2
                        neighbor   = irows (getMatrix neighbors2)
                        (x, y)     = (\(Node _ r c) -> (r, c)) node

irows :: [[DataType']] -> DataType'
irows []       = Node "" 0 0
irows (l:rest) = if neighbor == Node "" 0 0 then irows rest else neighbor
                    where
                        neighbor = icols l

icols :: [DataType'] -> DataType'
icols [] = Node "" 0 0
icols (h:rest) = if h == Node "" 0 0 then icols rest
                    else h

getOrientation :: DataType' -> DataType' -> String
getOrientation (Node id y x) (Node id2 y2 x2)
    | id == "" || id2 == "" = ""
    | x == x2 && y == y2 = format "\\Vertex[x=%, y=%] {%}" [show x2, show y2,id2]
    | x == x2 && y < y2  = format "\\NO(%) {%}" [id2, id]
    | x == x2 && y > y2  = format "\\SO(%) {%}" [id2, id]
    | x > x2 && y == y2  = format "\\EA(%) {%}" [id2, id]
    | x < x2 && y > y2   = format "\\NOEA(%) {%}" [id2, id]
    | x > x2 && y > y2   = format "\\SOEA(%) {%}" [id2, id]
    | x < x2 && y == y2  = format "\\WE(%) {%}" [id2, id]
    | x > x2 && y < y2   = format "\\NOWE(%) {%}" [id2, id]
    | x < x2 && y < y2   = format "\\SOWE(%) {%}" [id2, id]
    -- 0 0
    -- 0 1
-- toOrientations' :: Mapper -> (Int, Int) -> String
-- toOrientations' (M (l:rest)) (r, c) = 
test1 = build (Node "" 0 0) 3

test2 = insert (Node "n1" 0 0) test1

test3 = insert (Node "n2" 0 1) test2 

test4 = insert (Node "n3" 1 0) test3

test5 = insert (Node "n4" 1 1) test4

test6 = insert (Node "n5" 2 2) test5

test1_1 = insert (Node "n2" 0 1) (insert (Node "n1" 0 0) test1)