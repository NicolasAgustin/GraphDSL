--{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use drop" #-}
{-# HLINT ignore "Use take" #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Matriz where

import Ast
import Data.List (elemIndex)

newtype Matrix a = M [[a]] deriving (Eq)

type Coord = (Integer, Integer)

instance (Show a) => Show (Matrix a) where
    show (M [])       = ""
    show (M (f:rest)) = "| " ++ r f ++ "\n" ++ show (M rest)
        where
            r l = foldr ((\x y -> x ++ " | " ++ y) . show) "" l

make :: Show a => [a] -> [String]
make = map show

getMatrix :: Matrix a -> [[a]]
getMatrix (M m) = m

empty :: Matrix a
empty = M [[]]

build :: a -> Int -> Matrix a
build e 0 = M [[]]
build e n = M $ replicate n (replicate n e)

dimensions :: Matrix a -> (Integer, Integer)
dimensions (M []) = (0, 0)
dimensions (M m@(l:rest)) = (toInteger $ length m - 1, toInteger $ length l - 1)

get :: Coord -> Matrix a -> a
get (row, col) = get' (fromIntegral row, fromIntegral col)

get' :: (Int, Int) -> Matrix a -> a
get' (row, col) (M []) = error "Index out of range"
get' (row, col) (M f)  = (f !! row) !! col

set :: Coord -> a -> Matrix a -> Matrix a
set (row, col) = set' (fromIntegral row, fromIntegral col)

set' :: (Int, Int) -> a -> Matrix a -> Matrix a
set' (row, col) e (M []) = M [[e]]
set' (row, col) e (M m)  = M (above ++ [new_row] ++ below)
                where
                    entire_row     = m !! row
                    (left, right)  = (take col entire_row, drop (col+1) entire_row)
                    new_row        = left ++ [e] ++ right
                    (above, below) = (take row m, drop (row+1) m)

mmap :: (a -> b) -> Matrix a -> Matrix b
mmap _ (M [])       = M []
mmap f (M (l:rest)) = M $ map f l : getMatrix (mmap f (M rest))

search :: (Eq a) => a -> Matrix a -> Coord
search e m = search' e m (0, 0) (fst $ dimensions m)

search' :: (Eq a) => a -> Matrix a -> Coord -> Integer -> Coord
search' _ (M []) (r, c) rs       = (r, c)
search' e (M (l:rest)) (r, c) rs = if b && r <= rs then (r, i) else search' e (M rest) (r+1, c) rs
                                            where
                                                (b, i) = case elemIndex e l of
                                                            Nothing -> (False, 0)
                                                            Just i  -> (True, toInteger i)

firstCoord :: Coord -> Coord 
firstCoord (x, y) = (x1, y1)
                    where 
                        x1 = if x-1 < 0 then x else x-1
                        y1 = if y-1 < 0 then y else y-1

secondCoord :: Coord -> (Int, Int) -> Coord 
secondCoord (x, y) (lx, ly) = (x1, y1)
                                where 
                                    x1 = if x+1 == toInteger lx then x else x+1
                                    y1 = if y+1 == toInteger ly then y else y+1

near :: (Eq a) => a -> Matrix a -> Matrix a
near e (M []) = M []
near e ma@(M (l:rest)) = neighbors (fp, sp) ma
                            where 
                                scoord = search e ma
                                fp = firstCoord scoord 
                                sp = secondCoord scoord (length l, length (l:rest)) 

fst' :: (a, a) -> a
fst' (x,_) = x

-- printt :: (Coord, Coord) -> IO ()
-- printt c = do
--             print ("new_rows: " ++ show a)
--             print ("new_rows_b: " ++ show b)
--             where
--                 (a, b) = extractNear c m

sublist :: (Eq a) => Integer -> Integer -> [a] -> [a]
sublist from to l = if from == to then [l !! fromIntegral from] else l !! fromIntegral from : sublist (from+1) to l

neighbors :: (Eq a) => (Coord, Coord) -> Matrix a -> Matrix a
neighbors (r, c) (M []) = M []
neighbors (r, c) (M m)  = M new_columns
                                where
                                    new_rows   = sublist (fst r) (fst c) m
                                    new_columns = map (sublist (snd r) (snd c)) new_rows

calculateLimit :: Coord -> Coord -> (Coord, Coord)
calculateLimit (ri, ci) (rs, cs) = ((r_upper, c_upper), (r_lower, c_lower))
                                where
                                    r_upper = if (ri-1) >= 0 then ri-1 else ri
                                    c_upper = if (ci-1) >= 0 then ci-1 else ci
                                    r_lower = if (ri+1) < rs then ri+1 else ri
                                    c_lower = if (ci+1) < cs then ci+1 else ci

test :: Matrix DataType'
test = build (Node "id" 0 0) 4

m = M [
    [1,2,3,4,5],
    [6,7,8,9,10],
    [11,12,13,14,15],
    [16,17,18,19,20],
    [21,22,23,24,25]
    ]
