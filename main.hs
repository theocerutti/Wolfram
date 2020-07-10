--
-- EPITECH PROJECT, 2020
-- TEK2
-- File description:
-- main
--

module Main where

import GetOpt
import System.Environment
import System.Exit

main :: IO ()
main = do getArgs >>= wolfram

pattern30 = [0, 0, 0, 1, 1, 1, 1, 0]
pattern90 = [0, 1, 0, 1, 1, 0, 1, 0]
pattern110 = [0, 1, 1, 0, 1, 1, 1, 0]

wolfram :: [String] -> IO ()
wolfram args
    | (ruleNb < 0 || ruleNb > 255) = exitWith (ExitFailure 84)
    | (getOpt (args_param args) 'e') == 84 = exitWith (ExitFailure 84)
    | otherwise = rule (fillBinary (toBinary ruleNb)) (args_param args)
    where ruleNb = getOpt (args_param args) 'r'

fillBinary :: [Int] -> [Int]
fillBinary x
    | length x == 8 = x
fillBinary x = f x
    where
        f res
            | length res == 8 = res
            | otherwise = f (0:res)

toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary 1 = [1]
toBinary n
    | n `mod` 2 == 0 = toBinary (n `div` 2) ++ [0]
    | otherwise = toBinary (n `div` 2) ++ [1]

rule :: [Int] -> [(Char, Int)] -> IO ()
rule pattern args = f [1] 0
    where
        f cells_before n
            | n == (getOpt args 'l' - 1) + (getOpt args 's') + 1 = return ()
            | otherwise = print_line cells_before args n >> f (get_next_cells cells_before pattern) (n + 1)

get_from_neightboor :: [Int] -> [Int] -> Int
get_from_neightboor neight pattern
    | neightC == "111" = pattern !! 0
    | neightC == "110" = pattern !! 1
    | neightC == "101" = pattern !! 2
    | neightC == "100" = pattern !! 3
    | neightC == "011" = pattern !! 4
    | neightC == "010" = pattern !! 5
    | neightC == "001" = pattern !! 6
    | neightC == "000" = pattern !! 7
    where neightC = concatMap show neight

get_this_neightboor :: [Int] -> Int -> Int
get_this_neightboor before pos
    | pos >= length before = 0
    | pos < 0 = 0
    | otherwise = before !! pos

get_neightboor :: [Int] -> [Int] -> Int -> [Int]
get_neightboor before pattern pos = f [] (-1)
    where
        f res n
            | n == 2 = reverse res
            | otherwise = f ((get_this_neightboor before (n + pos - 1)):res) (n + 1)

get_next_cells :: [Int] -> [Int] -> [Int]
get_next_cells before pattern = f [] 0
    where
        f res n
            | n >= length before + 2 = reverse res
            | otherwise = f ((get_from_neightboor (get_neightboor before pattern n) pattern):res) (n + 1)

print_space :: [(Char, Int)] -> Int -> Bool -> IO ()
print_space args n before
    | nb_space <= 0 = return ()
    | otherwise = (putStr (replicate (nb_space + add + move) ' '))
    where nb_space = ((getOpt args 'w') `div` 2 - n)
          add
            | before == True = 0
            | getOpt args 'w' `mod` 2 == 0 = -1
            | otherwise = 0
          move
            | before == True = getOpt args 'm'
            | otherwise = (-(getOpt args 'm'))

print_line :: [Int] -> [(Char, Int)] -> Int -> IO ()
print_line before args n
    | getOpt args 'w' == 0 = putChar '\n' >> return ()
    | n < (getOpt args 's') = return ()
    | otherwise = print_space args n True >> print_cells before offset args >> print_space args n False >> putChar '\n'
    where
        offset
            | length before > (getOpt args 'w') = (length before - (getOpt args 'w')) `div` 2
            | otherwise = 0

print_cells :: [Int] -> Int -> [(Char, Int)] -> IO ()
print_cells xs offset args = f xs 0 offset args
    where
        f xs n offset args
            | (n + offset) >= length xs = return ()
            | n >= getOpt args 'w' = return ()
            | (xs !! (n + offset)) == 0 = putChar ' ' >> f xs (n + 1) offset args
            | (xs !! (n + offset)) == 1 = putChar '*' >> f xs (n + 1) offset args
            | otherwise = error "Error in print cells"