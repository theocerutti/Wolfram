--
-- EPITECH PROJECT, 2020
-- TEK2
-- File description:
-- get_opt
--

module GetOpt
(
    args_param,
    getOpt
) where

isNum s = case reads s :: [(Integer, String)] of
  [(_, "")] -> True
  _         -> False

list_to_couple_param :: String -> String -> (Char, Int)
list_to_couple_param x y
    | isNum y == False = ('e', 84)
    | x == "--rule" = ('r', read y::Int)
    | x == "--start" = ('s', read y::Int)
    | x == "--lines" = ('l', read y::Int)
    | x == "--window" = ('w', read y::Int)
    | x == "--move" = ('m', read y::Int)
    | otherwise = ('e', 84)

list_to_list_param :: [String] -> [(Char, Int)]
list_to_list_param [] = [('e', 84)]
list_to_list_param x = f [] x 0
    where
        f res x n
            | length x `mod` 2 /= 0 = [('e', 84)]
            | n >= length x = res
            | otherwise = f ((list_to_couple_param (x !! n) (x !! (n + 1))):res) x (n + 2)

has_flag_err :: (Char, Int) -> Bool
has_flag_err param
    | y < 0 && x /= 'm' = True
    | otherwise = False
    where x = fst(param)
          y = snd(param)

has_opt_error :: [(Char, Int)] -> Bool
has_opt_error x = f x 0
    where
        f params n
            | n == length params = False
            | has_flag_err (params !! n) == True = True
            | otherwise = f params (n + 1)

args_param :: [String] -> [(Char, Int)]
args_param x
    | has_opt_error (list_to_list_param x) == True = [('e', 84)]
    | otherwise = list_to_list_param x

return_default_value_flag :: Char -> Int
return_default_value_flag 's' = 0
return_default_value_flag 'l' = -1
return_default_value_flag 'w' = 80
return_default_value_flag 'm' = 0
return_default_value_flag _ = 0

getOpt :: [(Char, Int)] -> Char -> Int
getOpt opts opt = f opts opt 0 0
    where
        f opts opt res n
            | n == length opts = return_default_value_flag opt
            | opt == fst(opts !! n) = snd (opts !! n)
            | otherwise = f opts opt res (n + 1)