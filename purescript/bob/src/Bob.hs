import           Data.Char

-- import Prelude hiding (last)
trim :: String -> String
trim = undefined

hey :: String -> String
hey prompt
  | question && not yelling = "Sure."
  | yelling = "Whoa, chill out!"
  | notSayingAnything = "Fine. Be that way!"
  | otherwise = "Whatever."
  where
    prompt' = trim prompt
    question = last prompt' == '?'
    yelling = (&&) <$> all isUpper <*> (/= []) $ filter isAlpha prompt'
    notSayingAnything = prompt' == ""
