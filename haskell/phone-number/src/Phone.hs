module Phone
  ( number
  ) where

number :: String -> Maybe String
number xs
  | isValid = Just xs'
  | otherwise = Nothing
  where
    xs' =
      dropWhile (== '1') $
      concat $
      map (filter (\x -> x `elem` ['0' .. '9'])) $
      words xs
    isValid =
      (length xs' == 10) &&
      (\(a:_:_:b:_) ->
         a `elem` ['2' .. '9'] && b `elem` ['2' .. '9'])
        xs'
