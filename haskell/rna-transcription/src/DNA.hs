module DNA (toRNA) where


toRNA :: String -> Either Char String
toRNA = traverse transcribeDnaToRna
  where
  transcribeDnaToRna :: Char -> Either Char Char
  transcribeDnaToRna c = case c of
        'G' -> Right 'C'
        'C' -> Right 'G'
        'T' -> Right 'A'
        'A' -> Right 'U'
        x -> Left x

