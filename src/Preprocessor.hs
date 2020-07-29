module Preprocessor (prepareSource) where 

prepareSource :: String -> String 
prepareSource = 
    stripMeaninglessChars

stripMeaninglessChars :: String -> String 
stripMeaninglessChars = 
    filter meaningfulChar
    where
        meaningfulChar c = 
            c `elem` "+-><,.[]"