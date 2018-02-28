module SessionType where

import Data.List

import Control.Applicative 
import Control.Arrow

data BaseType = Int 
              | Bool 
              deriving (Show, Eq)

data SessionType = Skip 
                 | Out BaseType
                 | In BaseType 
                 | Seq SessionType SessionType 
                 | Accept [(String, SessionType)]
                 | Select [(String, SessionType)]
                 | Rec Char SessionType
                 | Var Char
                 deriving (Eq)

listShow :: [(String, SessionType)] -> String
listShow = intercalate ", " . map (\(name, s) -> name ++ ":" ++ show s)

instance Show SessionType where
    show Skip        = "Skip"
    show (Out b)     = '!' : (show b)
    show (In b)      = '?' : (show b)
    show (Seq s1 s2) = (show s1) ++ ";" ++ (show s2)
    show (Accept l)  = "&{" ++ listShow l ++ "}"
    show (Select l)  = "+{" ++ listShow l ++ "}"
    show (Rec c s)   = "rec " ++ [c] ++ "." ++ show s
    show (Var c)     = [c]

dual :: SessionType -> SessionType
dual Skip        = Skip
dual (Out t)     = In t
dual (In t)      = Out t
dual (Seq s1 s2) = Seq (dual s1) (dual s2)
dual (Accept xs) = Select $ map (\ (c, s) -> (c, dual s)) xs
dual (Select xs) = Accept $ map (\ (c, s) -> (c, dual s)) xs
dual (Rec c s)   = Rec c $ dual s
dual (Var c)     = Var c

maybeSplit (x:xs) = Just (x, xs)
maybeSplit _      = Nothing

parse :: String -> Maybe (SessionType, String)
parse [] = Nothing
parse xs = let split = dropWhile (==' ') xs
           in parse' split <|> parse'' split

-- parse for ;
parse' xs = do
        (s1, xs')   <- parse'' xs  
        (c, xs'')   <- maybeSplit xs'
        (s2, xs''') <- parse xs''
        if c == ';' then Just (Seq s1 s2, xs''') else Nothing

-- parse for the rest
parse'' ('S':'k':'i':'p':xs)       = Just (Skip, xs) 
parse'' ('?':xs)                   = first In <$> parseBaseType xs  
parse'' ('!':xs)                   = first Out <$> parseBaseType xs 
parse'' ('&':'{':xs)               = first Accept <$> parseListOfTypes xs
parse'' ('+':'{':xs)               = first Select <$> parseListOfTypes xs
parse'' ('r':'e':'c':' ':c:'.':xs) = first (Rec c) <$> parse xs
parse'' (x:xs)                     = Just (Var x, xs)
   

parseBaseType :: String -> Maybe (BaseType, String)
parseBaseType ('I':'n':'t':xs)     = Just (Int, xs)
parseBaseType ('B':'o':'o':'l':xs) = Just (Bool, xs)
parseBaseType _                    = Nothing 

parseListOfTypes :: String -> Maybe ([(String, SessionType)], String)
parseListOfTypes ('}':xs) = Just ([], xs)
parseListOfTypes xs       =
    let (name, xs') = break (==':') xs
        parsed      = parse (drop 1 xs')
    in parsed >>= (\(s, (x:xs'')) ->
        case x of 
            '}'       -> Just ([(name, s)], xs'')
            ','       -> first ((name, s):) <$> parseListOfTypes xs''
            otherwise -> Nothing)
            

-- Rest of code is used for testing 
test :: IO ()
test = do
    let tests = map show [ Skip
                         , Out Int
                         , In Bool
                         , Accept []
                         , Accept [("abc", Skip)]
                         , Select []
                         , Select [("skip", Skip)]
                         , (Seq (In Int) (Out Int))
                         , (Rec 'a' (Rec 'b' (In Int)))
                         ]
    
    let parsed = zip tests $ map parse tests
    mapM_ (putStrLn . show) parsed
        


a = Accept [ ("add", Seq (Out Int) (Seq (Out Int) (Seq (In Int) Skip)))
           , ("neg", Seq (In Int) (Seq (Out Int) Skip))
           ]

{--
output for main:
&{ add:?Int;?Int;!Int;Skip, neg:!Int;?Int;Skip }
+{ add:!Int;!Int;?Int;Skip, neg:?Int;!Int;Skip }
--}

main = putStrLn (show a) >>
       putStrLn (show (dual a))
