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


parseSessionType :: String -> Maybe SessionType
parseSessionType s = fst <$> parse s 
    where
        parse :: String -> Maybe (SessionType, String)
        parse [] = Nothing
        parse xs = let split = dropWhile (==' ') xs
                   in parseSeq split <|> parseRest split

        -- parse for ;
        -- parseSeq
        parseSeq :: String -> Maybe (SessionType, String)
        parseSeq xs = do
            (s1, xs')   <- parseRest xs  
            xs''        <- removeSemicolon xs'
            (s2, xs''') <- parse xs''
            return (Seq s1 s2, xs''') 
            where removeSemicolon :: String -> Maybe String
                  removeSemicolon (';':xs) = Just xs
                  removeSemicolon _        = Nothing

        -- parse for the rest
        parseRest :: String -> Maybe (SessionType, String)
        parseRest ('S':'k':'i':'p':xs)       = Just (Skip, xs) 
        parseRest ('?':xs)                   = first In <$> parseBaseType xs  
        parseRest ('!':xs)                   = first Out <$> parseBaseType xs 
        parseRest ('&':'{':xs)               = first Accept <$> parseListOfTypes xs
        parseRest ('+':'{':xs)               = first Select <$> parseListOfTypes xs
        parseRest ('r':'e':'c':' ':c:'.':xs) = first (Rec c) <$> parse xs
        parseRest (x:xs)                     = Just (Var x, xs)
           

        parseBaseType :: String -> Maybe (BaseType, String)
        parseBaseType ('I':'n':'t':xs)     = Just (Int, xs)
        parseBaseType ('B':'o':'o':'l':xs) = Just (Bool, xs)
        parseBaseType _                    = Nothing 

        parseListOfTypes :: String -> Maybe ([(String, SessionType)], String)
        parseListOfTypes ('}':xs) = Just ([], xs)
        parseListOfTypes xs       = do
            let (name, xs') = break (==':') xs
            (s, (x:xs''))  <- parse (drop 1 xs')
            case x of 
                '}'       -> Just ([(name, s)], xs'')
                ','       -> first ((name, s):) <$> parseListOfTypes xs''
                otherwise -> Nothing
                    

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
    
    let parsed = zip tests $ map parseSessionType tests 
    mapM_ putStrLn $ map show parsed

        


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
