{-# LANGUAGE InstanceSigs #-}
module Lib2
    ( Query(..),
    parseQuery,
    State(..),
    emptyState,
    stateTransition,
    Flower(..),
    Garden(..)
    ) where

import qualified Data.Char as C
import qualified Data.List as L

-- | An entity which represets user input.
-- It should match the grammar from Laboratory work #1.
-- Currently it has no constructors but you can introduce
-- as many as needed.
data Query
  = Add Flower 
  | Add_Garden Garden
  | Remove Flower
  | ShowState
  deriving (Show, Eq)

data Flower = Flower 
  { species :: String,
    color :: String,
    height :: Int
  } deriving (Show, Eq)

data Garden = Garden 
  { flowers :: [Flower]
  } deriving (Show, Eq)

data State = State 
  { garden :: Garden
  } deriving (Show, Eq)

-- <string>
parseString :: String -> Either String (String, String)
parseString [] = Left "String Error: empty input"
parseString str = 
    let (beforeSpace, rest) = break (== ' ') str
    in if null rest
       then Right (beforeSpace, "")  
       else Right (beforeSpace, tail rest)

-- <number>
parseNumber :: String -> Either String (Int, String)
parseNumber [] = Left " Num Error: empty input"
parseNumber str =
    let
        digits = L.takeWhile C.isDigit str
        rest = drop (length digits) str
    in
        case digits of
            [] -> Left (rest ++ " not a number")
            _ -> Right (read digits, rest)

parseChar :: Char -> String -> Either String (Char, String)
parseChar c [] = Left ("Char Error: empty input")
parseChar c s@(h:t) = if c == h then Right (c, t) else Left (c : " is not found in " ++ s)

-- <flower> ::= <species> ' ' <color> ' ' <height>
parseFlower :: String -> Either String (Flower, String)
parseFlower [] = Left "Empty input"
parseFlower str = 
  case parseString str of
    Left err -> Left err
    Right (species, rest) -> 
      case parseString rest of 
        Left err -> Left err
        Right (color, rest2) -> 
          case parseNumber rest2 of 
            Left err -> Left err
            Right (height, rest3) ->
              let flower = Flower 
                    { species = species,
                      color = color,
                      height = height
                    }
              in Right (flower, rest3)

-- <garden> ::= <flower> ' ' <garden> | <flower>
parseGarden :: String -> Either String ([Flower], String)
parseGarden str =
    case parseFlower str of
        Left _ -> Right ([], str)
        Right (flower, remaining) ->
            if null remaining
            then Right ([flower], "")
            else case parseChar ' ' remaining of
                Left _ -> Right ([flower], remaining)
                Right (_, rest) ->
                    case parseGarden rest of
                        Left err -> Left err
                        Right (otherFlowers, remainingAfter) ->
                            Right (flower : otherFlowers, remainingAfter)

-- | The instances are needed basically for tests

-- | Parses user's input.
-- The function must have tests.
parseQuery :: String -> Either String Query
parseQuery str = 
  case parseString str of
    Left err -> Left err
    Right ("Add", rest) ->
      case parseFlower rest of
        Left err -> Left err
        Right (flower, remaining) -> Right (Add flower)
    Right ("Remove", rest) ->
      case parseFlower rest of
        Left err -> Left err
        Right (flower, remaining) -> Right (Remove flower)
    Right ("Add_Garden", rest) ->
      case parseGarden rest of
        Left err -> Left err
        Right (flowers, remaining) -> Right (Add_Garden (Garden flowers))
    Right ("ShowState", rest) -> Right (ShowState)
    Right _ -> Left "Query Error: unrecognized query command"



-- | An entity which represents your program's state.
-- Currently it has no constructors but you can introduce
-- as many as needed.


-- | Creates an initial program's state.
-- It is called once when the program starts.
emptyState :: State
emptyState = State {garden = Garden []}

-- | Updates a state according to a query.
-- This allows your program to share the state
-- between repl iterations.
-- Right contains an optional message to print and
-- an updated program's state.

stateTransition :: State -> Query -> Either String (Maybe String, State)
stateTransition st query = case query of
    Add flower ->
        let currentFlowers = flowers (garden st)
        in Right (Nothing, st {garden = Garden (flower : currentFlowers)})
    Add_Garden newGarden ->
        Right (Nothing, st {garden = newGarden})
    Remove flower ->
        let currentFlowers = flowers (garden st)
        in if flower `notElem` currentFlowers
           then Left "Flower not found in the garden."
           else Right (Nothing, st {garden = Garden (filter (/= flower) currentFlowers)})
    ShowState ->
        Right (Just (show st), st) 