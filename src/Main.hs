{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Lens
import Data.Text (Text, pack, unpack, center)
import Monomer
import TextShow
import Data.Char
import Data.Maybe
import Text.Read
import Data.List
-- For the dynamic error messages
import Control.Concurrent (threadDelay)
import Control.Monad.IO.Class (liftIO)
-- For the history function
import System.IO
import Control.Monad
import Control.Exception
import System.IO.Unsafe (unsafePerformIO)
import Control.DeepSeq (deepseq)
import Monomer.Core.Combinators

import Data.List.Split (splitOn)
import Monomer.Core.Style (TextStyle)


import qualified Monomer.Lens as L
import Monomer.Event.Lens (HasRightShift(rightShift))

import System.Directory (doesFileExist)
import System.IO (writeFile)



-- Define the application model and events
newtype AppModel = AppModel {
  _currentInput :: Text
} deriving (Eq, Show)

makeLenses 'AppModel

data AppEvent = AddDigit Char
              | AddOperation Char
              | AddFunction String
              | Calculate
              | Clear
              | TimerEvent
              | NoOp
              | ClearMem
              | HistoryUp
              | HistoryDown
              | ToggleFun
              | SetInput String
  deriving (Eq, Show)

-- Token and utility functions for expression parsing
data Token = Num Double
          | Op Char
          | Sqrt
          | Cos
          | Tan
          | Sin
          | Log
          | Ln
          | E
          | Abs
          | Comma
          | Pi
          | Exp
          | Fact
          | Mod
          | ErrorToken String
    deriving Show

tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c:cs)
    | isDigit c || (c == '.' && isDigit (head cs)) =
        let (num, rest) = span (\x -> isDigit x || x == '.') str in Num (read num) : tokenize rest
    | isSpace c = tokenize cs
    | c == '+' = Op '+' : tokenize cs
    | c == '-' = Op '-' : tokenize cs
    | c == '×' = Op '×' : tokenize cs
    | c == '÷' = Op '÷' : tokenize cs
    | c == '(' = Op '(' : tokenize cs
    | c == ')' = Op ')' : tokenize cs
    | c == '%' = Mod    : tokenize cs
    | take 1 str == "π"     = Pi    : tokenize (drop 1 str)
    | take 1 str == ","     = Comma : tokenize (drop 1 str)
    | take 1 str == "√"     = Sqrt  : tokenize (drop 1 str)
    | take 3 str == "cos"   = Cos   : tokenize (drop 3 str)
    | take 3 str == "tan"   = Tan   : tokenize (drop 3 str)
    | take 3 str == "sin"   = Sin   : tokenize (drop 3 str)
    | take 3 str == "log"   = Log   : tokenize (drop 3 str)
    | take 2 str == "ln"    = Ln    : tokenize (drop 2 str)
    | take 3 str == "abs"   = Abs   : tokenize (drop 3 str)
    | take 1 str == "e"     = E     : tokenize (drop 1 str)
    | take 1 str == "^"     = Exp   : tokenize (drop 1 str)
    | take 1 str == "!"     = Fact  : tokenize (drop 1 str)
    | otherwise = [ErrorToken "[Error 201]: Unrecognized input symbol and not able to tokenize"] -- ErrorToken is a new constructor in Token data type

-- Evaluate a list of tokens
eval :: [Token] -> String
eval tokens = case parseExpr tokens of
  (result, []) -> formatOutput result
  (_, ErrorToken msg : _) -> msg -- Properly handle ErrorToken
  _ -> "[Error 100]: Incorrect Syntax with operation usage"

parseExpr :: [Token] -> (Double, [Token])
parseExpr = parseSum  -- As the highest level of precedence

-- Handle addition and subtraction
parseSum :: [Token] -> (Double, [Token])
parseSum tokens =
    let (num1, rest1) = parseProduct tokens
    in case rest1 of
        (Op '+' : rest2) -> let (num2, rest3) = parseSum rest2 in (num1 + num2, rest3)
        (Op '-' : rest2) -> let (num2, rest3) = parseSum rest2 in (num1 - num2, rest3)
        _ -> (num1, rest1)

-- Handle multiplication and division
parseProduct :: [Token] -> (Double, [Token])
parseProduct tokens =
    let (num1, rest1) = parseFactor tokens
    in case rest1 of
        (Op '×' : rest2) -> let (num2, rest3) = parseProduct rest2 in (num1 * num2, rest3)
        (Op '÷' : rest2) -> let (num2, rest3) = parseProduct rest2 in (num1 / num2, rest3)
        (Mod : rest2) ->
            case parseFactor rest2 of
                (num2, rest3) | num2 == 0 -> (0, [ErrorToken "[Error 102]: Division by zero in mod not possible { x%0 }"])
                              | otherwise -> (fromIntegral (floor num1 `mod` floor num2), rest3)
        _ -> (num1, rest1)

parseFactor :: [Token] -> (Double, [Token])
parseFactor (ErrorToken msg : _) = (0, [ErrorToken msg])  -- Propagate the error message
parseFactor [] = (0, [ErrorToken "[Error 103]: No more tokens and expected a number, an operation, or a function"])  -- Return an error message
parseFactor tokens = case tokens of
-- Check for ex. _√(_)
    (Num n : Sqrt : rest) -> let (sqrtResult, rest') = parseFactor (Sqrt : rest)    in (n * sqrtResult, rest')
-- Check for ex. _cos(_)
    (Num n : Cos  : rest) -> let (cosResult, rest')  = parseFactor (Cos  : rest)    in (n * cosResult, rest')
-- Check for ex. _tan(_)
    (Num n : Tan  : rest) -> let (tanResult, rest')  = parseFactor (Tan  : rest)    in (n * tanResult, rest')
-- Check for ex. _sin(_)
    (Num n : Sin  : rest) -> let (sinResult, rest')  = parseFactor (Sin  : rest)    in (n * sinResult, rest')
-- Check for ex. _log(_)
    (Num n : Log  : rest) -> let (logResult, rest')  = parseFactor (Log  : rest)    in (n * logResult, rest')
-- Check for ex. _ln(_)
    (Num n : Ln   : rest) -> let (lnResult, rest')   = parseFactor (Ln   : rest)    in (n * lnResult, rest')
-- Check for ex. _e(_)
    (Num n : E    : rest) -> let (eResult, rest')    = parseFactor (E    : rest)    in (n * eResult, rest')
-- Check for ex. _π(_)
    (Num n : Pi   : rest) -> let (piResult, rest')   = parseFactor (Pi   : rest)    in (n * piResult, rest')
-- Check for ex. _abs(_)
    (Num n : Abs  : rest) -> let (absResult, rest')  = parseFactor (Abs  : rest)    in (n * absResult, rest')
-- Check for ex. _^(_)
    (Num n : Exp : rest) ->
        case rest of
            (Op '(' : rest') ->
                let (expResult, restAfterExp) = parseExpr rest'
                in case restAfterExp of
                    (Op ')' : restFinal) -> (n ** expResult, restFinal)
                    _ -> (0, [ErrorToken "[Error 104]: Missing closing parenthesis for exponent operator { x^(x }"])
            _ -> (0, [ErrorToken "[Error 205]: Expected opening parenthesis after exponent operator { x^ }"])
-- Check for ex. -_!
    (Op '-' : Num n : Fact : rest) -> (0, [ErrorToken "[Error 106]: Factorial not defined for negative numbers"])
-- Check for ex. _!
    (Num n : Fact : rest) -> case factorial (round n) of
            Just factResult -> (fromIntegral factResult, rest)
            Nothing -> (0, [ErrorToken "[Error 207]: Factorial computation failed"])
-- Check for ex. _(_) *making sure there are a set of paranthesis and a number before that is multiplied*
    (Num n : Op '(' : rest) ->
        let (exprResult, restAfterExpr) = parseExpr rest in
        case restAfterExpr of
            (Op ')' : restAfterParen) ->
                let (nextFactorResult, finalRest) = parseImplicitMultiplication (n * exprResult) restAfterParen
                in (nextFactorResult, finalRest)
            _ -> (0, [ErrorToken "[Error 208]: Missing closing parenthesis on implicit case"])
-- Check for ex. _ _ 
-- Making sure that having either no mathematical symbol before a function
    (Num n : rest) -> parseImplicitMultiplication n rest
-- Check for --_ to make number negative
    (Op '-' : rest) -> let (n, rest') = parseFactor rest in (-n, rest')
-- Check for ex. (_) full set of parenthesis
    (Op '(' : rest) ->
        let (exprResult, restAfterExpr) = parseExpr rest in
        case restAfterExpr of
            (Op ')' : Num n : restAfterNum) -> let (factorResult, restAfterFactor) = parseFactor (Num n : restAfterNum)
                                               in (exprResult * factorResult, restAfterFactor)
            (Op ')' : restAfterParen) -> parseImplicitMultiplication exprResult restAfterParen
            _ -> (0, [ErrorToken "[Error 209]: Missing closing parenthesis"])
-- Check for ex. √(_,_)
    (Sqrt : rest) -> parseSqrtWithBase rest
-- Check for ex. abs(_)
    (Abs  : rest) -> let (n, rest') = parseFactor rest in (abs n, rest')
-- Check for ex. cos(_)
    (Cos  : rest) -> let (n, rest') = parseFactor rest in (cos n, rest')
-- Check for ex. tan(_)
    (Tan  : rest) -> let (n, rest') = parseFactor rest in (Prelude.tan n, rest')
-- Check for ex. sin(_)
    (Sin  : rest) -> let (n, rest') = parseFactor rest in (sin n, rest')
-- Check for ex. log(_,_)
    (Log  : rest) -> parseLogOrLnWithBase logBase 10 rest
-- Check for ex. ln(_,_)
    (Ln   : rest) -> parseLogOrLnWithBase logBase (exp 1) rest
-- Check for ex. e
    (E    : rest) -> (exp 1, rest)
-- Check for ex. π
    (Pi   : rest) -> (pi, rest)
    _             -> (0, [ErrorToken "[Error 210]: Parse exhaused and syntax incorrect"])

-----------------------------------------------------------------------------
                            -- Storage Functions --
-----------------------------------------------------------------------------

appendToFile :: FilePath -> String -> IO ()
appendToFile filePath input = do
    fileExists <- doesFileExist filePath
    unless fileExists $ writeFile filePath ""
    contents <- readFile filePath
    let modifiedInput = removePolonskyMode input
    let history = lines contents
    let shouldAppend = null history || (not (null history) && last history /= modifiedInput)
    when shouldAppend $ appendFile filePath (modifiedInput ++ "\n")



readValidInputs :: FilePath -> IO [String]
readValidInputs filePath = do
    fileExists <- doesFileExist filePath
    unless fileExists $ writeFile filePath ""  -- Create an empty file if it doesn't exist
    contents <- bracket
        (openFile filePath ReadMode)
        (\h -> hClose h)
        (\h -> do
            contents <- hGetContents h
            deepseq contents (return contents))
    return (if null contents then [""] else lines contents)

removePolonskyMode :: String -> String
removePolonskyMode input = replace "       !!!Polonsky Mode!!!" "" input

navigateHistory :: [String] -> String -> Bool -> String
navigateHistory history currentInput isUp =
  let currentIndex = fromMaybe (-1) $ elemIndex currentInput history
      newIndex = if isUp then currentIndex - 1 else currentIndex + 1
  in if newIndex >= 0 && newIndex < length history
     then history !! newIndex
     else currentInput

-----------------------------------------------------------------------------
                            -- Helper Functions --
-----------------------------------------------------------------------------

-- Helper function to parse Log(_) or Log(_,_) same thing with Ln!
parseLogOrLnWithBase :: (Double -> Double -> Double) -> Double -> [Token] -> (Double, [Token])
parseLogOrLnWithBase f defaultBase tokens = case tokens of
    (Op '(' : rest) ->
        let (firstNum, restAfterFirstNum) = parseFactor rest in
        case restAfterFirstNum of
            (Comma : secondNumRest) ->
                let (secondNum, finalRest) = parseFactor secondNumRest
                in case finalRest of
                    (Op ')' : rest') -> (f firstNum secondNum, rest')
                    _ -> (0, [ErrorToken "[Error 211]: Missing closing parenethesis after log/ln with base { log(x,y }"])
            (Op ')' : rest') -> (f defaultBase firstNum, rest')
            _ -> (0, [ErrorToken "[Error 212]: Missing comma or closing parenthesis after log/ln { log(x }"])
    _ -> (0, [ErrorToken "[Error 113]: Expected pair of parenthesis after log/ln { log x,y }"])

-- Helper function to parse Sqrt(_) or Sqrt(_,_)
parseSqrtWithBase :: [Token] -> (Double, [Token])
parseSqrtWithBase tokens = case tokens of
    (Op '(' : rest) ->
        let (firstNum, restAfterFirstNum) = parseExpr rest in
        case restAfterFirstNum of
            (Comma : secondNumRest) ->
                let (secondNum, finalRest) = parseExpr secondNumRest
                in case finalRest of
                    (Op ')' : rest') -> (secondNum ** (1 / firstNum), rest')  -- nth root of secondNum
                    _ -> (0, [ErrorToken "[Error 213]: Missing closing parenthesis or comma after sqrt with base { √(x,y }"])
            (Op ')' : rest') -> (sqrt firstNum, rest')  -- Regular square root
            _ -> (0, [ErrorToken "[Error 214]: Missing closing parenthesis after sqrt { √(x }"])
    _ -> (0, [ErrorToken "[Error 115]: Missing opening parenthesis after sqrt { √ x,y }"])


-- Handle implicit multiplication around a parenthesis
parseImplicitMultiplication :: Double -> [Token] -> (Double, [Token])
parseImplicitMultiplication prevResult tokens = case tokens of
    (Num n : rest) -> let (nextFactorResult, finalRest) = parseFactor (Num n : rest)
                      in (prevResult * nextFactorResult, finalRest)
    _ -> (prevResult, tokens)

-- Handle the factorial operator
factorial :: Integer -> Maybe Integer
factorial n
  | n < 0     = Nothing
  | n == 0    = Just 1
  | otherwise = fmap (n *) (factorial (n - 1))


-- Make sure the calculator won't fail with too large of an output
upperLimit :: Double
upperLimit = 1e30  -- Example limit, adjust as needed

-- Helper function to format output
formatOutput :: Double -> String
formatOutput num =
    if fromIntegral (floor num) == num
    then show (floor num)  -- Whole number
    else show num          -- Double

isError :: String -> Bool
isError = isPrefixOf "[Error"

delay :: Int -> IO ()
delay ms = threadDelay (ms * 1000)

replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

-- Add a horizontal spacer
hSpacer :: Double -> WidgetNode s e
hSpacer size = spacer `styleBasic` [width size]

-- Add a vertical spacer
vSpacer :: Double -> WidgetNode s e
vSpacer size = spacer `styleBasic` [height size]

funModeMarker :: String
funModeMarker = "       !!!Polonsky Mode!!!"

inputLabelStyle :: [TextStyle]
inputLabelStyle = [textColor white, textSize 24, textFont "Bold"]



-----------------------------------------------------------------------------
                            -- GUI Functions --
-----------------------------------------------------------------------------

-- Function to evaluate a string expression and make sure the output is not too large
evaluateExpression :: String -> String
evaluateExpression input =
    let tokens = tokenize input
        resultString = eval tokens
    in case readMaybe resultString :: Maybe Double of
        Just result -> if result > upperLimit
                       then "Too big!"
                       else formatOutput result
        Nothing -> resultString -- If it's not a number, it's an error message

-- Other things
buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where
    polonskyModeStyle = [textColor red, textSize 45, textFont "Italic"]
    isFunMode = funModeMarker `isInfixOf` (unpack $ model ^. currentInput)
  
    funModeLabel = label (pack (if isFunMode then funModeMarker else "")) `styleBasic` polonskyModeStyle 

    displayInput = if isFunMode 
                   then replace funModeMarker "" (unpack $ model ^. currentInput) 
                   else unpack $ model ^. currentInput
    
    widgetTree = vstack [
        label (pack displayInput),
        funModeLabel,
        if not isFunMode
        then standardbuttons
        else funModebuttons
        ] `styleBasic` [padding 20]
        
    -- Style for circular buttons
    circularButtonStyle =       [radius 60, width 120, height 120, padding 5, textSize 45, bgColor black, textColor white, textFont "Medium"]
    orangeCircularButtonStyle = [radius 60, width 120, height 120, padding 5, textSize 45, bgColor orange, textColor white, textFont "Medium"]
    greyCircularButtonStyle =   [radius 60, width 120, height 120, padding 5, textSize 45, bgColor gray, textColor black, textFont "Medium"]
    redCircularButtonStyle =    [radius 60, width 120, height 120, padding 5, textSize 45, bgColor red, textColor black, textFont "Medium"]
    wideCircularButtonStyle =   [radius 60, width 250, height 120, padding 5, textSize 45, bgColor black, textColor white, textFont "Medium"]

    -- Standard Buttons with Circular Style
    standardbuttons = vstack [
            hstack [button "AC" Clear `styleBasic` greyCircularButtonStyle,             hSpacer 10,         button "fun" ToggleFun `styleBasic` greyCircularButtonStyle,    hSpacer 10,         button "x^2" (AddFunction "^(2)") `styleBasic` greyCircularButtonStyle,     hSpacer 10,         button "÷" (AddOperation '÷') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "7" (AddDigit '7') `styleBasic` circularButtonStyle,         hSpacer 10,         button "8" (AddDigit '8') `styleBasic` circularButtonStyle,     hSpacer 10,         button "9" (AddDigit '9') `styleBasic` circularButtonStyle,                 hSpacer 10,         button "x" (AddOperation '×') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "4" (AddDigit '4') `styleBasic` circularButtonStyle,         hSpacer 10,         button "5" (AddDigit '5') `styleBasic` circularButtonStyle,     hSpacer 10,         button "6" (AddDigit '6') `styleBasic` circularButtonStyle,                 hSpacer 10,         button "-" (AddOperation '-') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "1" (AddDigit '1') `styleBasic` circularButtonStyle,         hSpacer 10,         button "2" (AddDigit '2') `styleBasic` circularButtonStyle,     hSpacer 10,         button "3" (AddDigit '3') `styleBasic` circularButtonStyle,                 hSpacer 10,         button "+" (AddOperation '+') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "0" (AddDigit '0') `styleBasic` wideCircularButtonStyle,     hSpacer 10,                                                                                             button "." (AddOperation '.') `styleBasic` circularButtonStyle,             hSpacer 10,         button "=" Calculate `styleBasic` orangeCircularButtonStyle]
        ]

    -- Fun Mode Buttons with Circular Style
    funModebuttons = vstack [
            hstack [button "AC" Clear `styleBasic` circularButtonStyle,                 hSpacer 10,         button "fun" ToggleFun `styleBasic` circularButtonStyle,            hSpacer 10,         button "(" (AddOperation '(') `styleBasic` greyCircularButtonStyle,         hSpacer 10,         button ")" (AddOperation ')') `styleBasic` greyCircularButtonStyle],
            vSpacer 10,
            hstack [button "√" (AddFunction "√(") `styleBasic` circularButtonStyle,     hSpacer 10,         button "log" (AddFunction "log(") `styleBasic` circularButtonStyle, hSpacer 10,         button "ln" (AddFunction "ln(") `styleBasic` circularButtonStyle,           hSpacer 10,         button "sin" (AddFunction "sin(") `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "e" (AddFunction "e") `styleBasic` circularButtonStyle,      hSpacer 10,         button "%" (AddOperation '%') `styleBasic` circularButtonStyle,     hSpacer 10,         button "!" (AddFunction "!") `styleBasic` circularButtonStyle,              hSpacer 10,         button "cos" (AddFunction "cos(") `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "abs" (AddFunction "abs(") `styleBasic` circularButtonStyle, hSpacer 10,         button "π" (AddFunction "π") `styleBasic` circularButtonStyle,      hSpacer 10,         button "," (AddFunction ", ") `styleBasic` circularButtonStyle,             hSpacer 10,         button "tan" (AddFunction "tan(") `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "cmem" ClearMem `styleBasic` redCircularButtonStyle,         hSpacer 10,         button "Λ" HistoryDown `styleBasic` redCircularButtonStyle,         hSpacer 10,         button "V" HistoryUp `styleBasic` redCircularButtonStyle,                   hSpacer 10,         button "=" Calculate `styleBasic` orangeCircularButtonStyle]
        ]

-- Handling events
handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
    AddDigit digit  ->  [Model $ model & currentInput <>~ pack [digit]]
    AddOperation op ->  [Model $ model & currentInput <>~ pack [op]]
    AddFunction func -> [Model $ model & currentInput <>~ pack func]

    Calculate ->
        let input = unpack $ model ^. currentInput
            cleanedInput = removePolonskyMode input -- Remove "Polonsky Mode" from input
            output = evaluateExpression cleanedInput
        in if not (isError output) -- Check if the output is not an error
        then [ Model $ model & currentInput .~ pack output
                , Task $ do
                    liftIO $ appendToFile "validInputs.txt" cleanedInput -- Use cleanedInput here
                    return NoOp  -- Returns a NoOp event
                ]
        else [ Model $ model & currentInput .~ pack output
                , Task $ do
                    delay 2400  -- Wait for 2400 ms (2.4 seconds)
                    return TimerEvent  -- Trigger TimerEvent after the delay
                ]

    ToggleFun ->
        let currentInputText = unpack (model ^. currentInput)
            newInputText = if funModeMarker `isInfixOf` currentInputText
                           then replace funModeMarker "" currentInputText
                           else currentInputText ++ funModeMarker
        in [Model $ model & currentInput .~ pack newInputText]

    HistoryUp ->
        [ Task $ do
            history <- readValidInputs "validInputs.txt"
            let updatedInput = navigateHistory history (removePolonskyMode $ unpack $ model ^. currentInput) True
            return $ SetInput (if funModeMarker `isInfixOf` unpack (model ^. currentInput) then updatedInput ++ funModeMarker else updatedInput)
        ]

    HistoryDown ->
        [ Task $ do
            history <- readValidInputs "validInputs.txt"
            let updatedInput = navigateHistory history (removePolonskyMode $ unpack $ model ^. currentInput) False
            return $ SetInput (if funModeMarker `isInfixOf` unpack (model ^. currentInput) then updatedInput ++ funModeMarker else updatedInput)
        ]
    
    SetInput input ->   [Model $ model & currentInput .~ pack input]
    NoOp ->             []  -- Handle NoOp event by doing nothing
    TimerEvent ->       [Model $ model & currentInput .~ ""]
    Clear ->            [Model $ model & currentInput .~ ""]
    ClearMem ->         [Task $ liftIO (writeFile "validInputs.txt" "") >> return NoOp]  -- Clear the file content

-----------------------------------------------------------------------------
                            -- Main Function --
-----------------------------------------------------------------------------

main :: IO ()
main = startApp model handleEvent buildUI config where
config = [
    appWindowTitle "Polonsky's Big Brain Calculator",
    appWindowIcon "./assets/images/icon.png",
    appTheme darkTheme,
    appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
    appFontDef "Medium" "./assets/fonts/Roboto-Medium.ttf",
    appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
    appFontDef "Italic" "./assets/fonts/Roboto-Italic.ttf",
    appWindowResizable True,
    appWindowBorder True
    ]
model = AppModel ""
