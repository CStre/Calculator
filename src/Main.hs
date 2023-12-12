{-
   File: Calculator.hs
   Description: A calculator application in Haskell using the Monomer library.
   Author: Collin Streitman
   Date: 12-8-2023
   Version: 1.1.0
   How to Run: Use the following commands to build and run the application:
              1. stack build
              2. .stack-work/dist/x86_64-linux/Cabal-3.6.3.0/build/app/app
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

{-
-----------------------------------------------------------------------------
                            
                            -- File Imports --

-----------------------------------------------------------------------------
-}

import Control.Lens                                     -- (1) For lens operations like `makeLenses` and `^.`, enabling concise syntax for manipulating data structures.
import Data.Text (Text, pack, unpack, center)           -- (2) For handling Text data types, used throughout the app for text manipulation.
import Monomer                                          -- (3) Main GUI library for building and managing the user interface.
import TextShow                                         -- (4) Possibly for converting data to text, not directly used but might be a dependency.
import Data.Char                                        -- (5) Provides character manipulation functions, used in 'tokenize' for character checks like `isDigit`, `isSpace`.
import Data.Maybe                                       -- (6) For handling Maybe types, used in 'navigateHistory' for handling potential Nothing values with `fromMaybe`.
import Text.Read                                        -- (7) For parsing strings into other types, used in 'evaluateExpression' with `readMaybe`.
import Data.List                                        -- (8) Provides functions for list manipulation, used in various places like 'isError', 'replace', and 'navigateHistory'.
import Control.Concurrent (threadDelay)                 -- (9) For introducing delays in program execution, used in 'delay'.
import Control.Monad.IO.Class (liftIO)                  -- (10) For lifting IO actions into monads, used in 'handleEvent'.
import System.IO                                        -- (11) For file input/output operations, used in 'appendToFile', 'readValidInputs'.
import Control.Monad                                    -- (12) Provides utility functions for monadic operations, used with `unless`, `when` in file operations.
import Control.Exception                                -- (13) For handling exceptions, used in 'readValidInputs' for safe file reading.
import System.IO.Unsafe (unsafePerformIO)               -- (14) Allows IO operations in non-IO contexts, possibly a dependency of another import.
import Control.DeepSeq (deepseq)                        -- (15) For fully evaluating data structures, used in 'readValidInputs' for strict file content reading.
import Monomer.Core.Combinators                         -- (16) Provides combinators for building GUI elements in Monomer.
import Data.List.Split (splitOn)                        -- (17) For splitting strings, used in 'replace' function.
import Monomer.Core.Style (TextStyle)                   -- (18) For defining text styles in the GUI.
import qualified Monomer.Lens as L                      -- (19) For more lens operations specific to Monomer, aiding in concise UI code.
import Monomer.Event.Lens (HasRightShift(rightShift))   -- (20) For handling specific event-related lens operations in Monomer, possibly a dependency.
import System.Directory (doesFileExist)                 -- (21) For checking the existence of files, used in 'appendToFile', 'readValidInputs'.
import System.IO (writeFile)                            -- (22) For writing data to files, used in 'appendToFile', 'readValidInputs'.
import Monomer.Main.Platform (defaultWindowSize)        -- (23) This is used to define the size of the window
import Monomer.Main.Types (MainWindowState)             -- (24) This is used to define the size of the window
import Monomer.Core.WidgetTypes
import Monomer.Main.UserUtil



{-
-----------------------------------------------------------------------------
                            
                            -- AppModel --

-----------------------------------------------------------------------------

1.  AppModel represents the state of the application. In the context of this 
    calculator application, it specifically keeps track of the user's input at 
    any given moment.
2.  The model is integral to the operation of the Monomer library, which is 
    used for building the GUI. Monomer operates in a reactive manner, where 
    changes in the model trigger updates in the user interface.
-----------------------------------------------------------------------------
-}
data AppModel = AppModel {
  _currentInput :: Text,
  _shouldExit :: Bool
} deriving (Eq, Show)

makeLenses 'AppModel

{-
-----------------------------------------------------------------------------
                            
                            -- AppEvent --

-----------------------------------------------------------------------------
-}

data AppEvent = AddDigit Char       -- Digit is added to expression
              | AddOperation Char   -- Arithmetic operation added to expression (like +, -, ×, ÷)
              | AddFunction String  -- Mathematical operation added to expression (like sin, cos, log)
              | Calculate           -- Performs the calculation based on the current input or expression.
              | Clear               -- Clears the current input
              | TimerEvent          -- Used for events to be auto-cleared like Errors
              | NoOp                -- When nothing happenes; used in the histroy
              | ClearMem            -- Used for clearing the system memory
              | HistoryUp           -- This is used to look up to memory
              | HistoryDown         -- This is used to look down from memory
              | ToggleFun           -- Toggle for the special Polonsky Mode
              | SetInput String     -- Used to set the input to a specific thing
              | ExitApp             -- 
              | CloseApp            --
              | InitiateExit
        deriving (Eq, Show)

{-
-----------------------------------------------------------------------------
                                
                                -- Token --

-----------------------------------------------------------------------------
-}

data Token = Num Double     -- Used for floating-point numbers x.y
        | Op Char           -- Represents an operator such as '+', '-', '×', '÷', etc.
        | Sqrt              -- Square Root operator √(x)
        | Cos               -- Cosine operator cos(x)
        | Tan               -- Tangent operator tan(x)
        | Sin               -- Sin operator sin(x)
        | Log               -- Logarithm operator log(x) or log(x, y)
        | Ln                -- Natural Log operator ln(x)
        | E                 -- Euler's Number e
        | Abs               -- Absolute Value operator abs(x)
        | Comma             -- Comma operator '(', ')'
        | Pi                -- Pi operator π
        | Exp               -- Exponent operator for 2 x^(2)
        | Fact              -- Factorial operator x!
        | Mod               -- Modulo operator x%y
        | ErrorToken String -- Error handler for descriptive system
    deriving Show

{-
-----------------------------------------------------------------------------
                            
                            -- Tokenize --

-----------------------------------------------------------------------------

The tokenize function takes a String as input and converts it into a list of 
Token elements, each representing a distinct component of a mathematical 
expression, such as numbers, operators, and functions. It uses pattern matching 
and string manipulation to accurately identify and classify each part of the 
input string into the appropriate token type for further processing in the 
application.

-----------------------------------------------------------------------------
-}

tokenize :: String -> [Token]
tokenize [] = []
tokenize str@(c:cs)
    | c == '.' && null cs = [ErrorToken "[Error 302]: Lone decimal point without digits { x._ }"]
    | c == '.' && isDigit (head cs) =
        let (num, rest) = span (\x -> isDigit x || x == '.') ('0':str) in Num (read num) : tokenize rest  -- Case _.x: Prepends '0' to form a valid number.
    | isDigit c || (c == '.' && isDigit (head cs)) =
        let (num, rest) = span (\x -> isDigit x || x == '.') str in -- Case xy or .x: String starts with a digit or a decimal followed by a digit.
        if countDots num > 1 -- Check for multiple dots in a number.
        then [ErrorToken "[Error 304]: Multiple dots in number { x.y.z }"]
        else if last num == '.' -- Check for dot not followed by a number.
             then Num (read (num ++ "0")) : tokenize rest
             else Num (read num) : tokenize rest
    | c == '.' = [ErrorToken "[Error 303]: Decimal point preceding a function or symbol { .f(x) or .!}"] -- Case .f(x): Decimal point directly before a function or symbol.
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
    where -- Helper function to count dots in a string.
        countDots = length . filter (== '.')

{-
-----------------------------------------------------------------------------
                   
                   
                   
                   
                    -- Expression Evaluation (PARSER) --




-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
-- Evaluates a list of tokens representing a mathematical expression.
eval :: [Token] -> String
eval tokens = case parseExpr tokens of
  (result, []) -> formatOutput result -- Formats the result if no tokens are left
  (_, ErrorToken msg : _) -> msg -- Returns an error message if an ErrorToken is encountered
  _ -> "[Error 100]: Incorrect Syntax with operation usage" -- Default error message for incorrect syntax

------------ Expression Number 2 ------------
-- Parses a list of tokens and evaluates the expression starting with the highest precedence operation.
parseExpr :: [Token] -> (Double, [Token])
parseExpr = parseSum  -- Entry point for expression parsing

------------ Expression Number 3 ------------
-- Handle addition and subtraction
parseSum :: [Token] -> (Double, [Token])
parseSum tokens =
    let (num1, rest1) = parseProduct tokens
    in case rest1 of
        (Op '+' : rest2) -> let (num2, rest3) = parseSum rest2 in (num1 + num2, rest3)
        (Op '-' : rest2) -> let (num2, rest3) = parseSum rest2 in (num1 - num2, rest3)
        _ -> (num1, rest1)

------------ Expression Number 4 ------------
-- Handle multiplication and division
parseProduct :: [Token] -> (Double, [Token])
parseProduct tokens =
    let (num1, rest1) = parseFactor tokens
    in case rest1 of
        (Op '×' : rest2) -> let (num2, rest3) = parseProduct rest2 in (num1 * num2, rest3)
        (Op '÷' : rest2) -> let (num2, rest3) = parseProduct rest2 in (num1 / num2, rest3)
        (Mod : rest2) ->
            case parseFactor rest2 of -- Handle modulo operation
                (num2, rest3) | num2 == 0 -> (0, [ErrorToken "[Error 102]: Division by zero in mod not possible { x%0 }"])
                              | otherwise -> (fromIntegral (floor num1 `mod` floor num2), rest3)
        _ -> (num1, rest1) -- No multiplication, division, or modulo, return current results

------------ Expression Number 5 ------------
parseFactor :: [Token] -> (Double, [Token])
parseFactor (ErrorToken msg : _) = (0, [ErrorToken msg])                            -- Propagate the error message
parseFactor [] = (0, [ErrorToken "[Error 103]: No more tokens as was exhausted"])   -- Return an error message
parseFactor tokens = case tokens of
    (Num n : Sqrt : rest) -> let (sqrtResult, rest') = parseFactor (Sqrt : rest)    in (n * sqrtResult, rest')  -- Check for ex. _√(_)
    (Num n : Cos  : rest) -> let (cosResult, rest')  = parseFactor (Cos  : rest)    in (n * cosResult, rest')   -- Check for ex. _cos(_)
    (Num n : Tan  : rest) -> let (tanResult, rest')  = parseFactor (Tan  : rest)    in (n * tanResult, rest')   -- Check for ex. _tan(_)
    (Num n : Sin  : rest) -> let (sinResult, rest')  = parseFactor (Sin  : rest)    in (n * sinResult, rest')   -- Check for ex. _sin(_)
    (Num n : Log  : rest) -> let (logResult, rest')  = parseFactor (Log  : rest)    in (n * logResult, rest')   -- Check for ex. _log(_)
    (Num n : Ln   : rest) -> let (lnResult, rest')   = parseFactor (Ln   : rest)    in (n * lnResult, rest')    -- Check for ex. _ln(_)
    (Num n : E    : rest) -> let (eResult, rest')    = parseFactor (E    : rest)    in (n * eResult, rest')     -- Check for ex. _e(_)
    (Num n : Pi   : rest) -> let (piResult, rest')   = parseFactor (Pi   : rest)    in (n * piResult, rest')    -- Check for ex. _π(_)
    (Num n : Abs  : rest) -> let (absResult, rest')  = parseFactor (Abs  : rest)    in (n * absResult, rest')   -- Check for ex. _abs(_)
    (Num n : Exp : rest) -> -- Check for ex. _^(2)
        case rest of
            (Op '(' : rest') ->
                let (expResult, restAfterExp) = parseExpr rest'
                in case restAfterExp of
                    (Op ')' : restFinal) -> (n ** expResult, restFinal)
                    _ -> (0, [ErrorToken "[Error 104]: Missing closing parenthesis for exponent operator { x^(x }"])
            _ -> (0, [ErrorToken "[Error 205]: Expected opening parenthesis after exponent operator { x^ }"])
    (Op '-' : Num n : Fact : rest) -> (0, [ErrorToken "[Error 106]: Factorial not defined for negative numbers"])   -- Check for ex. -_!
    (Num n : Fact : rest) -> case factorial (round n) of                                                            -- Check for ex. _!
            Just factResult -> (fromIntegral factResult, rest)
            Nothing -> (0, [ErrorToken "[Error 207]: Factorial computation failed or overloaded. 28! MAX"])
    (Num n : Op '(' : rest) -> -- Check for ex. _(_) *making sure there are a set of paranthesis and a number before that is multiplied*
        let (exprResult, restAfterExpr) = parseExpr rest in
        case restAfterExpr of
            (Op ')' : restAfterParen) ->
                let (nextFactorResult, finalRest) = parseImplicitMultiplication (n * exprResult) restAfterParen
                in (nextFactorResult, finalRest)
            _ -> (0, [ErrorToken "[Error 208]: Missing closing parenthesis on implicit case"])
-- Making sure that having either no mathematical symbol before a function
    (Num n : rest) -> parseImplicitMultiplication n rest                -- Check for ex. _ _ 
    (Op '-' : rest) -> let (n, rest') = parseFactor rest in (-n, rest') -- Check for --_ to make number negative
    (Op '(' : rest) ->                                                  -- Check for ex. (_) full set of parenthesis
        let (exprResult, restAfterExpr) = parseExpr rest in
        case restAfterExpr of
            (Op ')' : Num n : restAfterNum) -> let (factorResult, restAfterFactor) = parseFactor (Num n : restAfterNum)
                                               in (exprResult * factorResult, restAfterFactor)
            (Op ')' : restAfterParen) -> parseImplicitMultiplication exprResult restAfterParen
            _ -> (0, [ErrorToken "[Error 209]: Missing closing parenthesis"])
    (Sqrt : rest) -> parseSqrtWithBase rest                                         -- Check for ex. √(_,_)
    (Abs  : rest) -> let (n, rest') = parseFactor rest in (abs n, rest')            -- Check for ex. abs(_)
    (Cos  : rest) -> let (n, rest') = parseFactor rest in (cos n, rest')            -- Check for ex. cos(_)
    (Tan  : rest) -> let (n, rest') = parseFactor rest in (Prelude.tan n, rest')    -- Check for ex. tan(_)
    (Sin  : rest) -> let (n, rest') = parseFactor rest in (sin n, rest')            -- Check for ex. sin(_)
    (Log  : rest) -> parseLogOrLnWithBase logBase 10 rest                           -- Check for ex. log(_,_)
    (Ln   : rest) -> parseLogOrLnWithBase logBase (exp 1) rest                      -- Check for ex. ln(_,_)
    (E    : rest) -> (exp 1, rest)                                                  -- Check for ex. e
    (Pi   : rest) -> (pi, rest)                                                     -- Check for ex. π
    _             -> (0, [ErrorToken "[Error 210]: Parse exhaused and syntax incorrect"]) -- When parser is exhaused

{-
-----------------------------------------------------------------------------
                    
                    
                    
                    
                    -- Storage and History Management --




-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
appendToFile :: FilePath -> String -> IO () -- Appends a given string to a file at the specified file path.
appendToFile filePath input = do
    fileExists <- doesFileExist filePath ---- Check if the file exists. If not, create an empty file.
    unless fileExists $ writeFile filePath ""
    contents <- readFile filePath  -- Read the current contents of the file.
    let modifiedInput = removePolonskyMode input  -- Modify the input to remove any special modes (e.g., Polonsky Mode).
    let history = lines contents  -- Convert the file contents into a list of lines (history).
    let shouldAppend = null history || (not (null history) && last history /= modifiedInput) -- Determine if the modified input should be appended to the file.
    when shouldAppend $ appendFile filePath (modifiedInput ++ "\n") -- Append the modified input to the file if the above condition is true.

------------ Expression Number 2 ------------
readValidInputs :: FilePath -> IO [String] -- Reads a list of previously entered valid inputs from a specified file.
readValidInputs filePath = do -- Check if the file exists. If not, create an empty file.
    fileExists <- doesFileExist filePath
    unless fileExists $ writeFile filePath ""  -- Create an empty file if it doesn't exist
    contents <- bracket -- Safely open and read the file's contents, ensuring proper resource handling.
        (openFile filePath ReadMode)
        (\h -> hClose h) -- Ensure the file is closed after reading.
        (\h -> do
            contents <- hGetContents h
            deepseq contents (return contents)) -- Force the full evaluation of the file contents.
    return (if null contents then [""] else lines contents) -- Return the file contents as a list of lines. 
                                                            -- If the file is empty, return a list with an empty string.
------------ Expression Number 3 ------------
removePolonskyMode :: String -> String -- Removes a specific marker (Polonsky Mode) from the input string.
removePolonskyMode input = replace "       !!!Polonsky Mode!!!" "" input
-- This function is used to clean up the input string before it is evaluated or stored.

------------ Expression Number 4 ------------
navigateHistory :: [String] -> String -> Bool -> String -- Navigates through the input history of the calculator application.
navigateHistory history currentInput isUp =
    let currentIndex = fromMaybe (-1) $ elemIndex currentInput history -- Determine the current index of the input in the history.
        newIndex = if isUp then currentIndex - 1 else currentIndex + 1 -- Calculate the new index based on the navigation direction.
    in if newIndex >= 0 && newIndex < length history -- Return the historical input corresponding to the new index.
        then history !! newIndex -- If the new index is out of bounds, return the current input.
        else currentInput

{-
-----------------------------------------------------------------------------
                            
                            
                            
                            
                            -- Helper Functions --





-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
-- Parses logarithmic expressions with a specified base, such as log(x, y) or ln(x).
-- 'f' is a function representing the logarithm computation.
-- 'defaultBase' is used when only one argument is provided (e.g., ln(x) is log(e, x)).
parseLogOrLnWithBase :: (Double -> Double -> Double) -> Double -> [Token] -> (Double, [Token])
parseLogOrLnWithBase f defaultBase tokens = case tokens of
    (Op '(' : rest) ->
        -- Parse the first number inside the parentheses.
        let (firstNum, restAfterFirstNum) = parseFactor rest in
        case restAfterFirstNum of
            -- Case for two arguments: log(x, y).
            (Comma : secondNumRest) ->
                let (secondNum, finalRest) = parseFactor secondNumRest
                in case finalRest of
                    (Op ')' : rest') -> (f firstNum secondNum, rest') -- Apply the log function with two arguments.
                    _ -> (0, [ErrorToken "[Error 211]: Missing closing parenethesis after log with base { log(x,y }"])
            -- Case for one argument: log(x) or ln(x).
            (Op ')' : rest') -> (f defaultBase firstNum, rest') -- Apply the log function with the default base.
            _ -> (0, [ErrorToken "[Error 212]: Missing comma or closing parenthesis after { log(x or ln(x }"])
    _ -> (0, [ErrorToken "[Error 113]: Expected pair of parenthesis after log/ln { log x,y }"])

------------ Expression Number 2 ------------
-- Parses square root expressions, handling both regular and nth roots.
parseSqrtWithBase :: [Token] -> (Double, [Token])
parseSqrtWithBase tokens = case tokens of
    (Op '(' : rest) ->
        -- Parse the first number (base of the root, if provided).
        let (firstNum, restAfterFirstNum) = parseExpr rest in
        case restAfterFirstNum of
            -- Case for nth root: √(n, x).
            (Comma : secondNumRest) ->
                let (secondNum, finalRest) = parseExpr secondNumRest
                in case finalRest of
                    (Op ')' : rest') -> (secondNum ** (1 / firstNum), rest')  -- nth root of secondNum
                    _ -> (0, [ErrorToken "[Error 213]: Missing closing parenthesis/comma after sqrt with base { √(x,y }"])
             -- Case for regular square root: √(x).
            (Op ')' : rest') -> (sqrt firstNum, rest')  -- Regular square root
            _ -> (0, [ErrorToken "[Error 214]: Missing closing parenthesis after sqrt { √(x }"])
    _ -> (0, [ErrorToken "[Error 115]: Missing opening parenthesis after sqrt { √ x,y }"])

------------ Expression Number 3 ------------
-- Handle implicit multiplication around a parenthesis
parseImplicitMultiplication :: Double -> [Token] -> (Double, [Token])
parseImplicitMultiplication prevResult tokens = case tokens of
    (Num n : rest) -> -- If a number follows, multiply it with the previous result.
        let (nextFactorResult, finalRest) = parseFactor (Num n : rest)
        in (prevResult * nextFactorResult, finalRest)
    _ -> (prevResult, tokens) -- No number following, return the previous result as is.

------------ Expression Number 4 ------------
-- Handle the factorial operator
factorial :: Integer -> Maybe Integer -- Calculates the factorial of a given integer.
factorial n
  | n < 0     = Nothing -- Factorial is not defined for negative numbers.
  | n > 29    = Nothing -- Limit set to avoid integer overflow (can be adjusted).
  | n == 0    = Just 1  -- Base case: 0! = 1.
  | otherwise = fmap (n *) (factorial (n - 1)) -- Recursive calculation of factorial.

------------ Expression Number 5 ------------
-- Defines an upper limit for the calculator to prevent overflow issues.
upperLimit :: Double
upperLimit = 1e30  -- Said limit, adjusted as needed

------------ Expression Number 6 ------------
-- Formats the output of a calculation, converting it into a String.
formatOutput :: Double -> String
formatOutput num =
    if fromIntegral (floor num) == num -- Check if the result is a whole number and display accordingly.
    then show (floor num)  -- If whole number, display without decimal.
    else show num          -- If not, display as a floating-point number.

------------ Expression Number 7 ------------
-- Checks if a given string represents an error message.
isError :: String -> Bool
isError = isPrefixOf "[Error" -- Error messages start with "[Error".

------------ Expression Number 8 ------------
-- Introduces a delay in the program's execution.
delay :: Int -> IO ()
delay ms = threadDelay (ms * 1000) -- Delay in milliseconds.

------------ Expression Number 9 ------------
-- Replaces occurrences of a substring within a string with another substring.
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old -- Uses split and intercalate to perform replacement.

------------ Expression Number 10 ------------
-- Creates a horizontal spacer widget in the UI.
hSpacer :: Double -> WidgetNode s e
hSpacer size = spacer `styleBasic` [width size] -- Sets the width of the spacer.

------------ Expression Number 11 ------------
-- Creates a vertical spacer widget in the UI.
vSpacer :: Double -> WidgetNode s e
vSpacer size = spacer `styleBasic` [height size] -- Sets the height of the spacer.

------------ Expression Number 12 ------------
-- String constant representing a special mode in the calculator.
funModeMarker :: String
funModeMarker = "       !!!Polonsky Mode!!!" -- Used to identify Polonsky Mode.

------------ Expression Number 13 ------------
-- Defines styling settings for text elements in the UI.
inputTextStyle :: [StyleState]
inputTextStyle = [textColor black, textSize 30] -- Sets text color, size, and font.

{-
-----------------------------------------------------------------------------
                    
                    
                    
                    
                    
                    -- GUI Components and Event Handling  --




                    
-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
-- Evaluates a mathematical expression given as a string.
-- It tokenizes the input, evaluates it, and ensures the result is within a numeric limit.evaluateExpression :: String -> String
evaluateExpression input =
    let tokens = tokenize input -- Tokenize the input string.
        resultString = eval tokens -- Evaluate the tokens.
    in case readMaybe resultString :: Maybe Double of
        Just result -> if result > upperLimit
                       then "[Error 100]: Too big!" -- Check if result exceeds the upper limit.
                       else formatOutput result -- Format the result for display.
        Nothing -> resultString -- If it's not a number, it's an error message

------------ Expression Number 2 ------------
-- Constructs the user interface for the calculator.
buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
buildUI wenv model = widgetTree where

    ---- Additional UI setup code and style definitions ----
    
    -- Define cleanedInput by removing "!!!Polonsky Mode!!!" if it exists
    cleanedInput = if funModeMarker `isInfixOf` (unpack $ model ^. currentInput)
                then replace funModeMarker "" (unpack $ model ^. currentInput)
                else unpack $ model ^. currentInput
    
    currentInputText = label (pack cleanedInput) `styleBasic` inputTextStyle
    
    polonskyModeStyle = [textColor red, textSize 45, textFont "Italic"]
    isFunMode = funModeMarker `isInfixOf` (unpack $ model ^. currentInput)
    
    funModeLabel = label (pack (if isFunMode then funModeMarker else "")) `styleBasic` polonskyModeStyle 
    
    displayInput = if isFunMode 
                   then replace funModeMarker "" (unpack $ model ^. currentInput) 
                   else unpack $ model ^. currentInput 
    
    -- Assemble the UI elements into a vertical stack.
    widgetTree = vstack [
        vSpacer 30,
        currentInputText,
        vSpacer 10,
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
    exitButton =                [radius 60, width 250, height 80, padding 5, textSize 45, bgColor green, textColor white, textFont "Medium"]

    -- Define different button layouts for standard and fun mode.
    standardbuttons = vstack [
            hstack [button "AC" Clear `styleBasic` greyCircularButtonStyle,             hSpacer 10,         button "fun" ToggleFun `styleBasic` greyCircularButtonStyle,    hSpacer 10,         button "x^2" (AddFunction "^(2)") `styleBasic` greyCircularButtonStyle,     hSpacer 10,         button "÷" (AddOperation '÷') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "7" (AddDigit '7') `styleBasic` circularButtonStyle,         hSpacer 10,         button "8" (AddDigit '8') `styleBasic` circularButtonStyle,     hSpacer 10,         button "9" (AddDigit '9') `styleBasic` circularButtonStyle,                 hSpacer 10,         button "x" (AddOperation '×') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "4" (AddDigit '4') `styleBasic` circularButtonStyle,         hSpacer 10,         button "5" (AddDigit '5') `styleBasic` circularButtonStyle,     hSpacer 10,         button "6" (AddDigit '6') `styleBasic` circularButtonStyle,                 hSpacer 10,         button "-" (AddOperation '-') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "1" (AddDigit '1') `styleBasic` circularButtonStyle,         hSpacer 10,         button "2" (AddDigit '2') `styleBasic` circularButtonStyle,     hSpacer 10,         button "3" (AddDigit '3') `styleBasic` circularButtonStyle,                 hSpacer 10,         button "+" (AddOperation '+') `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "0" (AddDigit '0') `styleBasic` wideCircularButtonStyle,     hSpacer 10,                                                                                             button "." (AddOperation '.') `styleBasic` circularButtonStyle,             hSpacer 10,         button "=" Calculate `styleBasic` orangeCircularButtonStyle],
            vSpacer 20,
            hstack [hSpacer 130, button "Done" ExitApp `styleBasic` exitButton, hSpacer 100]
        ]

    -- Define different button layouts for standard and fun mode.
    funModebuttons = vstack [
            hstack [button "AC" Clear `styleBasic` circularButtonStyle,                 hSpacer 10,         button "fun" ToggleFun `styleBasic` circularButtonStyle,            hSpacer 10,         button "(" (AddOperation '(') `styleBasic` greyCircularButtonStyle,         hSpacer 10,         button ")" (AddOperation ')') `styleBasic` greyCircularButtonStyle],
            vSpacer 10,
            hstack [button "√" (AddFunction "√(") `styleBasic` circularButtonStyle,     hSpacer 10,         button "log" (AddFunction "log(") `styleBasic` circularButtonStyle, hSpacer 10,         button "ln" (AddFunction "ln(") `styleBasic` circularButtonStyle,           hSpacer 10,         button "sin" (AddFunction "sin(") `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "e" (AddFunction "e") `styleBasic` circularButtonStyle,      hSpacer 10,         button "%" (AddOperation '%') `styleBasic` circularButtonStyle,     hSpacer 10,         button "!" (AddFunction "!") `styleBasic` circularButtonStyle,              hSpacer 10,         button "cos" (AddFunction "cos(") `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "abs" (AddFunction "abs(") `styleBasic` circularButtonStyle, hSpacer 10,         button "π" (AddFunction "π") `styleBasic` circularButtonStyle,      hSpacer 10,         button "," (AddFunction ", ") `styleBasic` circularButtonStyle,             hSpacer 10,         button "tan" (AddFunction "tan(") `styleBasic` orangeCircularButtonStyle],
            vSpacer 10,
            hstack [button "cmem" ClearMem `styleBasic` redCircularButtonStyle,         hSpacer 10,         button "Λ" HistoryDown `styleBasic` redCircularButtonStyle,         hSpacer 10,         button "V" HistoryUp `styleBasic` redCircularButtonStyle,                   hSpacer 10,         button "=" Calculate `styleBasic` orangeCircularButtonStyle],
            vSpacer 20,
            hstack [hSpacer 130, button "Done" ExitApp `styleBasic` exitButton, hSpacer 100]
        ]

------------ Expression Number 3 ------------
-- Handles different events in the calculator application.
handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
    AddDigit digit  ->  [Model $ model & currentInput <>~ pack [digit]] -- Add a digit to the current input.
    AddOperation op ->  [Model $ model & currentInput <>~ pack [op]]    -- Add an operation to the current input.
    AddFunction func -> [Model $ model & currentInput <>~ pack func]    -- Add a function to the current input.

    Calculate ->  -- Perform calculation based on the current input.
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
                    delay 2000  -- Wait for 2000 ms (2 seconds)
                    return TimerEvent  -- Trigger TimerEvent after the delay
                ]

    ToggleFun -> -- Toggle the special 'Fun Mode'.
        let currentInputText = unpack (model ^. currentInput)
            newInputText = if funModeMarker `isInfixOf` currentInputText
                           then replace funModeMarker "" currentInputText
                           else currentInputText ++ funModeMarker
        in [Model $ model & currentInput .~ pack newInputText]

    HistoryUp -> -- Navigate up in the history.
        [ Task $ do
            history <- readValidInputs "validInputs.txt"
            let updatedInput = navigateHistory history (removePolonskyMode $ unpack $ model ^. currentInput) True
            return $ SetInput (if funModeMarker `isInfixOf` unpack (model ^. currentInput) then updatedInput ++ funModeMarker else updatedInput)
        ]

    HistoryDown -> -- Navigate down in the history.
        [ Task $ do
            history <- readValidInputs "validInputs.txt"
            let updatedInput = navigateHistory history (removePolonskyMode $ unpack $ model ^. currentInput) False
            return $ SetInput (if funModeMarker `isInfixOf` unpack (model ^. currentInput) then updatedInput ++ funModeMarker else updatedInput)
        ]
    
    ExitApp -> 
        [ Model $ model & currentInput .~ "Goodbye!"
        , Task $ do
            delay 2000
            return InitiateExit
        ]
    
    InitiateExit -> 
        [exitApplication] -- Request to exit the application
    
    SetInput input ->   [Model $ model & currentInput .~ pack input]    -- Set the input to a specific value.
    NoOp ->             []                                              -- Do nothing for NoOp events.
    TimerEvent ->       [Model $ model & currentInput .~ ""]            -- Clear the current input on a TimerEvent.
    Clear ->            [Model $ model & currentInput .~ ""]            -- Clear the current input.
    ClearMem ->         [Task $ liftIO (writeFile "validInputs.txt" "") >> return NoOp]  -- Clear the memory file.


------------ Expression Number 4 ------------
main :: IO () -- The main entry point of the program.
main = startApp model handleEvent buildUI config where

    config = [ -- Configuration for the application window (title, icon, theme, fonts, etc.).
        appWindowTitle                "Polonsky's Big Brain Calculator",
        appWindowIcon                 "./assets/images/calculator.png",
        appTheme darkTheme,
        appFontDef          "Regular" "./assets/fonts/Roboto-Regular.ttf",
        appFontDef          "Medium"  "./assets/fonts/Roboto-Medium.ttf",
        appFontDef          "Bold"    "./assets/fonts/Roboto-Bold.ttf",
        appFontDef          "Italic"  "./assets/fonts/Roboto-Italic.ttf",
        appWindowResizable False, -- Cannot resize
        appWindowBorder False, -- Does not have border
        appWindowState (MainWindowNormal (550, 890))  -- Set the initial window state to normal with size 600x600
        ]
    
    model = AppModel {
        _currentInput = "", 
        _shouldExit = False
    } -- Initial model of the application.
