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

1.  Used for the `makeLenses` and `^.`
2.  Used for manipulating Text values in AppModel and various functions
3.  Main library for GUI
4.  Not directly used, a dependency of another import
5.  Used in 'tokenize' for 'isDigit', 'isSpace'
6.  Used in 'navigateHistory' for 'fromMaybe'
7.  Used in 'evaluateExpression' for 'readMaybe'
8.  Used in 'isError', 'replace', and 'navigateHistory' for list manipulations
9.  Used in 'delay'
10. Used in 'handleEvent' for 'liftIO'
11. Used in 'appendToFile', 'readValidInputs'
12. Used in 'appendToFile', 'readValidInputs'
13. Used in 'readValidInputs'
14. Not directly used, could be a dependency of another import
15. Used in 'readValidInputs'
16. Used for GUI combinators
17. Used for GUI combinators
18. Used for styling in GUI functions
19. Used for lens operations in Monomer
20. Not directly used, could be a dependency of another import
21. Used in 'appendToFile', 'readValidInputs'
22. Used in 'appendToFile', 'readValidInputs'
-----------------------------------------------------------------------------
-}

-- 1
import Control.Lens
-- 2
import Data.Text (Text, pack, unpack, center)
-- 3
import Monomer
-- 4
import TextShow
-- 5
import Data.Char
-- 6
import Data.Maybe
-- 7
import Text.Read
-- 8
import Data.List
-- 9
import Control.Concurrent (threadDelay)
-- 10
import Control.Monad.IO.Class (liftIO)
-- 11
import System.IO
-- 12
import Control.Monad
-- 13
import Control.Exception
-- 14
import System.IO.Unsafe (unsafePerformIO)
-- 15
import Control.DeepSeq (deepseq)
-- 16
import Monomer.Core.Combinators
-- 17
import Data.List.Split (splitOn)
-- 18
import Monomer.Core.Style (TextStyle)
-- 19
import qualified Monomer.Lens as L
-- 20
import Monomer.Event.Lens (HasRightShift(rightShift))
-- 21
import System.Directory (doesFileExist)
-- 22
import System.IO (writeFile)

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
newtype AppModel = AppModel {
  _currentInput :: Text
} deriving (Eq, Show)

makeLenses 'AppModel

{-
-----------------------------------------------------------------------------
                            -- AppEvent --
-----------------------------------------------------------------------------

1.  AddDigit Char:      
        Represents an event where a digit is added to the current 
        expression. The Char parameter signifies the digit to be 
        added.

2.  AddOperation Char:  
        Corresponds to an event where an arithmetic operation 
        (like +, -, ×, ÷) is added to the expression. The operation 
        is passed as a Char.

3.  AddFunction String: 
        This event is triggered when a mathematical function (like 
        sin, cos, log) is added. The specific function is 
        represented as a String.   

4.  Calculate:          
        Signifies an event to perform the calculation based on 
        the current input or expression.

5.  Clear:          
        Indicates an event to clear the current input or reset 
        the calculator.

6.  TimerEvent:         
        Used for events that are based on a timer, for handling 
        timeouts or delays in the application.

7.  NoOp:               
        Represents a "no operation" event, essentially an event 
        where nothing happens. It can be useful for default cases 
        or as a placeholder.

8.  ClearMem:                   
        This event corresponds to clearing stored memory or 
        history in the calculator.

9.  HistoryUp and HistoryDown:  
        These events are used for navigating through the 
        history of calculations or inputs.

10. ToggleFun:                  
        This is an event to toggle a special mode in the 
        calculator, a more advanced alternate set of 
        functionalities.

11. SetInput String:            
        Represents an event to set the calculator's 
        input to a specific string value.

-----------------------------------------------------------------------------
-}

data AppEvent = AddDigit Char       --Number 1
              | AddOperation Char   --Number 2
              | AddFunction String  --Number 3
              | Calculate           --Number 4
              | Clear               --Number 5
              | TimerEvent          --Number 6
              | NoOp                --Number 7
              | ClearMem            --Number 8
              | HistoryUp           --Number 9
              | HistoryDown         --Number 9
              | ToggleFun           --Number 10
              | SetInput String     --Number 11
  deriving (Eq, Show)

{-
-----------------------------------------------------------------------------
                                -- Token --
-----------------------------------------------------------------------------

1.  Num Double: 
        Represents a numerical value. The Double type is used to handle floating-point 
        numbers, allowing for both integers and decimals in the calculations.

2.	Op Char: 
        Represents an operator. The Char value could be any character representing 
        an arithmetic operation, such as '+', '-', '×', '÷', etc.

3.	Sqrt: 
        Symbolizes the square root operation. It indicates that a square root 
        function will be applied to a subsequent numerical value or expression.

4.	Cos, Tan, Sin: 
        Represent the trigonometric functions cosine, tangent, and sine, 
        respectively. These are used to perform trigonometric calculations on
        numbers or expressions that follow.

5.	Log, Ln: 
        Stand for logarithmic functions. Log typically represents a logarithm with 
        a base of 10, whereas Ln is the natural logarithm (base e).

6.	E: 
        Represents the mathematical constant e (approximately 2.71828), which is 
        the base of the natural logarithm.

7.	Abs: 
        Represents the absolute value function, used to return the absolute 
        (non-negative) value of a number or expression.

8.	Comma: 
        Used as a delimiter, primarily in functions that require more than one 
        argument, like the logarithm with a specific base (Log).

9.	Pi: 
        Represents the mathematical constant π (Pi).

10.	Exp: 
        Symbolizes the exponentiation operation. It is used to raise a number 
        to the power of another number.

11.	Fact: 
        Represents the factorial operation, denoted as '!'. It is used to 
        calculate the factorial of a number.

12.	Mod: 
        Represents the modulo operation, denoted as '%'. It calculates the 
        remainder of the division of one number by another.

13.	ErrorToken String: 
        Used to handle errors in parsing or interpreting the expression. The String 
        contains an error message describing the nature of the error.

-----------------------------------------------------------------------------
-}

data Token = Num Double      --Number 1
          | Op Char          --Number 2
          | Sqrt             --Number 3
          | Cos              --Number 4
          | Tan              --Number 4
          | Sin              --Number 4
          | Log              --Number 5
          | Ln               --Number 5
          | E                --Number 6
          | Abs              --Number 7
          | Comma            --Number 8
          | Pi               --Number 9
          | Exp              --Number 10
          | Fact             --Number 11
          | Mod              --Number 12
          | ErrorToken String--Number 13
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
    | c == '.' =
        if null cs
        then [Num 0.0]  -- Handle the case where the input is just a period
        else if isFunctionStart (head cs)
             then [ErrorToken "[Error 303]: Invalid use of '.' before a function or symbol"]
             else let (num, rest) = span (\x -> isDigit x || x == '.') str
                      dotCount = length $ filter (== '.') num
                      correctedNum = if head num == '.' then '0':num else num
                  in if dotCount <= 1 then Num (read correctedNum) : tokenize rest
                     else [ErrorToken "[Error 302]: Invalid number format. Must follow x.y"]
    | isDigit c = 
        let (num, rest) = span isDigit str
        in Num (read num) : tokenize rest
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
    where
        isFunctionStart :: Char -> Bool
        isFunctionStart x = any (x ==) ['+', '-', '×', '÷', '(', ')', '%', 'π', ',', '√', 'c', 't', 's', 'l', 'a', 'e', '^', '!']

{-
-----------------------------------------------------------------------------
                    -- Expression Evaluation (PARSER) --
-----------------------------------------------------------------------------

1.  eval :: [Token] -> String

    The eval function is responsible for evaluating a list of tokens that 
    represent a mathematical expression. It takes a list of Token as input 
    and returns a String as output. The function first parses the expression 
    using parseExpr and then formats the output using formatOutput. It also 
    handles error tokens by returning appropriate error messages, ensuring 
    that any syntactical or operational errors in the expression are reported 
    back to the user.

2. parseExpr :: [Token] -> (Double, [Token])

    The parseExpr function is a key component responsible for parsing and 
    evaluating mathematical expressions represented as a list of tokens. It 
    serves as the entry point to the expression evaluation logic, starting 
    with the highest precedence operation (addition and subtraction in this 
    case) and delegating to more specific parsing functions like parseSum, 
    parseProduct, and parseFactor. This function utilizes a recursive 
    approach to handle complex, nested expressions, ensuring correct 
    evaluation order and handling of different mathematical operations and 
    functions.

3. parseSum :: [Token] -> (Double, [Token])

    The parseSum function is designed to parse and evaluate addition and subtraction 
    operations within a mathematical expression. It operates on a list of tokens, 
    identifying and processing tokens that represent addition (+) and subtraction (-) 
    operations according to their precedence in the expression. The function 
    recursively computes the sum or difference of the numerical values

4. parseProduct :: [Token] -> (Double, [Token])
    
    The parseProduct function handles the parsing and evaluation of multiplication 
    and division operations within a mathematical expression. It processes a list 
    of tokens (representing the expression) and identifies multiplication (×), 
    division (÷), and modulo (%) operations.

5. parseFactor :: [Token] -> (Double, [Token])
    
    The parseFactor function is responsible for parsing and evaluating the 
    lowest level of expressions in a mathematical calculation, “Factors". It 
    handles a variety of inputs, including numbers, parentheses, mathematical 
    constants (like π and e), unary operations (like factorial and square 
    root), and implicit multiplication.


-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
eval :: [Token] -> String
eval tokens = case parseExpr tokens of
  (result, []) -> formatOutput result
  (_, ErrorToken msg : _) -> msg 
  _ -> "[Error 100]: Incorrect Syntax with operation usage"

------------ Expression Number 2 ------------
parseExpr :: [Token] -> (Double, [Token])
parseExpr = parseSum  -- As the highest level of precedence

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
            case parseFactor rest2 of
                (num2, rest3) | num2 == 0 -> (0, [ErrorToken "[Error 102]: Division by zero in mod not possible { x%0 }"])
                              | otherwise -> (fromIntegral (floor num1 `mod` floor num2), rest3)
        _ -> (num1, rest1)

------------ Expression Number 5 ------------
parseFactor :: [Token] -> (Double, [Token])
parseFactor (ErrorToken msg : _) = (0, [ErrorToken msg])  -- Propagate the error message
parseFactor [] = (0, [ErrorToken "[Error 103]: No more tokens as was exhausted"])  -- Return an error message
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
-- When parser is exhaused
    _             -> (0, [ErrorToken "[Error 210]: Parse exhaused and syntax incorrect"])

{-
-----------------------------------------------------------------------------
                    -- Storage and History Management --
-----------------------------------------------------------------------------

1.  appendToFile :: FilePath -> String -> IO ()
    
    The appendToFile is designed to add a given string input to a specified file, 
    used for maintaining a history of valid inputs in the calculator application. 
    It first checks if the file exists, creating it if it doesn't, and then reads 
    the current contents to determine if the new input is different from the last 
    entry. If the new input is indeed different, it appends this input to the 
    file, ensuring that the history is updated only with unique, consecutive 
    entries.

2.  readValidInputs :: FilePath -> IO [String]
    
    The readValidInputs function reads and retrieves a list of previously entered 
    valid inputs from a specified file. It first checks if the file exists and 
    creates an empty one if it doesn't. Then, it safely opens and reads the 
    file's contents, ensuring proper resource handling with bracket, and returns 
    the inputs as a list of strings.

3.  removePolonskyMode :: String -> String

	The removePolonskyMode function is a utility function designed to modify the 
    calculator's input string by removing a specific marker, identified here as 
    "Polonsky Mode". This function is used to clean up the input string before 
    it is evaluated or stored in the application's history, ensuring that any 
    special mode indicators do not interfere with the calculation logic or the 
    recording of inputs.

4.  navigateHistory :: [String] -> String -> Bool -> String

    The navigateHistory function manages user navigation through the input 
    history of the calculator application. It takes a list of historical inputs, 
    the current input, and a Boolean indicating navigation direction (up or down 
    through the history). This function then calculates the new index based on 
    the current position and the navigation direction, and returns the 
    corresponding historical input, enabling users to easily access and reuse 
    their previous entries.

-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
appendToFile :: FilePath -> String -> IO ()
appendToFile filePath input = do
    fileExists <- doesFileExist filePath
    unless fileExists $ writeFile filePath ""
    contents <- readFile filePath
    let modifiedInput = removePolonskyMode input
    let history = lines contents
    let shouldAppend = null history || (not (null history) && last history /= modifiedInput)
    when shouldAppend $ appendFile filePath (modifiedInput ++ "\n")

------------ Expression Number 2 ------------
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

------------ Expression Number 3 ------------
removePolonskyMode :: String -> String
removePolonskyMode input = replace "       !!!Polonsky Mode!!!" "" input

------------ Expression Number 4 ------------
navigateHistory :: [String] -> String -> Bool -> String
navigateHistory history currentInput isUp =
  let currentIndex = fromMaybe (-1) $ elemIndex currentInput history
      newIndex = if isUp then currentIndex - 1 else currentIndex + 1
  in if newIndex >= 0 && newIndex < length history
     then history !! newIndex
     else currentInput

{-
-----------------------------------------------------------------------------
                            -- Helper Functions --
-----------------------------------------------------------------------------

1.  parseLogOrLnWithBase :: (Double -> Double -> Double) -> Double -> [Token] -> (Double, [Token])

    The parseLogOrLnWithBase function parses and evaluates logarithmic 
    expressions with a specified base, such as log(x, y) or ln(x). It takes 
    a function f representing the logarithm computation, a default base 
    (like 10 for log or e for ln), and a list of tokens that represent a 
    mathematical expression. The function intelligently handles various cases, 
    including the presence or absence of a base in the expression, and errors 
    such as missing parentheses or commas.

2.  parseSqrtWithBase :: [Token] -> (Double, [Token])

    The parseSqrtWithBase function is designed to parse square root 
    expressions with an optional base from a list of tokens. It handles 
    two formats: a regular square root (√(x)) and a nth root with a specified 
    base (√(n,x)). This function uses pattern matching to interpret the tokens, 
    extract the numerical values for the base and the number under the root 
    (if provided), and computes the appropriate root value, returning the result 
    along with any remaining unparsed tokens.

3.  parseImplicitMultiplication :: Double -> [Token] -> (Double, [Token])

    The parseImplicitMultiplication function is designed to handle implicit 
    multiplication in mathematical expressions. It takes a previously calculated 
    result and a list of tokens (representing the remaining part of the expression) 
    as inputs. If the next token is a number, it multiplies this number with the 
    previously calculated result, effectively handling cases where an explicit 
    multiplication operator is omitted 
    (e.g., 2(3) being interpreted as 2 * 3)

4.  factorial :: Integer -> Maybe Integer

    The factorial function calculates the factorial of a given integer. 
    It recursively multiplies the number by the factorial of the number minus 
    one, continuing this process until it reaches zero. The function gracefully 
    handles negative input by returning Nothing

5.  upperLimit :: Double

    The upperLimit function sets a threshold for the maximum value that the 
    calculator can handle. It is defined as a constant (upperLimit :: Double) 
    with a value of 1e30.

6.  formatOutput :: Double -> String

    The formatOutput function is designed to format the output of a calculator 
    operation. It checks whether the result is a whole number and, if so, it 
    displays it as an integer (without a decimal point). In cases where 
    the result is not a whole number, it displays it as a floating-point number.

7.  isError :: String -> Bool

    The isError function is a utility function used to check if a given String 
    represents an error message. It takes a String as input and returns a 
    Bool. This function works by checking if the input string starts with 
    the specific error message prefix "[Error", using the isPrefixOf 
    function from the Data.List module, which is a standard approach for 
    string pattern matching in Haskell.

8.  delay :: Int -> IO ()

    The delay function is another utility function for introducing a delay or 
    pause in the program's execution. Specifically, it uses threadDelay, which 
    suspends the current thread for a specified number of milliseconds. In this 
    context, delay ms pauses the execution for ms milliseconds, allowing for 
    timed behaviors in the application, such as temporary display of messages or 
    controlled pacing of certain actions.

9.  replace :: String -> String -> String -> String

    The replace function is for replacing a specific substring within a string 
    with another substring. It takes three arguments: the substring to be replaced 
    (old), the substring to replace with (new), and the original string. The 
    function works by splitting the original string at occurrences of the old 
    substring and then intercalating (joining) these split parts with the new 
    substring.

10. hSpacer :: Double -> WidgetNode s e

    The hSpacer function is designed to create a horizontal spacer widget within 
    the graphical user interface of the calculator application. It takes a Double 
    value as input, representing the width of the spacer in pixels. This function 
    is primarily used to add horizontal spacing between UI elements, such as buttons, 
    in the application's layout.

11. vSpacer :: Double -> WidgetNode s e

	Same as the above function but creates spacing along a vertical axis.

12. funModeMarker :: String

    The funModeMarker is a string constant defined as " !!!Polonsky Mode!!!". 
    This marker is used to identify the special mode within the calculator 
    application, referred to as "Polonsky Mode". When this mode is active, 
    the calculator's behavior or display changes, and the presence of this 
    marker in the user input or application state is used to trigger or indicate 
    this special mode.

13. inputLabelStyle :: [TextStyle]

    The inputLabelStyle is not a function but a list of TextStyle settings defined 
    for styling text elements in the GUI. This function was intended to be 
    implemented to style the calculator input but after many hours of trial and 
    error, I decided to leave it an example of the structure I was working towards. 

-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
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

------------ Expression Number 2 ------------
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
                    _ -> (0, [ErrorToken "[Error 213]: Missing closing parenthesis/comma after sqrt with base { √(x,y }"])
            (Op ')' : rest') -> (sqrt firstNum, rest')  -- Regular square root
            _ -> (0, [ErrorToken "[Error 214]: Missing closing parenthesis after sqrt { √(x }"])
    _ -> (0, [ErrorToken "[Error 115]: Missing opening parenthesis after sqrt { √ x,y }"])

------------ Expression Number 3 ------------
-- Handle implicit multiplication around a parenthesis
parseImplicitMultiplication :: Double -> [Token] -> (Double, [Token])
parseImplicitMultiplication prevResult tokens = case tokens of
    (Num n : rest) -> let (nextFactorResult, finalRest) = parseFactor (Num n : rest)
                      in (prevResult * nextFactorResult, finalRest)
    _ -> (prevResult, tokens)

------------ Expression Number 4 ------------
-- Handle the factorial operator
factorial :: Integer -> Maybe Integer
factorial n
  | n < 0     = Nothing
  | n == 0    = Just 1
  | otherwise = fmap (n *) (factorial (n - 1))

------------ Expression Number 5 ------------
-- Make sure the calculator won't fail with too large of an output
upperLimit :: Double
upperLimit = 1e30  -- Example limit, adjust as needed

------------ Expression Number 6 ------------
-- Helper function to format output
formatOutput :: Double -> String
formatOutput num =
    if fromIntegral (floor num) == num
    then show (floor num)  -- Whole number
    else show num          -- Double

------------ Expression Number 7 ------------
isError :: String -> Bool
isError = isPrefixOf "[Error"

------------ Expression Number 8 ------------
delay :: Int -> IO ()
delay ms = threadDelay (ms * 2000)

------------ Expression Number 9 ------------
replace :: String -> String -> String -> String
replace old new = intercalate new . splitOn old

------------ Expression Number 10 ------------
-- Add a horizontal spacer
hSpacer :: Double -> WidgetNode s e
hSpacer size = spacer `styleBasic` [width size]

------------ Expression Number 11 ------------
-- Add a vertical spacer
vSpacer :: Double -> WidgetNode s e
vSpacer size = spacer `styleBasic` [height size]

------------ Expression Number 12 ------------
funModeMarker :: String
funModeMarker = "       !!!Polonsky Mode!!!"

------------ Expression Number 13 ------------
inputLabelStyle :: [TextStyle]
inputLabelStyle = [textColor white, textSize 24, textFont "Bold"]

{-
-----------------------------------------------------------------------------
                    -- GUI Components and Event Handling  --
-----------------------------------------------------------------------------

1.  evaluateExpression :: String -> String

    The evaluateExpression function serves as the core component for processing 
    and evaluating mathematical expressions input by the user. It takes a string 
    as input, tokenizes it into a list of mathematical tokens using the tokenize 
    function, and then evaluates these tokens to a result string with the eval 
    function. This function also checks if the result is within a predefined numeric 
    limit, returning a formatted output or an error message accordingly.

2.  buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent

    The buildUI function is responsible for creating the user interface of a 
    calculator application. It constructs a graphical interface using the Monomer 
    library, which includes input fields to display and enter mathematical expressions, 
    as well as buttons for digits, operators, and functions. The function also dynamically 
    adjusts the UI to indicate when the calculator is in "Polonsky Mode," a special mode 
    for advanced functions.

3.  handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> 
    AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]

    The handleEvent function in the provided code is responsible for processing 
    and responding to various events triggered in the calculator application. 
    It takes the current widget environment, widget node, the application model, 
    and the incoming event as inputs. Depending on the type of event, it can 
    update the calculator's input, perform calculations, toggle between standard 
    and "Polonsky Mode," and handle other actions such as clearing the input or 
    navigating the history of inputs. It also handles the appending of valid inputs 
    to a file and includes a delay for certain events.

4.  main :: IO ()

    The main :: IO() function is the entry point of the Haskell program. It sets up 
    the graphical user interface (GUI) for a calculator application, defines the model 
    and event handling functions, and runs the Monomer framework to start the 
    interactive calculator. The GUI displays a calculator interface with buttons 
    for numbers, operations, and functions, and it responds to user input, updating the 
    calculator's state and displaying results in real-time.

-----------------------------------------------------------------------------
-}

------------ Expression Number 1 ------------
-- Function to evaluate a string expression and make sure the output is not too large
evaluateExpression :: String -> String
evaluateExpression input =
    let tokens = tokenize input
        resultString = eval tokens
    in case readMaybe resultString :: Maybe Double of
        Just result -> if result > upperLimit
                       then "[Error 100]: Too big!"
                       else formatOutput result
        Nothing -> resultString -- If it's not a number, it's an error message

------------ Expression Number 2 ------------
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

------------ Expression Number 3 ------------
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

------------ Expression Number 4 ------------
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
