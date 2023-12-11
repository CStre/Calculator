# **Haskell Advanced Calculator**
## *Pseudocode Project Outline*
### Application Model and Events
#### AppModel:
##### newtype AppModel = AppModel { _currentInput :: Text }
* AppModel represents the state of the application. In the context of this calculator application, it specifically keeps track of the user's input at any given moment.
* The model is integral to the operation of the Monomer library, which is used for building the GUI.

#### AppEvent:
##### data AppEvent = AddDigit Char | AddOperation Char | AddFunction String | Calculate | Clear | TimerEvent | NoOp | ClearMem | HistoryUp | HistoryDown | ToggleFun | SetInput String
     deriving (Eq, Show)
* **AddDigit Char:** The Char parameter signifies the digit to be added.
* **AddOperation Char:** Arithmetic operation (like +, -, ×, ÷) is added. The operation is passed as a Char.
* **AddFunction String:** A mathematical function like (sin, cos, log) is added. 
* **Calculate:** Performs the calculation based on the current input or expression.
* **Clear:** Indicates an event to clear the current input or reset the calculator.
* **TimerEvent:** Used for events that are based on a timer, for handling timeouts of error messages.
* **NoOp:** Represents a "no operation" event, essentially an event where nothing happens. This is used when the calculate is writing to the appended file.
* **ClearMem:** Corresponds to clearing stored memory or history in the calculator.
* **HistoryUp and HistoryDown:** These events are used for navigating through the history of calculations.
* **ToggleFun:** This is an event to toggle the special Polonsky Mode in the calculator.
* **SetInput String:** Represents an event to set the calculator's input to a specific string value.

#### Tokenization and Parsing
##### Token:
##### data Token = Num Double | Op Char | Sqrt | Cos | Tan | Sin | Log | Ln | E | Abs | Comma | Pi | Exp | Fact | Mod | ErrorToken String
     deriving Show
* **Num Double:** Represents a numerical value. 
* **Op Char:** Represents an operator, such as *'+', '-', '×', '÷', etc.*
* **Sqrt:** Symbolizes the square root operation. 
* **Cos, Tan, Sin:** Represent the trigonometric functions cosine, tangent, and sine, respectively. 
* **Log, Ln:** Stand for logarithmic functions. Log typically represents a logarithm with a base of 10, whereas Ln is the natural logarithm (base e).
* **E:** Represents the mathematical constant *e (approximately 2.71828)*, which is the base of the ln.
* **Abs:** Represents the absolute value function, used to return the absolute (non-negative) value.
* **Comma:** Used as a delimiter, primarily in functions that require more than one argument, like the logarithm with a specific base (Log) but also one like in the x^(2) function.
* **Pi:** Represents the mathematical constant *π (Pi)*.
* **Exp:** Symbolizes the exponentiation operation. 
* **Fact:** Represents the factorial operation, denoted as *'!'*. This is why I implemented the upperLimit function and the factorial function.
* **Mod:** Represents the modulo operation, denoted as *'%'*. 
* **ErrorToken String:** Used to handle errors in parsing or interpreting the expression. Starting with _[Error xxx]:_

#### Tokenize:
#### tokenize :: String -> [Token]
* The tokenize function takes a String as input and converts it into a list of Token elements, each representing a distinct component of a mathematical expression, such as numbers, operators, and functions. It uses pattern matching and string manipulation to accurately identify and classify each part of the input string into the appropriate token type for further processing in the application.

### Expression Evaluation

#### Evaluator:
#### eval :: [Token] -> String
* The eval function is responsible for evaluating a list of tokens that represent a mathematical expression. It takes a list of Token as input and returns a String as output. The function first parses the expression using parseExpr and then formats the output using formatOutput. It also handles error tokens by returning appropriate error messages in the code.

#### parseExpr :: [Token] -> (Double, [Token])
* The parseExpr function is a key component responsible for parsing and evaluating mathematical expressions represented as a list of tokens. It serves as the entry point to the expression evaluation logic, starting with the highest precedence operation (addition and subtraction in this case) and delegating to more specific parsing functions like parseSum, parseProduct, and parseFactor. 

#### parseSum :: [Token] -> (Double, [Token])
* The parseSum function is designed to parse and evaluate addition and subtraction operations within a mathematical expression. It operates on a list of tokens, identifying and processing tokens that represent addition (+) and subtraction (-) operations according to their precedence in the expression.

#### parseProduct :: [Token] -> (Double, [Token])
* The parseProduct function handles the parsing and evaluation of multiplication and division operations within a mathematical expression. It processes a list of tokens (representing the expression) and identifies multiplication (×), division (÷), and modulo (%) operations.

#### parseFactor :: [Token] -> (Double, [Token])
* The parseFactor function is responsible for parsing and evaluating the lowest level of expressions in a mathematical calculation, “Factors". It handles a variety of inputs, including numbers, parentheses, mathematical constants (like π and e), unary operations (like factorial and square root), and implicit multiplication.

### Storage and History Management

#### appendToFile :: FilePath -> String -> IO ()
* The appendToFile is designed to add a given string input to a specified file, used for maintaining a history of valid inputs in the calculator application. It first checks if the file exists, creating it if it doesn't, and then reads the current contents to determine if the new input is different from the last entry. If the new input is indeed different, it appends this input to the file, ensuring that the history is updated only with unique, consecutive entries.

#### readValidInputs :: FilePath -> IO [String]
* The readValidInputs function reads and retrieves a list of previously entered valid inputs from a specified file. It first checks if the file exists and creates an empty one if it doesn't. Then, it safely opens and reads the file's contents, ensuring proper resource handling with bracket, and returns the inputs as a list of strings.

#### removePolonskyMode :: String -> String
* The removePolonskyMode function is a utility function designed to modify the calculator's input string by removing a specific marker, identified here as "Polonsky Mode". This function is used to clean up the input string before it is evaluated or stored in the application's history, ensuring that any special mode indicators do not interfere with the calculation logic or the recording of inputs.

#### navigateHistory :: [String] -> String -> Bool -> String
* The navigateHistory function manages user navigation through the input history of the calculator application. It takes a list of historical inputs, the current input, and a Boolean indicating navigation direction (up or down through the history). This function then calculates the new index based on the current position and the navigation direction, and returns the corresponding historical input, enabling users to easily access and reuse their previous entries.

### Helper Functions

#### parseLogOrLnWithBase :: (Double -> Double -> Double) -> Double -> [Token] -> (Double, [Token])
* The parseLogOrLnWithBase function parses and evaluates logarithmic expressions with a specified base, such as log(x, y) or ln(x). It takes a function f representing the logarithm computation, a default base (like 10 for log or e for ln), and a list of tokens that represent a mathematical expression. The function intelligently handles various cases, including the presence or absence of a base in the expression, and errors such as missing parentheses or commas.

#### parseSqrtWithBase :: [Token] -> (Double, [Token])
* The parseSqrtWithBase function is designed to parse square root expressions with an optional base from a list of tokens. It handles two formats: a regular square root (√(x)) and a nth root with a specified base (√(n,x)). This function uses pattern matching to interpret the tokens, extract the numerical values for the base and the number under the root (if provided), and computes the appropriate root value, returning the result along with any remaining unparsed tokens.

#### parseImplicitMultiplication :: Double -> [Token] -> (Double, [Token])
* The parseImplicitMultiplication function is designed to handle implicit multiplication in mathematical expressions. It takes a previously calculated result and a list of tokens (representing the remaining part of the expression) as inputs. If the next token is a number, it multiplies this number with the previously calculated result, effectively handling cases where an explicit multiplication operator is omitted (e.g., 2(3) being interpreted as 2 * 3)

#### factorial :: Integer -> Maybe Integer
* The factorial function calculates the factorial of a given integer. It recursively multiplies the number by the factorial of the number minus one, continuing this process until it reaches zero. The function gracefully handles negative input by returning Nothing

#### upperLimit :: Double
* The upperLimit function sets a threshold for the maximum value that the calculator can handle. It is defined as a constant (upperLimit :: Double) with a value of 1e30.

#### formatOutput :: Double -> String
* The formatOutput function is designed to format the output of a calculator operation. It checks whether the result is a whole number and, if so, it displays it as an integer (without a decimal point). In cases where the result is not a whole number, it displays it as a floating-point number.

#### isError :: String -> Bool
* The isError function is a utility function used to check if a given String represents an error message. It takes a String as input and returns a Bool. This function works by checking if the input string starts with the specific error message prefix "[Error", using the isPrefixOf function from the Data.List module, which is a standard approach for string pattern matching in Haskell.

#### delay :: Int -> IO ()
* The delay function is another utility function for introducing a delay or pause in the program's execution. Specifically, it uses threadDelay, which suspends the current thread for a specified number of milliseconds. In this context, delay ms pauses the execution for ms milliseconds, allowing for timed behaviors in the application, such as temporary display of messages or controlled pacing of certain actions.

#### replace :: String -> String -> String -> String
* The replace function is for replacing a specific substring within a string with another substring. It takes three arguments: the substring to be replaced (old), the substring to replace with (new), and the original string. The function works by splitting the original string at occurrences of the old substring and then intercalating (joining) these split parts with the new substring.

#### hSpacer :: Double -> WidgetNode s e
* The hSpacer function is designed to create a horizontal spacer widget within the graphical user interface of the calculator application. It takes a Double value as input, representing the width of the spacer in pixels. This function is primarily used to add horizontal spacing between UI elements, such as buttons, in the application's layout.

#### vSpacer :: Double -> WidgetNode s e
* Same as the above function but creates spacing along a vertical axis.

#### funModeMarker :: String
* The funModeMarker is a string constant defined as " !!!Polonsky Mode!!!". This marker is used to identify the special mode within the calculator application, referred to as "Polonsky Mode". When this mode is active, the calculator's behavior or display changes, and the presence of this marker in the user input or application state is used to trigger or indicate this special mode.

#### inputLabelStyle :: [TextStyle]
* The inputLabelStyle is not a function but a list of TextStyle settings defined for styling text elements in the GUI. This function was intended to be implemented to style the calculator input but after many hours of trial and error, I decided to leave it an example of the structure I was working towards. 
GUI Components and Event Handling 

#### evaluateExpression :: String -> String
* The evaluateExpression function serves as the core component for processing and evaluating mathematical expressions input by the user. It takes a string as input, tokenizes it into a list of mathematical tokens using the tokenize function, and then evaluates these tokens to a result string with the eval function. This function also checks if the result is within a predefined numeric limit, returning a formatted output or an error message accordingly.

#### buildUI :: WidgetEnv AppModel AppEvent -> AppModel -> WidgetNode AppModel AppEvent
* The buildUI function is responsible for creating the user interface of a calculator application. It constructs a graphical interface using the Monomer library, which includes input fields to display and enter mathematical expressions, as well as buttons for digits, operators, and functions. The function also dynamically adjusts the UI to indicate when the calculator is in "Polonsky Mode," a special mode for advanced functions.

#### handleEvent :: WidgetEnv AppModel AppEvent -> WidgetNode AppModel AppEvent -> AppModel -> AppEvent -> [AppEventResponse AppModel AppEvent]
* The handleEvent function in the provided code is responsible for processing and responding to various events triggered in the calculator application. It takes the current widget environment, widget node, the application model, and the incoming event as inputs. Depending on the type of event, it can update the calculator's input, perform calculations, toggle between standard and "Polonsky Mode," and handle other actions such as clearing the input or navigating the history of inputs. It also handles the appending of valid inputs to a file and includes a delay for certain events.

#### main :: IO ()
* The main :: IO() function is the entry point of the Haskell program. It sets up the graphical user interface (GUI) for a calculator application, defines the model and event handling functions, and runs the Monomer framework to start the interactive calculator. The GUI displays a calculator interface with buttons for numbers, operations, and functions, and it responds to user input, updating the calculator's state and displaying results in real-time.
