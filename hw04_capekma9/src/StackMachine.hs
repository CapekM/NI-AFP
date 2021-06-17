module StackMachine where

import qualified Data.Map as Map
import qualified Data.Sequence as Seq
import qualified Data.Stack as Stack

import Control.Program

-- | Input is sequence of values (type Value is defined in Control.Program)
type Input = Seq.Seq Value
-- | Output is sequence of values (type Value is defined in Control.Program)
type Output = Seq.Seq Value

-- | Memory of computer stores on address of type Address a value of type Value (see Control.Program)
type Memory = Map.Map Address Value
-- | Lookup directory of subprograms (labels are marking starts of parts of a program where you can jump with JU or JZ)
type SubprogramDir = Map.Map Label Program
-- | Computer stack can store addresses and values
type ComputerStack = Stack.Stack (Either Address Value)


-- | Run program with given input (memory and stack should be empty at start)
-- | If there is a problem, error is raised ("Empty stack", "Not value", "Not address", "No input", "Unknown label", "Division by 0", "Uninitialized memory"), see tests
-- TODO: implement running the program
runProgram :: Program -> Input -> Output
runProgram p i = run_instructions Stack.empty Map.empty Map.empty p i Seq.empty

-- Feel free to create more helper functions
run_instructions :: ComputerStack -> Memory -> SubprogramDir -> Program -> Input -> Output -> Output
run_instructions _ _ _ EOP _ out = out
run_instructions s mem sub (l `Marks` p) inp out = run_instructions s mem (Map.insert l p sub) p inp out
run_instructions s mem sub (i `Then` p) inp out = case i of
    (TA a) -> run_instructions (Stack.push (Left a) s) mem sub p inp out
    (TV v) -> run_instructions (Stack.push (Right v) s) mem sub p inp out
    WR -> case (get_stack_value s) of
        Right v -> run_instructions (Stack.pop s) mem sub p inp (out Seq.|> v)
        Left e -> error e
    RD -> case (check_input inp) of
        Right v -> run_instructions (Stack.push (Right v) s) mem sub p (Seq.drop 1 inp) out
        Left e -> error e
    AD -> case (help_add s) of
        Right v -> run_instructions (Stack.push (Right v) (Stack.pop (Stack.pop s))) mem sub p inp out 
        Left e -> error e
    SB -> case (help_sub s) of
        Right v -> run_instructions (Stack.push (Right v) (Stack.pop (Stack.pop s))) mem sub p inp out 
        Left e -> error e
    MT -> case (help_mul s) of
        Right v -> run_instructions (Stack.push (Right v) (Stack.pop (Stack.pop s))) mem sub p inp out 
        Left e -> error e
    DI -> case (help_div s) of
        Right v -> run_instructions (Stack.push (Right v) (Stack.pop (Stack.pop s))) mem sub p inp out 
        Left e -> error e
    DR -> case (get_stack_address s) of
        Left e -> error e
        Right adr -> case get_value_from_memory adr mem of
            Right val -> run_instructions (Stack.push (Right val) (Stack.pop s)) mem sub p inp out
            Left e -> error e
        where
            get_value_from_memory :: Address -> Memory -> Either String Value
            get_value_from_memory a m = case (Map.lookup a m) of
                Just v -> Right v
                Nothing -> Left "Uninitialized memory"
    ST -> case (get_stack_value s) of
        Left e -> error e
        Right v -> case get_stack_address (Stack.pop s) of
            Right adr -> run_instructions (Stack.pop (Stack.pop s)) (Map.insert adr v mem) sub p inp out
            Left e -> error e
    (JU l) -> case (get_program l new_sub) of
        Left e -> error e
        Right v -> run_instructions s mem new_sub v inp out
        where new_sub = find_sub sub l p

    (JZ l) -> case (get_stack_value s) of
        Left e -> error e
        Right 0 -> case (get_program l new_sub) of
            Left e -> error e
            Right prog -> run_instructions (Stack.pop s) mem new_sub prog inp out
        Right _ -> run_instructions (Stack.pop s) mem sub p inp out
        where new_sub = find_sub sub l p

get_program :: Label -> SubprogramDir -> Either String Program
get_program l sub = case (Map.lookup l sub) of
    Just v -> Right v
    Nothing -> Left "Unknown label"

find_sub :: SubprogramDir -> Label -> Program -> SubprogramDir
find_sub sub l EOP = sub
find_sub sub l (_ `Then` p) = find_sub sub l p
find_sub sub l (lbl `Marks` p)
    | (Map.member l sub) = sub
    | l == lbl = Map.insert lbl p sub
    | otherwise = find_sub nextD l p
        where 
            nextD = Map.insert lbl p sub

help_div :: ComputerStack -> Either String Value
help_div s = case (get_two_values s) of
            Right (a, 0) -> Left "Division by 0"
            Right (a, b) -> Right (a `div` b)
            Left e -> Left e

help_mul :: ComputerStack -> Either String Value
help_mul s = case (get_two_values s) of
            Right (a, b) -> Right (a * b)
            Left e -> Left e

help_sub :: ComputerStack -> Either String Value
help_sub s = case (get_two_values s) of
            Right (a, b) -> Right (a - b)
            Left e -> Left e

help_add :: ComputerStack -> Either String Value
help_add s = case (get_two_values s) of
            Right (a, b) -> Right (a + b)
            Left e -> Left e

get_two_values :: ComputerStack -> Either String (Value, Value)
get_two_values s = case get_stack_value s of
                    Left e -> Left e
                    Right val1 -> case get_stack_value (Stack.pop s) of
                        Left e -> Left e
                        Right val2 -> Right (val1, val2)

get_stack_address :: ComputerStack -> Either String Address
get_stack_address s = case (Stack.top s) of
            Left v -> Right v
            _       -> Left "Not address"

get_stack_value :: ComputerStack -> Either String Value
get_stack_value s = case (Stack.top s) of
            Right v -> Right v
            _       -> Left "Not value"

check_input :: Input -> Either String Value
check_input i = case (Seq.viewl i) of
            (v Seq.:< _) -> Right v
            Seq.EmptyL   -> Left "No input"
