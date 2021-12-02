import Data.Char
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map


type Var = String
type Val = Int

type Store = Map Var Val

data Operation = LoadVal Val | WriteVar Var | ReadVar Var | Add | Multiply | Subtract | Divide | ReturnVal

type ByteCode = [Operation]
type Stack = [Val]

runByteCodeRecursion :: ByteCode -> Stack -> Store -> Maybe Val
runByteCodeRecursion byteCode stack store
  | null byteCode = Nothing
  | otherwise = aux byteCode stack store
  where
    insertVarInStore var stack store = Map.insert var (head stack) store
    readVarFromStore var stack store = (maybeToList(Map.lookup var store)) ++ stack

    sumFirstTwo [] = []
    sumFirstTwo (x:y:rest) = (x + y) : rest

    subtractFirstTwo [] = []
    subtractFirstTwo (x:y:rest) = (y - x) : rest

    multiplyFirstTwo [] = []
    multiplyFirstTwo (x:y:rest) = (x * y) : rest

    divideFirstTwo [] = []
    divideFirstTwo (x:y:rest) =  ( y `div` x) : rest

    aux  (ReturnVal:_) [] _ = Nothing
    aux  (ReturnVal:_) stack _ = Just (head stack)
    aux (currentOperation:remainingByteCode) stack store = case currentOperation of LoadVal val -> runByteCodeRecursion remainingByteCode (val:stack) store
                                                                                    WriteVar var -> runByteCodeRecursion remainingByteCode (tail stack) (insertVarInStore var stack store)
                                                                                    ReadVar var ->  runByteCodeRecursion remainingByteCode (readVarFromStore var stack store) store
                                                                                    Add -> runByteCodeRecursion remainingByteCode (sumFirstTwo stack) store
                                                                                    Subtract -> runByteCodeRecursion remainingByteCode ( subtractFirstTwo stack) store
                                                                                    Multiply -> runByteCodeRecursion remainingByteCode ( multiplyFirstTwo stack) store
                                                                                    Divide -> runByteCodeRecursion remainingByteCode ( divideFirstTwo stack) store


runByteCode :: ByteCode -> Maybe Val
runByteCode byteCode = runByteCodeRecursion byteCode [] Map.empty
