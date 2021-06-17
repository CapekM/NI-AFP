module Control.Program where

import qualified Data.ByteString.Char8 as C

-- | Value used for computing
type Value = Int

-- | Address for memory addressing (memory storing values of type Value)
type Address = Word

-- | Program labels type
type Label = C.ByteString

-- | Stack computer program's instructions
data Instruction = TA Address -- Push address to stack (shared stack for values and addresses)
                 | TV Value   -- Push value to stack (shared stack for values and addresses)
                 | DR         -- Replace address at the top of stack by value from memory at that address
                 | ST         -- Store value on top of stack to memory to address under the top of stack and pop those
                 | WR         -- Write value from top of stack to output
                 | RD         -- Read value from input to top of stack
                 | AD         -- Add two values from stack and replace them by result
                 | SB         -- Subtract (as above first - second)
                 | MT         -- Multiply (as above)
                 | DI         -- Divide (as above first / second)
                 | JU Label   -- Jump to label
                 | JZ Label   -- Jump to label if zero is on top of stack and remove it (otherwise just remove)
                 deriving (Show, Read, Eq)

-- | Stack computer program
data Program = EOP                        -- End of program
             | Instruction `Then` Program -- Instruction followed by rest of program
             | Label `Marks` Program      -- Marked program by label (for jumping)
             deriving (Show, Read, Eq)

-- Note: This tells how the operators will be treated in infix (right associative + priority)
infixr 0 $.
infixr 0 $:

-- | Sequencing infix operator
($.) :: Instruction -> Program -> Program
($.) = Then

-- | Marking infix operator
($:) :: Label -> Program -> Program
($:) = Marks
