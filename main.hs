
-- some names so that every assignment can have some symbol.
-- obviously quite constrained for this toy.
data Var = A | B | C | D | E | F | G deriving (Show, Eq)

data Value = Value Var Bool deriving (Show, Eq)

-- a list of values, which are implicitly OR'd together
-- because the whole thing works on CNF or a conjuction of such clauses
data Clause = Clause [Value] deriving (Show, Eq)

-- a conjunction of clauses, which are implicitly AND'd together
-- because the whole thing works on CNF or a conjuction of internally disjoint clauses.
data Problem = Problem [Clause] deriving (Show, Eq)


main :: IO ()
main = do
    let problem = Problem [
            Clause [Value A True, Value B True, Value C False],
            Clause [Value A False, Value B True, Value C True],
            Clause [Value A True, Value B False, Value C True],
            Clause [Value A True, Value B True, Value C True]]
    putStrLn(show(problem))
