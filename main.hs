import qualified Data.Set as Set
-- some names so that every assignment can have some symbol.
-- obviously quite constrained for this toy.
data Var = A | B | C | D | E | F | G deriving (Show, Eq, Ord)

data Value = Value Var Bool deriving (Show, Eq)

-- a list of values, which are implicitly OR'd together
-- because the whole thing works on CNF or a conjuction of such clauses
data Clause = Clause [Value] deriving (Show, Eq)

-- a conjunction of clauses, which are implicitly AND'd together
-- because the whole thing works on CNF or a conjuction of internally disjoint clauses.
data Problem = Problem [Clause] deriving (Show, Eq)

-- for any assignment of values we should be able to determine
-- whether it satisfies the problem or not.
data Assignment = Assignment [Value] deriving (Show, Eq)

varSet :: Problem -> Set.Set Var
varSet (Problem clauses) = (Set.fromList (map (\(Value v _) -> v) (concatMap (\(Clause values) -> values) clauses)))

satisfiesValue :: Assignment -> Value -> Bool
satisfiesValue (Assignment candidateValues) (Value name v) = any (\(Value name' v') -> name == name' && v == v') candidateValues 

satisfiesClause :: Assignment -> Clause ->  Bool
satisfiesClause assignment (Clause values) = any (satisfiesValue assignment) values

satisfies :: Problem -> Assignment -> Bool
satisfies (Problem clauses) assignment = all (satisfiesClause assignment) clauses

enumerateAssignments :: [Var] -> [Assignment]
enumerateAssignments vars = map Assignment (sequence [[Value v True, Value v False] | v <- vars])

solve :: Problem -> [Assignment]
solve problem = filter (satisfies problem) (enumerateAssignments (Set.toList (varSet problem)))

main :: IO ()
main = do
    let problem = Problem [
            Clause [Value A True, Value B True, Value C False],
            Clause [Value A False, Value B True, Value C True],
            Clause [Value A True, Value B False, Value C True],
            Clause [Value A False, Value B False, Value C False]]
    putStrLn(show(problem))
    let a1 = Assignment [Value A True, Value B True, Value C True]
    let a2 = Assignment [Value A True, Value B True, Value C False]
    let a1Solve = satisfies problem a1
    let a2Solve = satisfies problem a2
    putStrLn(show(a1Solve))
    putStrLn(show(a2Solve))
    let solution = (head (take 1 (solve problem)))
    putStrLn(show(solution))
