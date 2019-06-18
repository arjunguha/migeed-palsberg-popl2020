{-# Language ViewPatterns #-}
{-# Language LambdaCase #-}

module Maximality where
import Control.Monad (guard)
import Data.Maybe (isJust)
import Lang
import TypeCheck
import Data.Monoid (First (..), mconcat)
import Data.Maybe
import Counting


first :: [Maybe a] -> Maybe a
first = getFirst . mconcat . map First


findWellTypedMigrationsAtDepth :: Int -> Expr -> Env -> [Expr]
findWellTypedMigrationsAtDepth n t env
  | not $ type_checks t env = 
    []
  | n > 0 = 
    concat 
    [ findWellTypedMigrationsAtDepth (n-1) s env 
    | s <- get_the_next_term t 
    ]
  | otherwise = 
    [ t ]

    -- | Returns the maximal migration closest to the term.
findWellTypedMigrationsLimited :: Int -> Expr -> Env -> [Expr]
findWellTypedMigrationsLimited d t env =
  concat [ findWellTypedMigrationsAtDepth n t env | n <- [0..d]]

-- | Returns the maximal migration closest to the term.
findWellTypedMigrations :: Expr -> Env -> [Expr]
findWellTypedMigrations t env =
  findWellTypedMigrationsLimited (migration_limit t env * count_types t) t env
      

-- | finds ALL maximal migration in exactly depth N, if they exists.
findMaximalMigrationsAtDepth :: Int -> Expr -> Env -> [Expr]
findMaximalMigrationsAtDepth n t env =
  [ t' | t' <- findWellTypedMigrationsAtDepth n t env, ismaximal t' env]


-- | Returns the maximal migration closest to the term.
findAllMaximalMigrations :: Expr -> Env -> [Expr]
findAllMaximalMigrations t env =
  concat [ findMaximalMigrationsAtDepth n t env | n <- [0..maximalNumberOfSteps+1]]
  where
    maximalNumberOfSteps = 
      (migration_limit t env)  * (count_types t)


-- | finds the maximal migration in exactly depth N, if it exists.
findMaximalMigration :: Int -> Expr -> Env -> Maybe Expr
findMaximalMigration n t env =
  case findMaximalMigrationsAtDepth n t env of
    x:_ -> Just x
    [] -> Nothing

-- | Returns the maximal migration closest to the term.
closestMaximalMigration :: Expr -> Env -> Maybe Expr
closestMaximalMigration t env =
    case findAllMaximalMigrations t env of
    x:_ -> Just x
    [] -> Nothing

--for a given level in the lattice, check if anything is maximal
is_any_term_maximal :: [Expr] -> Env -> Maybe Expr
is_any_term_maximal [] _ = Nothing
is_any_term_maximal (x:xs) env = case (ismaximal x env) of
    True -> Just x
    False -> (is_any_term_maximal xs env)

--is this current migration maximal?
ismaximal :: Expr -> Env -> Bool
ismaximal e env = not (try_all_better_terms e env)

--make sure better terms do not type-check
-- False if any better term type-checks (one level only)
try_all_better_terms :: Expr -> Env -> Bool
try_all_better_terms e env =
    any 
    (\e' -> type_checks e' env) 
    (get_the_next_term e)

--does this type-check?
type_checks :: Expr -> Env -> Bool
type_checks e env = 
    case (typecheck e env) of
          Just t -> True
          otherwise -> False

--get the next more precise types (one level up the lattice)
get_the_next_type :: Vtype -> [Vtype]
get_the_next_type Tdyn = [(Tdyn ~> Tdyn), Tint, Tbool]
get_the_next_type (Tfun t1 t2) = 
     [(Tfun s t2) | s <- (get_the_next_type t1)] ++
     [(Tfun t2 s) | s <- (get_the_next_type t2)] 
get_the_next_type _ = []

--get the better term one level up the lattice
get_the_next_term :: Expr -> [Expr]
get_the_next_term (Lam typ x e) = 
    [(Lam s x e) | s <- (get_the_next_type typ)] ++
    [(Lam typ x e') | e' <- (get_the_next_term e)]

get_the_next_term (App e1 e2) = 
    [(App e1' e2) | e1' <- (get_the_next_term e1)] ++
    [(App e1 e2') | e2' <- (get_the_next_term e2)]
get_the_next_term _ = []

get_the_next_well_typed_term :: Expr -> Env -> [Expr]
get_the_next_well_typed_term e env = 
    [trm | trm <- (get_the_next_term e), (type_checks trm env)]


type_test_succ = do
  let app_y = (Lam (Tdyn ~> Tdyn) "y" (Vv "x"))
  let app_x = (App app_y (Vv "x"))
  let app_xx = (App app_x (Vv "x"))
  let lam = (Lam Tdyn "x" app_xx)
  print(get_the_next_well_typed_term lam tenv)


test_printer = do
  -- let app_5 = (App (Lam Tdyn "x" (Vv "x")) (Vi 5))

  -- let app_f_5 = (App (Vv "f") app_5)

  -- let lam_z = (Lam Tdyn "z"  app_f_5 )

  -- let app_f_true = (App (Vv "f") (Vb True))

  -- let lam_f = (Lam Tdyn "f" (App lam_z app_f_true))

  -- let lam_y = (Lam Tdyn "y" (Vv "y"))

  -- let my_app = (App  lam_f lam_y)



  let my_succ = (App (Vv "succ") (App (Lam Tdyn "y" (Vv "y")) 
                (App (Lam Tdyn "x" (Vv "x")) (Vb True)) ) )


  print(my_succ)
  print(findWellTypedMigrations my_succ tenv)
  -- print(closestMaximalMigration my_app tenv)





