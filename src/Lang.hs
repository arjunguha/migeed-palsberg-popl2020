{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
module Lang where

import Control.DeepSeq
import GHC.Generics

import Data.Data
import Data.List as List
import Data.Map as Map
import Data.Set as Set

type Name = String

data Vtype = Tint
           | Tbool
           | Tdyn
           | Tfun Vtype Vtype
           deriving (Eq, Data, Generic, NFData)

infixr 7 ~>
(~>) = Tfun

data Expr = Vi Int
          | Vb Bool
          | Vv Name
          | App Expr Expr
          | Lam Vtype Name Expr
          deriving (Eq, Data, Generic, NFData)

type Env = [(Name,Vtype)]


--Just to compare the examples
tenv =  [("succ", Tint ~> Tint), ("+", Tint ~> Tint ~> Tint)] 


-- printing
instance Show Vtype where
    showsPrec n = \case
        Tint  -> showString "int"
        Tbool -> showString "bool" 
        Tdyn -> showString "any"
        Tfun a b -> showParen (n > fun_prec) (
            showsPrec (fun_prec + 1) a 
            . showString " -> " 
            . showsPrec fun_prec b
            )
        where fun_prec = 9  

instance Show Expr where
    showsPrec n = \case
        Vi i -> shows i
        Vb True -> showString "true"
        Vb False -> showString "false"
        Vv x -> showString x
        Lam typ x term -> showParen (n > lam_prec) (
            showString "fun " 
            .showString x 
            .showString " : "
            . showsPrec 0 typ
            .showString " . " 
            . showsPrec lam_prec term
            )
        App (App (Vv "+") e1) e2 -> showParen (n > app_prec) (
            showsPrec (app_prec + 1) e1
            .showString " + "
            .showsPrec (app_prec + 1) e2
            )
        App e1 e2 -> showParen (n > fun_prec) (
            showsPrec fun_prec e1
            . showString " " 
            . showsPrec (fun_prec + 1) e2
            )

        where 
            lam_prec = 9
            fun_prec = 11
            app_prec = 10
