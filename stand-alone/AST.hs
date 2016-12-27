module AST
  ( TopDeclaration (..)
  , Value   (..)
  , Expr    (..)
  , Guard   (..)
  , Pattern (..)
  ) where

import Common

data TopDeclaration
  = Value'    Value
  | Datatype  Location Name [Name] [(Location, Name, [Expr])]
  | Typeclass Location
  deriving Show

instance Locate TopDeclaration where
  locate a = case a of
    Value' x -> locate x
    Datatype x _ _ _ -> x
    Typeclass x -> x

data Value = Value
  { loc       :: Location
  , name      :: Name
  , contract  :: Maybe Expr
  , arguments :: [Name]
  , body      :: Expr
  } deriving Show

instance Locate Value where locate = loc

type L = Location

data Expr
  = LitUnit       L
  | VarValue      L Name
  | VarType       L Name
  | Apply         L Expr Expr
  | ApplyContract L Expr Expr
  | Lambda        L Name Expr
  | Where         L Expr [Value]
  | Intrinsic     L Name
  | If            L Expr Expr Expr
  | Case          L Expr [(Pattern, Guard)]
  deriving Show

instance Locate Expr     where
  locate a = case a of
    LitUnit       x -> x
    VarValue      x _ -> x
    VarType       x _ -> x
    Apply         x _ _ -> x
    ApplyContract x _ _ -> x
    Lambda        x _ _ -> x
    Where         x _ _ -> x
    Intrinsic     x _ -> x
    If            x _ _ _ -> x
    Case          x _ _ -> x
    
data Pattern
  = Wildcard     L
  | PatternVar   L Name
  | PatternApply L Pattern Pattern
  | As           L Name Pattern
  deriving Show

instance Locate Pattern where
  locate a = case a of
    Wildcard     x -> x
    PatternVar   x _ -> x
    PatternApply x _ _ -> x
    As           x _ _ -> x

data Guard
  = Unguarded L Expr
  | Guard     L Expr Expr
  | Guard'    L Expr Expr Guard
  deriving Show

instance Locate Guard where
  locate a = case a of
    Unguarded x _ -> x
    Guard     x _ _ -> x
    Guard'    x _ _ _ -> x

