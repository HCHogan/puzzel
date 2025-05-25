{-# LANGUAGE OrPatterns #-}

module Core.Syntax where

type Name = String

type IsRec = Bool

type CoreExpr = Expr Name

data Expr a
  = EVar Name
  | ENum Int
  | EAp (Expr a) (Expr a)
  | EConstr Int Int
  | ELet IsRec
  | ECase (Expr a) [Alter a]
  | ELam [a] (Expr a)
  deriving (Show, Eq)

type Alter a = (Int, [a], Expr a)

type CoreAlt = Alter Name

bindersOf :: [(a, b)] -> [a]
bindersOf = map fst

rhssOf :: [(a, b)] -> [b]
rhssOf = map snd

isAtomicExpr :: Expr a -> Bool
isAtomicExpr ((EVar _); (ENum _)) = True
isAtomicExpr _ = False

type Program a = [ScDefn a]

type CoreProgram = Program Name

type ScDefn a = (Name, [a], Expr a)

type CoreScDefn = ScDefn Name

preludeDefs :: CoreProgram
preludeDefs =
  [ ("I", ["x"], EVar "x"),
    ("K", ["x", "y"], EVar "x"),
    ("K1", ["x", "y"], EVar "y"),
    ( "S",
      ["f", "g", "x"],
      EAp
        (EAp (EVar "f") (EVar "x"))
        (EAp (EVar "g") (EVar "x"))
    ),
    ( "compose",
      ["f", "g", "x"],
      EAp
        (EVar "f")
        (EAp (EVar "g") (EVar "x"))
    ),
    ( "twice",
      ["f"],
      EAp
        (EAp (EVar "compose") (EVar "f"))
        (EVar "f")
    )
  ]

