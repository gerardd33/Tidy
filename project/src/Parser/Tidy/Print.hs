{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for Parser.
--   Generated by the BNF converter.

module Parser.Tidy.Print where

import qualified Parser.Tidy.Abs
import Data.Char

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 (map ($ "") $ d []) "" where
  rend i ss = case ss of
    "["      :ts -> showChar '[' . rend i ts
    "("      :ts -> showChar '(' . rend i ts
    "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
    "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
    "}"      :ts -> new (i-1) . showChar '}' . new (i-1) . rend (i-1) ts
    [";"]        -> showChar ';'
    ";"      :ts -> showChar ';' . new i . rend i ts
    t  : ts@(p:_) | closingOrPunctuation p -> showString t . rend i ts
    t        :ts -> space t . rend i ts
    _            -> id
  new i     = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc
  prtList :: Int -> [a] -> Doc
  prtList i = concatD . map (prt i)

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt = prtList

instance Print Char where
  prt _ s = doc (showChar '\'' . mkEsc '\'' s . showChar '\'')
  prtList _ s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q s = case s of
  _ | s == q -> showChar '\\' . showChar s
  '\\'-> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  _ -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Parser.Tidy.Abs.UpperCaseIdent where
  prt _ (Parser.Tidy.Abs.UpperCaseIdent i) = doc $ showString $ i

instance Print Parser.Tidy.Abs.LowerCaseIdent where
  prt _ (Parser.Tidy.Abs.LowerCaseIdent i) = doc $ showString $ i

instance Print Parser.Tidy.Abs.Program where
  prt i e = case e of
    Parser.Tidy.Abs.ProgramEntrypoint classdecls -> prPrec i 0 (concatD [prt 0 classdecls])

instance Print [Parser.Tidy.Abs.ClassIdent] where
  prt = prtList

instance Print Parser.Tidy.Abs.ClassIdent where
  prt i e = case e of
    Parser.Tidy.Abs.CIdent uppercaseident -> prPrec i 0 (concatD [prt 0 uppercaseident])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [Parser.Tidy.Abs.ClassDecl] where
  prt = prtList

instance Print Parser.Tidy.Abs.ClassDecl where
  prt i e = case e of
    Parser.Tidy.Abs.ClassDeclConcrete classtype classident inheritance classbody -> prPrec i 0 (concatD [prt 0 classtype, doc (showString "class"), prt 0 classident, prt 0 inheritance, prt 0 classbody])
    Parser.Tidy.Abs.ClassDeclAbstract classtype classident inheritance classbody -> prPrec i 0 (concatD [doc (showString "abstract"), prt 0 classtype, doc (showString "class"), prt 0 classident, prt 0 inheritance, prt 0 classbody])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.Tidy.Abs.Inheritance where
  prt i e = case e of
    Parser.Tidy.Abs.SuperclassAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.SuperclassPresent classident -> prPrec i 0 (concatD [doc (showString "extends"), prt 0 classident])

instance Print Parser.Tidy.Abs.ClassBody where
  prt i e = case e of
    Parser.Tidy.Abs.ClassBodyEmpty -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.ClassBodyFilled valuessection variablessection functionssection actionssection -> prPrec i 0 (concatD [doc (showString "{"), prt 0 valuessection, prt 0 variablessection, prt 0 functionssection, prt 0 actionssection, doc (showString "}")])

instance Print Parser.Tidy.Abs.ClassType where
  prt i e = case e of
    Parser.Tidy.Abs.MMutable -> prPrec i 0 (concatD [doc (showString "mutable")])
    Parser.Tidy.Abs.MImmutable -> prPrec i 0 (concatD [doc (showString "immutable")])
    Parser.Tidy.Abs.MSingleton -> prPrec i 0 (concatD [doc (showString "singleton")])

instance Print Parser.Tidy.Abs.ValuesSection where
  prt i e = case e of
    Parser.Tidy.Abs.ValuesAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.ValuesPresent valsbody -> prPrec i 0 (concatD [doc (showString "values:"), prt 0 valsbody])

instance Print Parser.Tidy.Abs.VariablesSection where
  prt i e = case e of
    Parser.Tidy.Abs.VariablesAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.VariablesPresent varsbody -> prPrec i 0 (concatD [doc (showString "variables:"), prt 0 varsbody])

instance Print Parser.Tidy.Abs.FunctionsSection where
  prt i e = case e of
    Parser.Tidy.Abs.FunctionsAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.FunctionsPresent fsbody -> prPrec i 0 (concatD [doc (showString "functions:"), prt 0 fsbody])

instance Print Parser.Tidy.Abs.ActionsSection where
  prt i e = case e of
    Parser.Tidy.Abs.ActionsAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.ActionsPresent asbody -> prPrec i 0 (concatD [doc (showString "actions:"), prt 0 asbody])

instance Print Parser.Tidy.Abs.ValSBody where
  prt i e = case e of
    Parser.Tidy.Abs.ValuesSBody valuedecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 valuedecls, doc (showString "}")])

instance Print Parser.Tidy.Abs.ValueIdent where
  prt i e = case e of
    Parser.Tidy.Abs.VIdent lowercaseident -> prPrec i 0 (concatD [prt 0 lowercaseident])

instance Print Parser.Tidy.Abs.ValueType where
  prt i e = case e of
    Parser.Tidy.Abs.ValueTypeClass classident -> prPrec i 0 (concatD [prt 0 classident])
    Parser.Tidy.Abs.ValueTypeGeneric classident classidents -> prPrec i 0 (concatD [prt 0 classident, doc (showString "["), prt 0 classidents, doc (showString "]")])
    Parser.Tidy.Abs.ValueTypeFunction methodtype -> prPrec i 0 (concatD [doc (showString "get"), prt 0 methodtype])
    Parser.Tidy.Abs.ValueTypeAction methodtype -> prPrec i 0 (concatD [doc (showString "do"), prt 0 methodtype])

instance Print [Parser.Tidy.Abs.ValueDecl] where
  prt = prtList

instance Print Parser.Tidy.Abs.ValueDecl where
  prt i e = case e of
    Parser.Tidy.Abs.PublicValueDecl valuedeclproper -> prPrec i 0 (concatD [prt 0 valuedeclproper, doc (showString ";")])
    Parser.Tidy.Abs.PrivateValueDecl valuedeclproper -> prPrec i 0 (concatD [doc (showString "private"), prt 0 valuedeclproper, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [Parser.Tidy.Abs.ValueDeclProper] where
  prt = prtList

instance Print Parser.Tidy.Abs.ValueDeclProper where
  prt i e = case e of
    Parser.Tidy.Abs.UninitializedValue valueident valuetype -> prPrec i 0 (concatD [prt 0 valueident, doc (showString ":"), prt 0 valuetype])
    Parser.Tidy.Abs.InitializedValue valueident valuetype expr -> prPrec i 0 (concatD [prt 0 valueident, doc (showString ":"), prt 0 valuetype, doc (showString "="), prt 0 expr])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Parser.Tidy.Abs.VarSBody where
  prt i e = case e of
    Parser.Tidy.Abs.VariablesSBody valuedecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 valuedecls, doc (showString "}")])

instance Print Parser.Tidy.Abs.FSBody where
  prt i e = case e of
    Parser.Tidy.Abs.FSBodyEmpty -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.FSBodyFilled functiondecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 functiondecls, doc (showString "}")])

instance Print Parser.Tidy.Abs.FunctionIdent where
  prt i e = case e of
    Parser.Tidy.Abs.FIdent lowercaseident -> prPrec i 0 (concatD [prt 0 lowercaseident])

instance Print Parser.Tidy.Abs.MethodType where
  prt i e = case e of
    Parser.Tidy.Abs.FType parameterlist valuetype -> prPrec i 0 (concatD [prt 0 parameterlist, doc (showString "->"), prt 0 valuetype])

instance Print Parser.Tidy.Abs.ParameterList where
  prt i e = case e of
    Parser.Tidy.Abs.ParamList valuedeclpropers -> prPrec i 0 (concatD [doc (showString "("), prt 0 valuedeclpropers, doc (showString ")")])

instance Print [Parser.Tidy.Abs.FunctionDecl] where
  prt = prtList

instance Print Parser.Tidy.Abs.FunctionDecl where
  prt i e = case e of
    Parser.Tidy.Abs.OverrideFunctionDecl functionident methodtype functionbody -> prPrec i 0 (concatD [doc (showString "override"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 functionbody])
    Parser.Tidy.Abs.PublicFunctionDecl functionident methodtype functionbody -> prPrec i 0 (concatD [prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 functionbody])
    Parser.Tidy.Abs.PrivateFunctionDecl functionident methodtype functionbody -> prPrec i 0 (concatD [doc (showString "private"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 functionbody])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.Tidy.Abs.FunctionBody where
  prt i e = case e of
    Parser.Tidy.Abs.FunctionBodyOneLine expr -> prPrec i 0 (concatD [prt 0 expr])
    Parser.Tidy.Abs.FunctionBodyMultiLine expr withvalues -> prPrec i 0 (concatD [doc (showString "{"), prt 0 expr, doc (showString "}"), prt 0 withvalues])

instance Print Parser.Tidy.Abs.WithValues where
  prt i e = case e of
    Parser.Tidy.Abs.WithValuesAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.WithValuesPresent valuessection -> prPrec i 0 (concatD [doc (showString "with"), prt 0 valuessection])

instance Print Parser.Tidy.Abs.ASBody where
  prt i e = case e of
    Parser.Tidy.Abs.ASBodyEmpty -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.ASBodyFilled actiondecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 actiondecls, doc (showString "}")])

instance Print [Parser.Tidy.Abs.ActionDecl] where
  prt = prtList

instance Print Parser.Tidy.Abs.ActionDecl where
  prt i e = case e of
    Parser.Tidy.Abs.OverrideActionDecl functionident methodtype actionbody -> prPrec i 0 (concatD [doc (showString "override"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 actionbody])
    Parser.Tidy.Abs.PublicActionDecl functionident methodtype actionbody -> prPrec i 0 (concatD [prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 actionbody])
    Parser.Tidy.Abs.PrivateActionDecl functionident methodtype actionbody -> prPrec i 0 (concatD [doc (showString "private"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 actionbody])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.Tidy.Abs.ActionBody where
  prt i e = case e of
    Parser.Tidy.Abs.ActionBodyOneLine expr -> prPrec i 0 (concatD [prt 0 expr])
    Parser.Tidy.Abs.ActionBodyMultiLine exprs -> prPrec i 0 (concatD [doc (showString "{"), prt 0 exprs, doc (showString "}")])

instance Print [Parser.Tidy.Abs.Expr] where
  prt = prtList

instance Print Parser.Tidy.Abs.Expr where
  prt i e = case e of
    Parser.Tidy.Abs.ELiteral literal -> prPrec i 10 (concatD [prt 0 literal])
    Parser.Tidy.Abs.ELocalValue valueident -> prPrec i 10 (concatD [prt 0 valueident])
    Parser.Tidy.Abs.EGetExpr getexpr -> prPrec i 9 (concatD [prt 0 getexpr])
    Parser.Tidy.Abs.EDoExpr doexpr -> prPrec i 9 (concatD [prt 0 doexpr])
    Parser.Tidy.Abs.ELambdaFunction lambdafunction -> prPrec i 8 (concatD [prt 0 lambdafunction])
    Parser.Tidy.Abs.ELambdaAction lambdaaction -> prPrec i 8 (concatD [prt 0 lambdaaction])
    Parser.Tidy.Abs.ELocalFunctionCall functioncall -> prPrec i 7 (concatD [doc (showString "local"), prt 0 functioncall])
    Parser.Tidy.Abs.ELocalActionCall actioncall -> prPrec i 7 (concatD [doc (showString "local"), prt 0 actioncall])
    Parser.Tidy.Abs.ECtorCall constructorcall -> prPrec i 7 (concatD [prt 0 constructorcall])
    Parser.Tidy.Abs.EImperativeControlFlow imperativecontrolflow -> prPrec i 6 (concatD [prt 0 imperativecontrolflow])
    Parser.Tidy.Abs.EFunctionalControlFlow functionalcontrolflow -> prPrec i 6 (concatD [prt 0 functionalcontrolflow])
    Parser.Tidy.Abs.ELocalValueDecl localvaluedecl -> prPrec i 5 (concatD [prt 0 localvaluedecl])
    Parser.Tidy.Abs.EUnaryNot expr -> prPrec i 4 (concatD [doc (showString "not"), prt 5 expr])
    Parser.Tidy.Abs.EUnaryMinus expr -> prPrec i 4 (concatD [doc (showString "-"), prt 5 expr])
    Parser.Tidy.Abs.EMultiply expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "*"), prt 4 expr2])
    Parser.Tidy.Abs.EDivide expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "/"), prt 4 expr2])
    Parser.Tidy.Abs.EAdd expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "+"), prt 3 expr2])
    Parser.Tidy.Abs.ESubtract expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "-"), prt 3 expr2])
    Parser.Tidy.Abs.EConcatenate expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "++"), prt 3 expr2])
    Parser.Tidy.Abs.ERelationalOperator expr1 relationaloperator expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 0 relationaloperator, prt 2 expr2])
    Parser.Tidy.Abs.EBooleanOperator expr1 booleanoperator expr2 -> prPrec i 0 (concatD [prt 1 expr1, prt 0 booleanoperator, prt 0 expr2])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.Tidy.Abs.Literal where
  prt i e = case e of
    Parser.Tidy.Abs.LInt n -> prPrec i 0 (concatD [prt 0 n])
    Parser.Tidy.Abs.LBool boolean -> prPrec i 0 (concatD [prt 0 boolean])
    Parser.Tidy.Abs.LChar c -> prPrec i 0 (concatD [prt 0 c])
    Parser.Tidy.Abs.LString str -> prPrec i 0 (concatD [prt 0 str])
    Parser.Tidy.Abs.LVoid void -> prPrec i 0 (concatD [prt 0 void])

instance Print Parser.Tidy.Abs.Boolean where
  prt i e = case e of
    Parser.Tidy.Abs.BTrue -> prPrec i 0 (concatD [doc (showString "True")])
    Parser.Tidy.Abs.BFalse -> prPrec i 0 (concatD [doc (showString "False")])

instance Print Parser.Tidy.Abs.Void where
  prt i e = case e of
    Parser.Tidy.Abs.VPass -> prPrec i 0 (concatD [doc (showString "Pass")])

instance Print Parser.Tidy.Abs.LocalValueDecl where
  prt i e = case e of
    Parser.Tidy.Abs.LocalVDecl valuedecl -> prPrec i 0 (concatD [doc (showString "value"), prt 0 valuedecl])

instance Print Parser.Tidy.Abs.LambdaFunction where
  prt i e = case e of
    Parser.Tidy.Abs.LambdaFunctionOneLine parameterlist expr -> prPrec i 0 (concatD [doc (showString "get"), prt 0 parameterlist, doc (showString "->"), prt 0 expr, doc (showString ";")])
    Parser.Tidy.Abs.LambdaFunctionMultiLine parameterlist expr -> prPrec i 0 (concatD [doc (showString "get"), prt 0 parameterlist, doc (showString "->"), doc (showString "{"), prt 0 expr, doc (showString "}")])

instance Print Parser.Tidy.Abs.LambdaAction where
  prt i e = case e of
    Parser.Tidy.Abs.LambdaActionOneLine parameterlist expr -> prPrec i 0 (concatD [doc (showString "do"), prt 0 parameterlist, doc (showString "->"), prt 0 expr, doc (showString ";")])
    Parser.Tidy.Abs.LambdaActionMultiLine parameterlist exprs -> prPrec i 0 (concatD [doc (showString "do"), prt 0 parameterlist, doc (showString "->"), doc (showString "{"), prt 0 exprs, doc (showString "}")])

instance Print Parser.Tidy.Abs.ArgumentList where
  prt i e = case e of
    Parser.Tidy.Abs.ArgListAbsent -> prPrec i 0 (concatD [])
    Parser.Tidy.Abs.ArgListPresent functionarguments -> prPrec i 0 (concatD [doc (showString "("), prt 0 functionarguments, doc (showString ")")])

instance Print [Parser.Tidy.Abs.FunctionArgument] where
  prt = prtList

instance Print Parser.Tidy.Abs.FunctionArgument where
  prt i e = case e of
    Parser.Tidy.Abs.FunctionArg expr -> prPrec i 0 (concatD [prt 0 expr])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print Parser.Tidy.Abs.FunctionCall where
  prt i e = case e of
    Parser.Tidy.Abs.FCall functionident argumentlist -> prPrec i 0 (concatD [doc (showString "."), prt 0 functionident, prt 0 argumentlist])

instance Print Parser.Tidy.Abs.ActionCall where
  prt i e = case e of
    Parser.Tidy.Abs.ACall functionident argumentlist -> prPrec i 0 (concatD [doc (showString "#"), prt 0 functionident, prt 0 argumentlist])

instance Print Parser.Tidy.Abs.ConstructorCall where
  prt i e = case e of
    Parser.Tidy.Abs.CCall classident argumentlist -> prPrec i 0 (concatD [prt 0 classident, prt 0 argumentlist])

instance Print Parser.Tidy.Abs.GetExpr where
  prt i e = case e of
    Parser.Tidy.Abs.GetExprInstance valueident functioncall -> prPrec i 0 (concatD [prt 0 valueident, prt 0 functioncall])
    Parser.Tidy.Abs.GetExprStatic classident functioncall -> prPrec i 0 (concatD [prt 0 classident, prt 0 functioncall])
    Parser.Tidy.Abs.GetExprChain getexpr functioncall -> prPrec i 0 (concatD [prt 0 getexpr, prt 0 functioncall])

instance Print Parser.Tidy.Abs.DoExpr where
  prt i e = case e of
    Parser.Tidy.Abs.DoExprInstance valueident actioncall -> prPrec i 0 (concatD [prt 0 valueident, prt 0 actioncall])
    Parser.Tidy.Abs.DoExprStatic classident actioncall -> prPrec i 0 (concatD [prt 0 classident, prt 0 actioncall])
    Parser.Tidy.Abs.DoExprChain getexpr actioncall -> prPrec i 0 (concatD [prt 0 getexpr, prt 0 actioncall])

instance Print Parser.Tidy.Abs.ImperativeControlFlow where
  prt i e = case e of
    Parser.Tidy.Abs.IWhile expr exprs -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 exprs, doc (showString "}")])
    Parser.Tidy.Abs.IForeach valuedecl expr exprs -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 valuedecl, doc (showString "in"), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 exprs, doc (showString "}")])
    Parser.Tidy.Abs.IIf expr exprs optionalelsebranch -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 exprs, doc (showString "}"), prt 0 optionalelsebranch])

instance Print Parser.Tidy.Abs.OptionalElseBranch where
  prt i e = case e of
    Parser.Tidy.Abs.ElsePresent exprs -> prPrec i 0 (concatD [doc (showString "else"), doc (showString "{"), prt 0 exprs, doc (showString "}")])
    Parser.Tidy.Abs.ElseAbsent -> prPrec i 0 (concatD [])

instance Print Parser.Tidy.Abs.FunctionalControlFlow where
  prt i e = case e of
    Parser.Tidy.Abs.FIfThenElse expr thenbranch elsebranch -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 thenbranch, prt 0 elsebranch])
    Parser.Tidy.Abs.FMatch expr matchcases -> prPrec i 0 (concatD [doc (showString "match"), prt 0 expr, doc (showString "{"), prt 0 matchcases, doc (showString "}")])

instance Print Parser.Tidy.Abs.ThenBranch where
  prt i e = case e of
    Parser.Tidy.Abs.ThenOneLine expr -> prPrec i 0 (concatD [doc (showString "then"), prt 0 expr, doc (showString ";")])
    Parser.Tidy.Abs.ThenMultiLine expr -> prPrec i 0 (concatD [doc (showString "then"), doc (showString "{"), prt 0 expr, doc (showString "}")])

instance Print Parser.Tidy.Abs.ElseBranch where
  prt i e = case e of
    Parser.Tidy.Abs.ElseOneLine expr -> prPrec i 0 (concatD [doc (showString "else"), prt 0 expr, doc (showString ";")])
    Parser.Tidy.Abs.ElseMultiLine expr -> prPrec i 0 (concatD [doc (showString "else"), doc (showString "{"), prt 0 expr, doc (showString "}")])
    Parser.Tidy.Abs.ElseIf expr thenbranch elsebranch -> prPrec i 0 (concatD [doc (showString "elif"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 thenbranch, prt 0 elsebranch])

instance Print [Parser.Tidy.Abs.MatchCase] where
  prt = prtList

instance Print Parser.Tidy.Abs.MatchCase where
  prt i e = case e of
    Parser.Tidy.Abs.Case pattern_ expr -> prPrec i 0 (concatD [doc (showString "case"), prt 0 pattern_, doc (showString "->"), prt 0 expr])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print Parser.Tidy.Abs.Pattern where
  prt i e = case e of
    Parser.Tidy.Abs.TypePattern classident -> prPrec i 0 (concatD [prt 0 classident])

instance Print Parser.Tidy.Abs.RelationalOperator where
  prt i e = case e of
    Parser.Tidy.Abs.RLess -> prPrec i 0 (concatD [doc (showString "<")])
    Parser.Tidy.Abs.RLessEqual -> prPrec i 0 (concatD [doc (showString "<=")])
    Parser.Tidy.Abs.RGreater -> prPrec i 0 (concatD [doc (showString ">")])
    Parser.Tidy.Abs.RGreaterEqual -> prPrec i 0 (concatD [doc (showString ">=")])
    Parser.Tidy.Abs.REqual -> prPrec i 0 (concatD [doc (showString "==")])
    Parser.Tidy.Abs.RNotEqual -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print Parser.Tidy.Abs.BooleanOperator where
  prt i e = case e of
    Parser.Tidy.Abs.BAnd -> prPrec i 0 (concatD [doc (showString "and")])
    Parser.Tidy.Abs.BOr -> prPrec i 0 (concatD [doc (showString "or")])

