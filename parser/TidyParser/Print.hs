{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

-- | Pretty-printer for TidyParser.
--   Generated by the BNF converter.

module TidyParser.Print where

import qualified TidyParser.Abs
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

instance Print TidyParser.Abs.UpperCaseIdent where
  prt _ (TidyParser.Abs.UpperCaseIdent i) = doc $ showString $ i

instance Print TidyParser.Abs.LowerCaseIdent where
  prt _ (TidyParser.Abs.LowerCaseIdent i) = doc $ showString $ i

instance Print TidyParser.Abs.Program where
  prt i e = case e of
    TidyParser.Abs.ProgramEntrypoint classdecls -> prPrec i 0 (concatD [prt 0 classdecls])

instance Print [TidyParser.Abs.ClassIdent] where
  prt = prtList

instance Print TidyParser.Abs.ClassIdent where
  prt i e = case e of
    TidyParser.Abs.CIdent uppercaseident -> prPrec i 0 (concatD [prt 0 uppercaseident])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print [TidyParser.Abs.ClassDecl] where
  prt = prtList

instance Print TidyParser.Abs.ClassDecl where
  prt i e = case e of
    TidyParser.Abs.ClassDeclConcrete classtype classident inheritance classbody -> prPrec i 0 (concatD [prt 0 classtype, doc (showString "class"), prt 0 classident, prt 0 inheritance, prt 0 classbody])
    TidyParser.Abs.ClassDeclAbstract classtype classident inheritance classbody -> prPrec i 0 (concatD [doc (showString "abstract"), prt 0 classtype, doc (showString "class"), prt 0 classident, prt 0 inheritance, prt 0 classbody])
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print TidyParser.Abs.Inheritance where
  prt i e = case e of
    TidyParser.Abs.SuperclassAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.SuperclassPresent classident -> prPrec i 0 (concatD [doc (showString "extends"), prt 0 classident])

instance Print TidyParser.Abs.ClassBody where
  prt i e = case e of
    TidyParser.Abs.ClassBodyEmpty -> prPrec i 0 (concatD [])
    TidyParser.Abs.ClassBodyFilled valuessection variablessection functionssection actionssection -> prPrec i 0 (concatD [doc (showString "{"), prt 0 valuessection, prt 0 variablessection, prt 0 functionssection, prt 0 actionssection, doc (showString "}")])

instance Print TidyParser.Abs.ClassType where
  prt i e = case e of
    TidyParser.Abs.MMutable -> prPrec i 0 (concatD [doc (showString "mutable")])
    TidyParser.Abs.MImmutable -> prPrec i 0 (concatD [doc (showString "immutable")])
    TidyParser.Abs.MSingleton -> prPrec i 0 (concatD [doc (showString "singleton")])

instance Print TidyParser.Abs.ValuesSection where
  prt i e = case e of
    TidyParser.Abs.ValuesAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.ValuesPresent valsbody -> prPrec i 0 (concatD [doc (showString "values:"), prt 0 valsbody])

instance Print TidyParser.Abs.VariablesSection where
  prt i e = case e of
    TidyParser.Abs.VariablesAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.VariablesPresent varsbody -> prPrec i 0 (concatD [doc (showString "variables:"), prt 0 varsbody])

instance Print TidyParser.Abs.FunctionsSection where
  prt i e = case e of
    TidyParser.Abs.FunctionsAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.FunctionsPresent fsbody -> prPrec i 0 (concatD [doc (showString "functions:"), prt 0 fsbody])

instance Print TidyParser.Abs.ActionsSection where
  prt i e = case e of
    TidyParser.Abs.ActionsAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.ActionsPresent asbody -> prPrec i 0 (concatD [doc (showString "actions:"), prt 0 asbody])

instance Print TidyParser.Abs.ValSBody where
  prt i e = case e of
    TidyParser.Abs.ValuesSBody valuedecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 valuedecls, doc (showString "}")])

instance Print TidyParser.Abs.ValueIdent where
  prt i e = case e of
    TidyParser.Abs.VIdent lowercaseident -> prPrec i 0 (concatD [prt 0 lowercaseident])

instance Print TidyParser.Abs.ValueType where
  prt i e = case e of
    TidyParser.Abs.ValueTypeClass classident -> prPrec i 0 (concatD [prt 0 classident])
    TidyParser.Abs.ValueTypeGeneric classident classidents -> prPrec i 0 (concatD [prt 0 classident, doc (showString "["), prt 0 classidents, doc (showString "]")])
    TidyParser.Abs.ValueTypeFunction methodtype -> prPrec i 0 (concatD [doc (showString "get"), prt 0 methodtype])
    TidyParser.Abs.ValueTypeAction methodtype -> prPrec i 0 (concatD [doc (showString "do"), prt 0 methodtype])

instance Print [TidyParser.Abs.ValueDecl] where
  prt = prtList

instance Print TidyParser.Abs.ValueDecl where
  prt i e = case e of
    TidyParser.Abs.PublicValueDecl valuedeclproper -> prPrec i 0 (concatD [prt 0 valuedeclproper, doc (showString ";")])
    TidyParser.Abs.PrivateValueDecl valuedeclproper -> prPrec i 0 (concatD [doc (showString "private"), prt 0 valuedeclproper, doc (showString ";")])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print [TidyParser.Abs.ValueDeclProper] where
  prt = prtList

instance Print TidyParser.Abs.ValueDeclProper where
  prt i e = case e of
    TidyParser.Abs.UninitialisedValue valueident valuetype -> prPrec i 0 (concatD [prt 0 valueident, doc (showString ":"), prt 0 valuetype])
    TidyParser.Abs.InitialisedValue valueident valuetype expr -> prPrec i 0 (concatD [prt 0 valueident, doc (showString ":"), prt 0 valuetype, doc (showString "="), prt 0 expr])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print TidyParser.Abs.VarSBody where
  prt i e = case e of
    TidyParser.Abs.VariablesSBody valuedecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 valuedecls, doc (showString "}")])

instance Print TidyParser.Abs.FSBody where
  prt i e = case e of
    TidyParser.Abs.FSBodyEmpty -> prPrec i 0 (concatD [])
    TidyParser.Abs.FSBodyFilled functiondecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 functiondecls, doc (showString "}")])

instance Print TidyParser.Abs.FunctionIdent where
  prt i e = case e of
    TidyParser.Abs.FIdent lowercaseident -> prPrec i 0 (concatD [prt 0 lowercaseident])

instance Print TidyParser.Abs.MethodType where
  prt i e = case e of
    TidyParser.Abs.FType parameterlist valuetype -> prPrec i 0 (concatD [prt 0 parameterlist, doc (showString "->"), prt 0 valuetype])

instance Print TidyParser.Abs.ParameterList where
  prt i e = case e of
    TidyParser.Abs.ParamList valuedeclpropers -> prPrec i 0 (concatD [doc (showString "("), prt 0 valuedeclpropers, doc (showString ")")])

instance Print [TidyParser.Abs.FunctionDecl] where
  prt = prtList

instance Print TidyParser.Abs.FunctionDecl where
  prt i e = case e of
    TidyParser.Abs.OverrideFunctionDecl functionident methodtype functionbody -> prPrec i 0 (concatD [doc (showString "override"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 functionbody])
    TidyParser.Abs.PublicFunctionDecl functionident methodtype functionbody -> prPrec i 0 (concatD [prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 functionbody])
    TidyParser.Abs.PrivateFunctionDecl functionident methodtype functionbody -> prPrec i 0 (concatD [doc (showString "private"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 functionbody])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print TidyParser.Abs.FunctionBody where
  prt i e = case e of
    TidyParser.Abs.FunctionBodyOneLine expr -> prPrec i 0 (concatD [prt 0 expr])
    TidyParser.Abs.FunctionBodyMultiLine expr withvalues -> prPrec i 0 (concatD [doc (showString "{"), prt 0 expr, doc (showString "}"), prt 0 withvalues])

instance Print TidyParser.Abs.WithValues where
  prt i e = case e of
    TidyParser.Abs.WithValuesAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.WithValuesPresent valuessection -> prPrec i 0 (concatD [doc (showString "with"), prt 0 valuessection])

instance Print TidyParser.Abs.ASBody where
  prt i e = case e of
    TidyParser.Abs.ASBodyEmpty -> prPrec i 0 (concatD [])
    TidyParser.Abs.ASBodyFilled actiondecls -> prPrec i 0 (concatD [doc (showString "{"), prt 0 actiondecls, doc (showString "}")])

instance Print [TidyParser.Abs.ActionDecl] where
  prt = prtList

instance Print TidyParser.Abs.ActionDecl where
  prt i e = case e of
    TidyParser.Abs.OverrideActionDecl functionident methodtype actionbody -> prPrec i 0 (concatD [doc (showString "override"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 actionbody])
    TidyParser.Abs.PublicActionDecl functionident methodtype actionbody -> prPrec i 0 (concatD [prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 actionbody])
    TidyParser.Abs.PrivateActionDecl functionident methodtype actionbody -> prPrec i 0 (concatD [doc (showString "private"), prt 0 functionident, doc (showString ":"), prt 0 methodtype, doc (showString "="), prt 0 actionbody])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print TidyParser.Abs.ActionBody where
  prt i e = case e of
    TidyParser.Abs.ActionBodyOneLine expr -> prPrec i 0 (concatD [prt 0 expr])
    TidyParser.Abs.ActionBodyMultiLine exprs -> prPrec i 0 (concatD [doc (showString "{"), prt 0 exprs, doc (showString "}")])

instance Print [TidyParser.Abs.Expr] where
  prt = prtList

instance Print TidyParser.Abs.Expr where
  prt i e = case e of
    TidyParser.Abs.ELiteral literal -> prPrec i 10 (concatD [prt 0 literal])
    TidyParser.Abs.ELocalValue valueident -> prPrec i 10 (concatD [prt 0 valueident])
    TidyParser.Abs.EGetExpr getexpr -> prPrec i 9 (concatD [prt 0 getexpr])
    TidyParser.Abs.EDoExpr doexpr -> prPrec i 9 (concatD [prt 0 doexpr])
    TidyParser.Abs.ELambdaFunction lambdafunction -> prPrec i 8 (concatD [prt 0 lambdafunction])
    TidyParser.Abs.ELambdaAction lambdaaction -> prPrec i 8 (concatD [prt 0 lambdaaction])
    TidyParser.Abs.ELocalFunctionCall functioncall -> prPrec i 7 (concatD [doc (showString "local"), prt 0 functioncall])
    TidyParser.Abs.ELocalActionCall actioncall -> prPrec i 7 (concatD [doc (showString "local"), prt 0 actioncall])
    TidyParser.Abs.ECtorCall constructorcall -> prPrec i 7 (concatD [prt 0 constructorcall])
    TidyParser.Abs.EImperativeControlFlow imperativecontrolflow -> prPrec i 6 (concatD [prt 0 imperativecontrolflow])
    TidyParser.Abs.EFunctionalControlFlow functionalcontrolflow -> prPrec i 6 (concatD [prt 0 functionalcontrolflow])
    TidyParser.Abs.ELocalValueDecl localvaluedecl -> prPrec i 5 (concatD [prt 0 localvaluedecl])
    TidyParser.Abs.EUnaryNot expr -> prPrec i 4 (concatD [doc (showString "not"), prt 5 expr])
    TidyParser.Abs.EUnaryMinus expr -> prPrec i 4 (concatD [doc (showString "-"), prt 5 expr])
    TidyParser.Abs.EMultiply expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "*"), prt 4 expr2])
    TidyParser.Abs.EDivide expr1 expr2 -> prPrec i 3 (concatD [prt 3 expr1, doc (showString "/"), prt 4 expr2])
    TidyParser.Abs.EAdd expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "+"), prt 3 expr2])
    TidyParser.Abs.ESubtract expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "-"), prt 3 expr2])
    TidyParser.Abs.EConcatenate expr1 expr2 -> prPrec i 2 (concatD [prt 2 expr1, doc (showString "++"), prt 3 expr2])
    TidyParser.Abs.ERelationalOperator expr1 relationaloperator expr2 -> prPrec i 1 (concatD [prt 1 expr1, prt 0 relationaloperator, prt 2 expr2])
    TidyParser.Abs.EBooleanOperator expr1 booleanoperator expr2 -> prPrec i 0 (concatD [prt 1 expr1, prt 0 booleanoperator, prt 0 expr2])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print TidyParser.Abs.Literal where
  prt i e = case e of
    TidyParser.Abs.LInt n -> prPrec i 0 (concatD [prt 0 n])
    TidyParser.Abs.LBool boolean -> prPrec i 0 (concatD [prt 0 boolean])
    TidyParser.Abs.LChar c -> prPrec i 0 (concatD [prt 0 c])
    TidyParser.Abs.LString str -> prPrec i 0 (concatD [prt 0 str])
    TidyParser.Abs.LVoid void -> prPrec i 0 (concatD [prt 0 void])

instance Print TidyParser.Abs.Boolean where
  prt i e = case e of
    TidyParser.Abs.BTrue -> prPrec i 0 (concatD [doc (showString "True")])
    TidyParser.Abs.BFalse -> prPrec i 0 (concatD [doc (showString "False")])

instance Print TidyParser.Abs.Void where
  prt i e = case e of
    TidyParser.Abs.VPass -> prPrec i 0 (concatD [doc (showString "Pass")])

instance Print TidyParser.Abs.LocalValueDecl where
  prt i e = case e of
    TidyParser.Abs.LocalVDecl valuedecl -> prPrec i 0 (concatD [doc (showString "value"), prt 0 valuedecl])

instance Print TidyParser.Abs.LambdaFunction where
  prt i e = case e of
    TidyParser.Abs.LambdaFunctionOneLine parameterlist expr -> prPrec i 0 (concatD [doc (showString "get"), prt 0 parameterlist, doc (showString "->"), prt 0 expr, doc (showString ";")])
    TidyParser.Abs.LambdaFunctionMultiLine parameterlist expr -> prPrec i 0 (concatD [doc (showString "get"), prt 0 parameterlist, doc (showString "->"), doc (showString "{"), prt 0 expr, doc (showString "}")])

instance Print TidyParser.Abs.LambdaAction where
  prt i e = case e of
    TidyParser.Abs.LambdaActionOneLine parameterlist expr -> prPrec i 0 (concatD [doc (showString "do"), prt 0 parameterlist, doc (showString "->"), prt 0 expr, doc (showString ";")])
    TidyParser.Abs.LambdaActionMultiLine parameterlist exprs -> prPrec i 0 (concatD [doc (showString "do"), prt 0 parameterlist, doc (showString "->"), doc (showString "{"), prt 0 exprs, doc (showString "}")])

instance Print TidyParser.Abs.ArgumentList where
  prt i e = case e of
    TidyParser.Abs.ArgListAbsent -> prPrec i 0 (concatD [])
    TidyParser.Abs.ArgListPresent functionarguments -> prPrec i 0 (concatD [doc (showString "("), prt 0 functionarguments, doc (showString ")")])

instance Print [TidyParser.Abs.FunctionArgument] where
  prt = prtList

instance Print TidyParser.Abs.FunctionArgument where
  prt i e = case e of
    TidyParser.Abs.FunctionArg expr -> prPrec i 0 (concatD [prt 0 expr])
  prtList _ [] = concatD []
  prtList _ [x] = concatD [prt 0 x]
  prtList _ (x:xs) = concatD [prt 0 x, doc (showString ","), prt 0 xs]

instance Print TidyParser.Abs.FunctionCall where
  prt i e = case e of
    TidyParser.Abs.FCall functionident argumentlist -> prPrec i 0 (concatD [doc (showString "."), prt 0 functionident, prt 0 argumentlist])

instance Print TidyParser.Abs.ActionCall where
  prt i e = case e of
    TidyParser.Abs.ACall functionident argumentlist -> prPrec i 0 (concatD [doc (showString "#"), prt 0 functionident, prt 0 argumentlist])

instance Print TidyParser.Abs.ConstructorCall where
  prt i e = case e of
    TidyParser.Abs.CCall classident argumentlist -> prPrec i 0 (concatD [prt 0 classident, prt 0 argumentlist])

instance Print TidyParser.Abs.GetExpr where
  prt i e = case e of
    TidyParser.Abs.GetExprInstance valueident functioncall -> prPrec i 0 (concatD [prt 0 valueident, prt 0 functioncall])
    TidyParser.Abs.GetExprStatic classident functioncall -> prPrec i 0 (concatD [prt 0 classident, prt 0 functioncall])
    TidyParser.Abs.GetExprChain getexpr functioncall -> prPrec i 0 (concatD [prt 0 getexpr, prt 0 functioncall])

instance Print TidyParser.Abs.DoExpr where
  prt i e = case e of
    TidyParser.Abs.DoExprInstance valueident actioncall -> prPrec i 0 (concatD [prt 0 valueident, prt 0 actioncall])
    TidyParser.Abs.DoExprStatic classident actioncall -> prPrec i 0 (concatD [prt 0 classident, prt 0 actioncall])
    TidyParser.Abs.DoExprChain getexpr actioncall -> prPrec i 0 (concatD [prt 0 getexpr, prt 0 actioncall])

instance Print TidyParser.Abs.ImperativeControlFlow where
  prt i e = case e of
    TidyParser.Abs.IWhile expr exprs -> prPrec i 0 (concatD [doc (showString "while"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 exprs, doc (showString "}")])
    TidyParser.Abs.IForeach valuedecl expr exprs -> prPrec i 0 (concatD [doc (showString "for"), doc (showString "("), prt 0 valuedecl, doc (showString "in"), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 exprs, doc (showString "}")])
    TidyParser.Abs.IIf expr exprs optionalelsebranch -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), doc (showString "{"), prt 0 exprs, doc (showString "}"), prt 0 optionalelsebranch])

instance Print TidyParser.Abs.OptionalElseBranch where
  prt i e = case e of
    TidyParser.Abs.ElsePresent exprs -> prPrec i 0 (concatD [doc (showString "else"), doc (showString "{"), prt 0 exprs, doc (showString "}")])
    TidyParser.Abs.ElseAbsent -> prPrec i 0 (concatD [])

instance Print TidyParser.Abs.FunctionalControlFlow where
  prt i e = case e of
    TidyParser.Abs.FIfThenElse expr thenbranch elsebranch -> prPrec i 0 (concatD [doc (showString "if"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 thenbranch, prt 0 elsebranch])
    TidyParser.Abs.FMatch expr matchcases -> prPrec i 0 (concatD [doc (showString "match"), prt 0 expr, doc (showString "{"), prt 0 matchcases, doc (showString "}")])

instance Print TidyParser.Abs.ThenBranch where
  prt i e = case e of
    TidyParser.Abs.ThenOneLine expr -> prPrec i 0 (concatD [doc (showString "then"), prt 0 expr, doc (showString ";")])
    TidyParser.Abs.ThenMultiLine expr -> prPrec i 0 (concatD [doc (showString "then"), doc (showString "{"), prt 0 expr, doc (showString "}")])

instance Print TidyParser.Abs.ElseBranch where
  prt i e = case e of
    TidyParser.Abs.ElseOneLine expr -> prPrec i 0 (concatD [doc (showString "else"), prt 0 expr, doc (showString ";")])
    TidyParser.Abs.ElseMultiLine expr -> prPrec i 0 (concatD [doc (showString "else"), doc (showString "{"), prt 0 expr, doc (showString "}")])
    TidyParser.Abs.ElseIf expr thenbranch elsebranch -> prPrec i 0 (concatD [doc (showString "elif"), doc (showString "("), prt 0 expr, doc (showString ")"), prt 0 thenbranch, prt 0 elsebranch])

instance Print [TidyParser.Abs.MatchCase] where
  prt = prtList

instance Print TidyParser.Abs.MatchCase where
  prt i e = case e of
    TidyParser.Abs.Case pattern_ expr -> prPrec i 0 (concatD [doc (showString "case"), prt 0 pattern_, doc (showString "->"), prt 0 expr])
  prtList _ [] = concatD []
  prtList _ (x:xs) = concatD [prt 0 x, prt 0 xs]

instance Print TidyParser.Abs.Pattern where
  prt i e = case e of
    TidyParser.Abs.TypePattern classident -> prPrec i 0 (concatD [prt 0 classident])

instance Print TidyParser.Abs.RelationalOperator where
  prt i e = case e of
    TidyParser.Abs.RLess -> prPrec i 0 (concatD [doc (showString "<")])
    TidyParser.Abs.RLessEqual -> prPrec i 0 (concatD [doc (showString "<=")])
    TidyParser.Abs.RGreater -> prPrec i 0 (concatD [doc (showString ">")])
    TidyParser.Abs.RGreaterEqual -> prPrec i 0 (concatD [doc (showString ">=")])
    TidyParser.Abs.REqual -> prPrec i 0 (concatD [doc (showString "==")])
    TidyParser.Abs.RNotEqual -> prPrec i 0 (concatD [doc (showString "!=")])

instance Print TidyParser.Abs.BooleanOperator where
  prt i e = case e of
    TidyParser.Abs.BAnd -> prPrec i 0 (concatD [doc (showString "and")])
    TidyParser.Abs.BOr -> prPrec i 0 (concatD [doc (showString "or")])
