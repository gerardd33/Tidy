module Interpreter.Common.Utils.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types


pass :: Object
pass = BuiltinObject VoidObject

localReferenceType :: ObjectType
localReferenceType = objectTypeFromClassName "__local"

builtinMethodIdentifier :: MethodIdent -> MethodIdent
builtinMethodIdentifier (MethodIdentifier (LowerCaseIdent name)) = methodIdentifierFromName $ "__builtin_" ++ name

builtinMethodIdentifierFromName :: String -> MethodIdent
builtinMethodIdentifierFromName name = methodIdentifierFromName $ "__builtin_" ++ name

localReferenceIdentifier :: ObjectIdent
localReferenceIdentifier = objectIdentifierFromName "local"

thisReferenceIdentifier :: ObjectIdent
thisReferenceIdentifier = objectIdentifierFromName "this"

objectTypeForBuiltinObject :: BuiltinObject -> ObjectType
objectTypeForBuiltinObject (IntObject _)    = intType
objectTypeForBuiltinObject (BoolObject _)   = boolType
objectTypeForBuiltinObject (CharObject _)   = charType
objectTypeForBuiltinObject (StringObject _) = stringType
objectTypeForBuiltinObject VoidObject       = voidType

anyType :: ObjectType
anyType = objectTypeFromClassName "Any"

intType :: ObjectType
intType = objectTypeFromClassName "Int"

boolType :: ObjectType
boolType = objectTypeFromClassName "Bool"

charType :: ObjectType
charType = objectTypeFromClassName "Char"

stringType :: ObjectType
stringType = objectTypeFromClassName "String"

voidType :: ObjectType
voidType = objectTypeFromClassName "Void"

builtinClasses :: [ClassDecl]
builtinClasses = [simpleBuiltinClass "Int", simpleBuiltinClass "Bool", simpleBuiltinClass "Char",
                  simpleBuiltinClass "String", simpleBuiltinClass "Void", systemBuiltinClassDeclaration,
                  simpleBuiltinClass "__local"]

simpleBuiltinClass :: String -> ClassDecl
simpleBuiltinClass name = ClassDeclaration MConcrete MImmutable
    (classIdentifierFromName name) SuperclassAbsent ClassBodyEmpty

systemBuiltinClassDeclaration :: ClassDecl
systemBuiltinClassDeclaration = ClassDeclaration MConcrete MSingleton classIdent SuperclassAbsent systemBuiltinClassBody
    where classIdent = classIdentifierFromName "System"

systemBuiltinClassBody :: ClassBody
systemBuiltinClassBody = ClassBodyFilled ValuesAbsent VariablesAbsent FunctionsAbsent (ActionsPresent actionDecls)
    where actionDecls = [exitBuiltinActionDeclaration, assertBuiltinActionDeclaration,
                         assertEqualsBuiltinActionDeclaration, printBuiltinActionDeclaration,
                         printLineBuiltinActionDeclaration]

shouldHaveUniformTypes :: MethodIdent -> Bool
shouldHaveUniformTypes (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_assertEquals" -> True
    _                        -> False

getBuiltinMethodType :: MethodIdent -> MethodType
getBuiltinMethodType (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_exit"         -> exitBuiltinMethodType
    "__builtin_assert"       -> assertBuiltinMethodType
    "__builtin_assertEquals" -> assertEqualsBuiltinMethodType
    "__builtin_print"        -> printBuiltinMethodType
    "__builtin_printLine"    -> printLineBuiltinMethodType

builtinWithImplicitContext :: MethodIdent -> Bool
builtinWithImplicitContext (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_assert"       -> True
    "__builtin_assertEquals" -> True
    _                        -> False

exitBuiltinMethodType :: MethodType
exitBuiltinMethodType = MethodTypeSignature (ParameterList [codeParam]) voidType
    where codeParam = ObjectDeclarationProper (objectIdentifierFromName "code") intType Uninitialized

exitBuiltinActionDeclaration :: ActionDecl
exitBuiltinActionDeclaration = ActionDeclaration MNonOverriding MPublic methodIdent exitBuiltinMethodType actionBody
    where methodIdent = methodIdentifierFromName "exit"
          builtinMethodIdent = builtinMethodIdentifierFromName "exit"
          actionBody = ActionBodyOneLine (EBuiltin builtinMethodIdent)

assertBuiltinMethodType :: MethodType
assertBuiltinMethodType = MethodTypeSignature (ParameterList [predicateParam, contextParam]) voidType
    where predicateParam = ObjectDeclarationProper (objectIdentifierFromName "predicate") boolType Uninitialized
          contextParam = ObjectDeclarationProper (objectIdentifierFromName "context") stringType Uninitialized

assertBuiltinActionDeclaration :: ActionDecl
assertBuiltinActionDeclaration = ActionDeclaration MNonOverriding MPublic methodIdent assertBuiltinMethodType actionBody
    where methodIdent = methodIdentifierFromName "assert"
          builtinMethodIdent = builtinMethodIdentifierFromName "assert"
          actionBody = ActionBodyOneLine (EBuiltin builtinMethodIdent)

assertEqualsBuiltinMethodType :: MethodType
assertEqualsBuiltinMethodType = MethodTypeSignature (ParameterList [param1, param2, contextParam]) voidType
    where param1 = ObjectDeclarationProper (objectIdentifierFromName "param1") anyType Uninitialized
          param2 = ObjectDeclarationProper (objectIdentifierFromName "param2") anyType Uninitialized
          contextParam = ObjectDeclarationProper (objectIdentifierFromName "context") stringType Uninitialized

assertEqualsBuiltinActionDeclaration :: ActionDecl
assertEqualsBuiltinActionDeclaration = ActionDeclaration MNonOverriding MPublic methodIdent assertEqualsBuiltinMethodType actionBody
    where methodIdent = methodIdentifierFromName "assertEquals"
          builtinMethodIdent = builtinMethodIdentifierFromName "assertEquals"
          actionBody = ActionBodyOneLine (EBuiltin builtinMethodIdent)

printBuiltinMethodType :: MethodType
printBuiltinMethodType = MethodTypeSignature (ParameterList [valueParam]) voidType
    where valueParam = ObjectDeclarationProper (objectIdentifierFromName "value") anyType Uninitialized

printBuiltinActionDeclaration :: ActionDecl
printBuiltinActionDeclaration = ActionDeclaration MNonOverriding MPublic methodIdent printBuiltinMethodType actionBody
    where methodIdent = methodIdentifierFromName "print"
          builtinMethodIdent = builtinMethodIdentifierFromName "print"
          actionBody = ActionBodyOneLine (EBuiltin builtinMethodIdent)

printLineBuiltinMethodType :: MethodType
printLineBuiltinMethodType = printBuiltinMethodType

printLineBuiltinActionDeclaration :: ActionDecl
printLineBuiltinActionDeclaration = ActionDeclaration MNonOverriding MPublic methodIdent printLineBuiltinMethodType actionBody
    where methodIdent = methodIdentifierFromName "printLine"
          builtinMethodIdent = builtinMethodIdentifierFromName "printLine"
          actionBody = ActionBodyOneLine (EBuiltin builtinMethodIdent)
