module Interpreter.Common.Utils.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types


pass :: Object
pass = BuiltinObject VoidObject

localReferenceType :: ObjectType
localReferenceType = objectTypeFromClassName "__local"

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

getBuiltinMethodType :: MethodIdent -> MethodType
getBuiltinMethodType (MethodIdentifier (LowerCaseIdent methodName)) = case methodName of
    "__builtin_twice" -> twiceBuiltinMethodType

systemBuiltinClassDeclaration :: ClassDecl
systemBuiltinClassDeclaration = ClassDeclaration MConcrete MSingleton classIdent SuperclassAbsent systemBuiltinClassBody
    where classIdent = classIdentifierFromName "System"

systemBuiltinClassBody :: ClassBody
systemBuiltinClassBody = ClassBodyFilled ValuesAbsent VariablesAbsent (FunctionsPresent functionDecls) ActionsAbsent
    where functionDecls = [twiceBuiltinFunctionDeclaration]

twiceBuiltinFunctionDeclaration :: FunctionDecl
twiceBuiltinFunctionDeclaration = FunctionDeclaration MNonOverriding MPublic methodIdent twiceBuiltinMethodType functionBody
    where methodIdent = methodIdentifierFromName "twice"
          builtinMethodIdent = builtinMethodIdentifierFromName "twice"
          functionBody = FunctionBodyOneLine (EBuiltin builtinMethodIdent ArgumentListAbsent)

twiceBuiltinMethodType :: MethodType
twiceBuiltinMethodType = MethodTypeSignature (ParameterList []) intType
