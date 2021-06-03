module Interpreter.Common.Utils.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types


pass :: Object
pass = BuiltinObject VoidObject

localReferenceType :: ObjectType
localReferenceType = objectTypeFromClassName "__local"

builtinMethodIdentifierFromName :: String -> MethodIdent
builtinMethodIdentifierFromName name = methodIdentifierFromName $ "__method_" ++ name

localReferenceIdentifier :: ObjectIdent
localReferenceIdentifier = objectIdentifierFromName "local"

thisReferenceIdentifier :: ObjectIdent
thisReferenceIdentifier = objectIdentifierFromName "this"

objectTypeForBuiltinObject :: BuiltinObject -> ObjectType
objectTypeForBuiltinObject (IntObject _)    = intType
objectTypeForBuiltinObject (BoolObject _)   = objectTypeFromClassName "Bool"
objectTypeForBuiltinObject (CharObject _)   = objectTypeFromClassName "Char"
objectTypeForBuiltinObject (StringObject _) = objectTypeFromClassName "String"
objectTypeForBuiltinObject VoidObject       = objectTypeFromClassName "Void"

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
builtinClasses = map builtinClassDeclFromName builtinClassNames

builtinClassNames :: [String]
builtinClassNames = ["Int", "Bool", "Char", "String", "Void", "__local"]

builtinSingletonClasses :: [ClassDecl]
builtinSingletonClasses = [systemBuiltinClassDeclaration]

builtinClassDeclFromName :: String -> ClassDecl
builtinClassDeclFromName name = ClassDeclaration MConcrete MImmutable
    (classIdentifierFromName name) SuperclassAbsent ClassBodyEmpty

systemBuiltinClassDeclaration :: ClassDecl
systemBuiltinClassDeclaration = ClassDeclaration MConcrete MSingleton classIdent SuperclassAbsent systemBuiltinClassBody
    where classIdent = classIdentifierFromName "System"

systemBuiltinClassBody :: ClassBody
systemBuiltinClassBody = ClassBodyFilled ValuesAbsent VariablesAbsent (FunctionsPresent functionDecls) ActionsAbsent
    where functionDecls = [twiceBuiltinFunctionDeclaration]

twiceBuiltinFunctionDeclaration :: FunctionDecl
twiceBuiltinFunctionDeclaration = FunctionDeclaration MNonOverriding MPublic methodType functionBody
    where methodIdent = methodIdentifierFromName "twice"
          builtinMethodIdent = builtinMethodIdentifierFromName "twice"
          methodType = MethodTypeSignature (ParameterList []) intType
          functionBody = FunctionBodyOneLine (EBuiltin methodIdent ArgumentListAbsent)
