module Interpreter.Common.Utils.Builtin where

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Types


pass :: Object
pass = BuiltinObject VoidObject

localReferenceType :: ObjectType
localReferenceType = simpleObjectTypeFromClassName "__local"

builtinMethodIdentifier :: MethodIdent -> MethodIdent
builtinMethodIdentifier methodIdent = methodIdentifierFromName $ "__builtin_" ++ methodNameFromIdentifier methodIdent

builtinMethodIdentifierFromName :: String -> MethodIdent
builtinMethodIdentifierFromName name = methodIdentifierFromName $ "__builtin_" ++ name

localReferenceIdentifier :: ObjectIdent
localReferenceIdentifier = objectIdentifierFromName "local"

thisReferenceIdentifier :: ObjectIdent
thisReferenceIdentifier = objectIdentifierFromName "this"

objectTypeForBuiltinObject :: BuiltinObject -> ObjectType
objectTypeForBuiltinObject (IntObject _)            = intType
objectTypeForBuiltinObject (BoolObject _)           = boolType
objectTypeForBuiltinObject (CharObject _)           = charType
objectTypeForBuiltinObject (StringObject _)         = stringType
objectTypeForBuiltinObject VoidObject               = voidType
objectTypeForBuiltinObject (ListObject _ classType) = listType classType

anyType :: ObjectType
anyType = simpleObjectTypeFromClassName "Any"

intType :: ObjectType
intType = simpleObjectTypeFromClassName "Int"

boolType :: ObjectType
boolType = simpleObjectTypeFromClassName "Bool"

charType :: ObjectType
charType = simpleObjectTypeFromClassName "Char"

stringType :: ObjectType
stringType = simpleObjectTypeFromClassName "String"

voidType :: ObjectType
voidType = simpleObjectTypeFromClassName "Void"

listType :: ClassType -> ObjectType
listType genericParam = ObjectTypeClass classType
    where classType = GeneralClassType (classIdentifierFromName "List") (GenericParameterPresent [genericParam])

builtinClasses :: [ClassDecl]
builtinClasses = [simpleBuiltinClass "Int", simpleBuiltinClass "Bool", simpleBuiltinClass "Char",
                  simpleBuiltinClass "String", simpleBuiltinClass "Void", builtinGenericClass "List",
                  systemBuiltinClassDeclaration, simpleBuiltinClass "__local"]

builtinClassNames :: [String]
builtinClassNames = ["Int", "Bool", "Char", "String", "Void", "List", "System", "__local"]

simpleBuiltinClass :: String -> ClassDecl
simpleBuiltinClass name = ClassDeclaration MConcrete MImmutable classType SuperclassAbsent ClassBodyEmpty
    where classType = simpleClassTypeFromName name

builtinGenericClass :: String -> ClassDecl
builtinGenericClass name = ClassDeclaration MConcrete MImmutable classType SuperclassAbsent ClassBodyEmpty
    where classIdent = classIdentifierFromName name
          classType = GeneralClassType classIdent singleGenericParameter

singleGenericParameter :: GenericParameter
singleGenericParameter = GenericParameterPresent [simpleClassTypeFromName "A"]

systemBuiltinClassDeclaration :: ClassDecl
systemBuiltinClassDeclaration = ClassDeclaration MConcrete MSingleton classType SuperclassAbsent systemBuiltinClassBody
    where classType = GeneralClassType (classIdentifierFromName "System") GenericParameterAbsent

systemBuiltinClassBody :: ClassBody
systemBuiltinClassBody = ClassBodyFilled ValuesAbsent VariablesAbsent FunctionsAbsent (ActionsPresent actionDecls)
    where actionDecls = [exitBuiltinActionDeclaration, assertBuiltinActionDeclaration,
                         assertEqualsBuiltinActionDeclaration, printBuiltinActionDeclaration,
                         printLineBuiltinActionDeclaration]

getConstructorParameterTypesForBuiltinClass :: ClassIdent -> [ObjectType]
getConstructorParameterTypesForBuiltinClass classIdent = case classNameFromIdentifier classIdent of
    "Int"    -> [intType]
    "Bool"   -> [boolType]
    "Char"   -> [charType]
    "String" -> [stringType]
    "Void"   -> []
    _        -> []

shouldHaveUniformTypes :: MethodIdent -> Bool
shouldHaveUniformTypes methodIdent = case methodNameFromIdentifier methodIdent of
    "__builtin_assertEquals" -> True
    _                        -> False

getBuiltinMethodType :: MethodIdent -> MethodType
getBuiltinMethodType methodIdent = case methodNameFromIdentifier methodIdent of
    "__builtin_exit"         -> exitBuiltinMethodType
    "__builtin_assert"       -> assertBuiltinMethodType
    "__builtin_assertEquals" -> assertEqualsBuiltinMethodType
    "__builtin_print"        -> printBuiltinMethodType
    "__builtin_printLine"    -> printLineBuiltinMethodType

builtinWithImplicitContext :: MethodIdent -> Bool
builtinWithImplicitContext methodIdent = case methodNameFromIdentifier methodIdent of
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
