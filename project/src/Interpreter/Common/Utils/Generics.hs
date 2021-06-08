module Interpreter.Common.Utils.Generics where

import qualified Data.Map                         as Map

import           Interpreter.Common.Types
import           Parser.Tidy.Abs

import           Interpreter.Common.Utils.Classes
import           Interpreter.Common.Utils.Types


isTypeGeneric :: ObjectType -> Bool
isTypeGeneric (ObjectTypeClass classType) = not $ null $ genericParameterListFromClassType classType

mapObjectTypeIfGeneric :: GenericsMap -> ObjectType -> ObjectType
mapObjectTypeIfGeneric genericsMap (ObjectTypeClass classType) =
    ObjectTypeClass $ mapClassTypeIfGeneric genericsMap classType

mapClassTypeIfGeneric :: GenericsMap -> ClassType -> ClassType
mapClassTypeIfGeneric genericsMap classType = if null genericParams
    then classTypeFromObjectType $ Map.findWithDefault objectType objectType genericsMap
    else GeneralClassType classIdent $ GenericParameterPresent mappedGenericParams
        where objectType = ObjectTypeClass classType
              genericParams = genericParameterListFromClassType classType
              classIdent = classIdentifierFromClassType classType
              mappedGenericParams = map (mapClassTypeIfGeneric genericsMap) genericParams
