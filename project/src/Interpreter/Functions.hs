module Interpreter.Functions where

import           Parser.Tidy.Abs


isActionMain :: ActionDecl -> Bool
isActionMain (PublicActionDecl (FIdent (LowerCaseIdent "main")) _ _)   = True
isActionMain (OverrideActionDecl (FIdent (LowerCaseIdent "main")) _ _) = True
isActionMain _                                                         = False

getActionBody :: ActionDecl -> ActionBody
getActionBody (OverrideActionDecl _ _ actionBody) = actionBody
getActionBody (PublicActionDecl _ _ actionBody)   = actionBody
getActionBody (PrivateActionDecl _ _ actionBody)  = actionBody
