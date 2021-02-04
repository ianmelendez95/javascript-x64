{-# LANGUAGE OverloadedStrings #-}

module Acorn.Syntax 
  ( AcornOutput (..)
  , AcornError (..)
  , Program (..)
  , ExpressionStatement (..)
  , Expression (..)
  , Identifier (..)
  , Literal (..)
  , Pos (..)
  , HasPos (pos)
  , ReplShow (replShow)
  ) where 

import Data.Aeson
import Data.Aeson.Types
import Data.Text (unpack)
import Data.Scientific (toRealFloat)

data AcornOutput = OutputError AcornError 
                 | OutputResult Program
                 deriving Show

data AcornError = AcornError 
  { acorn_error_name    :: String
  , acorn_error_message :: String
  , acorn_error_pos     :: Int
  , acorn_error_loc     :: (Int, Int)
  } deriving Show

data Program = Program Pos String [ExpressionStatement] -- { sourceType, body }
             deriving Show

data ExpressionStatement = ExpressionStatement Pos Expression
                         deriving Show

data Expression = Identifier Pos Identifier
                | ComputedMemberExpression Pos Expression Expression   -- { object, property }
                | IdentifierMemberExpression Pos Expression Identifier -- { object, identifier }
                | CallExpression Pos Expression [Expression]           -- { callee, arguments }
                | Literal Pos Literal
                deriving Show

data Identifier = IdentifierName Pos String
                deriving Show

data Literal = LiteralString Pos String 
             | LiteralBoolean Pos Bool
             | LiteralNull Pos
             | LiteralNumber Pos Double
             deriving Show

type Pos = (Int, Int)

class ReplShow a where 
  replShow :: a -> String

instance ReplShow ExpressionStatement where 
  replShow (ExpressionStatement _ e) = replShow e
instance ReplShow Expression where 
  replShow (Identifier _ ident) = replShow ident
  replShow (ComputedMemberExpression _ object property) = replShow object ++ "[" ++ replShow property ++ "]"
  replShow (IdentifierMemberExpression _ object identifier) = replShow object ++ "." ++ replShow identifier
  replShow (CallExpression _ callee _) = replShow callee ++ "(...)"
  replShow (Literal _ lit) = replShow lit
instance ReplShow Identifier where 
  replShow (IdentifierName _ name) = name
instance ReplShow Literal where 
  replShow (LiteralString _ str) = "\"" ++ str ++ "\""
  replShow (LiteralBoolean _ bool) = show bool
  replShow (LiteralNumber _ num) = show num
  replShow (LiteralNull _) = "null"

class HasPos a where 
  pos :: a -> Pos

instance HasPos Program where 
  pos (Program p _ _) = p
instance HasPos ExpressionStatement where 
  pos (ExpressionStatement p _) = p
instance HasPos Expression where 
  pos (Identifier p _) = p
  pos (ComputedMemberExpression p _ _) = p
  pos (IdentifierMemberExpression p _ _) = p
  pos (CallExpression p _ _) = p
  pos (Literal p _) = p
instance HasPos Identifier where 
  pos (IdentifierName p _) = p
instance HasPos Literal where 
  pos (LiteralString p _) = p
  pos (LiteralBoolean p _) = p
  pos (LiteralNumber p _) = p
  pos (LiteralNull p) = p

-- withObject :: String -> (Object -> Parser a) -> Value -> Parser a
-- (.:)       :: Object -> Text -> Parser a

instance FromJSON AcornOutput where 
  parseJSON value = withObject "AcornOutput" 
    (\v -> do tipe <- v .: "type"
              case tipe of 
                (String "Error") -> OutputError <$> parseJSON value
                (String "Program") -> OutputResult <$> parseJSON value
    ) 
    value

instance FromJSON AcornError where 
  parseJSON = withObject "AcornError" $ \v -> 
    AcornError <$> v .: "name"
               <*> v .: "message"
               <*> v .: "pos"
               <*> parseErrorLoc v

parseErrorLoc :: Object -> Parser (Int, Int)
parseErrorLoc v = do loc <- v .: "error loc"
                     withObject "acorn loc" 
                                (\eloc -> (,) <$> eloc .: "line" <*> eloc .: "column") 
                                loc

instance FromJSON Program where 
  parseJSON = withObject "Program" $ \v -> 
    Program <$> pPos v <?> Key "program pPos"
            <*> v .: "sourceType"
            <*> v .: "body"

instance FromJSON ExpressionStatement where 
  parseJSON = withObject "ExpressionStatement" $ \v -> 
    ExpressionStatement <$> pPos v <?> Key "expression statement pPos"
                        <*> v .: "expression"

instance FromJSON Expression where 
  parseJSON = withObject "Expression" $ \v -> 
    do vTypeValue <- v .: "type"
       case vTypeValue of 
         (String "Identifier") -> Identifier <$> pPos v <*> identifier v 
         (String "MemberExpression") -> memberExpression v
         (String "CallExpression") -> callExpression v
         (String "Literal") -> Literal <$> (pPos v <?> Key "literal pPos") <*> literal v
         _ -> error $ "Unrecognized expression type: " ++ show vTypeValue

instance FromJSON Identifier where 
  parseJSON = withObject "Identifier" identifier -- TODO be strict about the 'type'

instance FromJSON Literal where 
  parseJSON = withObject "Literal" literal       -- TODO be strict about the 'type'

identifier :: Object -> Parser Identifier
identifier v = IdentifierName <$> pPos v <*> v .: "name"

memberExpression :: Object -> Parser Expression
memberExpression v = do computed <- v .: "computed"
                        case computed of 
                          (Bool True) -> computedMemberExpression v
                          (Bool False) -> identifierMemberExpression v
                          _ -> error $ "Computed expected to be boolean: " ++ show computed

computedMemberExpression :: Object -> Parser Expression
computedMemberExpression v = ComputedMemberExpression <$> pPos v <?> Key "computed member expression pos"
                                                      <*> v .: "object"
                                                      <*> v .: "property"

identifierMemberExpression :: Object -> Parser Expression
identifierMemberExpression v = IdentifierMemberExpression <$> pPos v <?> Key "computed member expression pos"
                                                          <*> v .: "object"
                                                          <*> v .: "property" -- extract Identifier type

-- memberExpression v = MemberExpression <$> pPos v <?> Key "member expression pPos"
--                                       <*> v .: "computed" 
--                                       <*> v .: "object"
--                                       <*> v .: "property"

callExpression :: Object -> Parser Expression
callExpression v = CallExpression <$> pPos v <?> Key "call expression pPos"
                                  <*> v .: "callee"
                                  <*> v .: "arguments"

literal :: Object -> Parser Literal
literal v = do value <- v .: "value" 
               case value of 
                 (String text) -> LiteralString  <$> pPos v <*> pure (unpack text)
                 (Bool bool)   -> LiteralBoolean <$> pPos v <*> pure bool
                 (Number sci)  -> LiteralNumber  <$> pPos v <*> pure (toRealFloat sci)
                 Null          -> LiteralNull    <$> pPos v
                 _ -> error $ "Not a literal type: " ++ show value

pPos :: Object -> Parser (Int, Int)
pPos v = (,) <$> v .: "start" <*> v .: "end"