{-# LANGUAGE OverloadedStrings #-}

module Acorn.Output where 

import Data.Aeson
import Data.Aeson.Types
import Data.Text (unpack)
import Data.Scientific (toRealFloat)

data Output = OutputError AcornError 
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

data Expression = Identifier Pos String
                | MemberExpression Pos Bool Expression Expression -- { computed, object, property }
                | CallExpression Pos Expression [Expression]      -- { callee, arguments }
                | Literal Pos Literal
                deriving Show

data Literal = LiteralString Pos String 
             | LiteralBoolean Pos Bool
             | LiteralNull Pos
             | LiteralNumber Pos Double
             deriving Show

type Pos = (Int, Int)

-- withObject :: String -> (Object -> Parser a) -> Value -> Parser a
-- (.:)       :: Object -> Text -> Parser a

instance FromJSON Output where 
  parseJSON value = withObject "Output" 
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
parseErrorLoc v = v .: "error loc" >>= withObject "acorn loc" (\eloc -> (,) <$> eloc .: "line" <*> eloc .: "column")

instance FromJSON Program where 
  parseJSON = withObject "Program" $ \v -> 
    Program <$> loc v <?> Key "program loc"
            <*> v .: "sourceType"
            <*> v .: "body"

instance FromJSON ExpressionStatement where 
  parseJSON = withObject "ExpressionStatement" $ \v -> 
    ExpressionStatement <$> loc v <?> Key "expression statement loc"
                        <*> v .: "expression"

instance FromJSON Expression where 
  parseJSON = withObject "Expression" $ \v -> 
    do vTypeValue <- v .: "type"
       case vTypeValue of 
         (String "Identifier") -> identifier v
         (String "MemberExpression") -> memberExpression v
         (String "CallExpression") -> callExpression v
         (String "Literal") -> Literal <$> (loc v <?> Key "literal loc") <*> literal v
         _ -> undefined

identifier :: Object -> Parser Expression
identifier v = Identifier <$> loc v <*> v .: "name"

memberExpression :: Object -> Parser Expression
memberExpression v = MemberExpression <$> loc v <?> Key "member expression loc"
                                      <*> v .: "computed" 
                                      <*> v .: "object"
                                      <*> v .: "property"

callExpression :: Object -> Parser Expression
callExpression v = CallExpression <$> loc v <?> Key "call expression loc"
                                  <*> v .: "callee"
                                  <*> v .: "arguments"

literal :: Object -> Parser Literal
literal v = do value <- v .: "value" 
               case value of 
                 (String text) -> LiteralString  <$> loc v <*> pure (unpack text)
                 (Bool bool)   -> LiteralBoolean <$> loc v <*> pure bool
                 (Number sci)  -> LiteralNumber  <$> loc v <*> pure (toRealFloat sci)
                 Null          -> LiteralNull    <$> loc v
                 _ -> error $ "Not a literal type: " ++ show value

loc :: Object -> Parser (Int, Int)
loc v = (,) <$> v .: "start" <*> v .: "end"