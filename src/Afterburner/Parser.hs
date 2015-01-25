module Afterburner.Parser ( parseProgram ) where

import Control.Applicative
import Control.Monad.Identity
import qualified Data.Text as T
import Data.Text (Text)

import Text.Parsec.Expr
import qualified Text.Parsec.Token as Token
import Text.Parsec.Text
import Text.Parsec hiding ((<|>), many, optional)

import Afterburner.AST

-- Main interface

parseProgram :: SourceName -> Text -> Either ParseError Program
parseProgram = parse $ whitespace *> program <* eof

-- Lexical/token parsers

pascalDef :: Token.GenLanguageDef Text st Identity
pascalDef = Token.LanguageDef
  { Token.commentStart = "{"
  , Token.commentEnd = "}"
  , Token.commentLine = "//"
  , Token.nestedComments = True
  , Token.identStart = letter <|> char '_'
  , Token.identLetter = alphaNum <|> char '_'
  , Token.opStart = oneOf "+-/*<=>.,"
  , Token.opLetter = oneOf "+-/*<=>.,"
  , Token.reservedNames =
    [ "absolute",
      "and",
      "array",
      "asm",
      "begin",
      "case",
      "const",
      "constructor",
      "destructor",
      "div",
      "do",
      "downto",
      "else",
      "end",
      "file",
      "for",
      "function",
      "goto",
      "if",
      "implementation",
      "in",
      "inherited",
      "inline",
      "interface",
      "label",
      "mod",
      "nil",
      "not",
      "object",
      "of",
      "operator",
      "or",
      "packed",
      "procedure",
      "program",
      "record",
      "reintroduce",
      "repeat",
      "self",
      "set",
      "shl",
      "shr",
      "string",
      "then",
      "to",
      "type",
      "unit",
      "until",
      "uses",
      "var",
      "while",
      "with",
      "xor"
    ]
  , Token.reservedOpNames = []
  , Token.caseSensitive = False
  }

pascal :: Token.GenTokenParser Text st Identity
pascal = Token.makeTokenParser pascalDef

whitespace :: Parser ()
whitespace = Token.whiteSpace pascal

reserved :: String -> Parser ()
reserved = Token.reserved pascal

parens :: Parser a -> Parser a
parens = Token.parens pascal

brackets :: Parser a -> Parser a
brackets = Token.brackets pascal

identifier :: Parser Id
identifier = T.pack <$> Token.identifier pascal <?> "identifier"

operator :: String -> Parser ()
operator = Token.reservedOp pascal

equals :: Parser ()
equals = void $ Token.symbol pascal "="

comma :: Parser ()
comma = void $ Token.comma pascal

semicolon :: Parser ()
semicolon = void $ Token.symbol pascal ";"

colon :: Parser ()
colon = void $ Token.colon pascal

integer :: Parser Integer
integer = Token.integer pascal

-- Grammar parsers

program :: Parser Program
program = reserved "program" *>
          pure Program <*>
          identifier <*>
          (parens (many identifier) <|> pure []) <* semicolon <*>
          declarations <*>
          statement

declarations :: Parser Declarations
declarations = Declarations <$>
               constantDefinitions <*>
               typeDefinitions <*>
               variableDeclarations <*>
               procedureDeclarations

constantDefinitions :: Parser [ConstantDefinition]
constantDefinitions = reserved "const" *> many1 constantDefinition <|>
                      pure []

constantDefinition :: Parser ConstantDefinition
constantDefinition = ConstantDefinition <$>
                     identifier <* equals <*>
                     constant

constant :: Parser Constant
constant = (IntConstant <$> integer) <|>
           (StringConstant . T.pack <$> Token.stringLiteral pascal)

typeDefinitions :: Parser [TypeDefinition]
typeDefinitions = reserved "type" *> many1 typeDefinition <|>
                  pure []

typeDefinition :: Parser TypeDefinition
typeDefinition = TypeDefinition <$>
                 identifier <* equals <*>
                 typeSpecifier

typeSpecifier :: Parser TypeSpecifier
typeSpecifier =
  choice [ pure NamedType <*> identifier

         , pure PointerTo <* operator "^" <*> typeSpecifier

         , pure RangedInteger <*> integer <* operator ".." <*> integer

         , pure Array <* reserved "array" <*>
           dimensions <* reserved "of" <*>
           typeSpecifier

         , pure Record <* reserved "record" <*>
           fields <* reserved "end"

         , pure File <* reserved "file" <* reserved "of" <*>
           typeSpecifier
         ]
  <?> "type specifier"

-- Returns [VariableDeclaration] instead of VariableDeclaration
-- because one Pascal VariableDeclaration' nonterminal can result in
-- several AST declarations.
variableDeclarations :: Parser [VariableDeclaration]
variableDeclarations = reserved "var" *> pure concat <*> many1 variableDeclaration <|>
                       pure []

variableDeclaration :: Parser [VariableDeclaration]
variableDeclaration = do names <- identifier `sepBy1` comma
                         colon
                         t <- typeSpecifier
                         return [ VariableDeclaration name t | name <- names ]

procedureDeclarations :: Parser [ProcedureDeclaration]
procedureDeclarations = many procedureDeclaration

procedureDeclaration :: Parser ProcedureDeclaration
procedureDeclaration = procedure <|> function
  where procedure = pure ProcedureDeclaration <*
                    reserved "procedure" <*>
                    identifier <*>
                    parameters <*>
                    pure Nothing <* semicolon <*>
                    body <* semicolon
                    <?> "procedure declaration"
        function = pure ProcedureDeclaration <*
                   reserved "function" <*>
                   identifier <*>
                   parameters <* colon <*>
                   (Just <$> typeSpecifier) <* semicolon <*>
                   body <* semicolon
                   <?> "function declaration"
        body = Just <$> ((,) <$> declarations <*> compoundStatement) <|>
               reserved "forward" *> pure Nothing

parameters :: Parser Parameters
parameters = parens (concat <$> parameter `sepBy1` semicolon) <|>
             pure []
             <?> "parameters"

parameter :: Parser [Parameter]
parameter = reserved "var" *> parameter' ByReference <|>
            parameter' ByValue
            <?> "parameter"
  where parameter' passage = do
          names <- identifier `sepBy1` comma
          colon
          t <- typeSpecifier
          return [ Parameter name t passage | name <- names ]

dimensions :: Parser Dimensions
dimensions = brackets (Dimensions <$> dimension <*> (comma *> sepBy dimension comma))
            <?> "dimensions"

dimension :: Parser Dimension
dimension = FixedDimension <$> integer <* operator ".." <*> integer <|>
            VariableDimension <$> identifier
            <?> "dimension"

fields :: Parser [Field]
fields = concat <$> field `sepBy1` semicolon
         <?> "fields"

-- Returns [Field] instead of Field because one Pascal 'Field'
-- nonterminal can result in several AST fields.
field :: Parser [Field]
field = do names <- identifier `sepBy1` comma
           colon
           t <- typeSpecifier
           return [ Field name t | name <- names ]
        <?> "field"

compoundStatement :: Parser [Statement]
compoundStatement =
  reserved "begin" *> statement `sepBy` semicolon <* reserved "end"

statement :: Parser Statement
statement =
  choice [ CompoundStatement <$> compoundStatement

         , Assign <$> variable <* operator ":=" <*> expression

         , ProcedureCall <$> identifier <*> actuals

         , pure ForLoop <* reserved "for" <*> identifier <* operator ":=" <*>
           expression <*> direction <*> expression <* reserved "do" <*>
           statement

         , pure WhileLoop <* reserved "while" <*> expression
           <* reserved "do" <*> statement

         , branch

         , pure CaseStatement <* reserved "case" <*> expression
           <* reserved "of" <*> switchCase `sepBy1` semicolon

         , pure Repeat <* reserved "repeat" <*>
           statement `sepBy1` semicolon <*
           reserved "until" <*>
           expression

         , pure Skip
         ]

branch :: Parser Statement
branch = do reserved "if"
            cond <- expression
            reserved "then"
            tbranch <- statement
            do reserved "else"
               fbranch <- statement
               return $ IfThenElse cond tbranch fbranch
               <|> return (IfThen cond tbranch)

switchCase :: Parser Case
switchCase = Case <$> constant `sepBy1` comma <*> statement

direction :: Parser Direction
direction = reserved "to" *> pure UpTo <|>
            reserved "downto" *> pure DownTo

actuals :: Parser [Expression]
actuals = parens (expression `sepBy1` comma) <|> pure []

variable :: Parser Variable
variable = variable' =<< (VariableId <$> identifier)
           <?> "assignment"
  where variable' v =
          (variable' =<< VariableField v <$> (operator "." *> identifier)) <|>
          (variable' =<< pure (VariableDereference v) <* operator "^") <|>
          (variable' =<< VariableIndex v <$> brackets (expression `sepBy1` comma)) <|>
          pure v

expression :: Parser Expression
expression = buildExpressionParser table prim
             <?> "expression"

prim :: Parser Expression
prim =  parens expression
        <|> Constant <$> constant
        <|> FunctionCall <$> identifier <*> actuals
        <|> Variable <$> identifier
        <?> "primitive expression"

table :: [[Operator Text () Identity Expression]]
table   = [ [ prefix "-" PrefixMinus
            , prefix "+" PrefixPlus
            , prefixW "not" Not
            ]
          , [ binary "*" Times AssocLeft,
              binary "/" Divide AssocLeft
            , binaryW "div" Divide AssocLeft
            , binaryW "mod" Mod AssocLeft
            , binaryW "and" And AssocLeft
            ]
          , [ binary "+" Plus AssocLeft
            , binary "-" Minus   AssocLeft
            , binaryW "or" Or   AssocLeft
            ]
           ]
  where binary   name fun assoc = Infix  (operator name *> return fun) assoc
        binaryW  name fun assoc = Infix  (reserved name *> return fun) assoc
        prefix   name fun       = Prefix (operator name *> return fun)
        prefixW  name fun       = Prefix (reserved name *> return fun)


