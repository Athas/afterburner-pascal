-- | Afterburner Pascal AST, originally derived from
-- <http://webserv.jcu.edu/math//faculty/ERIC/cs465/pascal/pascfg.pdf>.
--
-- This is an AST designed for the internal workings of a compiler,
-- not to preserve exactly the original form of valid Pascal programs.
--
-- Specifically has been slightly simplified/normalised to remove
-- redundancy, for example removing the ability to declare several
-- variables or parameters of the same type in one declaration.  This
-- leads to no loss of expressivity, but slightly simplifies the rest
-- of the compiler.
module Afterburner.AST
       ( Id
       , Program (..)
       , Declarations (..)
       , ConstantDefinitions
       , TypeDefinitions
       , VariableDeclarations
       , ProcedureDeclarations
       , ConstantDefinition (..)
       , TypeDefinition (..)
       , VariableDeclaration (..)
       , ProcedureDeclaration (..)
       , Parameters
       , Parameter (..)
       , Passage (..)
       , Constant (..)
       , TypeSpecifier (..)
       , Statements
       , Statement (..)
       , Direction (..)
       , Case (..)
       , Variable (..)
       , Expression (..)
       , Dimensions (..)
       , Dimension (..)
       , Fields
       , Field (..)
       )
       where

import Data.Text (Text)

type Id = Text

data Program = Program { programId :: Id
                       , programParameters :: [Id]
                       , programDeclarations :: Declarations
                       , programStatement :: Statement
                       }
               deriving (Show)

data Declarations = Declarations { declConstantDefinitions :: ConstantDefinitions
                                 , declTypeDefinitions :: TypeDefinitions
                                 , declVariableDeclarations :: VariableDeclarations
                                 , declProcedureDeclarations :: ProcedureDeclarations
                                 }
                    deriving (Show)

type ConstantDefinitions = [ConstantDefinition]

type TypeDefinitions = [TypeDefinition]

type VariableDeclarations = [VariableDeclaration]

type ProcedureDeclarations = [ProcedureDeclaration]

data ConstantDefinition =
  ConstantDefinition { constantDefinitionId :: Id
                     , constantDefinitionValue :: Constant
                     }
  deriving (Show)

data TypeDefinition =
  TypeDefinition { typeDefinitionId :: Id
                 , typeDefinitionType :: TypeSpecifier
                 }
  deriving (Show)

data VariableDeclaration =
  VariableDeclaration { variableDeclarationIdList :: Id
                      , variableDeclarationType :: TypeSpecifier
                      }
  deriving (Show)

data ProcedureDeclaration =
  ProcedureDeclaration { procedureDeclarationId :: Id
                       , procedureDeclarationParameters :: Parameters
                       , prodecureDeclarationType :: Maybe TypeSpecifier
                       , procedureDeclarationBody :: Maybe (Declarations, Statements)
                       }
  deriving (Show)

type Parameters = [Parameter]

data Parameter =
  Parameter { parameterId :: Id
            , parameterType :: TypeSpecifier
            , parameterPassage :: Passage
            }
  deriving (Show)

data Passage = ByValue
             | ByReference
             deriving (Show)

data Constant = IntConstant Integer
              | StringConstant Text
  deriving (Show)

data TypeSpecifier = NamedType Id
                   | PointerTo TypeSpecifier
                   | RangedInteger Integer Integer
                   | Array Dimensions TypeSpecifier
                   | Record Fields
                   | File TypeSpecifier
                   deriving (Show)

type Statements = [Statement]

data Statement = CompoundStatement [Statement]
               | Assign Variable Expression
               | ProcedureCall Id [Expression]
               | ForLoop Id Expression Direction Expression Statement
               | WhileLoop Expression Statement
               | IfThen Expression Statement
               | IfThenElse Expression Statement Statement
               | CaseStatement Expression [Case]
               | Repeat [Statement] Expression
               | Skip
               deriving (Show)

data Direction = UpTo
               | DownTo
               deriving (Show)

data Case =
  Case { caseConstants :: [Constant]
       , caseStatement :: Statement
       }
  deriving (Show)

data Variable = VariableId Id
              | VariableField Variable Id
              | VariableDereference Variable
              | VariableIndex Variable [Expression]
              deriving (Show)

data Expression = Equal Expression Expression
                | NotEqual Expression Expression
                | LessThan Expression Expression
                | LessThanOrEqual Expression Expression
                | GreaterThan Expression Expression
                | GreaterThanOrEqual Expression Expression

                | Plus Expression Expression
                | Minus Expression Expression
                | Times Expression Expression
                | Divide Expression Expression
                | IntDivide Expression Expression
                | Mod Expression Expression
                | PrefixPlus Expression
                | PrefixMinus Expression

                | Or Expression Expression
                | And Expression Expression
                | Not Expression
                | FunctionCall Id [Expression]
                | Constant Constant
                | Variable Id
                deriving (Show)

data Dimensions =
  Dimensions { dimensionsFirst :: Dimension
             , dimensionsRest :: [Dimension]
             }
  deriving (Show)

data Dimension = FixedDimension Integer Integer
               | VariableDimension Id
               deriving (Show)

type Fields = [Field]

data Field =
  Field { fieldId :: Id
        , fieldType :: TypeSpecifier
        }
  deriving (Show)
