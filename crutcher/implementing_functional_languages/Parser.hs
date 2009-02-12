module Parser where

import Expr

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.ParserCombinators.Parsec.Token as PT
import qualified Text.ParserCombinators.Parsec.Expr as PE
import Text.ParserCombinators.Parsec ((<|>))
import qualified Text.ParserCombinators.Parsec.Language as PL

-- For main
import System.Environment
import Text.ParserCombinators.Parsec.Prim(parseTest)


parseCoreProgram :: P.Parser CoreProgram
parseCoreProgram = semiSep1 parseCoreScDefn

parseCoreScDefn :: P.Parser CoreScDefn
parseCoreScDefn = do
  n <- identifier
  ps <- parseNameList
  reservedOp "="
  e <- parseCoreExpr
  return (n, ps, e)


parseCoreExpr :: P.Parser CoreExpr
parseCoreExpr
   = PE.buildExpressionParser coreExprTable parseCoreTerm
 <|> parseCoreELet
 <|> parseCoreECase
 <|> parseCoreELam

coreExprTable :: PE.OperatorTable Char () CoreExpr
coreExprTable
  = [ [ binop "*" PE.AssocRight, binop "/" PE.AssocNone ],
      [ binop "+" PE.AssocRight, binop "-" PE.AssocNone ],
      [ binop "==" PE.AssocNone, binop "~=" PE.AssocNone,
        binop ">" PE.AssocNone, binop ">=" PE.AssocNone,
        binop "<" PE.AssocNone, binop "<=" PE.AssocNone ],
      [ binop "&" PE.AssocRight ],
      [ binop "|" PE.AssocRight ] ]

binop name assoc = PE.Infix (reservedOp name >> return f) assoc
  where
  f a b = EAp (EAp (EVar name) a) b


parseCoreTerm :: P.Parser CoreExpr
parseCoreTerm = (P.many1 parseCoreAExpr >>= return . foldl1 EAp)

parseCoreAExpr :: P.Parser CoreExpr
parseCoreAExpr
   = parseCoreEVar
 <|> parseCoreENum
 <|> parseCoreEConstr
 <|> parens parseCoreExpr


parseCoreECase :: P.Parser CoreExpr
parseCoreECase = do
  reserved "case"
  e <- parseCoreExpr
  reserved "of"
  as <- parseCoreAlts
  return $ ECase e as

parseCoreAlts :: P.Parser [CoreAlt]
parseCoreAlts = semiSep1 parseCoreAlt

parseCoreAlt :: P.Parser CoreAlt
parseCoreAlt = do
  t <- angles parseCoreENum >>= return . val
  ps <- parseNameList
  reservedOp "->"
  e <- parseCoreExpr
  return $ (t, ps, e)
  

parseCoreELet :: P.Parser CoreExpr
parseCoreELet = do
  r <- (reserved "let" >> return nonRecursive)
       <|> (reserved "letrec" >> return recursive)
  ds <- parseCoreDefns
  reserved "in"
  e <- parseCoreExpr
  return $ ELet r ds e

parseCoreDefns :: P.Parser [CoreDefn]
parseCoreDefns = semiSep1 parseCoreDefn

parseCoreDefn :: P.Parser CoreDefn
parseCoreDefn = do
  n <- parseCoreEVar >>= return . name
  reservedOp "="
  e <- parseCoreExpr
  return (n, e)


parseCoreELam :: P.Parser CoreExpr
parseCoreELam = do
  reservedOp "\\"
  ps <- parseNameList
  reservedOp "."
  e <- parseCoreExpr
  return $ ELam ps e
    

parseCoreEConstr :: P.Parser CoreExpr
parseCoreEConstr = do
  reserved "Pack"
  braces $ do
    t <- parseCoreENum >>= return . val
    comma
    a <- parseCoreENum >>= return . val
    return $ EConstr t a

parseCoreEVar :: P.Parser CoreExpr
parseCoreEVar = identifier >>= return . EVar

parseCoreENum :: P.Parser CoreExpr
parseCoreENum = natural >>= return . ENum . fromIntegral

parseNameList :: P.Parser [String]
parseNameList = P.many identifier


-- | Core language definition
coreLanguageDef :: PL.LanguageDef ()
coreLanguageDef =
  PL.emptyDef
  { PL.commentLine = "||"
  , PL.nestedComments = False
  , PL.identStart = P.letter
  , PL.reservedNames =
      ["Pack", "let", "letrec", "case", "of", "in"]
  , PL.reservedOpNames =
      ["\\", "->", ".",
       "+", "-", "*", "/", "&", "|",
       "<", "<=", "==", "~=", ">=", ">" ] }

-- | Core lexer
lexer :: PT.TokenParser ()
lexer = PT.makeTokenParser coreLanguageDef

-- | Lexer utilities
identifier = PT.identifier lexer
reserved = PT.reserved lexer
reservedOp = PT.reservedOp lexer
parens = PT.parens lexer
braces = PT.braces lexer
angles = PT.angles lexer
comma = PT.comma lexer
semiSep1 = PT.semiSep1 lexer
natural = PT.natural lexer
