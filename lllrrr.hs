import Text.Parsec
import Text.Parsec.String
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Text.Parsec.Language (haskellDef)
import Control.Monad.Identity
import qualified Control.Monad.State as S
import Data.List
import Data.Char

-- AST

type Identifier = (String, Int)

showIdentifier :: Identifier -> String
showIdentifier (x, n)
    | n == 0    = x
    | otherwise = x ++ "_" ++ (show n)

data Term = Variable Identifier
          | Lambda Identifier Term
          | App Term Term
--          deriving (Show)

instance Show (Term) where
    show (Variable v) = showIdentifier v
    show (Lambda x m) = "(\\" ++ showIdentifier x ++ " " ++ show m ++ ")"
    show (App m n) = "(" ++ (show m) ++ " " ++ (show n) ++ ")"

-- lexer

identifier :: Parser String
identifier = liftM (:[]) lower <|> many1 (satisfy isAsciiUpper)

parens :: Parser a -> Parser a
parens parser = do
    char '('
    x <- parser
    char ')'
    return x

optionalSpace :: Parser ()
optionalSpace = skipMany space

-- parser

term :: Parser Term
term = liftM (\s->Variable (s,0)) identifier
   <|> parens parseExpr

parseApp :: Parser Term
parseApp = do
    exprs <- sepBy term optionalSpace
    return $ foldl1 App exprs

parseLambdaExpr :: Parser Term
parseLambdaExpr = do
    string "\\"
    vars <- many1 identifier
    string "."
    body <- parseExpr
    return $ foldr (\s->Lambda(s,0)) body vars

parseExpr :: Parser Term
parseExpr = parseLambdaExpr <|> parseApp

parseDeclare :: Parser (Identifier, Term)
parseDeclare = do
    string "*"
    name <- identifier
    string "="
    body <- parseExpr
    return $ ((name,0), body)

-- eval

fv :: Term -> [Identifier]
fv (Variable v) = [v]
fv (Lambda x m) = (fv m) \\ [x]
fv (App m n) = union (fv m) (fv n)

gensym :: [Identifier] -> Identifier -> Identifier -> Identifier
gensym fvs x (y,m) = 
    head . filter (\v -> ((notElem v fvs) && v /= x)) . 
        map (\p->(y,p+m)) $ [1..]

subst :: Term -> Identifier -> Term -> Term
subst (Variable y) x n = if y == x then n else Variable y
subst (Lambda y m) x n
    | y == x    = (Lambda y m)
    | otherwise = Lambda z (subst (subst m y (Variable z)) x n)
        where
            z = gensym (fv (App m n)) x y
subst (App m1 m2) x n = App (subst m1 x n) (subst m2 x n)

eval :: Term -> Term
eval v@(Variable x)       = v
eval (Lambda x m)         = Lambda x $ eval m
eval (App m@(App (Variable _) _) n)  = App (eval m) $ n
eval (App m@(App _ _) n)  = eval (App (eval m) $ n)
eval (App (Lambda x m) n) = eval $ subst m x $ eval n
eval (App m n) = App m (eval n)

-- substitution
data Statement = Substitution Identifier Term
               | Expression Term
type Program = [Statement]
type Record = (Identifier, Term)
type Environment = [Record]
data ReturnValue = RVoid ()
                 | RTerm Term

declare :: Record -> S.State Environment ReturnValue
declare pair = S.state $ \pairs -> (RVoid (), pair:pairs)

apply :: Term -> S.State Environment ReturnValue
apply term = S.state $ \pairs -> (RTerm $ term' pairs, pairs)
    where
        term' pairs = foldr (\(name,body) t -> subst t name body) term pairs

execProgram :: Program -> Environment
execProgram ss = snd $ S.runState (foldr1 (>>) $ map f ss) []
    where
        f (Substitution name t) = declare (name, t)
        f (Expression t) = apply t       

-- exec

parseTerm :: String -> Either ParseError Term
parseTerm = parse parseExpr "lllrrr"


main :: IO()
main = do
    str <- getLine :: IO String
    case parse parseExpr "lllrrr" str of 
        Left err -> putStrLn . show $ err
        Right x  -> do
            putStrLn . show . eval $ x

