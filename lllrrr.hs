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
data Statement = Substitution (Identifier, Term)
               | Expression Term
               deriving (Show)
type Program = [Statement]
type Record = (Identifier, Term)
type Environment = [Record]
data ReturnValue = RVoid ()
                 | RTerm Term
                 deriving (Show)

parseSubst :: Parser Statement
parseSubst = do
    char '*'
    optionalSpace
    name <- identifier
    optionalSpace
    char '='
    optionalSpace
    body <- parseExpr
    return $ Substitution ((name,0), body)

parseValue :: Parser Statement
parseValue = liftM Expression parseExpr

parseStmt :: Parser Statement
parseStmt = parseSubst <|> parseValue

addRecord :: Record -> Environment -> Environment
addRecord r@(name,term) [] = [r]
addRecord r@(name,term) (r'@(name',term'):rs)
    | name == name' = r:rs
    | otherwise     = r':(addRecord r rs)

declare :: Record -> S.State Environment ReturnValue
declare (name, t) = S.state $ 
    \pairs -> (RVoid (), addRecord (name, eval $ f pairs) pairs)
        where
            f pairs = foldl (\t (n,b) -> subst t n b) t pairs

apply :: Term -> S.State Environment ReturnValue
apply term = S.state $ \pairs -> (RTerm $ eval $ f pairs, pairs)
    where
        f = foldl (\t (name, body) -> subst t name body) term

modifyenv (Substitution r) = declare r
modifyenv (Expression t) = apply t       

execProgram :: Program -> Environment
execProgram ss = snd $ S.runState (foldr1 (>>) $ map f ss) []
    where
        f (Substitution r) = declare r
        f (Expression t) = apply t       

readProgram :: [Either ParseError Statement] -> Either ParseError Environment
readProgram ss = sequence ss >>= (return . execProgram)

-- exec

main :: IO()
main = do
    input <- getContents
    let stmts = map (parse parseStmt "lllrrr") $ lines input
    let env   = readProgram stmts
    putStrLn . show $ env

