{-# LANGUAGE FlexibleContexts, LambdaCase, UndecidableInstances, ConstraintKinds, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Reduced Python Syntax and its compiler
module Syntax.Python.Compiler(compile, parse, lexical, PyLoc(..), PyTag(..), tagAs) where


import Syntax.Python.AST
import Data.Functor
import Data.Maybe
import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Applicative ((<|>), liftA2, asum)
import Syntax.Python.Parser (parseFile, SrcSpan)
import Language.Python.Common.AST hiding (List, Handler, Try, Raise, Conditional, Pass, Continue, Break, Return, Call, Var, Bool, Tuple, Global, NonLocal)
import qualified Language.Python.Common.AST as AST
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Bitraversable
import Control.Monad.Cond
import Language.Python.Common.SrcLocation (spanning)
import Language.Python.Common (Span, startRow, startCol)
import Data.Bifunctor (Bifunctor(second, first))
import Data.Function ((&))


todo :: String -> a
todo = error . ("COMPILER ERROR: " ++)


-------------------------------------------------------------------------------
-- Source code locations with extra Python-specific tags 
-------------------------------------------------------------------------------

data PyLoc = PyLoc SrcSpan (Maybe PyTag)
   deriving (Eq, Ord)
data PyTag = FrmTag
           | ClsStr
           | ClsTup
           | ClsDct
           | ClsNew
           | IniBnd
           | IniCll
   deriving (Eq, Ord, Show, Bounded, Enum)

instance Show PyLoc where
   show (PyLoc s t) = locStr ++ tagStr
    where locStr = show (startRow s) ++ ":" ++ show (startCol s)
          tagStr = maybe "" ((':':) . show) t

untagged :: SrcSpan -> PyLoc
untagged = flip PyLoc Nothing

tagAs :: PyTag -> PyLoc -> PyLoc
tagAs tag (PyLoc loc _) = PyLoc loc (Just tag)

spanningTagged :: PyLoc -> PyLoc -> PyLoc
spanningTagged (PyLoc a1 Nothing) (PyLoc a2 Nothing) = PyLoc (spanning a1 a2) Nothing
spanningTagged _ _ = error "spanning not supported for tagged locations"

-------------------------------------------------------------------------------
-- From String to AST
-------------------------------------------------------------------------------

-- | Parse a Python file to an AST
parse :: String  -- ^ filename
      -> String  -- ^ contents
      -> Maybe (Program PyLoc AfterLexicalAddressing)
parse nam = parseFile nam >=> (\ex -> runLexical <$> runReaderT (compile $ untagged <$> ex) Nothing)

-------------------------------------------------------------------------------
-- Simplification phase
-------------------------------------------------------------------------------

-- | Simplification phase monad
type SimplifyM m a = MonadReader (Maybe (Ide a)) m

thunkify :: a -> Stmt a AfterSimplification -> Stmt a AfterSimplification
thunkify a bdy = StmtExp () (Call (Lam [] bdy a ()) [] [] a) a

-- | Generate a (potentially namespaced) lhs pattern
namespacedLhs :: SimplifyM m a => Ide a -> m (Lhs a AfterSimplification)
namespacedLhs nam = asks lhs
   where lhs (Just v) = IdePat (NamespacedIde nam v)
         lhs Nothing  = IdePat nam

-- | Generate an assignment
assign :: SimplifyM m a => Ide a -> Exp a AfterSimplification -> m (Stmt a AfterSimplification)
assign nam e = namespacedLhs nam <&> flip (Assg ()) e

-- | Compile Python programs into the reduced Python syntax
compile :: SimplifyM m PyLoc => Module PyLoc -> m (Program PyLoc AfterSimplification)
compile (Module stmts) = Program . makeSeq <$> mapM compileStmt stmts

-- | Compile a statement in the Python reduced syntax
compileStmt :: SimplifyM m PyLoc => Statement PyLoc -> m (Stmt PyLoc AfterSimplification)
compileStmt (Fun nam ags _ bdy a)         = assign (Ide nam) =<< compileFun ags bdy a
compileStmt (While cnd bdy els a)         = Loop () (compileExp cnd) <$> compileSequence bdy <*> pure a
compileStmt (AsyncFun def _)              = error "not supported AsyncFun"
compileStmt (AST.Conditional grds els a)  = Conditional () <$> mapM (\(exp, st) -> fmap (compileExp exp,) (compileSequence st)) grds <*> compileSequence els <*> pure a
compileStmt (StmtExpr e a)                = pure (StmtExp () (compileExp e) a)
compileStmt (Import items _)              = error "import not supported"
compileStmt (FromImport items _ _)        = error "import not supported"
compileStmt (For vrs gen bdy els _)       = todo "for expressions"
compileStmt (Class nam ags bdy a)         = do
   assignment <- assign (Ide nam) (compileClassInstance a (ident_string nam) ags)
   ltt <- thunkify a <$> compileClassBdy (Ide nam) bdy
   return $ makeSeq [assignment, ltt]
compileStmt (Assign [Subscript e i a1] r a2) = pure $ flip (StmtExp ()) a2 $ Call (Read (compileExp e) (Ide (Ident "__setitem__" a1)) a1)
                                                                                  [compileExp i, compileExp r]
                                                                                  []
                                                                                  a2
compileStmt (Assign to expr _)               = Assg () <$> compileLhs to <*> return (compileExp expr)
compileStmt (AugmentedAssign to op exp a) = compileStmt (Assign [to] (BinaryOp (translateOp op) to exp a) a)
compileStmt (Decorated decs def _)        = todo "eval decorated function"
compileStmt (AST.Return expr a)           = pure $ Return () (fmap compileExp expr) a
compileStmt (AST.Raise rexp a)            = pure $ Raise  () (compileRaiseExp rexp) a
compileStmt (AST.Try bdy hds [] [] a)     = compileTry bdy hds a
compileStmt (AST.Try {})                  = todo "try with finally and/or else part"
compileStmt (With ctx bdy _)              = todo "eval with statement"
compileStmt (AsyncWith stmt _)            = todo "eval async with statement"
compileStmt (AST.Pass a)                  = return $ makeSeq []
compileStmt (AST.Break a)                 = return $ Break () a
compileStmt (AST.Continue a)              = return $ Continue () a
compileStmt (AST.Global as a)             = return $ makeSeq $ map (flip (Global ()) a . Ide) as
compileStmt (AST.NonLocal as a)           = return $ makeSeq $ map (flip (NonLocal ()) a . Ide) as
compileStmt (Delete exs _)                = todo "delete statement"
compileStmt (Assert exs _)                = todo "assertion statement"
compileStmt (AsyncFor {})                 = error "unsupported exp"
compileStmt (AnnotatedAssign {})          = error "unsupported exp"
compileStmt (Print {})                    = error "unsupported exp"
compileStmt (Exec {})                     = error "unsupported exp"


compileRaiseExp :: RaiseExpr PyLoc -> Exp PyLoc AfterSimplification
compileRaiseExp (RaiseV3 (Just (expr, Nothing))) = compileExp expr
compileRaiseExp _ = todo "unsupported raise expression with from or empty raise"

compileTry :: SimplifyM m PyLoc => Suite PyLoc -> [AST.Handler PyLoc] -> PyLoc -> m (Stmt PyLoc AfterSimplification)
compileTry bdy hds loc = Try () <$> compileSequence bdy <*> mapM compileHandler hds <*> pure loc
   where compileHandler (AST.Handler cls bdy _) = (compileClause cls,) <$> compileSequence bdy
         compileClause  (AST.ExceptClause Nothing _)               = Var (Ide (Ident "Exception" loc))
         compileClause  (AST.ExceptClause (Just (exc, Nothing)) _) = compileExp exc
         compileClause _                                           = todo "unsupported except clause"

-- | Compiles a sequence without introducing a block
compileSequence :: SimplifyM m PyLoc => Suite PyLoc -> m (Stmt PyLoc AfterSimplification)
compileSequence es = makeSeq <$> mapM compileStmt es

-- | Compiles a block (something that has a different lexical scope)
compileFun :: SimplifyM m PyLoc => [Parameter PyLoc] -> Suite PyLoc -> PyLoc -> m (Exp PyLoc AfterSimplification)
compileFun prs bdy a = Lam (compilePrs prs) <$> local (const Nothing) (compileSequence bdy) <*> pure a <*> pure ()

-- | Compile the parameters of a function
compilePrs :: [Parameter PyLoc] -> [Par PyLoc AfterSimplification]
compilePrs [] = []
compilePrs ((Param nam _ def a) : xs) = Prm (Ide nam) a  : compilePrs xs
compilePrs ((EndPositional _) : xs) = compilePrs xs
compilePrs ((VarArgsPos nam _ a) : xs) = VarArg (Ide nam) a : compilePrs xs
compilePrs ((VarArgsKeyword nam _ a) : xs) = VarKeyword (Ide nam) a : compilePrs xs
compilePrs ((UnPackTuple {}) : _) = error "unknown type of expression"

-------------------------------------------------------------------------------
-- Expressions
-------------------------------------------------------------------------------

-- | Compiles an expression into a reduced Python expression
compileExp :: Expr PyLoc -> Exp PyLoc AfterSimplification
-- | Variables
compileExp (AST.Var ident _) = Var (Ide ident)
-- literals
compileExp (Int i _ a) = Literal (Integer i a)
-- todo: add support for the real domain in the abstract domain
-- compileExp (Float f _ _)       = pure $ inject f
compileExp (Imaginary {})      = todo "eval imaginary numbers"
compileExp (AST.Bool b a)      = Literal (Bool b a)
compileExp (Ellipsis _)        = todo "nothing"
compileExp (ByteStrings _ _)   = todo "eval bytestrings"
compileExp (Strings ss a)      = Literal (String (concat ss) a)
-- compound expressions
compileExp c@(AST.Call fun arg a)     = compileCall fun arg a -- Call (compileExp fun) (map compileArg arg) a
compileExp (Subscript e i a)          = Call (Read (compileExp e) (Ide (Ident "__getitem__" a)) a) [compileExp i] [] a
compileExp (SlicedExpr e sl _)        = todo "eval sliced"
compileExp (Yield yld _)              = todo "eval yield"
compileExp (Generator comp _)         = todo "eval generator expression"
compileExp (Await ex _)               = todo "eval await expression"
compileExp (ListComp comp _)          = todo "eval list comprehension"
compileExp (AST.List exs a)           = Literal $ List (map compileExp exs) a
compileExp (Dictionary map a)         = Literal (compileDict map a)
compileExp (DictComp comp _)          = todo "eval dictionary comprehension"
compileExp (Set exs _)                = todo "eval sets"
compileExp (SetComp comp _)           = todo "eval set comprehension"
compileExp (Starred ex _)             = todo "eval starred expression"
compileExp (Paren ex _)               = compileExp ex
compileExp (CondExpr tru cnd fls _)   = todo "eval conditional expression"
compileExp (BinaryOp op left right a) = binaryToCall op left right a
compileExp (UnaryOp op arg _)         = todo "eval unary op"
compileExp (Dot rcv atr a)            = Read (compileExp rcv) (Ide atr) a
compileExp (Lambda ags bdy annot)     = Lam (compilePrs ags) (Return () (Just $ compileExp bdy) annot) annot () -- note: [] is because of no local variables
compileExp (AST.Tuple exs _)          = todo "eval tuples"
compileExp (LongInt {})               = todo "longInt"
compileExp (Float {})                 = todo "float"
compileExp (None {})                  = todo "none"
compileExp (UnicodeStrings {})        = todo "unicodeStrings"
compileExp (StringConversion {})      = todo "stringConversion"
--compileExp ex = error "unsupported expression"-- ++ show (pretty ex))

compileCall :: Expr PyLoc -> [Argument PyLoc] -> PyLoc -> Exp PyLoc AfterSimplification
compileCall fun args = Call (compileExp fun) posArgs kwArgs
   where (posArgs, kwArgs) = compileArgs args

compileArgs :: [Argument PyLoc] -> ([Exp PyLoc AfterSimplification], [(Ide PyLoc, Exp PyLoc AfterSimplification)])
compileArgs args = (posCompiled, kwCompiled)
   where (posArgs, kwArgs) = args & span (\case ArgExpr{} -> True
                                                _         -> False)
         posCompiled = posArgs & map (\case ArgExpr e _ -> compileExp e
                                            _           -> error "should not happen!")
         kwCompiled = kwArgs & map (\case ArgKeyword k e _ -> (Ide k, compileExp e)
                                          _                -> error "nonkeyword argument not allowed after keywords")

--findKeyword :: Span PyLoc => String -> [Arg PyLoc ξ] -> Exp PyLoc ξ -> PyLoc -> (Exp PyLoc ξ, PyLoc)
--findKeyword nam [] def defa = (def, defa)
--findKeyword needle (KeyArg (Ide (Ident nam a)) ex _ : _)  _ _
--   | nam == needle = (ex, a)
--findKeyword nam (_ : ags) def defa = findKeyword nam ags def defa

--positional :: [Arg a ξ] -> [Exp a ξ]
--positional = map (\case PosArg e _ -> e) . filter (\case PosArg _ _ -> True ; KeyArg {} -> False)

-- | Compiles a class definition
-- A class definition is compiled to 
compileClassInstance :: PyLoc -> String -> [Argument PyLoc] -> Exp PyLoc AfterSimplification
compileClassInstance a nam ags =
   -- First find out which meta-class to use for the instantation
   -- NOTE -- this transformation assumes that:
   -- * the variable 'type' is not shadowed
   -- * the metaclass expression is a simple expression (easily fixable though)
   let (posArgs, kwArgs) = compileArgs ags
       metaclass = fromMaybe (Var (Ide (Ident "type" a))) $ lookup "metaclass" $ map (first ideName) kwArgs  --findKeyword "metaclass" arguments (Var (Ide (Ident "type" a))) a
   in Call metaclass
           [Literal (String nam $ tagAs ClsStr a),
            Literal (Tuple posArgs $ tagAs ClsTup a),
            Literal (Dict [] $ tagAs ClsDct a)]
           []
           (tagAs ClsNew a)

-- | Compiles a class body
compileClassBdy :: SimplifyM m PyLoc => Ide PyLoc -> Suite PyLoc -> m (Stmt PyLoc AfterSimplification)
compileClassBdy nam bdy = makeSeq <$> mapM (local (const $ Just nam) . compileStmt) bdy

-- | Compiles the left-hand-side of an assignment
compileLhs :: SimplifyM m PyLoc => [Expr PyLoc] -> m (Lhs PyLoc AfterSimplification)
compileLhs [AST.Var ident _] = namespacedLhs (Ide ident)
compileLhs [Dot e x a] = return $ Field (compileExp e) (Ide x) a
compileLhs ex = error "unsupported expression as LHS in assignment"

-- | Translates a binary operation to a function call
binaryToCall :: Op PyLoc -> Expr PyLoc -> Expr PyLoc -> PyLoc -> Exp PyLoc AfterSimplification
binaryToCall op left right a =
   let compiledLeft  = compileExp left
       compiledRight = compileExp right
   in Call (Read compiledLeft (opToIde op) (spanningTagged (annot left) (annot op)))
           [compiledRight]
           []
           a

compileDict :: [DictKeyDatumList PyLoc] -> PyLoc -> Lit PyLoc AfterSimplification
compileDict bds = Dict (map compileBds bds)
   where compileBds (DictMappingPair kexpr vexpr) = (compileExp kexpr, compileExp vexpr)
         compileBds _ = error "unsupported dictionary entry (unwrapping)"

opToIde :: Op a -> Ide a
opToIde op = case op of
   Not a -> error "do not support no"
   Exponent a -> Ide (Ident "__pow__" a)
   LessThan a -> Ide (Ident "__lt__" a)
   GreaterThan a -> Ide (Ident "__gt__" a)
   Equality a -> Ide (Ident "__eq__" a)
   GreaterThanEquals a -> Ide (Ident "__ge__" a)
   LessThanEquals a -> Ide (Ident "__le__" a)
   NotEquals a -> Ide (Ident "__ne__" a)
   BinaryOr a -> Ide (Ident "__or__" a)
   Xor a -> Ide (Ident "__xor__" a)
   BinaryAnd a -> Ide (Ident "__and__" a)
   ShiftLeft a -> Ide (Ident "__lshift__" a)
   ShiftRight a -> Ide (Ident "__rshift__" a)
   Multiply a -> Ide (Ident "__mul__" a)
   Plus a -> Ide (Ident "__add__" a)
   Minus a -> Ide (Ident "__sub__" a)
   Divide a -> Ide (Ident "__truediv__" a)
   FloorDivide a -> Ide (Ident "__floordiv__" a)
   MatrixMult a -> Ide (Ident "__matmul__" a)
   Invert a -> Ide (Ident "__invert__" a)
   Modulo a -> Ide (Ident "__mod__" a)
   _ -> error "unsupported operator"



-- | Translates an assignment operation to a regular operation
translateOp :: AssignOp a -> Op a
translateOp op = case op of
   PlusAssign  a -> Plus  a
   MinusAssign a -> Minus a
   MultAssign  a -> Multiply  a
   DivAssign   a -> Divide a
   ModAssign   a -> Modulo a
   PowAssign   a -> Exponent a
   BinAndAssign a -> BinaryAnd a
   BinOrAssign  a -> BinaryOr a
   BinXorAssign a -> Xor a
   LeftShiftAssign a -> ShiftLeft a
   RightShiftAssign a -> ShiftRight a
   FloorDivAssign a -> FloorDivide a
   MatrixMultAssign a -> MatrixMult a

-------------------------------------------------------------------------------
-- Lexical addressing
-------------------------------------------------------------------------------

-- | A frame is a flat mapping from strings to lexical addresses
newtype Frame a = Frame { getFrame :: Map String (IdeLex a) }
-- | An environment is a linked list of frames
type Env a = [Frame a]

-- | Look something up in the given frame
lookupFrm :: String -> Frame a -> Maybe (IdeLex a)
lookupFrm nam = Map.lookup nam . getFrame

-- | Look something up in the environment
lookupEnv ::  String -> Env a -> IdeLex a
lookupEnv nam env  = fromMaybe (IdeGbl nam) (asum $ map (lookupFrm nam) env)

-- | Returns the nonlocal environment (i.e. skips the global frame)
nonlocalEnv :: Env a -> Env a
nonlocalEnv = tail

-- | Extends the given environment with a new frame consisting of the giving bindings
extendedEnv :: [(String, IdeLex a)] -> Env a -> Env a
extendedEnv = (:) . Frame . Map.fromList

-- | Lexical addresser state

data VarScope = LocalScope | NonLocalScope | GlobalScope
   deriving (Eq, Ord, Show)

data LexicalState = LexicalState {
   vars  :: Map String VarScope,
   fresh :: Int  -- ^ fresh variable counter
}

isScopedAs :: LexicalM m a => VarScope -> String -> m Bool
isScopedAs typ nam = gets (\s -> Map.lookup nam (vars s) == Just typ)

isLocal :: LexicalM m a => String -> m Bool
isLocal     = isScopedAs LocalScope
isNonLocal :: LexicalM m a => String -> m Bool
isNonLocal  = isScopedAs NonLocalScope
isGlobal :: LexicalM m a => String -> m Bool
isGlobal    = isScopedAs GlobalScope

getLocals :: LexicalM m a => m [String]
getLocals = gets (Map.keys . Map.filter (== LocalScope) . vars)

-- | Enters a new scope by resetting all global and nonlocal variables
-- and creating a new frame with the given bindigs to execute the given computation in
enterScope :: (LexicalM m a) => [(String, IdeLex a)] -> m b -> m (b, [String])
enterScope bds m = do
   -- first snapshot the current set of globals, nonlocals and locals
   snapshot <- get
   -- then continue with their reset
   modify (\s -> s { vars = Map.fromList (map (second $ const LocalScope) bds) })
   -- run the computation in the extended environment
   v <- local (extendedEnv bds) m
   -- reset to the snapshot, but keep the fresh
   l <- getLocals
   modify (\s' -> snapshot { fresh = fresh s' })
   -- return the value of the computation
   return (v, l)

addVar :: LexicalM m a => VarScope -> String -> m ()
addVar scp nam = modify (\s -> s { vars = Map.insertWith checkSame nam scp (vars s) })
   where checkSame a b
            | a == b = a
            | otherwise = error ("Var previously declared " ++ show a ++ " is now declared " ++ show b)

addLocal :: LexicalM m a => String -> m ()
addLocal    = addVar LocalScope
addNonLocal :: LexicalM m a => String -> m ()
addNonLocal = addVar NonLocalScope
addGlobal :: LexicalM m a => String -> m ()
addGlobal   = addVar GlobalScope


-- | Generate a new identifier based on the given identifier
genIde :: LexicalM m a => Ide a -> m (IdeLex a)
genIde ide = modify (\s -> s { fresh = fresh s + 1 }) >> gets (IdeLex ide . fresh)

-- | Lexical addresser monad
type LexicalM m a = (MonadReader (Env a) m, MonadState LexicalState m)

-- | Run the lexical addresser on the given program
runLexical :: Program a AfterSimplification     -- ^ the program to apply lexical addressing to
           -> Program a AfterLexicalAddressing
runLexical (Program stmt) = Program $ evalState (runReaderT (lexicalStmt stmt) []) initialLexState
      where initialLexState = LexicalState Map.empty 0

-- | Run the lexical addresser on the given program, but keep track of an effectful context
lexical :: (LexicalM m a) => Program a AfterSimplification -> m (Program a AfterLexicalAddressing)
lexical = fmap Program . lexicalStmt . programStmt

-- | Run the lexical addresser on a single statement
lexicalStmt :: (LexicalM m a) => Stmt a AfterSimplification -> m (Stmt a AfterLexicalAddressing)
lexicalStmt (NonLocal _ x _) = addNonLocal (ideName x) >> return (makeSeq [])
lexicalStmt (Global _ x _)   = addGlobal (ideName x) >> return (makeSeq [])
lexicalStmt (Seq _ as)       = makeSeq <$> mapM lexicalStmt as
lexicalStmt (Return _ e a)   = Return () <$> mapM lexicalExp e <*> pure a
lexicalStmt (Assg _ lhs e)   = Assg () <$> lexicalLhs lhs <*> lexicalExp e
lexicalStmt (Loop _ grd s a) = Loop () <$> lexicalExp grd <*> lexicalStmt s <*> pure a
lexicalStmt (Break _ a)      = return $ Break () a
lexicalStmt (Continue _ a)   = return $ Continue () a
lexicalStmt (Raise _ exp a)  = Raise () <$> lexicalExp exp <*> pure a
lexicalStmt (Try _ bdy hds a) = Try () <$> lexicalStmt bdy
                                       <*> mapM (\(exc, hdl) -> (,) <$> lexicalExp exc <*> lexicalStmt hdl) hds
                                       <*> pure a
lexicalStmt (Conditional _ cls els a)  =
   Conditional () <$> mapM (bimapM lexicalExp lexicalStmt) cls <*> lexicalStmt els <*> pure a
lexicalStmt (StmtExp _ e a)  = StmtExp () <$> lexicalExp e <*> pure a

-- | Lookup a string and return the corresponding lexical identifier
lookupLexIde :: (LexicalM m a) => Ide a -> m (Either (IdeLex a, Ide a) (IdeLex a))
lookupLexIde ide = condM [
      (isNonLocal name, asks (Right . lookupEnv name . nonlocalEnv)),
      (isGlobal name, return $ Right (IdeGbl name)),
      (pure True, case ide of
                    Ide i -> asks (Right . lookupEnv name)
                    NamespacedIde i ns -> asks (Left . (, i) . lookupEnv (ideName ns)))]
   where name = ideName ide

-- | Run the lexical addresser on the given expression
lexicalExp :: forall m a . (LexicalM m a) => Exp a AfterSimplification -> m (Exp a AfterLexicalAddressing)
lexicalExp (Var ide) = either (\(e, x) -> Read (Var e) x (annot (getIdeIdent $ lexIde e))) Var <$> lookupLexIde ide
lexicalExp (Lam prs stmt a ()) = do
      genPars <- mapM (overPar genIde) prs
      let parIdes = map parIde genPars
      let parNames = map (ideName . parIde) prs
      (bdy, lcs) <- enterScope (zip parNames parIdes) $ lexicalStmt stmt
      return $ Lam genPars bdy a lcs
   where overPar :: (Ide a -> m (IdeLex a)) -> Par a AfterSimplification -> m (Par a AfterLexicalAddressing)
         overPar f (Prm ide a)        = Prm <$> f ide <*> pure a
         overPar f (VarArg ide a)     = VarArg <$> f ide <*> pure a
         overPar f (VarKeyword ide a) = VarKeyword <$> f ide <*> pure a

lexicalExp (Read e x a)         = Read <$> lexicalExp e <*> pure x <*> pure a
lexicalExp (Call e ags kwa a)   = Call <$> lexicalExp e
                                       <*> mapM lexicalExp ags
                                       <*> mapM (\(ide, exp) -> (ide,) <$> lexicalExp exp) kwa
                                       <*> pure a
lexicalExp (Literal lit)        = Literal <$> lexicalLit lit

lexicalLit :: (LexicalM m a) => Lit a AfterSimplification -> m (Lit a AfterLexicalAddressing)
lexicalLit (Bool b a)    = return $ Bool b a
lexicalLit (Integer i a) = return $ Integer i a
lexicalLit (Real r a)    = return $ Real r a
lexicalLit (String i a)  = return $ String i a
lexicalLit (Tuple es a)  = Tuple <$> mapM lexicalExp es <*> pure a
lexicalLit (List es a)   = List  <$> mapM lexicalExp es <*> pure a 
lexicalLit (Dict bds a)  = Dict  <$> mapM (\(k,v) -> (,) <$> lexicalExp k <*> lexicalExp v) bds <*> pure a

lexicalLhs :: (LexicalM m a) => Lhs a AfterSimplification -> m (Lhs a AfterLexicalAddressing)
lexicalLhs (Field e x a) = Field <$> lexicalExp e <*> pure x <*> pure a
lexicalLhs (ListPat ps a ) = ListPat <$> mapM lexicalLhs ps <*> pure a
lexicalLhs (TuplePat ps a) = TuplePat <$> mapM lexicalLhs ps <*> pure a
lexicalLhs (IdePat x)      = do
      -- first check whether the variable is already a local or a global => register as local if not 
      unlessM (liftA2 (||) (isGlobal nam) (isNonLocal nam)) $ addLocal nam
      -- lookup the variable and paste it into the pattern
      either (\(e, x) -> Field (Var e) x (annot $ getIdeIdent x)) IdePat <$> lookupLexIde x
   where nam = ideName x

