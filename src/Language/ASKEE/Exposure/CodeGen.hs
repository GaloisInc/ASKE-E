{-# Language OverloadedStrings, BlockArguments #-}
{-# LANGUAGE RecordWildCards #-}
module Language.ASKEE.Exposure.CodeGen where

import           Data.Map  ( Map )
import qualified Data.Map  as Map
import           Data.Text ( Text )

import           Language.ASKEE.Panic           ( panic )
import qualified Language.ASKEE.C               as C
import           Language.ASKEE.Exposure.Syntax
import           Language.ASKEE.Exposure.TypeOf ( typeOf )

import Prettyprinter ( pretty, (<+>), vcat, hsep, punctuate, comma )

compileDecls :: [Decl] -> C.Doc
compileDecls ds =
  let models = [d | DModel d <- ds]
      modelNames = [n | ModelDecl (TypedName n _) _ <- models]
      measures = [d | DMeasure d <- ds]
      [main] = [d | DMain d <- ds]
  in  vcat $
        map compileModelInterface models ++
        map compileMeasure measures ++
        [compileMain modelNames main]


compileModelInterface :: ModelDecl -> C.Doc
compileModelInterface (ModelDecl name fields) = 
  vcat $ modelMaker : modelAccessors
  where
    modelMaker = 
      C.function
        (C.static <+> randomOfType (compileVar name)) 
        ("make_" <> compileVar name)
        []
        [ C.declare (C.static <+> compileVar name) "model_impl"
        , C.declareInit (C.static <+> modelOfType (compileVar name)) "model" (C.braces ["model_impl"])
        , C.declareInit (C.static <+> randomOfType (compileVar name)) "rand_model" (C.braces ["model"])
        , C.returnWith "rand_model"
        ]
    modelAccessors = 
      [ C.function (compileType t) ("get_" <> C.ident f) [compileVar name <+> C.ident "m"] [C.returnWith (C.member "m" (C.ident f))]
      | (f, t) <- Map.toList fields
      ]


-- `models` passed as a parameter to generate correct types for let
-- bindings, which we don't do at the moment
compileMain :: [Text] -> MainDecl -> C.Doc
compileMain _models (MainDecl stmts _output) =
  C.main $ map compileStmt stmts


inlineStatements :: [] Stmt -> [] Stmt
inlineStatements statements = map inlineStatement statements
  where
    inlineStatement :: Stmt -> Stmt
    inlineStatement s =
      case s of
        Let v e -> Let v (chase e)
        If ess ss -> If [(chase e', ss') | (e', ss') <- ess] ss
        Set i e -> Set i (chase e)

    chase :: Expr -> Expr
    chase e =
      case e of
        Lit _ -> e
        Var v ->
          case lets Map.!? v of
            Just e' -> chase e'
            Nothing -> Var v
        Call fn es -> Call fn (map chase es)
        Dot e' l t -> Dot (chase e') l t
        Measure m t as ats -> Measure (chase m) t (map chase as) ats
        At e1 e2 -> At (chase e1) (chase e2)
        Sample i e' -> Sample i (chase e')
        -- Trace e' -> Trace (chase e')
        Point pts -> Point [(t, chase e') | (t, e') <- pts]
        Probability e' -> Probability (chase e')

    lets :: Map Binder Expr
    lets = Map.fromList [ (v, e) | Let v e <- statements ]

compileStmt :: Stmt -> C.Doc
compileStmt s =
  case s of
    Let b e -> C.declareInit {- (compileType $ getType b) -} C.auto (compileVar b) (compileExpr e)
    If sThens sElse -> head $ foldr doThen (doStmts sElse) sThens
      where
      doThen (b,stmts) es = [C.ifThenElse (compileExpr' 0 b) (doStmts stmts) es]
      doStmts             = map compileStmt
    Set b e -> C.assign (compileVar b) (compileExpr e)

loop :: Int -> [C.Doc] -> C.Doc
loop bound = 
  C.for (C.declareInit' C.int loopVar (C.intLit 0))
        (loopVar C.< C.intLit bound) 
        (C.incr' loopVar) 
  where
    loopVar = C.ident "i"

compileType :: Type -> C.Doc
compileType ty =
  case ty of
    TypeNumber   -> C.double
    TypeBool     -> C.bool
    TypeVar tv   -> compileTVar tv
    TypeCon tcon -> 
      pretty (tconName tcon) <>
        C.typeArgList (map compileType (tconArgs tcon))
    TypeVector tv -> vectorOfType (compileType tv)
    TypeRandomVar (TypeVector ty') -> randomOfType (compileType ty')
    TypeRandomVar (TypeStream ty') -> randomOfType (compileType ty')
    TypeRandomVar ty' -> randomOfType (compileType ty')
    TypeStream t@(TypeCon _) -> compileType t -- XXX: might want to move this case to another function
    -- TypeStream _ -> panic "compileType" ["cannot compile non constructed stream type (yet)"]
    -- TypePoint _ -> panic "compileType" ["cannot compile point type (yet)"]
    _ -> panic "compileType" ["cannot compile type "<>show ty<>" (yet)"]

  where
  compileTVar :: TypeVar -> C.Doc
  compileTVar tv =
    case tv of
      TVFree {} -> panic "compileTVar" ["Unexpteced free type variable"]
      TVBound n -> tvarName n

vectorOfType :: C.Doc -> C.Doc
vectorOfType ty = C.ident "std::vector" <> C.angles [ty]

randomOfType :: C.Doc -> C.Doc
randomOfType ty = C.ident "Random" <> C.angles [ty]

modelOfType :: C.Doc -> C.Doc
modelOfType ty = C.ident "Model" <> C.angles [ty]

compileExpr :: Expr -> C.Doc
compileExpr = compileExpr' 15

compileExpr' :: Int -> Expr -> C.Doc
compileExpr' prec expr =
  case expr of
    Lit l     -> compileLiteral l
    Var (TypedName n (Just (TypeRandomVar (TypeStream (TypeCon _))))) -> 
      C.call ("make_" <> C.ident n) []
    Var v -> compileVar v
    Call f es -> compileCall prec f es
    Dot e l _ -> 
      case typeOf e of
        TypeRandomVar t -> 
          C.member (compileExpr' prec e) 
            (C.call ("select"<>C.angles [compileType $ typeOfField t l]) ["get_"<>C.ident l])
        _ -> C.member (compileExpr' prec e) (pretty l)
    At e1 e2 -> C.member (go e1) (C.call "at" [go e2])
    Sample n e -> 
      case typeOf e of
        TypeRandomVar (TypeVector _) -> C.member (go e) (C.call "take_non_measure_samples" [C.intLit n])
        TypeRandomVar _ -> C.member (go e) (C.call "take_measure_samples" [C.intLit n])
        _ -> panic "compileExpr'" ["tried to generate code for a measurement of a non-random thing"]
    Measure e (TypedName measureName _) _ _ -> 
      let measuredThing =
            case typeOf e of
              TypeRandomVar (TypeVector (TypeCon (TCon n _ _))) -> n
              _ -> panic "compileExpr'" [show e]
      in  C.member (go e) (C.call ("measure"<>C.typeArgList [C.ident measureName <> C.typeArgList [C.ident measuredThing]]) [])
    Probability e ->
      C.member (go e) (C.call "probability" [])
    _ -> error $ show expr
  where
    go = compileExpr' prec

typeOfField :: Type -> Label -> Type
typeOfField t l =
  case t of
    TypeNumber -> undefined
    TypeBool -> undefined
    TypePoint tys -> tys Map.! l
    TypeVar _ -> undefined
    TypeCon (TCon _ _ fs) -> fs Map.! l -- abstraction leak, oops
    TypeVector _ -> undefined
    TypeStream t' -> typeOfField t' l
    TypeRandomVar t' -> typeOfField t' l

compileVar :: TypedName -> C.Doc
compileVar = C.ident . getName

compileLiteral :: Literal -> C.Doc
compileLiteral lit =
  case lit of
    LitBool b -> C.boolLit b
    LitNum d  -> C.doubleLit d

compileCall :: Int -> FunctionName -> [Expr] -> C.Doc
compileCall prec f es =
  wrap
  case (f,es) of
    (Add, [e1,e2]) -> infOp "+" e1 e2
    (Multiply, [e1,e2]) -> infOp "*" e1 e2
    (Divide, [e1,e2]) -> infOp "/" e1 e2
    (Subtract, [e1,e2]) -> infOp "-" e1 e2
    (Negate, [e1]) -> uniOp "-" e1
    (Not, [e1]) -> uniOp "!" e1
    (LessThan, [e1,e2]) -> infOp "<" e1 e2
    (GreaterThan, [e1,e2]) -> infOp ">" e1 e2
    (LessThanEqual, [e1,e2]) -> infOp "<=" e1 e2
    (GreaterThanEqual, [e1,e2]) -> infOp ">=" e1 e2
    (Equal, [e1,e2]) -> infOp "==" e1 e2
    (NotEqual, [e1,e2]) -> infOp "!=" e1 e2
    (Or, [e1,e2]) -> infOp "||" e1 e2
    (And, [e1,e2]) -> infOp "&&" e1 e2
    (Range,_) -> C.callCon "Range" [ compileExpr' 15 e | e <- es ]
    -- XXX: `Range` should be in a separate namespace?

    _ -> panic "compileCall" [ "Unexpected call", show f, show es ]

  where
  wrap        = if thisPrec < prec then id else C.parens
  infOp x a b = compileExpr' thisPrec a <+> x <+> compileExpr' thisPrec b
  uniOp x a   = x <> compileExpr' thisPrec a

  -- lower number is higher precedence!
  thisPrec =
    case f of
      Add               -> 4
      Multiply          -> 3
      Divide            -> 3
      Subtract          -> 4
      Negate            -> 2
      Not               -> 2
      LessThan          -> 6
      GreaterThan       -> 6
      LessThanEqual     -> 6
      GreaterThanEqual  -> 6
      Equal             -> 7
      NotEqual          -> 7
      Or                -> 12
      And               -> 11
      Range             -> 0
      VMean             -> 0






compileMeasure :: MeasureDecl -> C.Doc
compileMeasure mdecl =
  vcat
  [ template
  , "struct" <+> name <+> "{"
  , C.nested $ map attr         (measureArgs mdecl) ++
               map (attr . fst) (measureVars mdecl)
  , C.nested [ con, addPoint, done ]
  ,"};"
  ]
  where
  name = measureCName (tnName (measureName mdecl))

  template =
    case measureTArgs mdecl of
      [] -> mempty
      as -> "template" <+> C.angles [ "typename" <+> tvarName a | a <- as ]

  attr b = C.declare (compileType (getType b)) (compileVar b)

  declareArg x  = C.arg (compileType (getType x)) (compileVar x)
  initArg x     = C.call (compileVar x) [compileVar x]
  initVar (x,e) = C.call (compileVar x) [compileExpr' 15 e]

  con =
    vcat [ name <> C.argList (map declareArg (measureArgs mdecl))
         , ":" <+> hsep (punctuate comma (map initArg (measureArgs mdecl) ++
                                          map initVar (measureVars mdecl)))
         , "{}"
         ]

  addPoint =
    let vDB = measureDataBinder mdecl
    in C.function C.void "addPoint"
          [ C.refArg (compileType (getType vDB)) (compileVar vDB) ]
          (map compileStmt (measureImpl mdecl))

  done = 
    let pointBinder = 
          case measureUntil mdecl of
            Just (b, _) | b == measureDataBinder mdecl -> b
            Just _ -> panic "" []
            Nothing -> measureDataBinder mdecl
        endCond =
          case measureUntil mdecl of
            Just (_, e) -> compileExpr e
            Nothing -> C.boolLit False
    in  C.function 
          C.bool
          "done" 
          [C.refArg (compileType (getType pointBinder)) (compileVar pointBinder)] 
          [C.returnWith endCond]

tvarName :: Int -> C.Doc
tvarName n = "T" <> C.intLit n

-- XXX: escape
tconCName :: Text -> C.Doc
tconCName = pretty

-- XXX: escape
varName :: Text -> C.Doc
varName = pretty

-- XXX: escape
labelName :: Text -> C.Doc
labelName = pretty

-- XXX: escape
measureCName :: Text -> C.Doc
measureCName = pretty

-- XXX: escape
measureEName :: Text -> C.Doc
measureEName = pretty
{-
-------------------------------------------------------------------------------
-- JSON

data JsonOutput =
    JsonObj    [(Text, JsonOutput)]
  | JsonVar    C.Doc
  | JsonVector C.Doc

mkJsonOutput :: C.Doc -> JsonOutput -> C.Doc
mkJsonOutput output o =
  case o of
    JsonVar v    -> write v
    JsonObj mems ->
      vcat $  write (C.stringLit "{")
           :  delimit (writeMem <$> mems)
           <> [write $ C.stringLit "}"]
    JsonVector v ->
      vcat
        [ write (C.stringLit "[")
        , C.foreachAuto "__i" v
          [ write (C.deref "__i")
          , C.ifThen ("__i" C.!= C.call "std::end" [v])
                    [ write litComma ]
          ]
        , write (C.stringLit "]")
        ]

  where
    litComma = C.stringLit ","
    write e = C.stmt (output C.<< e)
    quotedStrLit s = C.stringLit ("\\\"" <> s <> "\\\"")
    delimit = intersperse (write litComma)
    writeMem (name, val) =
      vcat [ write (quotedStrLit name)
           , write (C.stringLit ":")
           , mkJsonOutput output val
           ]

experimentToJson :: ExperimentDecl -> JsonOutput
experimentToJson decl =
  JsonObj (experimentStmts decl >>= stmtToJson)
  where
    measureFields n =
      case getType n of
        TypeCon (TCon _ _ fs) -> fs
        _ -> panic "experimentToJson"
                   ["Expecting measure to be of constructed type"]

    outputVar c n =
      (c <> "." <> n, JsonVar $ C.member (C.ident c) (C.ident n))

    stmtToJson stmt =
      case stmt of
        ESMeasure n _ ->
          [( tnName n
           , JsonObj (outputVar (tnName n) <$> Map.keys (measureFields n))
          )]
        ESTrace n _ ->
          [(tnName n, JsonVector (C.ident (tnName n)))]
        _ -> []
-}