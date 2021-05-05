{-# Language OverloadedStrings, BlockArguments #-}
module Language.ASKEE.Expo.CodeGen where

import Data.Text(Text)
import Data.Map(Map)
import qualified Data.Map as Map
import Data.List(intersperse)
import Prettyprinter(pretty,(<+>),vcat,hsep,punctuate,comma)
import qualified Prettyprinter as PP


import Language.ASKEE.Panic(panic)
import qualified Language.ASKEE.C as C
import Language.ASKEE.Expo.Syntax
import Language.ASKEE.Expo.TypeOf(typeOf)

{-
data ExperimentDecl =
  ExperimentDecl { experimentName :: Ident
                 , experimentArgs :: [Binder]
                 , experimentStmts :: [ExperimentStmt]
                 , experimentReturn :: Expr
                 }
  deriving Show
-

vectorOfType :: C.Doc -> C.Doc
vectorOfType ty = C.ident "std::vector" <> C.angles [ty]

compileMain :: MainDecl -> C.Doc
compileMain mndecl =
  vcat
    [ C.main (map compileMainStmt (mainStmts mndecl)) ]
  where
    compileMainStmt stmt =
      case stmt of
        MSSample samplesName samplesNum experName _ -> vcat $
          declareSamples experName samplesName :
          map declareOutput (mainOutput mndecl) ++
          [ loop samplesNum $
            [ declareExper experName
            , runExper experName
            , pushExperResult experName samplesName
            ] ++ concatMap (outputToStmt experName samplesName) (mainOutput mndecl)
          ]

    mkType ident = compileVar ident
    mkVar ident = "_" <> mkType ident

    declareExper :: Ident -> C.Doc
    declareExper experName = 
      C.declare (mkType experName) (mkVar experName)

    runExper :: Ident -> C.Doc
    runExper experName = 
      C.stmt $ C.call (C.member (mkVar experName) (C.ident "run")) []

    pushExperResult :: Ident -> Binder -> C.Doc
    pushExperResult experName samplesName =
      C.stmt $ C.call (C.member (compileVar samplesName) (C.ident "push_back")) [mkVar experName]

    outputToStmt :: Ident -> Binder -> Expr -> [C.Doc]
    outputToStmt experName samplesName e =
      case e of
        Dot (Var sample) sampleMember _ | samplesName == sample -> 
          let experMem = C.member (mkVar experName) (C.ident sampleMember)
              pushBack = C.member (C.ident $ "_"<>sampleMember) (C.ident "push_back")
          in  [ C.stmt $ C.call pushBack [experMem] ]
        _ -> []

    declareSamples :: Ident -> Binder -> C.Doc
    declareSamples experName samplesName = 
      C.declare (vectorOfType (compileVar experName)) (compileVar samplesName)

    declareOutput :: Expr -> C.Doc
    declareOutput e =
      case e of
        -- XXX: this is, at the very least, ugly
        Dot (Var (TypedName _ (Just (TypeVector (TypeCon (TCon _ _ tcFields)))))) lbl _ -> 
          C.declare (vectorOfType (compileType $ tcFields Map.! lbl)) (C.ident $ "_"<>lbl)
        _ -> C.nop

    loop :: Int -> [C.Doc] -> C.Doc
    loop bound stmts = 
      C.for (C.declareInit' C.int loopVar (C.intLit 0))
            (loopVar C.< C.intLit bound) 
            (C.incr' loopVar) 
            stmts
      where
        loopVar = C.ident "i"

compileExperiment :: ExperimentDecl -> C.Doc
compileExperiment exdecl =
  vcat
    [ "struct" <+> name <+> "{"
    , C.nested $ concatMap attrsFromStmt inlinedBody
    , C.nested [runFn $ compileSamples inlinedBody]
    , C.nested [
        C.function C.void "print" []
          [ mkJsonOutput "std::cout" (experimentToJson exdecl) ]]
    , "};"
    ]
  where
  inlinedBody = inlineVars Map.empty (experimentStmts exdecl)
  name = measureEName (tnName (experimentName exdecl))
  attrsFromStmt estmt =
    case estmt of
      ESLet x e -> [C.declare (compileType (typeOf e)) (compileVar x)]
      ESSample {} -> []
      ESMeasure b m ->
        [C.declare (compileType (getType (meMeasureName m))) (compileVar b)]
      ESTrace n _ ->
        [C.declare  (compileType (getType n)) (compileVar n) ]

  addMeasurePointName = C.ident "addPoint"

  runFn stmts =
    C.function C.void (C.ident "run") [] stmts
  done e = C.call (C.member e (C.ident "done")) []
  step e = C.callStmt (C.member e (C.ident "step")) []
  getPoint e = C.call (C.member e (C.ident "getPoint")) []
  callSample e r = C.call (C.ident "sample") [e, r]

  compileSamples stmts =
    case stmts of
      [] -> []
      ESSample s e:t ->
        [ C.declare (modelImplTy s) (modelImplName s)
        , C.declareInit (modelTy s) (modelName s) (C.braces [modelImplName s])
        , C.declareInit C.auto (compileVar s) (callSample (compileExpr' (seRange e)) (C.ident ("_model_" <> tnName s)))
        , C.while (C.not (done (compileVar s)))
          ((compileBodyStmt (tnName s) `concatMap` t) ++ [step (compileVar s)])
        ] ++ compileSamples t
      _:t -> compileSamples t


  modelTy s = C.ident "Model" <> C.angles [modelImplTy s]
  modelImplTy s = compileType $ getType s
  -- XXX: the name _model_ + tnName kinda sucks
  modelName s = C.ident ("_model_" <> tnName s)
  modelImplName s = C.ident ("_model_impl_" <> tnName s)

  compileBodyStmt sname estmt =
    case estmt of
      ESLet _ _      -> panic "compileBodyStmt" ["let should have been inlined"]
      ESMeasure b me
        | tnName (meDataset me) == sname ->
          [ C.callStmt (C.member (compileVar b) addMeasurePointName)
                       [getPoint $ C.ident sname] ]
        | otherwise -> []
      ESSample _ _ -> []
      ESTrace tn (Var ds)
        | tnName ds == sname ->
          [ C.callStmt (C.member (compileVar tn) "push_back")
                       [getPoint $ C.ident sname] ]
        | otherwise -> []
      ESTrace _ _ -> panic "compileBodyStmt" ["expecting trace to operate on a Var expr"]




compileMainEx :: TypedName -> C.Doc
compileMainEx exTName =
  C.main [ C.callStmt exName []
         , C.callStmt (C.member exName "run") []
         -- TODO: output
         ]
  where
    exName = compileVar exTName


-- TODO: generic traversal
inlineVars :: Map Text Expr -> [ExperimentStmt] -> [ExperimentStmt]
inlineVars lets stmts =
  case stmts of
    [] -> []
    stmt:stmts' ->
      case stmt of
        ESLet x e ->
          inlineVars (Map.insert (tnName x) (inlineExpr e) lets) stmts'

        ESMeasure b me ->
          ESMeasure b me { meArgs = inlineExpr <$> meArgs me
                         , meDataset = findAlias (meDataset me)
                         } : inlineVars lets stmts'

        ESSample e se ->
          let sample' =
                ESSample e se { seArgs = inlineExpr <$> seArgs se
                              , seRange = inlineExpr (seRange se)
                              }
          in sample':inlineVars lets stmts'

        ESTrace b t -> ESTrace b (inlineExpr t) : inlineVars lets stmts'
  where
    findAlias name =
      case Map.lookup (tnName name) lets of
        Just (Var v) -> findAlias v
        Nothing -> name
        Just _ -> panic "inlineVars" ["not expecting non Var expression as an data set alias"]

    inlineExpr expr =
      case expr of
        Lit _ -> expr
        Var i ->
          case tnName i `Map.lookup` lets of
            Just expr' -> expr'
            Nothing -> expr
        Call fname args -> Call fname (inlineExpr <$> args)
        Dot e l t -> Dot (inlineExpr e) l t
        Point flds -> Point (fmap inlineExpr <$> flds)



{-



  x = sample P [1..120 by 1]
  a = measure x with M1
  b = measure x with M2

  P x';
  Range t(1,1,120)
  x = Sample(t,x')

  M1<P> a;
  M2<P> b;
  while (!x.done()) {
    x.step();
    a.addPoint(x.getPoint())
    b.addPoint(x.getPoint())
  }



-}


--------------------------------------------------------------------------------

compileMeasure :: MeasureDecl -> C.Doc
compileMeasure mdecl =
  vcat
  [ template
  , "struct" <+> name <+> "{"
  , C.nested $ map attr         (measureArgs mdecl) ++
               map (attr . fst) (measureVars mdecl)
  , C.nested [ con, addPoint ]
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
  initVar (x,e) = C.call (compileVar x) [compileExpr 15 e]

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

--------------------------------------------------------------------------------
-- Statements

compileStmt :: Stmt -> C.Doc
compileStmt stmt =
  case stmt of
    Set x e -> C.assign (compileVar x) (compileExpr 15 e)
    Let x e -> C.declareInit (compileType (getType x)) (compileVar x)
                                                       (compileExpr 15 e)
    If sThens sElse -> head $ foldr doThen (doStmts sElse) sThens
      where
      doThen (b,stmts) es = [C.ifThenElse (compileExpr 0 b) (doStmts stmts) es]
      doStmts             = map compileStmt



--------------------------------------------------------------------------------
-- Expressions

compileExpr' :: Expr -> C.Doc
compileExpr'= compileExpr 15

compileExpr :: Int -> Expr -> C.Doc
compileExpr prec expr =
  case expr of
    Lit l     -> compileLiteral l
    Var x     -> compileVar x
    Call f es -> compileCall prec f es
    Dot e l _ -> C.member (compileExpr 1 e) (labelName l)
    -- Point [(Text, Expr)]

compileVar :: TypedName -> C.Doc
compileVar = varName . tnName

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
    (Range,_) -> C.callCon "Range" [ compileExpr 15 e | e <- es ]
    -- XXX: `Range` should be in a separate namespace?

    _ -> panic "compileCall" [ "Unexpected call", show f, show es ]

  where
  wrap        = if thisPrec < prec then id else C.parens
  infOp x a b = compileExpr thisPrec a <+> x <+> compileExpr thisPrec b
  uniOp x a   = x <> compileExpr thisPrec a

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


------------------------------------------------------------------------------
-- Types

compileTVar :: TypeVar -> C.Doc
compileTVar tv =
  case tv of
    TVFree {} -> panic "compileTVar" ["Unexpteced free type variable"]
    TVBound n -> tvarName n

compileType :: Type -> C.Doc
compileType ty =
  case ty of
    TypeNumber   -> C.double
    TypeBool     -> C.bool
    TypeVar tv   -> compileTVar tv
    TypeCon tcon -> tconCName (tconName tcon) <>
                              C.typeArgList (map compileType (tconArgs tcon))
    TypeStream t@(TypeCon _) -> compileType t
    TypeVector tv -> vectorOfType (compileType tv)
    TypeStream _ -> panic "compileType" ["cannot compile non constructed stream type (yet)"]
    TypePoint _ -> panic "compileType" ["cannot compile point type (yet)"]
{-
  | TypeStream Type -- stream/dataset
  | TypePoint (Map Text Type)
  -- | TypeCallable [Type] Type
-}

--------------------------------------------------------------------------------
-- Names

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