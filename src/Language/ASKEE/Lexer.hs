{-# Language MultiWayIf #-}

module Language.ASKEE.Lexer where

import qualified Control.Monad.State as State
import           Control.Monad.State(lift)

import Data.Text ( Text )
import Data.Maybe (fromJust)

data Located a =
  Located { locLine :: Int
          , locCol  :: Int
          , locVal  :: a
          }
          deriving (Show, Eq)

data Token = EOF
  | And
  | Assign
  | CloseBlock
  | CloseP
  | Colon
  | Cond
  | Divide
  | DivideAssign
  | Effect
  | Elif
  | Else
  | Event
  | GT
  | GTE
  | If
  | Indent
  | Dedent
  | LT
  | LTE
  | Let
  | LetDerived
  | Metadata
  | Minus
  | MinusAssign
  | Model
  | Newline
  | Not
  | OpenBlock
  | OpenP
  | Or
  | Otherwise
  | Plus
  | PlusAssign
  | Rate
  | Real Double
  | BlockSeparator
  | State
  | Sym Text
  | Then
  | Times
  | TimesAssign
  | When
  | Whitespace
  | Wrap Token
  deriving (Eq, Show)


data IndentState =
  IndentState { indLine :: Int
              , indStk  :: [Int]
              -- , indMode
              }
              
type Ind a = State.StateT IndentState (Either String) a

err :: String -> Ind a
err = lift . Left

setLine :: Int -> Ind ()
setLine i = State.modify (\s -> s { indLine = i })

pushIndent :: Int -> Ind ()
pushIndent i = State.modify (\s -> s {indStk = i:(indStk s)})

getCurCol :: Ind Int
getCurCol =
  do  cols <- State.gets indStk
      case cols of
        [] -> err "super fucky"
        i:_ -> pure i

popIndent :: Ind ()
popIndent =
  do  indentState <- State.get
      case indStk indentState of
        []   -> error "mega fucky"
        _:tl -> State.modify (\s -> s { indStk = tl })


handleLayout :: Located Token -> Ind [Located Token]
handleLayout t@(Located _ _ Newline) = pure [t]
handleLayout t =
  do  curLine <- State.gets indLine
      curStk <- State.gets indStk
      curCol <- getCurCol

      if | locLine t == curLine -> pure [t]
         | locCol t == curCol   -> setLine (locLine t) >> pure [t]
         | locCol t > curCol    -> 
           do setLine (locLine t)
              pushIndent (locCol t)
              pure [Located (locLine t) 1 Indent, t]
         | locCol t < curCol    ->
            case curStk of
              [] -> error "handleLayout: empty stack: super fucky"
              _:prevCol:stk' | prevCol < locCol t -> error "handleLayout: super fucky"
                             | otherwise ->
                                do  popIndent
                                    t' <- handleLayout t
                                    let d = Located (locLine t) 1 Dedent
                                    pure (d:t')


insertLayoutTokens :: [Located Token] -> Either String [Located Token]
insertLayoutTokens toks = do
  (toks, indState) <- State.runStateT stCmp (IndentState 1 [1])
  let toks' = concat toks
  (lastLine, lastCol) <- if length toks' == 0
    then Left "no tokens provided"
    else Right (locLine (last toks'), locCol (last toks'))
  let stackDepth = (length . indStk) indState
      dedents = replicate (stackDepth - 1) (Located lastLine lastCol Dedent)
  pure $ toks' ++ dedents
    -- concat <$> State.evalStateT stCmp (IndentState 1 [1])
  where
    stCmp = handleLayout `traverse` toks


colocate :: Token -> Located Token -> Located Token
colocate closer tok = tok { locVal = closer }

insertSeparators :: [Int] -> Maybe (Located Token) -> [Located Token] -> [Located Token]
insertSeparators []  _    _  = error "insertSeparators: empty stack (internal error)"
insertSeparators stk last [] = 
  colocate BlockSeparator (fromJust last) :
    [ colocate CloseBlock $ fromJust last | _ <- tail stk  ]
insertSeparators stk last ((Located _ _ Whitespace):ts) = insertSeparators stk last ts
insertSeparators (col:stk) last (t:ts)
  | locCol t <  col = 
    case last of
      Just (Located _ _ CloseBlock) -> colocate CloseBlock t : insertSeparators stk (Just $ colocate CloseBlock t) (t:ts)
      _ -> colocate BlockSeparator t : colocate CloseBlock t : insertSeparators stk (Just $ colocate CloseBlock t) (t:ts)
  | locCol t == col = 
    case last of
      Just (Located _ _ CloseBlock) -> t : insertSeparators (col:stk) (Just t) ts
      _ -> colocate BlockSeparator t : t : insertSeparators (col:stk) (Just t) ts
  | locCol t >  col = 
    case last of 
      Just (Located _ _ Colon) -> 
        colocate OpenBlock t : t : insertSeparators (locCol t:col:stk) (Just t) ts
      _ -> t : insertSeparators (col:stk) (Just t) ts 

doLayout :: [Located Token] -> [Located Token]
doLayout = insertSeparators [0] Nothing