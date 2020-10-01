{-# Language MultiWayIf #-}

module Language.ASKEE.Lexer where

import qualified Control.Monad.State as State
import           Control.Monad.State(lift)

data Located a =
  Located { locLine :: Int
          , locCol  :: Int
          , locVal  :: a
          }
          deriving (Show, Eq)

data Token = EOF
  | And
  | Assign
  | CloseP
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
  | Minus
  | MinusAssign
  | Model
  | Newline
  | Not
  | OpenP
  | Or
  | Plus
  | PlusAssign
  | Rate
  | Real Double
  | State
  | Sym String
  | Then
  | Times
  | TimesAssign
  | When
  | Wrap Token
  deriving (Eq, Show)


data IndentState =
  IndentState { indLine :: Int
              , indStk  :: [Int]
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
              [] -> error "super fucky"
              _:prevCol:stk' | prevCol < locCol t -> error "super fucky"
                             | otherwise ->
                                do  popIndent
                                    t' <- handleLayout t
                                    let d = Located (locLine t) 1 Dedent
                                    pure (d:t')


insertLayoutTokens :: [Located Token] -> Either String [Located Token]
insertLayoutTokens toks = 
    concat <$> State.evalStateT stCmp (IndentState 1 [1])
  where
    stCmp = handleLayout `traverse` toks




test2 :: IO ()
test2 = 
  do  let Right toks' = insertLayoutTokens toks
      _ <- print `traverse` toks'
      pure ()


toks :: [Located Token]
toks = 
  [Located {locLine = 1, locCol = 1, locVal = Model},
   Located {locLine = 1, locCol = 7, locVal = Sym "Test:"},
   Located {locLine = 1, locCol = 12, locVal = Newline},
   Located {locLine = 2, locCol = 1, locVal = State},
   Located {locLine = 2, locCol = 9, locVal = Sym "I"},
   Located {locLine = 2, locCol = 10, locVal = Assign},
   Located {locLine = 2, locCol = 12, locVal = Real 5.0},
   Located {locLine = 2, locCol = 16, locVal = Newline},
   Located {locLine = 3, locCol = 1, locVal = Newline},
   Located {locLine = 4, locCol = 1, locVal = Event},
   Located {locLine = 4, locCol = 9, locVal = Sym "Incr:"},
   Located {locLine = 4, locCol = 14, locVal = Newline},
   Located {locLine = 5, locCol = 1, locVal = Effect},
   Located {locLine = 5, locCol = 12, locVal = Newline},
   Located {locLine = 6, locCol = 1, locVal = Sym "I"},
   Located {locLine = 6, locCol = 8, locVal = PlusAssign},
   Located {locLine = 6, locCol = 11, locVal = Real 1.0},
   Located {locLine = 6, locCol = 13, locVal = Newline}]
