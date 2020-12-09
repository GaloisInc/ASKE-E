{-# Language MultiWayIf #-}

module Language.ASKEE.Lexer where

import Data.Text (Text)

data Located a =
  Located { locLine :: Int
          , locCol  :: Int
          , locVal  :: a
          }
          deriving (Show, Eq)

data Token = EOF
  | And
  | Assign
  | Boolean Bool
  | CloseBlock
  | CloseP
  | Colon
  | Cond
  | Divide
  | DivideAssign
  | Effect
  | Elif
  | Else
  | EQ
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
  | Assert
  deriving (Eq, Show)


-- | Annotate a token stream with layout information (BOPEN,BSEP,BCLOSE)
addLayout :: [Located Token] -> [Located Token]
addLayout ts =
  case filter keepTok ts of
    []       -> []
    t : more -> consumeToken [0] t more
  where
  keepTok t = case locVal t of
                Whitespace -> False
                _          -> True

consumeToken ::
  [Int]           {- ^ Layout stack (non-empty) -} ->
  Located Token   {- ^ Emity this token to the output -} ->
  [Located Token] {- ^ Rest of the tokens to process -} ->
  [Located Token] {- ^ Annotate stream -}
consumeToken stack thisToken more =
  thisToken :
  case map locVal (thisToken : more) of
    Model  : Sym _ : Colon : _ -> newBlock 2
    Event  : Sym _ : Colon : _ -> newBlock 2
    Effect :         Colon : _ -> newBlock 1
    Cond   :         Colon : _ -> newBlock 1
    _                          -> layout stack thisToken more

  where
  newBlock n =
    let (as,bs) = splitAt n more
        cs = case bs of
               [] -> t { locVal = OpenBlock } : layout (locCol t : stack) t []
                 where t = last as

               t : ts ->
                 t { locVal = OpenBlock } : t : layout (locCol t : stack) t ts
    in as ++ cs


layout ::
  [Int]           {- ^ Layout stack (non-empty) -} ->
  Located Token   {- ^ Previous token token -} ->
  [Located Token] {- ^ Tokens that need processing -} ->
  [Located Token] {- ^ Annotated stream -}
layout stack prevToken todo =
  case todo of
    [] -> [ prevToken { locVal = CloseBlock } | _ <- init stack ]
    t : more
      | thisCol < thisBlock ->
        t { locVal = CloseBlock } : layout otherBlocks prevToken todo
      | thisCol == thisBlock ->
        t { locVal = BlockSeparator } : consumeToken stack t more
      | otherwise -> consumeToken stack t more
      where
      thisBlock : otherBlocks = stack
      thisCol = locCol t



