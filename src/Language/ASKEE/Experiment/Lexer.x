{
{-# Language TemplateHaskell, OverloadedStrings, BlockArguments #-}
module Language.ASKEE.Experiment.Lexer
  ( Lexeme(..), Token(..)
  , lexer
  , SourceRange(..)
  , SourcePos(..)
  ) where

import AlexTools
import Data.Text (Text)
import qualified Data.Text as Text

}

$alpha      = [[A-Z][a-z]]
$digit      = [0-9]
$ws         = [\0\9\10\13\32]

@ident      = $alpha [_ $alpha $digit]*
@natural    = $digit+
@integer    = \-? @natural
@rational   = @integer "." @natural
-- XXX: exponents, maybe other bells+whistles

@comment    = "--" .* \n

:-

<comment> {
"{-"              { startComment }
"-}"              { endComment }
.                 ;
\n                ;
}

<0> {
$ws+              ;
@comment          ;
"{-"              { startComment }
"("               { lexeme OpenParen }
")"               { lexeme CloseParen }
","               { lexeme Comma }
":"               { lexeme Colon }

"."               { lexeme OpDot }
"="               { lexeme OpEq }
"=="              { lexeme OpEqEq }
"!"               { lexeme OpBang }
"!="              { lexeme OpBangEq }
"<"               { lexeme OpLt }
"<="              { lexeme OpLeq }
">"               { lexeme OpGt }
">="              { lexeme OpGeq }
"+"               { lexeme OpPlus }
"-"               { lexeme OpDash }
"*"               { lexeme OpStar }
"/"               { lexeme OpSlash }
"&&"              { lexeme OpAmpAmp }
"||"              { lexeme OpBarBar }


"experiment"      { lexeme KWexperiment }
"measure"         { lexeme KWmeasure }
"sample"          { lexeme KWsample }
"yield"           { lexeme KWyield }
"with"            { lexeme KWwith }
"let"             { lexeme KWlet }
"number"          { lexeme KWnumber }
"bool"            { lexeme KWbool }
"false"           { lexeme KWfalse }
"true"            { lexeme KWtrue }

@ident            { lexeme Ident }
@rational         { lexRational }
@integer          { lexInteger }

.                 { do { txt <- matchText
                       ; lexeme (TokError
                                  ("Unexpected character: " ++ Text.unpack txt))
                       } }
}

{

data Token =
    Ident
  | Integer !Integer
  | Rational !Double

  | OpenParen
  | CloseParen
  | Comma
  | Colon

  | OpPlus
  | OpDash
  | OpStar
  | OpSlash

  | OpEq
  | OpEqEq
  | OpBangEq
  | OpBang
  | OpLt
  | OpLeq
  | OpGt
  | OpGeq
  | OpAmpAmp
  | OpBarBar
  | OpDot

  | KWexperiment
  | KWmeasure
  | KWsample
  | KWwith
  | KWyield
  | KWlet
  | KWnumber
  | KWbool
  | KWfalse
  | KWtrue

  | TokError String
  | TokEOF
    deriving Show

lexInteger :: Action s [Lexeme Token]
lexInteger =
  do x <- Text.unpack <$> matchText
     lexeme $! Integer (read x)

lexRational :: Action s [Lexeme Token]
lexRational =
  do x <- Text.unpack <$> matchText
     lexeme $! Rational (read x)    -- XXX: looses precision



alexGetByte :: Input -> Maybe (Word8, Input)
alexGetByte = makeAlexGetByte (toEnum . fromEnum)

data LexState = Normal | InComment !SourceRange LexState

startComment :: Action LexState [Lexeme Token]
startComment =
  do r <- matchRange
     s <- getLexerState
     setLexerState (InComment r s)
     pure []

endComment :: Action LexState [Lexeme Token]
endComment =
  do ~(InComment _ s) <- getLexerState
     setLexerState s
     pure []

lexer :: Text -> Text -> [Lexeme Token]
lexer file txt = $makeLexer cfg (initialInput file txt)
  where
  -- dbg xs = trace (unlines [ show (Text.unpack (lexemeText l)) ++
  --            "\t" ++ show (lexemeToken l) |  l <- xs ]) xs

  eof p = Lexeme { lexemeToken = TokEOF
                 , lexemeText  = ""
                 , lexemeRange = AlexTools.range p
                 }
  cfg = LexerConfig { lexerInitialState = Normal
                    , lexerStateMode = \s -> case s of
                                               Normal -> 0
                                               InComment {} -> 1
                    , lexerEOF = \s p ->
                        [ case s of
                            Normal -> eof p
                            InComment r _ ->
                              Lexeme { lexemeToken =
                                           TokError "Unterminated comment."
                                     , lexemeText = ""
                                     , lexemeRange = r
                                     } ]

                    }

test :: String -> IO ()
test txt = mapM_ pp (lexer (Text.pack "(test)") (Text.pack txt))
  where
  pp l = putStrLn $ prettySourceRange (lexemeRange l) ++ ": " ++
                                      show (lexemeToken l)

}

