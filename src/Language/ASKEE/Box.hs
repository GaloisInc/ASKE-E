{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Language.ASKEE.Box where

import Data.Aeson
import Data.Map ( Map )
import Data.Text ( Text )

import Language.ASKEE.Expr
import Language.ASKEE.Print ( printExpr )
import Language.ASKEE.Syntax
import qualified Data.Map as Map
import qualified Data.ByteString.Lazy.Char8 as B


data Box = Box
  deriving Show

data Wire = Wire
  { wireSources :: Map Text WireEnd
  , wireTargets :: Map Text WireEnd
  , wireRate :: Expr
  }
  deriving Show

-- i.e. the thing at the "end" of a wire
data WireEnd = WireEnd
  { wireEndQuantity :: Double
  }
  deriving Show

instance ToJSON Box where
  toJSON Box =
    object []

instance ToJSON Wire where
  toJSON (Wire ss ts r) =
    object  [ "sources" .= ss
            , "targets" .= ts
            , "rate" .= show (printExpr r)
            ]

instance ToJSON WireEnd where
  toJSON (WireEnd q) =
    object [ "quantity" .= q ]

jsonify :: Model -> String
jsonify m = 
  let (bs, ws) = boxify m
      o = object  [ "state-variables" .= bs, "links" .= ws ]
  in  B.unpack $ encode o
  
boxify :: Model -> (Map Text Box, [Wire])
boxify Model{..} = (boxes, wires)
  where
  boxes :: Map Text Box
  boxes = 
    Map.fromList 
      [ (n, Box)
      | State n _ <- modelDecls
      ]

  wires :: [Wire]
  wires = map wirify modelEvents
  
  wirify :: Event -> Wire
  wirify Event{..} =
    let sources = [ (n, WireEnd q)
                  | (n, Sub (Var n') (LitD q)) <- eventEffect 
                  , n == n' ]
        targets = [ (n, WireEnd q)
                  | (n, Add (Var n') (LitD q)) <- eventEffect 
                  , n == n' ]
    in  Wire (Map.fromList sources) (Map.fromList targets) eventRate
        