module Main where

import Test.Hspec

import qualified Data.Set as Set
import qualified Data.Map as Map

import Model
import Prop
import CCS

main :: IO ()
main = hspec $
  describe "Modelcheck.check" $ do
    it "simpleRec satisifies simpleProp" $
      check simpleProp simpleRec `shouldBe` True

    it "protocol does not satisfy protocolProp" $
      check protocolProp protocol `shouldBe` False

-- Rec(P = a.P)
simpleRec :: CCS Char Char
simpleRec = rec' 'P' ['P' .= Pos 'a' :-> VarCCS 'P']

simpleProp :: Prop Char (Label Char) p
simpleProp = nu 'X' (Trans (Pos 'a') (VarProp 'X'))

-- complexRec :: CCS Char Char
-- complexRec = Rec 'P' $ Map.

-- |
-- @
-- Sender = a.Sender’
-- Sender' = b'.(d.Sender + c.Sender’)
-- Medium = b.(c'.Medium + e'.Medium)
-- Receiver = e.f.d'.Receiver
-- Protocol = (Sender || Medium || Receiver) \ {b, c, d, e}
-- @
protocol :: CCS String Char
protocol =
  rec' "Protocol"
  [ "Sender"   .= Pos 'a' :-> var "Sender'"
  , "Sender'"  .= Neg 'b' :-> (Pos 'd' :-> var "Sender" .+.
                               Pos 'c' :-> var "Sender'")
  , "Medium"   .= Pos 'b' :-> (Neg 'c' :-> var "Medium" .+.
                               Neg 'e' :-> var "Medium")
  , "Receiver" .= Pos 'e' :-> Pos 'f' :-> Neg 'd' :-> var "Receiver"
  , "Protocol" .= restrict "bcde" (var "Sender" :|:
                                   var "Medium" :|:
                                   var "Receiver")
  ]

protocolProp :: Prop Char (Label Char) p
protocolProp = inv (whenTrans (Pos 'a') (ev (Trans (Pos 'f') PTrue)))
  where
    inv propA = nu 'X' (propA :&&: whenTransDot (var 'X'))
    ev propA = mu 'X' (propA :||: (TransDot PTrue :&&: whenTransDot (var 'X')))
