module Tests where

import qualified Data.Set as Set
import qualified Data.Map as Map

import Modelcheck

-- Rec_P(P = a.b.Q, Q = b.P)
-- testProc :: CCS Char Char
-- testProc = Rec 'P' (
--   Map.fromList
--     [ ('P', Pos 'a' :-> Pos 'b' :-> VarCCS 'Q')
--     , ('Q', Pos 'b' :-> VarCCS 'P')])

-- Rec(P = a.P)
testProc :: CCS Char Char
testProc = Rec 'P' (Map.singleton 'P' (Pos 'a' :-> VarCCS 'P'))

-- Sender = a.Sender’
-- Sender' = b'.(d.Sender + c.Sender’)
-- Medium = b.(c'.Medium + e'.Medium)
-- Receiver = e.f.d'.Receiver
-- Protocol = (Sender || Medium || Receiver) \ {b, c, d, e}

protocol :: CCS String Char
protocol =
  Rec "Protocol" $ Map.fromList
  [ ("Sender", Pos 'a' :-> var "Sender'")
  , ("Sender'", Neg 'b' :-> ((Pos 'd' :-> var "Sender") .+.
                             (Pos 'c' :-> var "Sender'")))
  , ("Medium", Pos 'b' :-> ((Neg 'c' :-> var "Medium") .+.
                            (Neg 'e' :-> var "Medium")))
  , ("Receiver", Pos 'e' :-> Pos 'f' :-> Neg 'd' :-> var "Receiver")
  , ("Protocol", Restrict (Set.fromList "bcde")
                 (var "Sender" :|: var "Medium" :|: var "Receiver"))
  ]

inv :: Prop Char p l -> Prop Char p l
inv propA = nu 'X' (propA :&&: whenTransDot (var 'X'))

ev :: Prop Char p l -> Prop Char p l
ev propA = mu 'X' (propA :||: (TransDot PTrue :&&: whenTransDot (var 'X')))

testProp :: Prop Char p Char
testProp = inv (whenTrans (Pos 'a') (ev (Trans (Pos 'f') PTrue)))

-- testProp :: Prop Char Char Char
-- testProp = Nu 'X' Set.empty (Trans (Pos 'a') (VarProp 'X'))
