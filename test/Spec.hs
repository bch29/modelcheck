module Main where

import           Test.Hspec

import qualified Data.Map   as Map
import qualified Data.Set   as Set

import           CCS
import           Model
import           Petri
import           Prop

main :: IO ()
main = hspec $
  describe "Modelcheck.check" $ do
    it "simpleRec satisifies simpleProp" $
      check simpleProp simpleRec `shouldBe` True

    it "protocol does not satisfy protocolProp" $
      check protocolProp protocol `shouldBe` False

    it "testPetri satisfies petriProp" $
      check petriProp testPetri `shouldBe` True

    it "testPetri does not satisfy petriProp'" $
      check petriProp' testPetri `shouldBe` False

-- Rec(P = a.P)
simpleRec :: CCS Char Char
simpleRec = rec' 'P' ['P' .= 'a' .-> var 'P']

simpleProp :: Prop Char (Label Char) p
simpleProp = nu 'X' ('a' >-> var 'X')

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
  [ "Sender"   .=     'a' .-> var "Sender'"
  , "Sender'"  .= Neg 'b' .-> ('d' .-> var "Sender" .+.
                               'c' .-> var "Sender'")
  , "Medium"   .= 'b' .-> (Neg 'c' .-> var "Medium" .+.
                           Neg 'e' .-> var "Medium")
  , "Receiver" .= 'e' .-> 'f' .-> Neg 'd' .-> var "Receiver"
  , "Protocol" .= restrict "bcde" (var "Sender" :|:
                                   var "Medium" :|:
                                   var "Receiver")
  ]

protocolProp :: Prop Char (Label Char) p
protocolProp = inv ('a' |-> ev ('f' >-> PTrue))
  where
    inv propA = nu 'X' (propA :&&: whenTransAny (var 'X'))
    ev propA = mu 'X' (propA :||: (TransAny PTrue :&&: whenTransAny (var 'X')))

testPetri :: Petri Char Int
testPetri = Petri
  { petriEvents =
      Map.fromList [ 'a' .= (Set.fromList [0]   , Set.fromList [2]   )
                   , 'b' .= (Set.fromList [1]   , Set.fromList [3]   )
                   , 'c' .= (Set.fromList [2, 3], Set.fromList [0, 1])
                   , 'd' .= (Set.fromList [2, 3], Set.fromList [4]   )
                   ]
  , petriMarkings =
      Map.fromList [ 0 .= Marked
                   , 1 .= Marked
                   , 2 .= Unmarked
                   , 3 .= Unmarked
                   , 4 .= Unmarked
                   ]
  }

-- | nu X. <a><b><c>X or <b><a><c>X
petriProp :: Prop Char Char (Petri Char Int)
petriProp = nu 'X' ('a' >-> 'b' >-> 'c' >-> var 'X' :||:
                    'b' >-> 'a' >-> 'c' >-> var 'X'
                   )

-- | nu X. <a>X
petriProp' :: Prop Char Char (Petri Char Int)
petriProp' = nu 'X' ('a' >-> var 'X')
