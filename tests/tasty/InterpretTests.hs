module InterpretTests (tests, divZero) where

import Arbitrary ()
import Language.Fixpoint.Types.Refinements (Expr (..), Constant (I, R), Bop (Mod, Div))
import qualified SimplifyInterpreter
import Test.Tasty
  ( TestTree,
    localOption,
    testGroup,
  )
import Test.Tasty.QuickCheck
  ( Property,
    QuickCheckMaxSize (..),
    QuickCheckTests (..),
    testProperty,
    (===), (==>),
  )

tests :: TestTree
tests =
  withOptions $
    testGroup
      "interpret"
      [ testProperty "computes a fixpoint" (prop_fixpoint SimplifyInterpreter.interpret')
      ]
  where
    withOptions tests = localOption (QuickCheckMaxSize 4) (localOption (QuickCheckTests 500) tests)

divZero :: Expr -> Bool
divZero (EBin Mod (ECon (I _)) (ECon (I 0))) = True
divZero (EBin Mod (ECon (R _)) (ECon (R 0.0))) = True
divZero (EBin Div (ECon (I _)) (ECon (I 0))) = True
divZero (EBin Div (ECon (R _)) (ECon (R 0.0))) = True
divZero _ = False

prop_fixpoint :: (Expr -> Expr) -> Expr -> Property
prop_fixpoint f e = not (divZero e) ==> f e === f (f e)
