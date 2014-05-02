{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
module WithoutQuickCheck where
import Dummy

import Control.Monad
import Data.Functor

import Distribution.TestSuite hiding ( Result(..) )
import qualified Distribution.TestSuite as TS

import Test.QuickCheck hiding ( Result(..) )
import qualified Test.QuickCheck as QC  
import Test.QuickCheck.Modifiers        
import Test.QuickCheck.Property (Result(..), succeeded)

verify :: String -> (() -> IO QC.Result) -> Test
verify testName p = Test $ TestInstance
  { name = testName
  , run = do
    qcResult <- p ()
    case qcResult of
        QC.Success {..} -> return $ Finished TS.Pass
        QC.Failure {numShrinks,reason} -> return $ Finished
            $ TS.Fail $ "After "
                      ++ show numShrinks ++ " shrinks determined failure to be: "
                      ++ show reason
        _ -> return $ Finished $ TS.Fail "TODO(corey): add failure message"
  , tags = []
  , options = []
  , setOption = \_ _ -> Left "no options supported"
  }

tests :: IO [Test]
tests = concat <$> forM [1..20] (\i -> do
    -- check if that terminfo exists
    putStrLn $ "generating case: " ++ show i
    return [ verify "dummy" (dumbTest i) ]
    )

dumbTest :: Int -> () -> IO QC.Result
dumbTest i () = return $ QC.Success 0 [] ""
