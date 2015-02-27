import Data.Monoid

import qualified Test.Framework as T

import qualified TestLispJs as TJS
import qualified TestLispPy as TPY

main :: IO ()
main = do jsTests <- TJS.tests
          pyTests <- TPY.tests
          T.defaultMainWithOpts (jsTests ++ pyTests) mempty
