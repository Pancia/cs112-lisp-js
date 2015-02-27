import Data.Monoid

import qualified Test.Framework as T

import qualified TestLokiJS as TJS
import qualified TestLokiPY as TPY

main :: IO ()
main = do jsTests <- TJS.tests
          pyTests <- TPY.tests
          T.defaultMainWithOpts (jsTests ++ pyTests) mempty
