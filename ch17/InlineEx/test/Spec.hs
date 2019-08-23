import Test.QuickCheck
import Test.QuickCheck.Classes
import Test.QuickCheck.Checkers
import ListApplicative

main :: IO ()
main = do
  let trigger :: List (Int, String, Int)
      trigger = undefined
  quickBatch $ applicative trigger
