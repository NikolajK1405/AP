import qualified SPC_Tests
import Test.Tasty (defaultMain, localOption, mkTimeout)

main :: IO ()
main = defaultMain $ localOption (mkTimeout 1000000) SPC_Tests.tests
