import qualified Dormouse.StatusSpec as StatusSpecTests
import qualified Dormouse.UriSpec as UriSpecTests

main :: IO ()
main = do
  StatusSpecTests.tests
  UriSpecTests.tests
