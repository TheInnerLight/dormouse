import qualified Dormouse.StatusSpec as StatusSpecTests
import qualified Dormouse.UriSpec as UriSpecTests
import qualified Dormouse.Uri.ParserSpec as UriParserSpecTests

main :: IO ()
main = do
  StatusSpecTests.tests
  UriSpecTests.tests
  UriParserSpecTests.tests
