import qualified Dormouse.StatusSpec as StatusSpec
import qualified Dormouse.UriSpec as UriSpec
import qualified Dormouse.Uri.ParserSpec as UriParserSpec
import qualified Dormouse.Uri.QuerySpec as UriQuerySpec

main :: IO ()
main = do
  StatusSpec.tests
  UriSpec.tests
  UriParserSpec.tests
  UriQuerySpec.tests
