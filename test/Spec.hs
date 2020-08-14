import qualified Dormouse.StatusSpec as StatusSpec
import qualified Dormouse.UriSpec as UriSpec
import qualified Dormouse.Uri.ParserSpec as UriParserSpec
import qualified Dormouse.Uri.QuerySpec as UriQuerySpec
import qualified Dormouse.Uri.QQSpec as UriQQSpec

main :: IO ()
main = do
  StatusSpec.tests
  UriSpec.tests
  UriParserSpec.tests
  UriQuerySpec.tests
  UriQQSpec.tests
