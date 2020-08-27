import qualified Dormouse.StatusSpec as StatusSpec
import qualified Dormouse.UrlSpec as UrlSpec
import qualified Dormouse.Uri.ParserSpec as UriParserSpec
import qualified Dormouse.Uri.QuerySpec as UriQuerySpec
import qualified Dormouse.Uri.QQSpec as UriQQSpec
import qualified DormouseSpec as DormouseSpec

main :: IO ()
main = do
  DormouseSpec.tests
  StatusSpec.tests
  UrlSpec.tests
  UriParserSpec.tests
  UriQuerySpec.tests
  UriQQSpec.tests
