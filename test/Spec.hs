import qualified Dormouse.StatusSpec as StatusSpec
import qualified Dormouse.UriSpec as UriSpec
import qualified Dormouse.Uri.ParserSpec as UriParserSpec
import qualified Dormouse.Uri.QuerySpec as UriQuerySpec
import qualified Dormouse.Uri.QQSpec as UriQQSpec
import qualified DormouseSpec as DormouseSpec

main :: IO ()
main = do
  DormouseSpec.tests
  StatusSpec.tests
  UriSpec.tests
  UriParserSpec.tests
  UriQuerySpec.tests
  UriQQSpec.tests
