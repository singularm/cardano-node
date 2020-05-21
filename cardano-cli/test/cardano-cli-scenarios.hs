
import           Cardano.Prelude

import           Hedgehog.Main (defaultMain)
import           System.IO (BufferMode (..))
import qualified System.IO as IO

import qualified Test.Scenarios.ShelleyAddressBuild

main :: IO ()
main = do
  IO.hSetBuffering IO.stdout LineBuffering
  IO.hSetBuffering IO.stderr LineBuffering

  defaultMain [Test.Scenarios.ShelleyAddressBuild.tests]
