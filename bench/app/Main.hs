module Main (main) where

import Bench.Pacer.Utils qualified as Utils
import Pacer.Prelude hiding (IO)
import Test.Tasty.Bench
  ( Benchmark,
    bench,
    bgroup,
    defaultMain,
    nf,
  )
import Prelude (IO)

main :: IO ()
main =
  defaultMain
    [ benchMkSomeRuns,
      benchMkSomeRunsError
    ]

benchMkSomeRuns :: Benchmark
benchMkSomeRuns =
  bgroup
    "decode_runs"
    [ bench "100" $ nf Utils.decodeRuns params.runs_100_bs,
      bench "1_000" $ nf Utils.decodeRuns params.runs_1_000_bs,
      bench "10_000" $ nf Utils.decodeRuns params.runs_10_000_bs
    ]
  where
    params =
      MkTestParams
        { runs_100_bs = Utils.genRunsJson 100,
          runs_1_000_bs = Utils.genRunsJson 1_000,
          runs_10_000_bs = Utils.genRunsJson 10_000
        }

benchMkSomeRunsError :: Benchmark
benchMkSomeRunsError =
  bgroup
    "decode_runs_error"
    [ bench "100" $ nf Utils.decodeErrorRuns (Utils.genOverlappedRunsJson 100),
      bench "1_000" $ nf Utils.decodeErrorRuns (Utils.genOverlappedRunsJson 1_000),
      bench "10_000" $ nf Utils.decodeErrorRuns (Utils.genOverlappedRunsJson 10_000)
    ]

data TestParams = MkTestParams
  { runs_100_bs :: ByteString,
    runs_1_000_bs :: ByteString,
    runs_10_000_bs :: ByteString
  }
