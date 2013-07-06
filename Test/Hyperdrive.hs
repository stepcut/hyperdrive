{-# LANGUAGE FlexibleInstances #-}
module Test.Hyperdrive ( tests ) where

import Distribution.TestSuite

test1 :: TestInstance
test1 =
    TestInstance { run       = return (Finished Pass)
                 , name      = "test1"
                 , tags      = []
                 , options   = []
                 , setOption = \_ _ -> Right test1
                 }

tests :: IO [Test]
tests =
    return $
     [ testGroup "tests"
        [ Test test1
        ]
     ]
