module Test.Ccap.Codegen.FastDecoding
  ( specs
  ) where

import Prelude
import Data.Either (either)
import Data.Maybe (Maybe(..))
import Test.Ccap.Codegen.FastDecoding.Domains as Domains
import Test.Ccap.Codegen.FastDecoding.FastTest as FastTest
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (fail, shouldEqual)

specs :: Spec Unit
specs =
  describe "The fast decoding code" do
    it "Can round trip a sample" do
      let
        sample :: FastTest.Basic
        sample =
          { stringTest: "a"
          , intTest: 1
          , booleanTest: true
          , decimalTest: zero
          , stringOpt: Just "b"
          , intOpt: Nothing
          , booleanOpt: Just true
          , decimalOpt: Just zero
          , stringT: Domains.StringT "c"
          , intT: Domains.IntT 3
          , booleanT: Domains.BooleanT true
          , decimalT: Domains.DecimalT zero
          , stringOptT: Just (Domains.StringT "d")
          , intOptT: Just (Domains.IntT 5)
          , booleanOptT: Just (Domains.BooleanT true)
          , decimalOptT: Just (Domains.DecimalT zero)
          , ref: { intTest: 6, somethingA: [ false, true, false ] }
          , refOpt: Just { intTest: 7, somethingA: zero }
          , anotherRef: { intTest: 8, somethingA: 8 }
          , yetAnotherRef: Just { intTest: 9, somethingA: "z" }
          , arrayOfB: [ 10, 11, 12 ]
          , aC: [ true, true, true ]
          , xx: FastTest.Cons "a" (FastTest.Cons "b" (FastTest.Cons "c" FastTest.Nil))
          , yy: FastTest.MyTuple "one" 1
          }

        json = FastTest.jsonCodec_Basic.encode sample

        back = FastTest.jsonCodec_Basic.decode json
      either fail (_ `shouldEqual` sample) back
