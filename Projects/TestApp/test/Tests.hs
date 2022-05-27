import Test.Hspec (Spec, describe, it, shouldBe)
import Test.Hspec.Runner (configFastFail, defaultConfig, hspecWith)
import Lib

main :: IO ()
main = hspecWith defaultConfig {configFastFail = True} specs

specs :: Spec
specs =
    describe "Tests" $ do

        it "helloWorld" $ helloWorld `shouldBe` "Hello World!"

        it "hello2" $ hello2 `shouldBe` 2

        it "hello3" $ hello3 `shouldBe` 3

        it "getId for Person" $ getId (Person 123 "Bob" "Smith") `shouldBe` 123

        it "getId for UnknownPerson" $ getId (UnknownPerson "John") `shouldBe` 0

        it "makeList" $ makeList 5 [] `shouldBe` [5,4,3,2,1]

        it "funWhere" $ funWhere 5 `shouldBe` 8

        it "maybeAdd" $ maybeAdd (Just 5) `shouldBe` Just 6
        
