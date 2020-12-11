import Test.HUnit

tests = TestList [
    TestLabel "test1" test1
    ]

test1 = TestCase (assertEqual "Nums" 1 1)