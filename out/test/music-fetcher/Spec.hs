import Test.HUnit
import Lib

parseCandidatesTest :: Test
parseCandidatesTest = TestCase $ assertEqual "" ["Zoot Woman", "Grey Day"] (parseCandidatesFromTitle "Zoot Woman - Grey Day")

tests :: Test
tests = TestList [parseCandidatesTest]

main :: IO Counts
main = runTestTT tests
