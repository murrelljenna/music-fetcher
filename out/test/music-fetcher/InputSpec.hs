module InputSpec(spec) where
import Test.HUnit
import Input

parseCandidatesTest :: Test
parseCandidatesTest = TestCase $ assertEqual "" ["Zoot Woman", "Grey Day"] (parseCandidatesFromTitle "Zoot Woman - Grey Day")

spec :: Test
spec = TestList [parseCandidatesTest]

