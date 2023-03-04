module InputSpec(spec) where
import Test.HUnit
import Input

parseCandidatesTest :: Test
parseCandidatesTest = TestCase $ assertEqual "All expected inputs match expected results" expectedResults (parseCandidatesFromTitle <$> expectedInputs)

spec :: Test
spec = TestList [parseCandidatesTest]

fixture :: [(Title, [Candidate])]
fixture = [
  ("Zoot Woman - Grey Day",[("Zoot Woman", "Grey Day"), ("Grey Day", "Zoot Woman")])
  , ("Autechre - Montreal",[("Autechre", "Montreal"), ("Montreal", "Autechre")])
  ]

expectedInputs :: [Title]
expectedInputs = fst <$> fixture

expectedResults :: [[Candidate]]
expectedResults = snd <$> fixture