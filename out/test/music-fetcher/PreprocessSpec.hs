module PreprocessSpec(spec) where
import Test.HUnit
import Preprocess(Candidate(..), parseCandidates)
import FileInput

parseCandidatesTest :: Test
parseCandidatesTest = TestCase $ assertEqual
  "All expected inputs match expected results"
  expectedResults
  (parseCandidates <$> expectedInputs)

parseCandidatesIgnoresQuotes :: Test
parseCandidatesIgnoresQuotes = TestCase $ assertEqual
  "parseCandidates ignores extra quotes"
  expectedExtraQuotesResults
  (parseCandidates <$> expectedExtraQuotesInputs)

spec :: Test
spec = TestList [parseCandidatesTest, parseCandidatesIgnoresQuotes]

fixture :: [(Title, [Candidate])]
fixture = [
  ("Zoot Woman - Grey Day",[(Candidate "Zoot Woman" "Grey Day" "Zoot Woman - Grey Day")])
  , ("Autechre - Montreal",[Candidate "Autechre" "Montreal" "Autechre - Montreal"])
  , ("Bailey - Intelligent Drum & Bass",[Candidate "Bailey" "Intelligent Drum & Bass" "Bailey - Intelligent Drum & Bass"])
  , ("Back on the Chain Gang", [TitleOnly "Back on the Chain Gang" "Back on the Chain Gang"])
    , ("Eyes Without A Face", [TitleOnly "Eyes Without A Face" "Eyes Without A Face"])
  ]

expectedInputs :: [Title]
expectedInputs = fst <$> fixture

expectedResults :: [[Candidate]]
expectedResults = snd <$> fixture

extraQuotesFixture :: [(Title, [Candidate])]
extraQuotesFixture = [
  ("Zoot Woman - \'Grey Day\'",[(Candidate "Zoot Woman" "Grey Day" "Zoot Woman - 'Grey Day'")])
  , ("Autechre - \'Montreal\'",[Candidate "Autechre" "Montreal" "Autechre - 'Montreal'"])
  ]

expectedExtraQuotesInputs :: [Title]
expectedExtraQuotesInputs = fst <$> extraQuotesFixture

expectedExtraQuotesResults :: [[Candidate]]
expectedExtraQuotesResults = snd <$> extraQuotesFixture

