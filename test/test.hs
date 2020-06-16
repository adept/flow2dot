import Text.FlowDiagram
import Test.QuickCheck
import Control.Monad (liftM, liftM2, liftM3)

-- Parser tests
newtype Name = Name String
newtype Message = Message String


instance Arbitrary Name where
  arbitrary = liftM Name (listOf' $ elements "abcxyz_банк")

instance Arbitrary Message where
  -- words.unwords trick is needed to prevent Messages which contain only spaces
  arbitrary = liftM ((Message).unwords.words) $ frequency [ (50, listOf' $ elements "abcxyz_->; 123банк")
                                                          -- One special case which i decided to hard-code
                                                          , (1, return "foo -> bar")
                                                          ]

instance Arbitrary Flow where
  arbitrary = frequency [ (10, liftM3 Msg mkName mkName mkMsg)
                        , (5, liftM2 Action mkName mkMsg)
                        ]
    where
      mkName = do Name n <- arbitrary; return n
      mkMsg = do Message m <- arbitrary; return m

-- Taken from a unreleased version of quickcheck
-- Just added ' to the names
--   / Kolmodin
listOf' :: Gen a -> Gen [a]
listOf' gen = sized $ \n ->
  do k <- choose (1,n)
     vectorOf' k gen

vectorOf' :: Int -> Gen a -> Gen [a]
vectorOf' k gen = sequence [ gen | _ <- [1..k] ]


prop_reparse :: [Flow] -> Bool
prop_reparse x =
  let txt = unlines $ map showFlow x
      in x == parseFlow "" txt

prop_russian_k :: Bool
prop_russian_k =
  ( parseFlow "a->b" "A->B: клиент" == [Msg "A" "B" "клиент"] ) &&
  ( parseFlow "prod" "продавец -> клиент: подписание контракта, предоставление счета" == [Msg "продавец" "клиент" "подписание контракта, предоставление счета"] )

main = do
  quickCheck prop_reparse
  quickCheck prop_russian_k
