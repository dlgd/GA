import IA.GA
import Test.Tasty
import Test.Tasty.HUnit
import qualified Data.Sequence as S
import qualified Data.Bits as B
import System.Random

main :: IO ()
main = defaultMain properties

properties :: TestTree
properties = testGroup "Tests" [
             testCase "Hamming Weight list maximize" $
             assertBool "" $ hammingWeightGeneList FitnessMaximize ==
                        S.fromList [True,True,True]
             , testCase "Hamming Weight list minimize" $
               assertBool "" $ hammingWeightGeneList FitnessMinimize ==
                          S.fromList [False,False,False]
             , testCase "Hamming Weight bits maximize" $
             assertBool "" $ hammingWeightGeneBits FitnessMaximize ==
                        GeneBits 255 8
             , testCase "Hamming Weight bits minimize" $
               assertBool "" $ hammingWeightGeneBits FitnessMinimize ==
                          GeneBits 0 8
             ]

hammingWeightGeneList :: FitnessType -> GenoType (S.Seq Bool)
hammingWeightGeneList fitnessType = runGA (mkStdGen 42) 100 initialGenes
          (mkFitness fitnessType fitness)
          (mkSelect 1 binaryTournament)
          (mkCrossOver crossOverSeq)
          (mkMutate (1 / 2) mutateSeq)
    where initialGenes = S.fromList $ fmap S.fromList [[False, False, True],
                                                       [False, True, False]]
          fitness = length . S.filter (== True)

hammingWeightGeneBits :: FitnessType -> GenoType (GeneBits Int)
hammingWeightGeneBits fitnessType = runGA (mkStdGen 42) 100 initialGenes
          (mkFitness fitnessType fitness)
          (mkSelect 1 binaryTournament)
          (mkCrossOver crossOverBits)
          (mkMutate (1 / 2) mutateBits)
    where initialGenes = S.fromList [GeneBits 1 len, GeneBits 2 len]
          fitness (GeneBits b _) = B.popCount b
          len = 8
