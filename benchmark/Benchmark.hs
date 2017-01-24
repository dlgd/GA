import Criterion.Main
import IA.GA
import qualified Control.Exception as E
import qualified Data.Sequence as S
import Data.Sequence ((><))
import System.Random
import qualified Data.Bits as B

main :: IO ()
main = defaultMain [bench "hammingWeight list" (whnf hammingWeight FitnessMaximize)
                   , bench "hammingWeight bits" (whnf hammingWeight' FitnessMaximize)]

hammingWeight :: FitnessType -> GenoType (S.Seq Bool)
hammingWeight fitnessType = runGA (mkStdGen 42) 2000 initialGenes
          (mkFitness fitnessType fitness)
          (mkSelect 1 binaryTournament)
          (mkCrossOver crossOver)
          (mkMutate (1 / 2) mutate)
    where initialGenes = S.fromList $ fmap S.fromList
            [[False, False, True,False, False, True]
            , [False, True, False, False, True, False]
            , [False, False, True,False, False, True]
            , [False, True, False, False, True, False]]
          fitness = length . S.filter (== True)
          crossOver g g' = do
            let l = E.assert (S.length g == S.length g') (S.length g)
            i <- randomRSt (0, E.assert (l > 0) (l - 1))
            return $ S.take i g >< S.drop i g'
          mutate xs = do
            let l = S.length xs
            i <- randomRSt (0, E.assert (l > 0) (l - 1))
            newVal <- randomSt
            return $ S.update i newVal xs


hammingWeight' :: FitnessType -> GenoType (GeneBits Int)
hammingWeight' fitnessType = runGA (mkStdGen 42) 2000 initialGenes
          (mkFitness fitnessType fitness)
          (mkSelect 1 binaryTournament)
          (mkCrossOver crossOverBits)
          (mkMutate (1 / 2) mutateBits)
    where initialGenes = S.fromList [GeneBits 1 6, GeneBits 0 6, GeneBits 1 6, GeneBits 0 6]
          fitness (GeneBits b _) = B.popCount b
