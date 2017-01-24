module IA.GA
    (
      GenoType
    , GenoTypes
    , PhenoType
    , Population
    , Select
    , CrossOver
    , Mutate
    , Fitness
    , FitnessType(..)
    , mkFitness
    , mkSelect
    , mkCrossOver
    , mkMutate
    , binaryTournament
    , runGA
    , randomRSt
    , randomSt
    , GeneBits(..)
    , mutateBits
    , crossOverBits
    , mutateSeq
    , crossOverSeq
    ) where

import Control.Monad.State
import Data.Bits ((.&.), (.|.))
import Data.Ord (comparing)
import Data.Sequence ((|>),(><),ViewL(..))
import Data.Traversable as T
import System.Random
import qualified Control.Exception as E
import qualified Data.Bits as B
import qualified Data.Sequence as S

type GenoType a = a

type GenoTypes a = S.Seq (GenoType a)

type PhenoType a b = (GenoType a, b)

type Population a b = S.Seq (PhenoType a b)

data Select a b g = Select Int (Population a b -> State g (GenoTypes a))

newtype CrossOver a g =
    CrossOver (GenoTypes a -> State g (GenoTypes a))

data Mutate a g = Mutate Double (GenoType a -> State g (GenoType a))

data FitnessType = FitnessMaximize | FitnessMinimize

data Fitness a b = Fitness FitnessType (GenoType a -> b)

nextGen :: (RandomGen g, Ord b)
        => Population a b
        -> Fitness a b
        -> Select a b g
        -> CrossOver a g
        -> Mutate a g
        -> State g (Population a b)
nextGen pop
        fitness@(Fitness fitnessType _)
        (Select eliteCount selectFun)
        (CrossOver crossOverFun)
        (Mutate mutateRate mutateFun) =
            (sortPop fitnessType . (elitePop ><)) `fmap` newPop
    where elitePop = S.take eliteCount pop
          popLength = S.length pop
          takeBestPop = S.take $ E.assert (popLength > eliteCount)
                        (popLength - eliteCount)
          newGenes = selectFun pop >>= crossOverFun >>= traverse mutate
          newPop = (takeBestPop . toPop fitness) `fmap` newGenes
          mutate g = do
            i <- randomRSt (0, 1)
            if i <= mutateRate then
                mutateFun g
            else
                return g

sortPop :: Ord b => FitnessType -> Population a b -> Population a b
sortPop fitnessType = S.sortBy cmp
    where cmp = case fitnessType of
                  FitnessMaximize -> flip $ comparing snd
                  FitnessMinimize -> comparing snd

toPop :: Ord b => Fitness a b -> GenoTypes a -> Population a b
toPop (Fitness fitnessType fitnessFun) =
    sortPop fitnessType . fmap (\x -> (x, fitnessFun x))

runGA :: (RandomGen g, Ord b)
      => g
      -> Int
      -> GenoTypes a
      -> Fitness a b
      -> Select a b g
      -> CrossOver a g
      -> Mutate a g
      -> GenoType a
runGA rgen iterNb initialGenes fitness select crossOver mutate =
    let initialPop = toPop fitness initialGenes
        runNextGen p = nextGen p fitness select crossOver mutate
        finalGenoType = (fst . headSeq) `fmap` iterM iterNb runNextGen initialPop
    in
      evalState finalGenoType rgen

iterM :: (Monad m) => Int -> (a -> m a) -> a -> m a
iterM 0 _ a = return a
iterM n f a = f a >>= iterM (n - 1) f

headSeq :: S.Seq a -> a
headSeq s = h
    where (h :< _) = S.viewl s

tailSeq :: S.Seq a -> S.Seq a
tailSeq s = t
    where (_ :< t) = S.viewl s

randomSt :: (RandomGen g, Random a) => State g a
randomSt = state random

randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt a = state $ randomR a

binaryTournament :: (RandomGen g, Ord b) => Population a b -> State g (GenoType a)
binaryTournament pop = do
  let l = S.length pop
      l' = E.assert (l > 0) (l - 1)
  i <- randomRSt (0, l')
  j <- randomRSt (0, l')
  let (gi, si) = S.index pop i
      (gj, sj) = S.index pop j
  return $ if si > sj then gi else gj

mkSelect :: Int
         -> (Population a b -> State g (GenoType a))
         -> Select a b g
mkSelect eliteCount f =
  Select eliteCount (\pop -> T.sequence . fmap (const $ f pop) $ pop)

mkCrossOver ::(GenoType a -> GenoType a -> State g (GenoType a))
            -> CrossOver a g
mkCrossOver f = CrossOver go
    where go genoTypes = T.sequence . fmap (uncurry f) $ couples
              where g = genoTypes |> headSeq genoTypes
                    couples = S.zip g $ tailSeq g

mkMutate :: Double -> (GenoType a -> State g (GenoType a))
         -> Mutate a g
mkMutate = Mutate

mkFitness :: FitnessType -> (a -> b) -> Fitness a b
mkFitness = Fitness

data GeneBits a = GeneBits !a !Int
              deriving (Show, Eq)

mutateBits :: (RandomGen g, B.Bits a) => GeneBits a -> State g (GeneBits a)
mutateBits (GeneBits bits len) = do
  i <- randomRSt (0, E.assert (len > 0) (len - 1))
  return $ GeneBits (B.complementBit bits i) len

crossOverBits :: (RandomGen g, B.Bits a)
              => GeneBits a
              -> GeneBits a
              -> State g (GeneBits a)
crossOverBits g@(GeneBits _ len) g'@(GeneBits _ len') = do
  i <- randomRSt (0, E.assert (len == len' && len > 0) (len - 1))
  return $ mergeGeneBits g g' i

mergeGeneBits :: B.Bits a
              => GeneBits a
              -> GeneBits a
              -> Int
              -> GeneBits a
mergeGeneBits (GeneBits bits len) (GeneBits bits' len') i =
    GeneBits (mergeBits (bits .&. mask) (bits' .&. mask) i) len
        where mask = B.complement $ B.shift oneBits $ E.assert (len == len') len

mergeBits :: B.Bits a => a -> a -> Int -> a
mergeBits b b' i = b .&. leftMask .|. b' .&. rightMask
    where rightMask = B.shift oneBits i
          leftMask = B.complement rightMask

oneBits :: B.Bits a => a
oneBits = B.complement B.zeroBits

mutateSeq :: (RandomGen g, Random a) => S.Seq a -> State g (S.Seq a)
mutateSeq xs = do
  let l = S.length xs
  i <- randomRSt (0, E.assert (l > 0) (l - 1))
  newVal <- randomSt
  return $ S.update i newVal xs

crossOverSeq :: (RandomGen g) => S.Seq a -> S.Seq a -> State g (S.Seq a)
crossOverSeq g g' = do
  let l = E.assert (S.length g == S.length g') (S.length g)
  i <- randomRSt (0, E.assert (l > 0) (l - 1))
  return $ S.take i g >< S.drop i g'
