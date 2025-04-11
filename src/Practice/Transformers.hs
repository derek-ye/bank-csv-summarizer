module Practice.Transformers where

-- this is useless code


-- import Control.Monad.Trans.Maybe
-- import Control.Monad.Trans.State
-- import Control.Monad.Trans.Class (lift)

-- main :: IO ()
-- main = do
--   result <- runStateT (filterPosAndStoreLen nums) 0
--   case result of
--     (posList, numPos) -> putStrLn $ "number of positives: " ++ show numPos ++ "\nlist: " ++ show posList
--     Nothing -> "I just got cooked"
--   -- case result of
--   --   Just positives -> putStrLn $ "Positive numbers: " ++ show positives
--   --   Nothing -> putStrLn $ "Negative numbers booo"
--   where
--     nums = [1,2,-1,-5]

-- filterPositive :: Monad m => [Int] -> MaybeT m [Int]
-- filterPositive l = MaybeT $ pure result
--   where
--     positiveNums = filter (>= 0) l
--     result = case positiveNums of
--       [] -> Nothing
--       _ -> Just positiveNums

-- filterPosAndStoreLen :: [Int] -> StateT Int Maybe [Int]
-- filterPosAndStoreLen l = do
--   mPositives <- runMaybeT $ filterPositive l
--   case mPositives of
--     Just nums -> do
--       put (length nums)
--       pure nums
--     Nothing -> do
--       put 0
--       lift Nothing
