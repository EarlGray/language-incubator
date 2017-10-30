import Data.List (group)
import Data.Char
import Data.Word
import Debug.Trace

data Zip a = Zip { zbefore :: [a], zafter :: [a] } deriving Show

zipNil = Zip [] []

zipIsEnd (Zip _ rhs) = null rhs
zipForward (Zip lhs (v:rhs)) = Zip (v:lhs) rhs

zipBack (Zip [] rhs) = error $ "zipBack [] " ++ show rhs
zipBack (Zip (v:lhs) rhs) = Zip lhs (v:rhs)

zipToStart z@(Zip [] _) = z
zipToStart z@(Zip (_:_) _) = zipToStart $ zipBack z

zipToEnd z@(Zip _ []) = z
zipToEnd z@(Zip _ (_:_)) = zipToEnd $ zipForward z

zipFoldLeftUntil z hist until =
  case until (zipHead z) hist of
    Just hist' -> zipFoldLeftUntil (zipBack z) hist' until
    Nothing -> z
zipFoldRightUntil z hist until =
  case until (zipHead z) hist of
    Just hist' -> zipFoldRightUntil (zipForward z) hist' until
    Nothing -> z

zipInsBefore (Zip lhs rhs) v = Zip (v:lhs) rhs
zipInsAfter (Zip lhs rhs) v = Zip lhs (v:rhs)

zipFromList vs = Zip [] vs

zipUpdate (Zip lhs []) _ = error $ "zipUpdate " ++ show lhs
zipUpdate (Zip lhs (v:rhs)) modify = Zip lhs (modify v : rhs)
zipSet (Zip lhs (_:rhs)) v = Zip lhs (v:rhs)
zipHead (Zip _ (v:_)) = v


data BOp
    = Add Int | Mov Int | BrZ | BrNZ | Input
    | Output
    deriving (Show, Eq)
    
compile :: String -> Zip BOp
compile src = zipFromList $ preprocess src
  where
    preprocess = map opmap . filter (`elem` "+-<>,.[]")
    opmap '+' = Add 1
    opmap '-' = Add (-1)
    opmap '<' = Mov (-1) 
    opmap '>' = Mov 1
    opmap ',' = Input
    opmap '.' = Output
    opmap '[' = BrZ  -- to be filled later
    opmap ']' = BrNZ -- to be filled later
    
execute :: Zip BOp -> String -> String
execute ops input = exec 0 ops (Zip [] [0]) input
  where
    exec :: Int -> Zip BOp -> Zip Word8 -> String -> String
    exec i ops mem inp =
      if zipIsEnd ops then ""
      else if i >= 100000 then "\nPROCESS TIME OUT. KILLED!!!"
      else 
        let i1 = succ i
            ops' = zipForward ops
            dbg = concat [show i, ")\t`", show (zipHead ops), "`@", show (length $ zbefore ops), 
                          "\tat ", show $ length $ zbefore mem]
        in trace dbg $ case (zipHead ops) of
            Add inc ->
              let mem' = zipUpdate mem (+ (fromIntegral inc))
              in exec i1 ops' mem' inp
            Mov (-1) ->
              let mem' = zipBack mem
              in exec i1 ops' mem' inp
            Mov 1 -> 
              let mem' = zipForward mem
                  mem'' = if zipIsEnd mem' then zipInsAfter mem' 0 else mem'
              in exec i1 ops' mem'' inp
            Input ->
              let m:inp' = inp
                  mem' = zipSet mem (fromIntegral $ ord m)
              in exec i1 ops' mem' inp'
            Output ->
              let m = zipHead mem
              in {-trace ("put " ++ show m) $ -} chr (fromIntegral m) : exec i1 ops' mem inp
            BrZ ->
              if zipHead mem /= 0
              then exec i1 ops' mem inp
              else
                let until BrNZ 0 = Nothing
                    until BrNZ nested = Just (nested - 1)
                    until BrZ nested = Just (nested + 1)
                    until _ nested = Just nested
                    ops' = zipFoldRightUntil (zipForward ops) 0 until
                in exec i1 ops' mem inp
            BrNZ ->
              if zipHead mem == 0
              then exec i1 ops' mem inp
              else
                let until BrZ 0 = Nothing
                    until BrZ nested = Just (nested - 1)
                    until BrNZ nested = Just (nested + 1)
                    until _ nested = Just nested
                    ops' = zipFoldLeftUntil (zipBack ops) 0 until
                in {- trace (show ops') $ -} exec i1 ops' mem inp


main = do
    [n, m] <- map (read :: String -> Int) <$> words <$> getLine
    input <- init <$> getLine
    prog <- getContents
    let ops = compile prog
    putStrLn $ execute ops input
