import Control.Applicative
import Data.List
import Text.Printf

headOr :: String -> [a] -> a
headOr err [] = error err
headOr _ (hd : _ ) = hd

type Addr = Int

data InstrSet1
    = IS1Add Addr Int
    | IS1Sub Addr Int
    | IS1LSta Addr Int
    | IS1LEnd Addr Int
    | IS1Put Addr
    | IS1Get Addr
    | IS1Lbl Int

fromBFtoInstrSet1 prog = 
    case foldl' go ([], 0, 1, []) prog of
      (is1lst, _, _, _) -> demux $ reverse is1lst
  where
    demux = 
      let gradd (IS1Add p1 1) (IS1Add p2 1) = p1 == p2
          gradd (IS1Sub p1 1) (IS1Sub p2 1) = p1 == p2
          gradd _ _ = False
          collect gr = case gr of
                            (IS1Add p 1 : _) -> IS1Add p (length gr)
                            (IS1Sub p 1 : _) -> IS1Sub p (length gr)
                            [op] -> op
      in map collect . groupBy gradd
    go (res, addr, lbln, branches) op =
      case op of
        '>' -> (res, succ addr, lbln, branches)
        '<' -> (res, pred addr, lbln, branches)
        '+' -> ((IS1Add addr 1 : res), addr, lbln, branches)
        '-' -> ((IS1Sub addr 1 : res), addr, lbln, branches)
        '[' -> let res' = (IS1Lbl (negate lbln) : IS1LSta addr lbln : res) 
               in (res', addr, succ lbln, (lbln : branches))
        ']' -> 
          case branches of
            (lbl : branches') -> 
                let res' = (IS1Lbl lbl : IS1LEnd addr (negate lbl) : res)
                in (res', addr, lbln, branches')
            [] -> error "fromBFtoInstrSet1: mismatching ]"
        '.' -> ((IS1Put addr : res), addr, lbln, branches)
        ',' -> ((IS1Get addr : res), addr, lbln, branches)
        c -> error $ "fromBFtoInstrSet1 : unknown '" ++ [c] ++ "'"

showIS1 (IS1Add p n)  = printf "  add [%d] %d" p n
showIS1 (IS1Sub p n)  = printf "  sub [%d] %d" p n
showIS1 (IS1LSta p l) = printf "  bnz [%d] %c%d" p (if l < 0 then 'b' else 'f') (abs l)
showIS1 (IS1LEnd p l) = printf "  bz  [%d] %c%d" p (if l < 0 then 'b' else 'f') (abs l)
showIS1 (IS1Lbl l)    = printf "%c%d:" (if l < 0 then 'b' else 'f') (abs l)
showIS1 (IS1Get p)    = printf "  get [%d]" p
showIS1 (IS1Put p)    = printf "  put [%d]" p

instance Show InstrSet1 where show = showIS1

main = do
    bfprog <- filter (`elem` "+-><.,[]") <$> getContents
    mapM_ print $ fromBFtoInstrSet1 bfprog
    
