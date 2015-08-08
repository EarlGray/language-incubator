import qualified Data.Map as M

type Index = Int
type BBOff = Int

-- |
-- |   Definition of basic block in BF:
-- |   any sequence of commands without '[' and ']'
-- |
data BBCommands = BBAdd | BBSub | BBPut | BBGet | BBLeft | BBRight deriving Show

type BasicBlock = [BBCommands]

-- |  Effects from point of view of a cell:
-- |  mutations may be caused by the program or by input, or both
data CellEffect = CellEffect
  { cellInput   :: Maybe Index -- Just input_seq | Nothing
  , cellMutated :: Int         -- adder after initial state/last input
  } deriving (Show, Eq)
nullCellEffect = CellEffect Nothing 0

-- |
-- |   Symbolic value used for output:
-- |
data Value = Value BBOff CellEffect deriving Show


-- |  Effects from point of view of a basic block:
-- |  It is a tuple of:
-- |    1) changeset: a finite set of state mutations
-- |    2) eventset:  a finite sequence of i/o events
-- |    3) end offset: a finite offset after execution
data BBEffects = BBEffects
  { bbChangeSet :: M.Map BBOff CellEffect
  , bbEventSet  :: ([BBOff], [Value])
  , bbEndOffset :: BBOff
  } deriving Show

nullBBEffects = BBEffects
  { bbChangeSet = M.empty
  , bbEventSet  = ([], [])
  , bbEndOffset = 0
  }

parseBasicBlock :: String -> (BasicBlock, String)
parseBasicBlock = go []
  where
    go acc []         = (reverse acc, [])
    go acc ('+':cs)   = go (BBAdd:acc)   cs
    go acc ('-':cs)   = go (BBSub:acc)   cs
    go acc ('<':cs)   = go (BBLeft:acc)  cs
    go acc ('>':cs)   = go (BBRight:acc) cs
    go acc ('.':cs)   = go (BBPut:acc)   cs
    go acc (',':cs)   = go (BBGet:acc)   cs
    go acc s@('[':cs) = (reverse acc, s)
    go acc s@(']':cs) = (reverse acc, s)
    go acc (_:cs)     = go acc cs

-- |
-- |  Analyze effects of a basic block
-- |
inferBBEffects :: BasicBlock -> BBEffects
inferBBEffects ops = go nullBBEffects ops
  where -- cs = changset, es = eventset, pos = offset:
    go (BBEffects cs (ein, eout) pos) []
        = BBEffects cs (reverse ein, reverse eout) pos
    go (BBEffects cs es pos) (BBAdd:ops)
        = go (BBEffects (chngAdd 1    pos cs) es pos) ops
    go (BBEffects cs es pos) (BBSub:ops)
        = go (BBEffects (chngAdd (-1) pos cs) es pos) ops
    go (BBEffects cs es pos) (BBLeft:ops)
        = go (BBEffects cs es (pos - 1)) ops
    go (BBEffects cs es pos) (BBRight:ops)
        = go (BBEffects cs es (pos + 1)) ops
    go (BBEffects cs (ein, eout) pos) (BBGet:ops)
        = go (BBEffects (chngGet pos cs (length ein)) (pos:ein, eout) pos) ops
    go (BBEffects cs (ein, eout) pos) (BBPut:ops)
        = go (BBEffects cs (ein, (outEv pos cs):eout) pos) ops

    chngAdd adder pos changeset =
      let upd Nothing = Just $ CellEffect Nothing adder
          upd (Just (CellEffect inp mut)) =
            case (inp, mut + adder) of
              (Nothing, 0) -> Nothing
              (_      , n) -> Just (CellEffect inp n)
      in M.alter upd pos changeset
    chngGet pos cs ind = M.insert pos (CellEffect (Just ind) 0) cs
    outEv pos cs = case M.lookup pos cs of
      Just eff -> Value pos eff
      Nothing ->  Value pos nullCellEffect


-- |    Shortcuts and tests
bbparse cs = case parseBasicBlock cs of
              (cmds, []) -> cmds
bbeffects = inferBBEffects . bbparse

bbtests = [
  (">++++<-", BBEffects {
      bbChangeSet = M.fromList
        [(0, CellEffect Nothing (-1)) -- pos 0 mutated by -1
        ,(1, CellEffect Nothing 4) -- pos 1 mutated by +4
        ],
      bbEventSet = ([], []),  -- no input/output
      bbEndOffset = 0         -- the last position is the initial position
   }),
  (".,.", BBEffects {
      bbChangeSet = M.fromList [(0, CellEffect (Just 0) 0)],
      bbEventSet = ([0],  -- Input: one event overwriting pos 0
                    [Value 0 (CellEffect Nothing 0), -- initial value of pos 0, no mutation
                     Value 0 (CellEffect (Just 0) 0) -- the first read value, no mutation
                    ]),
      bbEndOffset = 0
  })]


-- |
-- |    Iterated blocks
-- |

data IteratedBlock
  = IBBasic BasicBlock
  | IBLoop [IteratedBlock]
  deriving Show

parseLoop :: String -> ([IteratedBlock], String)
parseLoop = go []
  where
    go acc (']':cs) = (reverse acc, cs)
    go acc ('[':cs) =
      case parseLoop cs of
        (body, cs') -> go (IBLoop body : acc) cs'
    go acc cs =
      case parseBasicBlock cs of
        (bb, cs') -> go (IBBasic bb : acc) cs'

parseTop :: String -> [IteratedBlock]
parseTop = go []
  where
    go acc "" = reverse acc
    go acc ('[':cs) =
      case parseLoop cs of
        (body, cs') -> go (IBLoop body : acc) cs'
    go acc cs =
      case parseBasicBlock cs of
        (bb, cs') -> go (IBBasic bb : acc) cs'

