module Language.Atom.Channel
  ( -- * Channel Declarations
    channel
  , ChanInput (..)
  , ChanOutput (..)
    -- * Channel operations
  , writeChannel
  , readChannel
  )
where


import Language.Atom.Elaboration
import Language.Atom.Expressions


-- Channel Declarations ------------------------------------------------

-- | Declare a typed channel. Returns channel input/output handles.
channel :: Expr a => Name -> a -> Atom (ChanInput a, ChanOutput a)
channel name init' = do
  name' <- addName name
  (st, (g, atom)) <- get
  let cin  = mkChanInput (gChannelId g) name'
      cout = mkChanOutput (gChannelId g) name'
      c    = constant init'
  put (st, ( g { gChannelId = gChannelId g + 1
               , gState = gState g ++ [StateChannel name c]
               }
           , atom
           )
      )
  return (cin, cout)

-- | Input side of a typed channel
data ChanInput a = ChanInput
  { cinID   :: Int
  , cinName :: Name
  }
  deriving (Eq, Show)

mkChanInput :: Expr a => Int -> Name -> ChanInput a
mkChanInput = ChanInput

-- | Output side of a typed channel
data ChanOutput a = ChanOutput
  { coutID   :: Int
  , coutName :: Name
  }
  deriving (Eq, Show)

mkChanOutput :: Expr a => Int -> Name -> ChanOutput a
mkChanOutput = ChanOutput


-- Channel Operations --------------------------------------------------

-- | Write a message to a typed channel. This is a NOP if there is an unread
-- message waiting in the channel.
writeChannel :: Expr a => ChanInput a -> a -> Atom ()
writeChannel _cin _e = error "writeChannel not implemented"

-- | Read a message from a typed channel. This operation returns the last
-- message and the boolean False if there is no new message waiting.
readChannel :: Expr a => ChanOutput a -> (a, Bool)
readChannel _cout = error "readChannel not implemented"
