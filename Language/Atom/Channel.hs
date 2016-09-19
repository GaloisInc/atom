module Language.Atom.Channel
  ( -- * Channel Declarations
    channel
  , ChanInput (..)
  , ChanOutput (..)
    -- * Channel operations
  , writeChannel
  , readChannel
  , condChannel
    -- * misc exports
  , channelPrefix
  )
where


import Language.Atom.Elaboration
import Language.Atom.Expressions
import Language.Atom.UeMap (newUE)


-- Channel Declarations ------------------------------------------------

-- | Declare a typed channel. Returns channel input/output handles.
channel :: Expr a
        => Name  -- ^ channel name
        -> a     -- ^ default value for the channel variable
        -> Atom (ChanInput, ChanOutput)
channel name init' = do
  -- add the __chanel_ prefix to the channel name before registering the name
  -- to try to separate the channel and variable namespaces somewhat
  let sName = channelPrefix ++ name
  name' <- addName sName
  (st, (g, atom)) <- get
  let cin  = mkChanInput (gChannelId g) name' init'
      cout = mkChanOutput (gChannelId g) name' init'
      c    = constant init'
      f    = constant False
  put (st, ( g { gChannelId = gChannelId g + 1
               , gState = gState g ++ [StateChannel sName c f]
               }
           , atom
           )
      )
  return (cin, cout)

-- | Input side of a typed channel
data ChanInput = ChanInput
  { cinID   :: Int
  , cinName :: Name
  , cinInit :: Const
  }
  deriving (Eq, Show)

mkChanInput :: Expr a => Int -> Name -> a -> ChanInput
mkChanInput i n c = ChanInput i n (constant c)

-- | Output side of a typed channel
data ChanOutput = ChanOutput
  { coutID   :: Int
  , coutName :: Name
  , coutInit :: Const
  }
  deriving (Eq, Show)

mkChanOutput :: Expr a => Int -> Name -> a -> ChanOutput
mkChanOutput i n c = ChanOutput i n (constant c)


-- Channel Operations --------------------------------------------------

class HasChan b where
  chanID   :: b -> Int
  chanName :: b -> Name
  chanInit :: b -> Const

instance HasChan ChanInput where
  chanID   = cinID
  chanName = cinName
  chanInit = cinInit

instance HasChan ChanOutput where
  chanID   = coutID
  chanName = coutName
  chanInit = coutInit

-- | State struct name prefix for channel variables
channelPrefix :: String
channelPrefix = "__channel_"

-- | Construct a channel variable which, in the C code generation case, is a stand-in
-- for part of the global state sructure (the part storing the channel
-- content).
chanVar :: HasChan b => b -> UV
chanVar c = UVChannel (chanID c) (chanName c) (chanInit c)

-- | Write a message to a typed channel. The write operation happens once
-- (i.e. the last writeChannel in the sequence is used) after the assignment
-- (computation) phase of the Atom's execution.
--
-- The write operation overwrites the content of the given channel.
writeChannel :: Expr a => ChanInput -> E a -> Atom ()
writeChannel cin e = do
  (st, (g, atom)) <- get
  let (h, st0) = newUE (ue e) st
  put (st0, (g, atom { atomChanWrite = Just (chanName cin, h) }))

-- | Read a message from a typed channel. This function returns an expression
-- representing the value of the last message written (or the initial content).
readChannel :: ChanOutput -> E a
readChannel c = VRef (V (chanVar c))

-- | Condition execution of an atom on the given channel containing an unread
-- message.
condChannel :: ChanOutput -> Atom ()
condChannel c = do
  -- TODO implement modify or switch Atom to use regular State monad
  (st, (g, atom)) <- get
  put (st, (g, atom { atomChanListen = Just (chanName c) }))
