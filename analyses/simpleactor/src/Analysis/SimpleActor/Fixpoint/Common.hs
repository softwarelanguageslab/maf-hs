{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
-- | Abstractions common to the sequential and actor-modular analysis
module Analysis.SimpleActor.Fixpoint.Common where

import Syntax.AST
import Domain.Scheme.Actors (Pid(..))
import Prelude hiding (exp)
import Domain.Scheme.Store
import Symbolic.AST ( PC )
import Analysis.Store (CountingMap)
import Analysis.Monad.Store (StoreT)
import RIO
import qualified RIO.Map as Map
import Domain.Core.AbstractCount (AbstractCount)
import Analysis.SimpleActor.Monad (SimpleActorContext(..), MailboxLabel, Message)
import Analysis.Actors.Mailbox.Partitioned.Graph (PartitionedGraph)
import Domain.Core.Count.BoundedCount (BoundedCount)
import Analysis.Actors.Mailbox.Partitioned.Partitions.UnitPartition (UnitPartition)
import Domain.SimpleActor (SymActorValue, SymVar)

------------------------------------------------------------
-- Shorthands
------------------------------------------------------------

type K = AdrCtx
type ActorExp = Exp
type ActorRef = Pid Exp K
type ActorVlu = SymActorValue K 
type ActorEnv = HashMap String ActorAdr
type ActorAdr = VarAdr K
type ActorMai = Map ActorRef PMB
type ActorSto = SchemeStore Exp K ActorVlu
type ActorPC  = PC SymVar
type ActorCou = Map ActorRef AbstractCount
type SymCou   = Map SymVar AbstractCount


------------------------------------------------------------
-- Messages and mailboxes
------------------------------------------------------------

-- The instantiated message type
type MsgPayload = ActorVlu
type MsgContext = (ActorPC, Map SymVar AbstractCount)
type Msg = Message MsgPayload MsgContext


-- The mailbox abstraction itself
type Partition = UnitPartition
type PMB = PartitionedGraph Partition Msg

type LabelCounts = Map MailboxLabel BoundedCount

------------------------------------------------------------
-- Contexts
------------------------------------------------------------

-- | Context for address allocations, function calls and actor references.
data AdrCtx = AdrCtx [Span]    -- k-cfa call sites
                     Int       -- ^ max number of elements in k-cfa
                     ActorCtx  -- ^ actor specific context sensitivity
            | InsensitiveCtx   -- context insensitive analysis
            deriving (Ord, Eq, Show, Generic)

instance NFData AdrCtx

-- | Context specific to the actor analysis
data ActorCtx = ActorCtx ActorRef
              | Empty
              deriving (Ord, Eq, Show, Generic)

instance NFData ActorCtx

-- | k-cfa instance for 'SimpleActorContext'
instance SimpleActorContext AdrCtx where
  pushCallSite s (AdrCtx callsites maxCallsites actCtx) = AdrCtx callsites' maxCallsites actCtx
    where callsites' = take maxCallsites (s : callsites)
  pushCallSite _ InsensitiveCtx = InsensitiveCtx

-- | The initial context
initialContext :: Int -> AdrCtx
initialContext maxCallsites = AdrCtx [] maxCallsites Empty

-- | An initial context for a context insensitive analysis
insensitiveContext :: AdrCtx
insensitiveContext = InsensitiveCtx

------------------------------------------------------------
-- Initial dynamic environment
------------------------------------------------------------

-- | The initial dynamic environment for an actor, only includes
-- the default sending behavior.
initialDynEnvironment :: Map String (VarAdr K)
initialDynEnvironment = Map.singleton "send^" (PrrAdr "send^")

------------------------------------------------------------
-- Stores
------------------------------------------------------------

-- | Shorthand for @StoreT@ by instantiating the backing storage
-- of the store.
type StoreT' k v = StoreT (CountingMap k v) k v

