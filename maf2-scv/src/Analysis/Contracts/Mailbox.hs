module Analysis.Contracts.Mailbox (ConstrainedMessage (..), AnnotateMessageT, runAnnotateMessageT) where

import qualified Domain.Scheme.Actors.Message as ActorScheme
import Analysis.Actors.Mailbox
import Analysis.Actors.Monad (ActorGlobalM (..))
import Analysis.Symbolic.Monad (MonadPathCondition (..))
import Control.Monad.Identity
import Control.Monad.Layer (MonadLayer (..))
import Control.Monad.Trans.Class
import Symbolic.AST

-- |  A variant of `SimpleMessage` that also contains
--  the path constraints of the path the message
--  was sent on.
data ConstrainedMessage msg
  = -- | a regular message
    RegularMessage {getMessage :: msg}
  | -- | messagers sent on a path with the given constraints as path condition
    ConstrainedMessage {getMessage :: msg, pc :: PC}
  deriving (Ord, Eq, Show)

-- | A version for Actor scheme of the constrained message
instance (ActorScheme.MessageDomain msg) => ActorScheme.MessageDomain (ConstrainedMessage msg) where 
   type Payload (ConstrainedMessage msg) = ActorScheme.Payload msg 
   type Tag (ConstrainedMessage msg) = ActorScheme.Tag msg

   message tag = RegularMessage . ActorScheme.message tag
   tag = ActorScheme.tag . getMessage
   payload = ActorScheme.payload . getMessage 

-- | Monad transformer that implements messagee sending
-- semantics by annotating the message with a the current
-- path condition.
newtype AnnotateMessageT v ref msg mb m a = AnnotateMessageT (IdentityT m a)
  deriving (Monad, Applicative, Functor, MonadTrans, MonadLayer)

instance (ActorGlobalM m ref (ConstrainedMessage v) mb, MonadPathCondition m v) => ActorGlobalM (AnnotateMessageT v ref (ConstrainedMessage v) mb m) ref (ConstrainedMessage v) mb where
  send ref (RegularMessage sm) = getPc >>= upperM . send ref . ConstrainedMessage sm
  send _ _ = error "can only annotate regular messages"
  getMailbox = upperM . getMailbox


-- | Run the `AnnotateMessageT` monad transformer
runAnnotateMessageT :: AnnotateMessageT v ref msg mb m a -> m a
runAnnotateMessageT (AnnotateMessageT ma) = runIdentityT ma
