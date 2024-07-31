-- | Abstractions for messages in the Actor Scheme
-- language.
module Domain.Scheme.Actors.Message(MessageDomain(..), matchesTagCP, SimpleMessage) where

import Data.Kind
import Domain (BoolDomain, Domain (..))
import Lattice (EqualLattice (..), CP)

class MessageDomain v where
  -- | The type of payload in the message domain
  type Payload v :: Type

  -- | The type of tags in the message domain
  type Tag v :: Type

  -- | Construct a message based on a concrete tag
  -- and list of abstract payload values
  message :: String -> [Payload v] -> v

  -- | Get the abstract tag of the message
  tag :: v -> Tag v

  -- | Get the abstract payload of the message
  payload :: v -> [Payload v]

  -- | Check whether the given concrete
  -- tag matches the abstract tag from the message
  matchesTag :: (Domain (Tag v) String, EqualLattice (Tag v), BoolDomain b) => String -> v -> b
  matchesTag t msg = tag msg `eql` inject t

-- | Same as `matchesTag` but fixes the output to the constant propagation lattice
matchesTagCP :: (MessageDomain v, Domain (Tag v) String, EqualLattice (Tag v)) => String -> v -> CP Bool
matchesTagCP = matchesTag

-- | Simple message representation with tags in the constant propagation lattice and a list 
-- of payload elements
data SimpleMessage v = SimpleMessage { messageTag :: CP String, messagePayload ::  [v] }

instance MessageDomain (SimpleMessage v) where
   type Payload (SimpleMessage v) = v
   type Tag (SimpleMessage v) = CP String

   message t = SimpleMessage (inject t)
   tag = messageTag
   payload = messagePayload
