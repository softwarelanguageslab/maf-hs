-- | Abstractions for messages in the Actor Scheme
-- language.
module Domain.Scheme.Actors.Message where

import Data.Kind
import Domain (BoolDomain, Domain (..))
import Lattice (EqualLattice (..))

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
