module Domain.Contract.Message where 

import Domain.Scheme.Actors.Message

-- | Typeclass to represent regular and enhanced messages (i.e., messages with contracts)
-- cf. Vandenbogaerde et al. (2024) Blame-Correct Support for Receiver Properties in Recursively-Structured Actor Contracts
class (MessageDomain v) => ContractMessageDomain v where
