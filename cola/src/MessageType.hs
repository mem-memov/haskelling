module MessageType (Message(..)) where

import AnswerQuestionType

data Message = Silence | Reference Question | Inquiry Answer deriving (Eq, Show, Read)