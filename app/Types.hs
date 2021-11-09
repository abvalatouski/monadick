module Types
  where

import           Control.Monad.Catch
import           Control.Monad.State
import           Data.HashMap.Strict     (HashMap, (!))
import qualified Data.HashMap.Strict     as HashMap
import           Lens.Micro
import           Lens.Micro.TH
import qualified Web.VK.Api.Mtl          as VK
import qualified Web.VK.Api.LongPoll.Mtl as VK

data BotStatus = Active | Sleeping
  deriving stock (Show, Eq)

newtype BotState = BotState
    { _botStatus :: BotStatus
    }
  deriving stock Show

makeLenses ''BotState

data Conversation = Conversation
    { _conversationId       :: !VK.Id
    , _conversationBotState :: !BotState
    }
  deriving stock Show

makeLenses ''Conversation

type Conversations = HashMap VK.Id Conversation

conversation :: VK.Id -> Lens' Conversations Conversation
conversation id = lens (! id) (flip $ HashMap.insert id)

newtype BotM a = BotM
    { runBotM :: StateT Conversations VK.LongPollM a
    }
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadIO
    , MonadState Conversations
    , MonadThrow
    , MonadCatch
    )

instance VK.MonadApi BotM where
    askApiConn = BotM $ lift VK.askApiConn

instance VK.MonadLongPoll BotM where
    askLongPollServer = BotM $ lift VK.askLongPollServer

runBot :: BotM a -> VK.ApiToken -> VK.ApiVersion -> VK.Id -> IO a
runBot action apiToken apiVersion groupId = do
    conn <- VK.mkApiConnDefault apiToken apiVersion
    VK.runLongPollApi (evalStateT (runBotM action) []) groupId conn
