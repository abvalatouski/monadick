import           Control.Monad

import           Control.Monad.State
import           Data.FileEmbed          (embedStringFile)
import qualified Data.HashMap.Strict     as HashMap
import           Data.Text               (Text)
import           Lens.Micro.Mtl
import qualified Web.VK.Api.Mtl          as VK
import qualified Web.VK.Api.LongPoll.Mtl as VK

import           ApiMethods
import           Commands
import           Types

main :: IO ()
main = do
    let apiToken   = $(embedStringFile "private/api-token")
        apiVersion = $(embedStringFile "private/api-version")
        groupId    = $(embedStringFile "private/group-id")
    runBot bot apiToken apiVersion groupId

bot :: BotM ()
bot = forever do
    events <- VK.awaitEvents
    forM_ events \case 
        VK.MessageNew message@VK.Message { .. } -> do
            conversations <- get
            unless (HashMap.member messagePeerId conversations) do
                let botState = BotState Sleeping
                conversation messagePeerId .= Conversation messagePeerId botState

            groupId <- VK.askGroupId
            case messageAction of
                Just (VK.ChatInviteUserById id) | id == -groupId -> do
                    let greeting =
                           "Привет, я Монадик!\n\
                            \Напишите /?, чтобы узнать обо мне больше." :: Text
                    sendMessage messagePeerId greeting
                _ ->
                    pure ()

            currentStatus <- use $ conversation messagePeerId . conversationBotState . botStatus
            case parseSomeCommand messageText of
                Just (Right command) ->
                    runSomeCommand command message
                Just (Left NoSuchCommand) ->
                    when (currentStatus == Active) do
                        sendMessage messagePeerId ("У меня нет такой команды." :: Text)
                Just (Left (CannotParseCommand (SomeCommand proxy))) ->
                    when (currentStatus == Active || commandName proxy == "on") do
                        let error = "Неверный синтаксис команды. Ожидалось:\n" <> commandSyntax proxy
                        sendMessage messagePeerId error
                Nothing ->
                    pure ()
        _ ->
            pure ()

addConversation :: VK.Id -> BotM ()
addConversation id = do
    let botState = BotState Sleeping
    conversation id .= Conversation id botState
