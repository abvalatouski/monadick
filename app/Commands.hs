module Commands
  where

import           Control.Monad
import           Data.Char
import           Data.Functor.Identity
import           Data.List
import           Data.Proxy
import           Text.Printf

import           Control.Monad.IO.Class
import           Data.Attoparsec.Text        (Parser, parseOnly)
import qualified Data.Attoparsec.Text        as Parse
import           Data.HashMap.Strict         (HashMap)
import qualified Data.HashMap.Strict         as HashMap
import           Data.Text                   (Text)
import qualified Data.Text.Lazy.Builder      as Text
import qualified Gsu.Physfac.Site.Pages.Home as Gsu
import qualified Gsu.Physfac.Site.Parser     as Gsu
import           Lens.Micro.Mtl
import qualified Web.VK.Api.Mtl              as VK
import qualified Web.VK.Api.LongPoll.Mtl     as VK

import           ApiMethods
import           Types

class Command a where
    runCommand    :: a -> VK.Message -> BotM ()
    commandName   :: Proxy a -> Text
    payloadParser :: Proxy a -> Parser a
    commandDesc   :: Proxy a -> Text.Builder
    commandSyntax :: Proxy a -> Text.Builder

commandHelp :: Command a => Proxy a -> Text.Builder
commandHelp proxy =
    let desc   = commandDesc proxy
        syntax = commandSyntax proxy
     in syntax <> "\n\n" <> desc

data SomeCommand f where
    SomeCommand :: forall a f. Command a => f a -> SomeCommand f

runSomeCommand :: SomeCommand Identity -> VK.Message -> BotM ()
runSomeCommand (SomeCommand (Identity command)) = runCommand command

data ParseCommandError
    = NoSuchCommand
    | CannotParseCommand (SomeCommand Proxy)

parseCommand ::
    HashMap Text (SomeCommand Proxy)
 -> Text
 -> Maybe (Either ParseCommandError (SomeCommand Identity))
parseCommand proxies = parse do
    void $ Parse.char '/'
    name <- Parse.takeTill isSpace
    Parse.skipSpace
    case HashMap.lookup name proxies of
        Just (SomeCommand proxy) -> do
            command <- parse (payloadParser proxy) <$> Parse.takeText
            case command of
                Just command ->
                    pure $ Right $ SomeCommand $ Identity command
                Nothing ->
                    pure $ Left $ CannotParseCommand (SomeCommand proxy)
        Nothing ->
            pure $ Left NoSuchCommand
  where
    parse parser =
        either (const Nothing) Just
      . parseOnly (parser <* Parse.endOfInput)

parseSomeCommand :: Text -> Maybe (Either ParseCommandError (SomeCommand Identity))
parseSomeCommand = parseCommand commandProxiesTable

-- Команды.

commandProxies :: [SomeCommand Proxy]
commandProxies =
    [ SomeCommand $ Proxy @Help
    , SomeCommand $ Proxy @On
    , SomeCommand $ Proxy @Off
    , SomeCommand $ Proxy @Print
    , SomeCommand $ Proxy @BellRings
    , SomeCommand $ Proxy @Weeks
    ]

commandProxiesTable :: HashMap Text (SomeCommand Proxy)
commandProxiesTable =
    HashMap.fromList
  $ fmap (\(SomeCommand proxy) -> (commandName proxy, SomeCommand proxy))
    commandProxies

-- Печатает текст.
newtype Print = Print Text

instance Command Print where
    runCommand (Print message) VK.Message { .. } = do
        status <- use $ conversation messagePeerId . conversationBotState . botStatus
        when (status == Active) do
            sendMessage messagePeerId message
    
    commandName = const
        "напиши"

    payloadParser = const do
        message <- Parse.takeText
        pure $ Print message

    commandDesc = const
        "Печатает текст."

    commandSyntax = const
        "/напиши\nТЕКСТ"

-- Включает бота.
data On = On

instance Command On where
    runCommand On VK.Message { .. } = do
        status <- use $ conversation messagePeerId . conversationBotState . botStatus
        if status == Active then
            sendMessage messagePeerId ("Я и так работаю." :: Text)
        else do
            conversation messagePeerId . conversationBotState . botStatus .= Active
            sendMessage messagePeerId ("Я проснулся!" :: Text)
    
    commandName = const
        "вкл"

    payloadParser = const do
        pure On

    commandDesc = const
        "Выводит бота из спящего режима.\n\
        \После выполнения это команды бот начнёт реагировать на сообщения.\n\
        \При добавлении в беседу бот находится в спящем состоянии."
    
    commandSyntax = const
        "/вкл"

-- Выключает бота.
data Off = Off

instance Command Off where
    runCommand Off VK.Message { .. } = do
        status <- use $ conversation messagePeerId . conversationBotState . botStatus
        when (status == Active) do
            sendMessage messagePeerId ("Ушёл спать.\nРазбудить меня можно командой /вкл." :: Text)
            conversation messagePeerId . conversationBotState . botStatus .= Sleeping
    
    commandName = const
        "выкл"

    payloadParser = const do
        pure Off
    
    commandDesc = const
        "Переводит бота в спящий режим.\n\
        \После выполнения этой команды бот будет реагировать только на команды /? и /вкл.\n\
        \При добавлении в беседу бот находится в спящем состоянии."
    
    commandSyntax = const
        "/выкл"

data Help = Help

instance Command Help where
    runCommand Help VK.Message { .. } = do
        let help =
                mconcat
              $ intersperse "\n\n##########\n\n"
              $ ("КОМАНДЫ" :)
              $ fmap (\(SomeCommand proxy) -> commandHelp proxy)
                commandProxies
        sendMessage messagePeerId help

    commandName = const
        "?"

    payloadParser = const do
        pure Help
    
    commandDesc = const
        "Показывает эту справочную информацию."
    
    commandSyntax = const
        "/?"

-- Выводит расписание звонков.
data BellRings = BellRings

instance Command BellRings where
    runCommand BellRings VK.Message { .. } = do
        let url = "https://old.gsu.by/physfac/"
        Gsu.HomePage bellRings _ <-
            liftIO . Gsu.fetch Gsu.homePage url =<< VK.apiConnManager <$> VK.askApiConn
        sendMessage messagePeerId (VK.WrapString $ render bellRings)
      where
        render =
            mconcat
          . intersperse "\n\n"
          . ("РАСПИСАНИЕ ЗВОНКОВ" :)
          . fmap show

    commandName = const
        "звонки"

    payloadParser = const do
        pure BellRings
    
    commandDesc = const
        "Выводит расписание звонков с сайта физфика."
    
    commandSyntax = const
        "/звонки"

-- Выводит расписание звонков.
data Weeks = Weeks

instance Command Weeks where
    runCommand Weeks VK.Message { .. } = do
        let url = "https://old.gsu.by/physfac/"
        Gsu.HomePage _ weeks <-
            liftIO . Gsu.fetch Gsu.homePage url =<< VK.apiConnManager <$> VK.askApiConn
        sendMessage messagePeerId (VK.WrapString $ render weeks)
      where
        render =
            mconcat
          . intersperse "\n\n" 
          . ("НЕДЕЛИ НАД И ПОД ЧЕРТОЙ" :)
          . fmap (\(Gsu.WeekPair above under) ->
                printf "НАД: %s | ПОД: %s" (show above) (show under))

    commandName = const
        "недели"

    payloadParser = const do
        pure Weeks
    
    commandDesc = const
        "Выводит расписание недель над и под чертой с сайта физфика."
    
    commandSyntax = const
        "/недели"
