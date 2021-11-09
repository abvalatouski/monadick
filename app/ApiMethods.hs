module ApiMethods
  where

import           Web.VK.Api.Mtl ((@=))
import qualified Web.VK.Api.Mtl as VK

-- See "https://vk.com/dev/messages.send".
sendMessage :: (VK.MonadApi m, VK.Encode a) => VK.Id -> a -> m ()
sendMessage userId text = do
    randomId <- VK.randomId
    VK.callMethod_ "messages.send"
        [ "peer_id"   @= userId
        , "random_id" @= randomId
        , "message"   @= text
        ]
