module WebSocket 
  ( WebSocket
  , socket
  , onOpen
  , onClose
  , onMessage
  , onError
  , send
  ) where

import Prelude

import Effect (Effect)
import Foreign (Foreign)

foreign import ffi_socket :: String -> Effect WebSocket
foreign import ffi_send :: forall a. WebSocket -> a -> Effect Unit
foreign import ffi_onOpen :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
foreign import ffi_onMessage :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
foreign import ffi_onClose :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
foreign import ffi_onError :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket

data WSEvent = WSEvent
  { message :: String
  }

type WebSocket = Foreign

socket 
  :: String 
  -> Effect WebSocket
socket = ffi_socket

onOpen :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
onOpen = ffi_onOpen

onMessage :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
onMessage = ffi_onMessage

onClose :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
onClose = ffi_onClose

onError :: forall event. WebSocket -> (event -> Effect Unit) -> WebSocket
onError = ffi_onError

send :: forall a. WebSocket -> a -> Effect Unit
send = ffi_send

