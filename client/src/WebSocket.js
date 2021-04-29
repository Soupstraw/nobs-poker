"use strict"

exports.ffi_socket = url => () => {
  console.log("url", url);
  return new WebSocket(url);
}

exports.ffi_onOpen = ws => fun => {
  ws.onopen = x => fun(x)();
  return ws;
}

exports.ffi_onMessage = ws => fun => {
  ws.onmessage = x => fun(x)();
  return ws;
}

exports.ffi_onClose = ws => fun => {
  ws.onclose = x => fun(x)();
  return ws;
}

exports.ffi_onError = ws => fun => {
  ws.onerror = x => fun(x)();
  return ws;
}

exports.ffi_send = ws => data => () => {
  ws.send(data);
}
