
var socket = new WebSocket("wss://localhost:8080/socket");

socket.onopen = function(_) {
  alert("Connection opened");
}

socket.onmessage = function(_) {
  alert("sending message");
}

