// This recieves messages of type "error_message" from the server
Shiny.addCustomMessageHandler("error_message",
  function(message) {
    alert(JSON.stringify(message));
  }
);