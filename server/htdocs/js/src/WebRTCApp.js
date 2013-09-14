
$(function(){

	//ppl we're clients to
	var clients = {};
	//ppl we're servers for
	var servers = {};

	//only use one instance of my stream
	MyStream = undefined;

	var connectionOpen = false;

	getUserMedia({video: true, audio: true}, 
		function(stream){

			MyStream = stream;

			//request the client list
			//TODO: ensure the websocket has been started, as technicaly, it might not have been

			if(connectionOpen == false){
				throw "Connection has not been open, refresh and wait";
			}

			websocketApp.connection.send(JSON.stringify({type: "connect_media"}));

			attachMediaStream($("video#me")[0], stream);

		}, function(){
			console.log("error")
		}
	);

	websocketApp.onmessage["client_list"] = function(event){
		console.log("WebRTCApp: client_list ", event.data);

		//don't try and connect until we have both client_list & MediaStream

		var in1 = JSON.parse(event.data)

		for(var i = 0; i < in1.message.length; i++){

			clients[in1.message[i]] = new WebRTCClient({to: in1.message[i], us: in1.to});

			// break; //do for just one
		}
	}

	//SERVER
	websocketApp.onmessage["sdpoffer"] = function(event){
		var in1 = JSON.parse(event.data);
		if(servers[in1.from] == undefined){
			//we need a new server
			servers[in1.from] = new WebRTCServer({us: in1.to, to: in1.from});
		}
		return servers[in1.from].onOfferMessage(event);
	}

	websocketApp.onmessage["candidateoffer"] = function(event){
		var in1 = JSON.parse(event.data);
		servers[in1.from].onCandidiateMessage(event);
	}

	//CLIENT
	websocketApp.onmessage["sdpanswer"] = function(event){
		var in1 = JSON.parse(event.data);
		clients[in1.from].onAnswerMessage(event)
	}

	websocketApp.onmessage["candidateanswer"] = function(event){
		var in1 = JSON.parse(event.data);
		clients[in1.from].onCandidiateMessage(event);
	}


	//do nothing, the server will start sending us data
	websocketApp.on("connectionopen", function(){

		connectionOpen = true;


	})
})