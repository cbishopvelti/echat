var WebRTCClient = WebRTC.extend({

	type: "client", 

	initialize: function(options){

		console.log("client init: ", app.onmessage);

		this.to = options.to;

		this.us = options.us;

		// websocketApp.onmessage["sdpanswer"] = this.onAnswerMessage.createDelegate(this);	
		// websocketApp.onmessage["candidateanswer"] = this.onCandidiateMessage.createDelegate(this);

		//initialize the RTC
		this.initializeRTC();
	}, 

	onAnswerMessage: function(event ){
		console.log("WebRTCClient: onAnswerMessage");

		var data = event.data;

		var signal = JSON.parse(data);

		var sdp = JSON.parse(signal.message);
		// sdp.sdp = sdp.sdp.replace(/\+/g, " "); // bug

		this.pc.setRemoteDescription(new RTCSessionDescription(
			sdp
		));

	},

	//depreciated
	// getMediaSuccess: function(stream){
	// 	console.log("WebRTCClient: getMediaSuccess (createOffer)");

	// 	if(MyStream == undefined){
	// 		MyStream = stream;
	// 	}else{
	// 		stream = MyStream;
	// 	}

	// 	this.pc.addStream(stream);

	// 	this.pc.createOffer(
	// 		this.gotDescription.createDelegate(this)
	// 	);
	// }
})