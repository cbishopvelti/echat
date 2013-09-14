
var WebRTCServer = WebRTC.extend({

	type: "server",

	initialize: function(options){

		this.us = options.us;
		this.to = options.to;


		// websocketApp.onmessage["sdpoffer"] = this.onOfferMessage.createDelegate(this);
		// websocketApp.onmessage["candidateoffer"] = this.onCandidiateMessage.createDelegate(this);


	},

	initializeRTC: function(){
		console.log("WebRTCServer.initializeRTC");

		WebRTC.prototype.initializeRTC.call(this);
	},

	onOfferMessage: function(event){

		console.log("WebRTCServer: onOfferMessage =================================");
		//todo, set to


		if(this.pc == undefined){
			console.log("initializeRTC");
			this.initializeRTC();
		}

		var data = event.data;

		var signal = JSON.parse(data);
		

		var sdp = JSON.parse(signal.message) 


		console.log("this.pc.setRemoteDescription: ", sdp);
		this.pc.setRemoteDescription(new RTCSessionDescription(
			// JSON.parse(decodeURIComponent(signal.message))
			sdp
		));

			//add stream and create answer
		this.pc.addStream(MyStream);

		this.pc.createAnswer(
			this.gotDescription.createDelegate(this)
		);
		

	}, 
	
	//depricated
	// getMediaSuccess: function(stream){

	// 	console.log( "WebRTCServer: getMediaSuccess (create answer)", stream );

	// 	if(MyStream == undefined){
	// 		MyStream = stream;
	// 	}else{
	// 		stream = MyStream;
	// 	}

	// 	// selfView.src = URL.createObjectURL(stream);
 //        this.pc.addStream(stream);

	// 	this.pc.createAnswer(
	// 		this.gotDescription.createDelegate(this)
	// 	);
	// }, 

});

