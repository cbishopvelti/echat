var WebRTC = Backbone.Model.extend({

	pc: undefined,

	type: undefined,

	to: undefined, //who we are connecting to

	us: undefined, //our address

	initializeRTC: function(){

		var servers = undefined;
		// if(webrtcDetectedBrowser == "firefox"){
		// 	//set this to firefox stun servers
		// 	var iceServers = {
		// 	    iceServers: [{
		// 	            url: 'stun:23.21.150.121'
		// 	        }
		// 	    ]
		// 	};
		// 	this.pc = new mozRTCPeerConnection(iceServers);
		// }else{
			
		// }

		this.pc = new RTCPeerConnection(servers);

		this.pc.onicecandidate = this.onIceCandidate.createDelegate(this);

		this.pc.onaddstream = this.onAddStream.createDelegate(this);
		

		if(this.type == "server"){
			// this.pc.createAnswer(
			// 	this.gotDescription.createDelegate(this)
			// );
		}else if(this.type == "client"){
			
			this.pc.addStream(MyStream); //we will have the stream

			this.pc.createOffer(
				this.gotDescription.createDelegate(this)
			);
		}
	},


	onCandidiateMessage: function(event){
		
		if(this.pc == undefined){
			this.initializeRTC();
		}

		var data = event.data;

		var signal = JSON.parse(data);

		//dk why this happens
		if(signal.message == null 
			|| signal.message == "null"
		){
			// this.pc.addIceCandidate( null );
			return;
		}
		this.pc.addIceCandidate(new RTCIceCandidate(JSON.parse( signal.message )));
	}, 	

	onIceCandidate: function(event){

		console.log("onIceCandidate");

		websocketApp.connection.send(JSON.stringify(
			{
				type: "candidate" + ((this.type == "server") ? "answer" : "offer"), 
				to: this.to,
				from: this.us,
				message: JSON.stringify( event.candidate )
			}
		));
	},

	onAddStream: function(event){

		//when a stream gets added
		console.log("onAddStream =======================");


		//new video for each stream
		var vid = $("<video autoplay=\"true\"></video>");
		$("#videos").append(vid);

		attachMediaStream( vid[0], event.stream );

		var aid = $("<audio autoplay=\"true\"></audio");
		$("#videos").append(aid);
		attachMediaStream( aid[0], event.stream );

		// attachMediaStream( $("video#remote")[0], event.stream);

	},

	gotDescription: function(desc){

		console.log("WebRTC: gotDescription", this.to, this.us);

		console.log("this.pc.setLocalDescription");
		this.pc.setLocalDescription(desc);

		websocketApp.connection.send(JSON.stringify({
			"type": "sdp" + ((this.type == "server") ? "answer": "offer"),
			"to" : this.to,
			"from" : this.us,
			"message": JSON.stringify(desc) 
		}))
	}, 

})