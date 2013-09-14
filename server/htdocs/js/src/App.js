
$(function(){
	var AppView = Backbone.View.extend({

		el: $("#app"),

		onmessage: {},

		initialize: function(){

			_.extend(this, Backbone.Events)

 		},

		//holds a set of onmessage handlers

		events: {
			"click #init-connection": "initConnection", 
			"click #send-message": "sendMessage", 
			"keyup #input": "keyUp",
		},
		
		initialize: function(){

		},

		render: function(){

		},
	
		initConnection: function(){
			
			if('WebSocket' in window){

				var connection = new WebSocket("ws://" + document.domain + ":8079/", ['chat']);

				this.connection = connection;
				
				var self = this;

				connection.onopen = function(event){
					console.log("onopen", event);
					self.trigger("connectionopen"); //the connection has be successfuly created
				}
				
				connection.onclose = function(event){
					console.log("onclose", event);
				}

				connection.onmessage = function(event){
					
					// console.log("onmessage", (new Date().getTime() - self.profile));
					// console.log("onmessage: ", JSON.parse(event.data ) );
					console.log("onmessage: ", event.data);

					var message = JSON.parse( event.data )
					if(message.type != "message" && self.onmessage[message.type] != undefined){
						self.onmessage[message.type].call(undefined, event);
						
						return; //given it to someone else to handle
					}
					message = message.message;


					$("#out").append(
						$("<div style=\"white-space:pre;\">" + message + "</div>" )
					)

					var height = _.reduce($("#out").children(), function(acc, item){
						acc += item.offsetHeight;
						return acc;
					}, 0 );

					$("#out").scrollTop(height);
				};
				
				connection.onerror = function(event){
					console.log("onerror", event);
				}
				
				
			}else{
				console.log("WebSocket not suported");
			}
			
		}, 
		
		sendMessage: function(e){
			this.profile = (new Date()).getTime();
			console.log("sendMessage");

			var message = $("#input").val();

			console.log("message", message);

			$("#input").val("");


			this.connection.send(JSON.stringify({type: "message", message: message}));
			//this.connection.send("this is a long string and should work well i hope what the duce why");
		},
		keyUp: function(event){

			if(event.keyCode == 13 && event.shiftKey == false){
				// this.sendMessage(event);
				this.sendMessage.call(this, event);
				return false;
			}
		}
		
	});
	
	websocketApp = new AppView();
	websocketApp.initConnection();
});