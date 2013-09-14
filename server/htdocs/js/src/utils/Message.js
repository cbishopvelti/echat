
var MessageModel = Backbone.Model.extend({

	connection: null, //the persistance connection

	/**
	responsible for initializing the connection with the server
	*/
	initConnection: function(){
		var out = ""
		+"<?xml version='1.0'?>"
		+"<stream:stream"
		+"	from='dave@localhost'"
		+"	to='dave2@localhost'"
		+"	version='1.0'"
		+"	xml:lang='en'"
		+"	xmlns='jabber:client'"
		+"	xmlns:stream='http://etherx.jabber.org/streams'"
		+">";

		if(!'WebSocket' in window){
			throw "WebSocket not supported";
		}

		this.connection = new WebSocket("ws://localhost:8080/kill/sarg_srv:service");
		this.connection.onopen = function(){
			console.log("Connection open");
		}

	}


});

Message = new MessageModel();