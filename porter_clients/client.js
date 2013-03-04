///----------------------------------------------------------------------
///
/// This module implements a javascript client that connects to the socket 
/// server for send & receive messages from/to it
/// debug and implements with node.js
///
/// Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
///----------------------------------------------------------------------

var PORT = 2070;
var HOST = '127.0.0.1';

var dgram = require('dgram');
var message = new Buffer('javascript');

var client = dgram.createSocket('udp4');
client.send(message, 0, message.length, PORT, HOST, function(err, bytes) {
    if (err) throw err;
    	client.on('message', function (message, remote) {
 		console.log('Incomming message: ' + message);
	});
});
