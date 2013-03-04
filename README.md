Porter
======

Broadcast UDP over Erlang

Provides the managment of clients connected to server, by provide an ID, also
support for send messages to a specific client.


Configuring & building
======

Porter needs a specific port for starts the udp server, to configure, just edit
environment var 'port' on the .app file under ebin directory.

Now when you have configured porter, just build the app:

		$ make 
		

Starting
======

To start the example of the app, just type:

		$ make start
		
An erlang shell must be opened and info message displayed:

		2013-03-04 13:09:54 [porter] up and running at: #Port<?.???>
	
	
Playing with clients:
======

Porter includes some client interfaces, you can use and test how implement yours, for example
let's do it with javascript client.

Go to porter_clients directory, and edit the client.js file, edit the ip address and port where server
porter udp is running, (or 127.0.0.1:2070 by default).
When configuration is ready, execute script with node.js:

		$ node client.js
		Incomming message: connection:keep alive
		
Now a javascript client is connected to the server, let's send a message to it with porter, from erlang
shell type:

		> porter:send_msg(<<"javascript">>, <<"hello javascript">>).
		{ok,{{127,0,0,1},55094}}
		
And the client must be notified:

		Incomming message: hello javascript

NOTE: the first argument on the function porter:send_msg/2 is the client id, it is provided when client connects
to the server for the first time, the first message (client -> server), the second argument is the message for the 
client (binary term).


Next versions:
======

-Must be implement a hang queue when a client is not connected but there is a message in queue.

