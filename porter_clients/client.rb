###----------------------------------------------------------------------
###
### This module implements a ruby client that connects to the socket 
### server for send & receive messages from/to it
###
### Created by : Jorge Garrido <jorge.garrido@morelosoft.com> [zgb]
###----------------------------------------------------------------------

require 'socket.so'
class UDPClient
  def initialize(host, port)
    @host = host
    @port = port
  end

  def connect
    @socket = UDPSocket.open
    @socket.send("ruby", 0, @host, @port)
    while true
      packet, client = @socket.recvfrom(1024)
      puts('Incomming message: ' + packet)
    end
  end
end
client = UDPClient.new("127.0.0.1", 2070)
client.connect
