local Helper = require( 'lctags.Helper' )
local Json = require( 'lctags.Json' )
local log = require( 'lctags.LogCtrl' )

local StatusServer = {}

--local socket = require( "socket" )

function StatusServer:setup( name, serverFlag, createFlag )
   name = string.gsub( name, "/", "" )
   self.name = name
   if socket then
      if serverFlag then
	 local server = assert(socket.bind("*", 5000))

	 self.client = server:accept()
      else
	 self.client = assert( socket.connect("127.0.0.1", 5000) )
      end
   else
      if serverFlag then
	 Helper.deleteMQueue( self.name .. "requestStatus" )
	 Helper.deleteMQueue( self.name .. "replyStatus" )
      end
      self.requestQueue = Helper.createMQueue(
	 name .. "requestStatus", serverFlag or createFlag )
      self.replyQueue = Helper.createMQueue(
	 name .. "replyStatus", serverFlag or createFlag )
      if not self.requestQueue or not self.replyQueue then
	 print( "failed to mqueue", serverFlag, createFlag )
	 os.exit( 1 )
      end
   end

   if serverFlag then
      self.statusList = {}
   end
   
   log( 1, "StatusServer:setup", serverFlag )
end

function StatusServer:new( name )
   self:setup( name, true )
   while true do
      local txt = self:get()
      local message = Json:convertFrom( txt )
      if not message then
	 Helper.deleteMQueue( self.name .. "requestStatus" )
	 Helper.deleteMQueue( self.name .. "replyStatus" )
	 log( 1, "StatusServer:server error end:", txt )
	 os.exit( 1 )
      end
      
      --log( 3, "StatusServer: command", message.command, message.value )
      self[ message.command ]( self, message.value )
   end
end

function StatusServer:get()
   if socket then
      return self.client:receive( '*l' )
   else
      return self.requestQueue:get()
   end
end

function StatusServer:request( command, value )
   local json = Json:convertTo( { command = command, value = value } )
   if socket then
      self.client:send( json .. "\n" )
   else
      self.requestQueue:put( json )
   end
end

function StatusServer:reply( value )
   local json = Json:convertTo( value )
   if socket then
      self.client:send( json .. "\n" )
   else
      self.replyQueue:put( json )
   end
end

function StatusServer:getReply()
   local txt
   if socket then
      txt = self.client:receive( '*l' )
   else
      txt = self.replyQueue:get()
   end
   if not txt then
      return nil
   end
   local obj = Json:convertFrom( txt )
   return obj
end

function StatusServer:connect( name, createFlag )
   self:setup( name, false, createFlag )
end


function StatusServer:requestEnd()
   self:request( "exit" )
end

function StatusServer:requestUpdateStatus( name, state )
   if not self.endFlag then
      self:request( "updateStatus", { name = name, state = state, time = os.clock() } )
   end
end

function StatusServer:requestEndStatus( name )
   self.endFlag = ture
   self:request( "updateStatus", { name = name, endFlag = true } )
end


function StatusServer:exit()
   self:reply( { endFlag = true } )
   Helper.deleteMQueue( self.name .. "requestStatus" )
   Helper.deleteMQueue( self.name .. "replyStatus" )
   log( 1, "StatusServer:server end" )
   os.exit( 0 )
end

function StatusServer:searchStatus( info )
   local findIndex
   for index, status in ipairs( self.statusList ) do
      if status.name == info.name then
	 return index
      end
   end
   return nil
end

function StatusServer:updateStatus( info )
   local findIndex = self:searchStatus( info )
   if info.endFlag then
      if findIndex then
	 table.remove( self.statusList, findIndex )
      end
      return
   end

   if not findIndex then
      table.insert( self.statusList,
		    { name = info.name, basetime = info.time, info = info } )
      findIndex = #self.statusList
   end
   self.statusList[ findIndex ].info = info
end

function StatusServer:getStatus()
   self:reply( self.statusList )
end

function StatusServer:requestGetStatus()
   self:request( "getStatus" )
   return self:getReply()
end

return StatusServer
