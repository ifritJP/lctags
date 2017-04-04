local Helper = require( 'lctags.Helper' )
local Json = require( 'lctags.Json' )
local log = require( 'lctags.LogCtrl' )
local sqlite3 = require( 'lsqlite3' )

local Server = {}

--local socket = require( "socket" )

function Server:setup( name, serverFlag )
   name = string.gsub( name, "/", "" )
   self.name = name
   self.serverFlag = serverFlag
   if socket then
      if serverFlag then
	 local server = assert(socket.bind("*", 5000))

	 self.client = server:accept()
      else
	 self.client = assert( socket.connect("127.0.0.1", 5000) )
      end
   else
      if serverFlag then
	 self:release()
      end
      self.lockObj = Helper.createLock( name .. "lock" )
      log( 1, "lockObj = ", self.lockObj, name, Helper.createLock )
      self.requestQueue = Helper.createMQueue( name .. "request" )
      self.replyQueue = Helper.createMQueue( name .. "reply" )
      if not self.requestQueue or not self.replyQueue then
	 os.exit( 1 )
      end
   end
   log( 1, "setup", serverFlag )
end

function Server:release()
   if self.serverFlag then
      if not socket then
	 Helper.deleteLock( self.name .. "lock" )
	 Helper.deleteMQueue( self.name .. "request" )
	 Helper.deleteMQueue( self.name .. "reply" )
      end
   end
end

function Server:new( name, db )
   if not db then
      return
   end
   self.db = db.db
   self:setup( name, true )
   while true do
      local messageTxt = self:get()
      local message = Json:convertFrom( messageTxt )
      if message == nil then
	 db:commit()
	 db:close()
	 self:release()
	 log( 1, "server end for illegal message" )
	 os.exit( 0 )
      end
      
      --log( 3, "Server: command", message.command, message.value )
      if type( message ) == "table" and message.command then
	 log( 2, "server command", message.command )
	 self[ message.command ]( self, message.value )
      end
   end
end

function Server:get()
   if socket then
      return self.client:receive( '*l' )
   else
      return self.requestQueue:get()
   end
end

function Server:request( command, value )
   local json = Json:convertTo( { command = command, value = value } )
   if socket then
      self.client:send( json .. "\n" )
   else
      self.requestQueue:put( json )
   end
end

function Server:reply( value )
   local json = Json:convertTo( value )
   if socket then
      self.client:send( json .. "\n" )
   else
      self.replyQueue:put( json )
   end
end

function Server:getReply()
   local txt
   if socket then
      txt = self.client:receive( '*l' )
   else
      txt = self.replyQueue:get()
   end
   local obj = Json:convertFrom( txt )
   return obj
end

function Server:connect( name )
   self:setup( name, false )
end


function Server:requestEnd()
   self:request( "exit" )
end


function Server:requestCommit()
   self:request( "commit" )
end

function Server:commit()
   self.db:commit()
   self.db:begin()
end

function Server:requestExec( stmt )
   self.lockObj:begin()
   self:request( "exec", stmt )
   local reply = self:getReply()
   self.lockObj:fin()
   return reply
end

function Server:requestTest( )
   self:request( "test" )
   return self:getReply()
end

function Server:exit()
   self.db:commit()
   self.db:close()
   self:release()
   log( 1, "server end" )
   os.exit( 0 )
end

function Server:exec( stmt )
   local result = self.db.db:exec( stmt )
   if result ~= sqlite3.OK then
      local err = self.db.db:errmsg()
      if err ~= "not an error" then
	 --log( 2, "Server:exec", err )
	 self:reply( { err = err } )
      else
	 self:reply( {} )
      end
   else
      log( 3, "exec", stmt )
      self:reply( {} )
   end
end

function Server:inq( query )
   local findItem
   local success, message = pcall(
      function()
	 for item in self.db.db:nrows( query ) do
	    self:reply( { item = item } )
	    local contFlag = self:get()
	    if not contFlag then
	       break
	    end
	 end
      end
   )
   if not success then
      self:reply( { err = message } )
   else
      self:reply( { fin = true } )
   end
end

function Server:requestInq( query, func )
   self.lockObj:begin()
   
   self:request( "inq", query )
   while true do
      local info = self:getReply()
      if info.err or info.fin then
	 if info.err then
	    log( 1, "requestInq: err", info.err, query )
	    os.exit( 1) 
	 end
	 self.lockObj:fin()
	 return info
      end
      self.requestQueue:put(
	 func( info.item ) and "true" or "false" )
   end
end



function Server:test()
   print( "test" )
   for index = 1, 10000 do
      local success, message = pcall(
	 function()
	    for item in self.db.db:nrows(
	       "SELECT * FROM etc WHERE keyName = 'projDir' LIMIT 1" ) do
	    end
	 end
      )
   end
   self:reply( {} )
end


return Server
