-- Copyright (C) 2017 ifritJP

local Helper = require( 'lctags.Helper' )

local displayLevel = 1
local prefix = "lctags:"

local LogCtrl = {}

LogCtrl.lastMessage = ""

function LogCtrl:log( level, ... )
   local logLevel = level
   local param = { ... }
   
   if level == nil and ... == nil then
      local info = debug.getinfo( 2 )
      print( info.source, info.currentline )
   end
   if type( level ) ~= "number" then
      logLevel = 3
      table.insert( param, 1, level )
   end
   if logLevel == 0 then
      local prev = displayLevel
      displayLevel = ...
      if displayLevel < 0 then
	 displayLevel = prev
      end
      return prev
   elseif logLevel == -1 then
      prefix = ...
      return
   elseif logLevel == -3 then
      return prefix
   elseif logLevel == -4 then
      return displayLevel
   end
   if logLevel > displayLevel then
      if logLevel <= 2 and self.server then
	 local message = ""
	 for index, val in ipairs( table.pack( ... ) ) do
	    message = message .. tostring( val ) .. "\t"
	 end
	 self.lastMessage = message
	 self.server:requestUpdateStatus( self.statusName, message )
      end
      return
   end
   if #param == 1 and type( param[ 1 ] ) == "function" then
      self:raw( logLevel, param[ 1 ]() )
   else
      self:raw( logLevel, table.unpack( param ) )
   end
end

function LogCtrl:getLastMessage()
   return self.lastMessage
end

function LogCtrl:raw( logLevel, ... )
   if logLevel > displayLevel then
      return
   end
   if prefix ~= "" then
      print( prefix, logLevel, ... )
   else
      print( logLevel, ... )
   end
   if logLevel <= 2 and self.server then
      local message = ""
      for index, val in ipairs( table.pack( ... ) ) do
	 message = message .. "\t" .. tostring( val )
      end
      self.server:requestUpdateStatus( self.statusName, message )
   end
   if logLevel == -2 then
      local debugInfo = debug.getinfo( 2 )
      local debugInfo2 = debug.getinfo( 3 )
      local debugInfo3 = debug.getinfo( 4 )
      local debugInfo4 = debug.getinfo( 5 )
      local debugInfo5 = debug.getinfo( 6 )
      local debugInfo6 = debug.getinfo( 7 )
      local debugInfo7 = debug.getinfo( 8 )
      print( debugInfo.short_src, debugInfo.currentline,
	     "\n", debugInfo2.short_src, debugInfo2.currentline,
	     "\n", debugInfo3.short_src, debugInfo3.currentline,
	     "\n", debugInfo4.short_src, debugInfo4.currentline,
	     "\n", debugInfo5.short_src, debugInfo5.currentline,
	     "\n", debugInfo6.short_src, debugInfo6.currentline,
	     "\n", debugInfo7.short_src, debugInfo7.currentline )
   end
end

function LogCtrl:calcTime( id, param )
   local nowTime = Helper.getTime( true )
   if self.prevTime then
      self:log( 2, "calcTime:", self.prevTimeId, self.prevTimeParam, nowTime - self.prevTime )
   end
   self:log( 2, "calcTime:", "start", self.prevTimeId, self.prevTimeParam )
   self.prevTime = nowTime
   self.prevTimeId = id
   self.prevTimeParam = param
end


function LogCtrl:setStatusServer( server, name )
   self.server = server
   self.statusName = name
end

function LogCtrl:openDB( readonly )
   if self.server then
      self.server:requestNotifyOpenClose( true, readonly )
   end
end

function LogCtrl:closeDB( readonly )
   if self.server then
      self.server:requestNotifyOpenClose( false, readonly )
   end
end


setmetatable( LogCtrl, { __call = function( func, ... ) return LogCtrl:log( ... ) end  } )

return LogCtrl
