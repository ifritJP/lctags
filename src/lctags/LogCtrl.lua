-- Copyright (C) 2017 ifritJP

local displayLevel = 1
local prefix = ""
local log = function( level, ... )

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
   end
   if logLevel > displayLevel then
      return
   end
   if prefix ~= "" then
      print( prefix, logLevel, table.unpack( param ) )
   else
      print( logLevel, table.unpack( param ) )
   end
   if logLevel == -2 then
      local debugInfo = debug.getinfo( 2 )
      local debugInfo2 = debug.getinfo( 3 )
      local debugInfo3 = debug.getinfo( 4 )
      local debugInfo4 = debug.getinfo( 5 )
      local debugInfo5 = debug.getinfo( 6 )
      print( debugInfo.short_src, debugInfo.currentline,
	     "\n", debugInfo2.short_src, debugInfo2.currentline,
	     "\n", debugInfo3.short_src, debugInfo3.currentline,
	     "\n", debugInfo4.short_src, debugInfo4.currentline,
	     "\n", debugInfo5.short_src, debugInfo5.currentline )
   end
end
--[[
log = function( ... )
end
--]]

return log
