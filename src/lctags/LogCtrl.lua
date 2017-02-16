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
      displayLevel = ...
      return
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
end
--[[
log = function( ... )
end
--]]

return log
