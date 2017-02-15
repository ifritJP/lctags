-- Copyright (C) 2017 ifritJP

local displayLevel = 1
local prefix = ""
local log = function( level, ... )

   local logLevel = level
   
   if level == nil and ... == nil then
      local info = debug.getinfo( 2 )
      print( info.source, info.currentline )
   end
   if type( level ) ~= "number" then
      logLevel = 3
   end
   if logLevel == 0 then
      displayLevel = ...
      print( "displayLevel", displayLevel )
      return
   elseif logLevel == -1 then
      prefix = ...
      return
   end
   if logLevel > displayLevel then
      return
   end
   if prefix ~= "" then
      print( prefix, logLevel, ... )
   else
      print( logLevel, ... )
   end
end
--[[
log = function( ... )
end
--]]

return log
