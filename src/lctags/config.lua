-- -*- coding:utf-8; mode:lua -*-

local config = {}

function config:getIgnorePattern()
   return {
      -- { "simple", "ignore.c" }, -- this is simple match. 
      -- { "lua", "^ignore.c$" }, -- this is lua pattern match.
   }
end


--[[
   This method is compile option converter from your compiler to clang.
   This is sample for armcc.
]]
local nextType = nil
function config:convertCompileOption( compiler, arg )
   if compiler == "armcc" then
      if nextType == "skip" then
	 nextType = nil
	 return "skip"
      elseif nextType == "opt" then
	 nextType = nil
	 return "opt", arg
      end
      if string.find( arg, "^-" ) then
	 if string.find( arg, "^-[JDo]" ) then
	    if arg == "-J" or arg == "-o" then
	       nextType = "opt"
	    end
	    if string.find( arg, "^-J" ) then
	       arg = string.gsub( arg, "^-J", "-I" )
	    end
	    return "opt", arg
	 end
	 return "skip"
      end
      return "src", arg
   end
end

return config
