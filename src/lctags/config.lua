-- -*- coding:utf-8; mode:lua -*-

local log = require( 'lctags.LogCtrl' )

local config = {}

function config:getIgnorePattern()
   return {
      -- { "simple", "ignore.c" }, -- this is simple match. 
      -- { "lua", "^ignore.c$" }, -- this is lua pattern match.
   }
end

function processParen( arg, macroParen )
   for paren in string.gmatch( arg, "[()]" ) do
      if paren == "(" then
	 macroParen = macroParen + 1
      else
	 macroParen = macroParen - 1
	 if macroParen < 0 then
	    log( 1, "unmatch arg paren", arg )
	    os.exit( 1 )
	 end
      end
   end
   return macroParen
end


--[[
   This method is compile option converter from your compiler to clang.
   This is sample for armcc.
]]
local macroParen = 0
local nextType = nil
function config:convertCompileOption( compiler, arg )
   if compiler == "armcc" then
      if nextType == "skip" then
	 nextType = nil
	 return "skip"
      elseif nextType == "opt" then
	 nextType = nil
	 return "opt", arg
      elseif nextType == "macroParen" then
	 macroParen = processParen( arg, macroParen )
	 if macroParen == 0 then
	    nextType = nil
	 end
	 return "opt", arg
      end
      if string.find( arg, "^-" ) then
	 if string.find( arg, "^-[JDo]" ) then
	    if string.find( arg, "^-D" ) then
	       macroParen = processParen( arg, macroParen )
	       if macroParen > 0 then
		  nextType = "macroParen"
	       end
	    end
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

function config:getDefaultOptionList( compiler )
   return {}
end

return config
