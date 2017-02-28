-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )

local gcc = {}

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

local nextType = nil
local macroParen = 0
function gcc:convertCompileOption( compiler, arg )
   if compiler == "gcc" then
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
	 if string.find( arg, "^-[IDo]" ) then
	    if string.find( arg, "^-D" ) then
	       macroParen = processParen( arg, macroParen )
	       if macroParen > 0 then
		  nextType = "macroParen"
	       end
	    end
	    if arg == "-I" or arg == "-o" then
	       nextType = "opt"
	    end
	    return "opt", arg
	 end
	 return "skip"
      end
      return "src", arg
   end
end

function gcc:getDefaultOptionList( compiler )
   return {}
end

return gcc
