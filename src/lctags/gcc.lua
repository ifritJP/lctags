-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local gcc = {}

local nextType = nil
function gcc:convertCompileOption( compiler, arg )
   if compiler == "gcc" then
      if nextType == "skip" then
	 nextType = nil
	 return "skip"
      elseif nextType == "opt" then
	 nextType = nil
	 return "opt", arg
      end
      if string.find( arg, "^-" ) then
	 if string.find( arg, "^-[IDo]" ) then
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

return gcc
