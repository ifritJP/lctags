-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )

local gcc = {}

local function processParen( arg, macroParen )
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

function gcc:createCompileOptionConverter( compiler )
   if compiler ~= "gcc" then
      return nil
   end
   local obj = {
      nextType = nil,
      macroParen = 0,
      convert = function( self, arg )
	 if compiler == "gcc" then
	    if self.nextType == "skip" then
	       self.nextType = nil
	       return "skip"
	    elseif self.nextType == "opt" then
	       self.nextType = nil
	       return "opt", arg
	    elseif self.nextType == "macroParen" then
	       self.macroParen = processParen( arg, self.macroParen )
	       if self.macroParen == 0 then
		  self.nextType = nil
	       end
	       return "opt", arg
	    end
	    if string.find( arg, "^-" ) then
	       if string.find( arg, "^-[IDo]" ) then
		  if string.find( arg, "^-D" ) then
		     self.macroParen = processParen( arg, self.macroParen )
		     if self.macroParen > 0 then
			self.nextType = "macroParen"
		     end
		  end
		  if arg == "-I" then
		     self.nextType = "opt"
		  end
		  if arg == "-o" then
		     self.nextType = "skip"
		     return "skip"
		  end
		  return "opt", arg
	       elseif string.find( arg, "-std=", 1, true ) then
		  return "opt", arg
               elseif arg == "-include" then
                  self.nextType = "skip"
                  return "skip"
               end
	       return "skip"
	    end
	    return "src", arg
	 end
      end,
   }
   return obj
end

function gcc:getDefaultOptionList( compiler )
   return {}
end

return gcc
