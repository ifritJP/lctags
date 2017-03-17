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
function config:convertCompileOption( compiler )
   if compiler ~= "armcc" then
      return nil
   end
   local obj = {
      macroParen = 0,
      nextType = nil,
      convert = function( self, arg )
	 if compiler == "armcc" then
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
	       if string.find( arg, "^-[JDoI]" ) then
		  if string.find( arg, "^-D" ) then
		     self.macroParen = processParen( arg, self.macroParen )
		     if self.macroParen > 0 then
			self.nextType = "macroParen"
		     end
		  end
		  if arg == "-J" then
		     self.nextType = "opt"
		  end
		  if arg == "-o" then
		     self.nextType = "skip"
		     return "skip"
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
      end,
   }
   return obj
end

function config:getDefaultOptionList( compiler )
   return {}
end

return config
