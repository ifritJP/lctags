-- -*- coding:utf-8; mode:lua -*-

local gcc = require( 'lctags.gcc' )
local log = require( 'lctags.LogCtrl' )

local config = {}

config.conf = gcc

function config:loadConfig( path, exitOnErr )
   local fileHandle = io.open( path, "r" )
   if fileHandle then
      fileHandle:close()
      local chunk, err = loadfile( path )
      if chunk then
	 self.conf = chunk()
	 return self
      end
      print( err )
   end
   if exitOnErr then
      print( "loadfile error", err )
      os.exit( 1 )
   end
   return nil
end


function config:getIgnorePattern()
   if self.conf and self.conf.getIgnorePattern then
      return self.conf:getIgnorePattern()
   end
   return {
      -- { "simple", "ignore.c" }, -- this is simple match. 
      -- { "lua", "^ignore.c$" }, -- this is lua pattern match.
   }
end

--[[
   This method is compile option converter from your compiler to clang.
]]
function config:createCompileOptionConverter( compiler )
   if self.conf and self.conf.createCompileOptionConverter then
      return self.conf:createCompileOptionConverter( compiler )
   end
   return nil
end

function config:getDefaultOptionList( compiler )
   if self.conf and self.conf.getDefaultOptionList then
      return self.conf:getDefaultOptionList( compiler )
   end
   return {}
end

function config:getClangIncPath()
   if self.clangIncPath then
      return self.clangIncPath
   end
   
   if self.conf and self.conf.getClangIncPath then
         local path = self.conf:getClangIncPath()
	 if path and path ~= "" then
	    self.clangIncPath = string.gsub( path, "/$", "" )
	    return self.clangIncPath
	 end
   end

   local clangVer = require( 'libclanglua.if' ).getClangVersion()
   clangVer3 = string.gsub(
      clangVer, "^clang version (%d+)%.(%d+)%.(%d+)[^%d].*", "%1.%2.%3" )
   clangVer2 = string.gsub( clangVer3, "^(%d+)%.(%d+)[^%d].*", "%1.%2" )


   self.clangIncPath = string.format( "/usr/lib/llvm-%s/lib/clang/%s/include",
				      clangVer2, clangVer3 )
   return self.clangIncPath
end


function config:getClangIncPathOp()
   if not self.clangIncPathOp then
      local path = self:getClangIncPath()
      if not path then
	 self.clangIncPathOp = ""
      else
	 self.clangIncPathOp = string.format( "-I%s", path )
      end
   end
   return self.clangIncPathOp
end

return config
