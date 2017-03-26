local Helper = require( 'lctags.Helper' )
local Option = require( 'lctags.Option' )
local log = require( 'lctags.LogCtrl' )

local Util = {}

function Util:getToken( filePath, line, column, fileContents )
   local txt = self:getFileLineText( filePath, line, fileContents )
   local front = string.sub( txt, 1, column )
   local tail = string.sub( txt, column + 1 )
   local num
   front, num = string.gsub( front, "^.*[^%w_]+([%w_]+)$", "%1" )
   if num == 0 then
      front = ""
   end
   tail, num = string.gsub( tail, "^([%w_]+)[^%w_]+.*$", "%1" )
   if num == 0 then
      tail = ""
   end
   return front .. tail
end


function Util:getFileLineText( filePath, line, fileContents )
   if line < 0 then
      return ""
   end
   local handle = io.open( filePath, "r" )
   if not handle then
      return "<not found>"
   end
   local lineNo = 1
   if fileContents then
      for text in string.gmatch( fileContents, "[^\n]*\n" ) do
	 if lineNo == line then
	    return string.sub( text, 1, #text - 1 )
	 end
	 lineNo = lineNo + 1
      end
   else
      repeat
	 local text = handle:read( '*l' )
	 if lineNo == line then
	    return text
	 end
	 lineNo = lineNo + 1
      until not text
   end
   return ""
end


function Util:printLocate(
      db, symbol, fileId, line, absFlag, printLine, fileContents )
   local fileInfo = db:getFileInfo( fileId )
   if fileInfo then
      if fileInfo.path == "" then
	 log( 2, "skip system file" )
	 return
      end
   else
	 log( 2, "unknown fileId", fileId )
	 return
   end
   
   local baseDir = absFlag and "" or os.getenv( "PWD" )
   local path = db:getSystemPath( fileInfo.path, baseDir )
   self:printLocateDirect( io.stdout, symbol, path, line, printLine, fileContents )
end

function Util:printLocateDirect(
      outputHandle, symbol, path, line, printLine, fileContents )
   if symbol == "" then
      symbol = "none"
   end
   -- GNU globalフォーマット
   outputHandle:write(
      string.format(
	 "%-16s %4d %-16s %s\n", symbol, line, path,
	 printLine and Util:getFileLineText( path, line, fileContents ) or "" ) )
end

function Util:calcFileDigest( path )
   if path then
   end
   local fileObj = io.open( path, "r" )
   if not fileObj then
      log( 1, "file open error", path )
      return nil
   end
   local digest = Helper.openDigest( "md5" )
   while true do
      local txt = fileObj:read( 1000 * 10 )
      if not txt then
	 return digest:fix()
      end
      digest:write( txt )
   end
end

function Util:calcTextDigest( text )
   local digest = Helper.openDigest( "md5" )
   digest:write( text )
   return digest:fix()
end

function Util:profile( func, path )
   if not Option:isValidProfile() then
      return func()
   end

   local profiler = require( 'ProFi' )
   profiler:start()
   
   local result = func()
   
   profiler:stop()
   profiler:writeReport( path )

   return result
end

function Util:mkdirWithParent( path )
   local dir = ""
   for name in string.gmatch( path, "[^/]+" ) do
      dir = dir .. "/" .. name 
      local result = Helper.mkdir( dir )
      if result ~= 0 and result ~= "EEXIST" then
	 log( 1, "mkdirWithParent", dir, result )
	 return
      end
   end
end

return Util

