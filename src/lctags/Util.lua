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
   if fileInfo.path == "" then
      log( 2, "skip system file" )
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



return Util

