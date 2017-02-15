-- Copyright (C) 2017 ifritJP


local log = require( 'lctags.LogCtrl' )
local DBCtrl = require( 'lctags.DBCtrl' )

local Query = {}

function Query:getFileLineText( filePath, line )
   if line < 0 then
      return ""
   end
   local handle = io.open( filePath, "r" )
   if not handle then
      return "<not found>"
   end
   local lineNo = 1
   repeat
      local text = handle:read( '*l' )
      if lineNo == line then
	 return text
      end
      lineNo = lineNo + 1
   until not text
   return ""
end

function Query:printLocate( db, symbol, fileId, line, absFlag, printLine )
   local fileInfo = db:getFileInfo( fileId )
   local baseDir = absFlag and "" or os.getenv( "PWD" )
   local path = db:getSystemPath( fileInfo.path, baseDir )
   -- GNU globalフォーマット
   print( string.format( "%-16s %4d %-16s %s", symbol, line, path,
			 printLine and self:getFileLineText( path, line ) or "" ) )
end

function Query:execWithDb( db, query, target )
   local absFlag = query:find( "a" )
   if query:find( "dump" ) then
      db:dump( 1 )
   elseif query:find( "r" ) then
      if not target then
	 return false
      end
      db:mapSymbolRefInfoList(
	 target,
	 function( item )
	    self:printLocate( db, target, item.fileId, item.line, absFlag, true )
	 end
      )
   elseif query:find( "t" ) then
      if not target then
	 return false
      end
      db:mapDeclInfoList(
	 target,
	 function( item )
	    self:printLocate( db, target, item.fileId, item.line, absFlag, true )
	 end
      )
   else
      if not target then
	 return false
      end
      db:mapDeclInfoList(
	 target,
	 function( item )
	    self:printLocate( db, target, item.fileId, item.line, absFlag, true )
	 end
      )
   end
end

function Query:exec( dbPath, query, target )
   local db = DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end

   self:execWithDb( db, query, target )

   db:close()

   return true
end

return Query
