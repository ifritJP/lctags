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
   elseif query:find( "P" ) then
      db:mapFile(
	 target and string.format( "path like '%%%s%%'", target ),
	 function( item )
	    self:printLocate( db, "path", item.id, 1, absFlag, false )
	    return true
	 end
      )
   elseif query:find( "c" ) then
      db:mapSimpleName(
	 target and string.format( "name like '%s%%'", target ),
	 function( item )
	    print( item.name )
	    return true
	 end
      )
   elseif query:find( "r" ) then
      if not target then
	 return false
      end
      db:mapSymbolRefInfoList(
	 target,
	 function( item )
	    self:printLocate( db, target, item.fileId, item.line, absFlag, true )
	    return true
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
	    return true
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
	    return true
	 end
      )
   end
end

function Query:exec( dbPath, query, target, useGlogalFlag )
   local db = dbPath and DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   if not db then
      if not useGlogalFlag then
	 log( 1, "db open error" )
	 os.exit( 1 )
      end
      local commad = string.format( "global %s %s", query, target or "" )
      local success, endType, code = os.execute( commad )
      if success then
	 os.exit( 0 )
      else
	 os.exit( code )
      end
   end

   self:execWithDb( db, query, target )

   db:close()

   return true
end

return Query
