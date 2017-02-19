-- Copyright (C) 2017 ifritJP

local sqlite3 = require("lsqlite3")
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )

local DBAccess = {}

local recordFile = nil
function DBAccess:recordSql( fileHandle )
   recordFile = fileHandle
end
   
function DBAccess:errorExit( level, ... )
   local debugInfo = debug.getinfo( level )
   log( 1, "Sqlite ERROR:        ", self.db:errmsg(),
	debugInfo.short_src, debugInfo.currentline, ... )
   os.exit()
end

function DBAccess:open( path, readonly, onMemoryFlag )
   local flag = nil
   if readonly then
      flag = sqlite3.OPEN_READONLY
   end
   local db
   if onMemoryFlag then
      db = sqlite3.open_memory()
   else
      db = sqlite3.open( path, flag )
   end
   if not db then
      log( 1, "open error." )
      return nil
   end

   local obj = {
      db = db,
      insertCount = 0,
      updateCount = 0,
      deleteCount = 0,
      selectCount = 0,
   }
   setmetatable( obj, { __index = DBAccess } )

   if not readonly then
      obj:exec( "PRAGMA journal_mode = MEMORY" )
   end
   
   return obj
end

function DBAccess:close()
   self.db:close()
   log( 2, string.format(
	   "insert:%d, update:%d, delete:%d, select:%d",
	   self.insertCount, self.updateCount, self.deleteCount, self.selectCount ) )
end

function DBAccess:mapRowList( tableName, condition, limit, attrib, func, ... )
   self.selectCount = self.selectCount + 1
   local query = nil
   if not attrib then
      attrib = "*"
   end
   if condition then
      query = string.format( "SELECT %s FROM %s WHERE %s", attrib, tableName, condition )
   else
      query = string.format( "SELECT %s FROM %s", attrib, tableName )
   end
   if limit then
      query = string.format( "%s LIMIT %d", query, limit )
   end

   local success, message = pcall(
      function( params )
	 for item in self.db:nrows( query ) do
	    local continue = func( item, table.unpack( params ) )
	    if not continue then
	       if continue == nil then
		  errorExit( 3, "func returned nil" )
	       end
	       break
	    end
	 end
      end, { ... }
   )
   if not success then
      self:errorExit( 3, message )
   end
end

function DBAccess:exec( stmt, errHandle )
   if recordFile then
      recordFile:write( stmt .. "\n" )
   end
   
   if self.db:exec( stmt ) ~= sqlite3.OK then
      if recordFile then
	 recordFile:write( self.db:errmsg() .. "\n" )
      end
      if errHandle then
	 errHandle( self, stmt, self.db:errmsg() )
	 
	 -- local debugInfo = debug.getinfo( 2 )
	 -- log( 1, "Sqlite ERROR:        ", self.db:errmsg(),
	 --      debugInfo.short_src, debugInfo.currentline )
      else
	 self:errorExit( 3, stmt )
      end
  end
end

function DBAccess:begin()
   self.db:busy_handler(
      function()
	 Helper.msleep( 100 )
	 return true
      end
   )
   --self:commit()
   self:exec( "BEGIN IMMEDIATE" )
   log( 2, "begin" )
end

function DBAccess:commit()
   --self:exec( [[ UPDATE lock SET locked = 0; ]] )
   self:exec(
      "COMMIT",
      function( db, stmt, message )
	 if not message:find( "no transaction is active" ) then
	    self:errorExit( 5, message )
	 end
      end
   )
end

function DBAccess:insert( tableName, values )
   self.insertCount = self.insertCount + 1
   self:exec(
      string.format( "INSERT INTO %s VALUES ( %s );", tableName, values ),
      function( db, stmt, message )
	 if not message:find( "UNIQUE constraint failed" ) then
	    self:errorExit( 5, message )
	 end
      end
   )
end

function DBAccess:update( tableName, set, condition )
   self.updateCount = self.updateCount + 1
   self:exec( string.format( "UPDATE %s SET %s WHERE %s",
				tableName, set, condition ) )
end

function DBAccess:delete( tableName, condition )
   self.deleteCount = self.deleteCount + 1
   log( 2, "delete:", tableName, condition )
   self:exec( string.format( "DELETE FROM %s WHERE %s", tableName, condition ) )
end

function DBAccess:createTables( sqlTxt )
   self:exec(
      sqlTxt,
      function( db, stmt, message )
	 if not message:find( "already exists" ) then
	    print( message )
	 end
      end
   )
end

return DBAccess
