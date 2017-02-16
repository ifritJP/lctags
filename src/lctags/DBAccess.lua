-- Copyright (C) 2017 ifritJP

local sqlite3 = require("lsqlite3")
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )

local DBAccess = {}

function DBAccess:errorExit( level, ... )
   local debugInfo = debug.getinfo( level )
   log( 1, "Sqlite ERROR:        ", self.db:errmsg(),
	debugInfo.short_src, debugInfo.currentline, ... )
   os.exit()
end

function DBAccess:open( path, readonly )
   local flag = nil
   if readonly then
      flag = sqlite3.OPEN_READONLY
   end
   local db = sqlite3.open( path, flag )
   if not db then
      return nil
   end
   
   --local db = sqlite3.open_memory()
   local obj = {
      db = db,
   }
   setmetatable( obj, { __index = DBAccess } )
   return obj
end

function DBAccess:close()
   self.db:close()
end

function DBAccess:mapRowList( tableName, condition, limit, attrib, func, ... )
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
	    func( item, table.unpack( params ) )
	 end
      end, { ... }
   )
   if not success then
      self:errorExit( 3, message )
   end
end

function DBAccess:exec( stmt )
   self:execLow( stmt )
end
   
function DBAccess:execLow( stmt, errHandle )
   if self.db:exec( stmt ) ~= sqlite3.OK then
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
   self:execLow( "BEGIN IMMEDIATE" )
   log( 2, "begin" )
end

function DBAccess:commit()
   --self:execLow( [[ UPDATE lock SET locked = 0; ]] )
   self:execLow(
      "COMMIT",
      function( db, stmt, message )
	 if not message:find( "no transaction is active" ) then
	    self:errorExit( 5, message )
	 end
      end
   )
end

function DBAccess:insert( tableName, values )
   self:execLow(
      string.format( "INSERT INTO %s VALUES ( %s );", tableName, values ),
      function( db, stmt, message )
	 if not message:find( "UNIQUE constraint failed" ) then
	    self:errorExit( 5, message )
	 end
      end
   )
end

function DBAccess:createTables( sqlTxt )
   self:execLow(
      sqlTxt,
      function( db, stmt, message )
	 if not message:find( "already exists" ) then
	    print( message )
	 end
      end
   )
end

return DBAccess
