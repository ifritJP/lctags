-- Copyright (C) 2017 ifritJP

local sqlite3 = require("lsqlite3")
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local Option = require( 'lctags.Option' )

local DBAccess = {}

local recordFile = nil
   
function DBAccess:errorExit( level, ... )
   log( -2, "Sqlite ERROR:", self.db:errmsg(), ... )
   os.exit( 1 )
end

function DBAccess:open( path, readonly, onMemoryFlag )
   recordFile = Option:getRecordSqlObj()
   
   log(3, "DBAccess:open" )
   local flag = nil
   if readonly then
      flag = sqlite3.OPEN_READONLY
   else
      flag = sqlite3.OPEN_READWRITE + sqlite3.OPEN_CREATE
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
      path = path,
      readonly = readonly,
      insertCount = 0,
      updateCount = 0,
      deleteCount = 0,
      selectCount = 0,
      uniqueCount = 0,
      lockLogFlag = true,
      beginFlag = false,
      time = 0,
      beginTime = 0,
      lockObj = Helper.createLock(),
   }
   setmetatable( obj, { __index = DBAccess } )

   db:busy_handler(
      function()
	 if obj.lockLogFlag then
	    obj.lockLogFlag = false
	    log( 2, "db is busy" )
	 end
	 obj.lockObj:begin()
	 obj.lockObj:fin()
	 return true
      end
   )

   return obj
end

function DBAccess:close()
   self.db:close()
   log( 2,
	string.format(
	   "time=%f, insert=%d, unique=%d, update=%d, delete=%d, select=%d",
	   self.time, self.insertCount, self.uniqueCount,
	   self.updateCount, self.deleteCount, self.selectCount ) )
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
		  self:errorExit( 3, "func returned nil" )
	       end
	       break
	    end
	 end
      end, { ... }
   )
   if not success then
      self:errorExit( 3, message, query )
   end
end

function DBAccess:exec( stmt, errHandle )
   local prev = os.clock()
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

   self.time = self.time + (os.clock() - prev)
end

function DBAccess:outputLog( message )
   local fileObj = io.open( self.path .. ".log", "a+" )
   local sec, usec = Helper.getTime()
   local logPrefix = log( -3 ) or ""
   fileObj:write( string.format( "%d.%06d %s %s\n",
				 sec % 1000, usec, logPrefix, message ) )
   fileObj:close()
end   

function DBAccess:begin( message )
   if self.readonly then
      log( 1, "db mode is read only" )
      os.exit( 1 )
      return
   end

   self.lockObj:begin()

   self.beginFlag = true
   self.lockLogFlag = true
   --self:commit()
   self:exec( "PRAGMA journal_mode = MEMORY" )
   self:exec( "BEGIN IMMEDIATE" )

   if message and Option:isValidLockLog() then
      self.lockLogMessage = message
      self:outputLog( message )
   end

   self.beginTime = os.clock()
   
   log( 2, "begin" )
end

function DBAccess:commit()
   if self.readonly then
      return
   end
   if not self.beginFlag then
      return
   end
   self.beginFlag = false
      
   
   self:exec(
      "COMMIT",
      function( db, stmt, message )
	 if not message:find( "no transaction is active" ) then
	    self:errorExit( 5, message )
	 end
      end
   )
   if self.lockLogMessage then
      self:outputLog( "commit" )
      self:outputLog(
	 string.format(
	    "time=%f, insert=%d, unique=%d, update=%d, delete=%d, select=%d",
	    self.time, self.insertCount, self.uniqueCount,
	    self.updateCount, self.deleteCount, self.selectCount ) )
   end
   
   self.lockObj:fin()
   log( 2, "commit:", os.clock() - self.beginTime )
end

function DBAccess:insert( tableName, values )
   self.insertCount = self.insertCount + 1
   self:exec(
      string.format( "INSERT INTO %s VALUES ( %s );", tableName, values ),
      function( db, stmt, message )
	 if not message:find( "UNIQUE constraint failed" ) then
	    self:errorExit( 5, message )
	 else
	    self.uniqueCount = self.uniqueCount + 1
	 end
      end
   )
end

function DBAccess:update( tableName, set, condition )
   self.updateCount = self.updateCount + 1
   local sql = string.format( "UPDATE %s SET %s", tableName, set )
   if condition then
      sql = string.format( "%s WHERE %s", sql, condition )
   end
   self:exec( sql )
end

function DBAccess:delete( tableName, condition )
   self.deleteCount = self.deleteCount + 1
   log( 2, "delete:", tableName, condition )
   self:exec( string.format( "DELETE FROM %s WHERE %s", tableName, condition ) )
end

function DBAccess:getRowNumber( tableName, condition )
   self.selectCount = self.selectCount + 1

   local number = 0
   local sql = "SELECT COUNT(*) FROM " .. tableName
   self:mapRowList( tableName, condition, 1, "COUNT(*)",
		    function( item )
		       number = item[ "COUNT(*)" ]
		       return false
		    end
   )
   return number
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
