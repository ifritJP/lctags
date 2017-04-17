-- Copyright (C) 2017 ifritJP

local sqlite3 = require("lsqlite3")
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local Option = require( 'lctags.Option' )
local Server = require( 'lctags.Server' )

local DBAccess = {}

local recordFile = nil
   
function DBAccess:errorExit( level, ... )
   log( -2, "Sqlite ERROR:", self.db:errmsg(), ... )
   os.exit( 1 )
end

function DBAccess:setRecordSqlObj( obj )
   recordFile = obj
end

function DBAccess:open( path, readonly, onMemoryFlag )
   local flag = nil
   if readonly then
      flag = sqlite3.OPEN_READONLY
   else
      flag = sqlite3.OPEN_READWRITE + sqlite3.OPEN_CREATE
   end
   log(3, "DBAccess:open", flag )

   local transLockObj = Helper.createLock()
   -- transLockObj:begin()
   local db
   if onMemoryFlag then
      db = sqlite3.open_memory()
   else
      db = sqlite3.open( path, flag )
   end
   -- transLockObj:fin()
   
   
   if not db then
      log( 1, "open error." )
      return nil
   end

   local server
   if not readonly and Option:isValidService() then
      server = Server
      server:connect( string.gsub( path, "/", "" ) )
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
      server = server,
      inLockFlag = nil,
      inActLockFlag = nil,
      actDepth = 0,
      transLockObj = transLockObj,
      --actLockObj = Helper.createLock("act"),
      writeAccessFlag = false,
      lockCount = 0,
   }
   --obj.actLockObj = obj.transLockObj
   setmetatable(
      obj,
      {
	 __index = DBAccess,
	 __gc = function()
	    if obj.inLockFlag then
	       transLockObj:fin()
	    end
	 end,
      }
   )

   --db:busy_timeout( 0 )

   db:busy_handler(
      function()
	 -- 更新アクセスと読み込みアクセスがバッティングすると busy になる。
	 -- 更新アクセス同士は、transLockObj で排他している。
	 -- 読み込みアクセスのみの場合、 busy にならないはず。
	 if obj.lockLogFlag then
	    obj.lockLogFlag = false
	    log( 2, "db is busy", obj.readonly, obj.writeAccessFlag,
		 obj.beginFlag, obj.inLockFlag, obj.inActLockFlag, obj.actDepth )
	 end
	 if not obj.inLockFlag then
	    -- 更新アクセスを優先し、読み込みアクセスは遅延させる。
	    -- 更新アクセスを止めると、更新処理が溜っていって並列性が下がるため。
	    Helper.msleep( 10 )
	    obj.lockCount = obj.lockCount + 1
	    -- obj:outputLog( "read busy " .. tostring( obj.transLockObj:isLocking() ) )
	    -- obj.transLockObj:begin()
	    -- obj:outputLog( "db is read busy" )
	    -- log( 2, "db is read busy",
	    -- 	 obj.readonly, obj.writeAccessFlag, obj.inActLockFlag, obj.actDepth )
	    -- obj.transLockObj:fin()
	 elseif not obj.beginFlag then
	    Helper.msleep( 10 )
	    obj.lockCount = obj.lockCount + 1
	 end
	 return true
      end
   )

   log( 3, "open", obj.server )

   return obj
end

function DBAccess:act( func, ... )
   local result
   --self.actLockObj:begin()
   self.inActLockFlag = "act"
   self.actDepth = self.actDepth + 1
   result = { func( self.db, ... ) }
   self.actDepth = self.actDepth - 1 
   self.inActLockFlag = nil
   --self.actLockObj:fin()
   return table.unpack( result )
end

function DBAccess:close()
   -- self.transLockObj:begin()
   -- self.inLockFlag = "close"
   self.db:close()
   -- self.inLockFlag = nil
   -- self.transLockObj:fin()

   if not self.readonly then
      log( 2,
	   string.format(
	      "time=%f, lock = %d, insert=%d, unique=%d, update=%d, delete=%d, select=%d",
	      self.time, self.lockCount, self.insertCount, self.uniqueCount,
	      self.updateCount, self.deleteCount, self.selectCount ) )
   end
end


function DBAccess:mapJoin( tableName, otherTable, on, condition, limit, attrib, func )
   local query = nil
   if not attrib then
      attrib = "*"
   end
   if condition then
      query = string.format( "SELECT %s FROM %s INNER JOIN %s ON %s WHERE %s",
			     attrib, tableName, otherTable, on, condition )
   else
      query = string.format( "SELECT %s FROM %s INNER JOIN %s ON %s",
			     attrib, tableName, otherTable, on )
   end
   if limit then
      query = string.format( "%s LIMIT %d", query, limit )
   end

   self:mapQuery( query, func )
end


function DBAccess:mapRowList( tableName, condition, limit, attrib, func )
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

   self:mapQuery( query, func )
end



function DBAccess:mapQuery( query, func )
   self.selectCount = self.selectCount + 1

   self.writeAccessFlag = false

   local success, message
   if self.server then
      local err
      local result = self.server:requestInq(
	 query,
	 function( item )
	    local continue = func( item )
	    if not continue then
	       if continue == nil then
		  err = true
		  return false
	       end
	    end
	 end
      )
      if err then
	 self:errorExit( 3, "func returned nil" )
      end
      if result.err then
	 success = false
	 message = result.err
      else
	 success = true
      end
   else
      while true do
	 success, message = pcall(
	    function()
	       -- 次の for 分を実行する。
	       -- for item in self.db:nrows( query )
	       -- self:act で排他をかけるために、 for in の文法を分解してコールする
	       local loopFunc, param, prev = self:act( self.db.nrows, query )
	       if loopFunc then
		  while true do
		     --self.actLockObj:begin()
		     self.actDepth = self.actDepth + 1
		     self.inActLockFlag = "inq"
		     local item = loopFunc( param, prev )
		     self.inActLockFlag = nil
		     self.actDepth = self.actDepth - 1		  
		     --self.actLockObj:fin()
		     if not item then
			break
		     end
		     local continue = func( item )
		     if not continue then
			if continue == nil then
			   self:errorExit( 3, "func returned nil" )
			end
			break
		     end
		  end
	       end
	    end
	 )
	 if success then
	    break
	 end
	 if not string.find( message, " is locked", 1, true ) then
	    log( 1, message )
	    break
	 end
      end
   end
   if not success then
      self:errorExit( 3, message, query )
   end
end



function DBAccess:exec( stmt, errHandle )
   local prev = os.clock()
   if recordFile then
      recordFile:write( stmt .. "\n" )
   end

   self.writeAccessFlag = true

   local errmsg
   if self.server then
      local reply = self.server:requestExec( stmt )
      errmsg = reply.err
   else
      if self:act( self.db.exec, stmt ) ~= sqlite3.OK then
	 errmsg = self:act( self.db.errmsg )
      end
   end
   if errmsg then
      if recordFile then
	 recordFile:write( errmsg .. "\n" )
      end
      if errHandle then
	 errHandle( self, stmt, errmsg )
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
   fileObj:write( string.format( "%d.%06d	%s	%s\n",
				 sec % 10000, usec, logPrefix, message ) )
   fileObj:close()
end   

function DBAccess:begin( message )
   log( 2, "begin", message )
   if self.readonly then
      log( 1, "db mode is read only" )
      os.exit( 1 )
      return
   end

   -- self.transLockObj:begin()
   -- self.inLockFlag = "begin"
   
   self.beginFlag = true
   self.lockLogFlag = true

   if not self.server then
      self:exec( "PRAGMA journal_mode = MEMORY" )
      self:exec( "PRAGMA synchronous = OFF" )
      self:exec( "BEGIN IMMEDIATE" )
      --self:exec( "BEGIN EXCLUSIVE" )
   end

   if message and Option:isValidLockLog() then
      self.lockLogMessage = message
      self:outputLog( message )
   end

   self.beginTime = os.clock()
   log( 2, "beginLock", os.date() )
end

function DBAccess:commit()
   if self.readonly then
      return
   end
   if not self.beginFlag then
      return
   end
   self.beginFlag = false

   local startTime = Helper.getTime( true )
   log( 2, "commit: start", os.date() )
   
   if not self.server then
      self:exec(
	 "COMMIT",
	 function( db, stmt, message )
	    if not message:find( "no transaction is active" ) then
	       self:errorExit( 5, message )
	    end
	 end
      )
   else
      self.server:requestCommit()
   end
   if self.lockLogMessage then
      self:outputLog(
	 string.format(
	    "commit time=%f, insert=%d, unique=%d, update=%d, delete=%d, select=%d",
	    self.time, self.insertCount, self.uniqueCount,
	    self.updateCount, self.deleteCount, self.selectCount ) )
   end


   -- self.inLockFlag = nil
   -- self.transLockObj:fin()

   log( 2, "commit: end", Helper.getTime( true ) - startTime, os.date() )
end

function DBAccess:insert( tableName, values )
   self.insertCount = self.insertCount + 1
   self:exec(
      string.format( "INSERT INTO %s VALUES ( %s );", tableName, values ),
      function( db, stmt, message )
	 if not message:find( "UNIQUE constraint failed" ) and
	    not message:find( " not unique" )
	 then
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
