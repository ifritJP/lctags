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
   if fileInfo.path == "" then
      log( 2, "skip system file" )
      return
   end
   
   local baseDir = absFlag and "" or os.getenv( "PWD" )
   local path = db:getSystemPath( fileInfo.path, baseDir )
   self:printLocateDirect( io.stdout, symbol, path, line, printLine )
end

function Query:printLocateDirect( outputHandle, symbol, path, line, printLine )
   -- GNU globalフォーマット
   outputHandle:write(
      string.format( "%-16s %4d %-16s %s\n", symbol, line, path,
		     printLine and self:getFileLineText( path, line ) or "" ) )
end


function Query:execWithDb( db, query, target )
   local absFlag = query:find( "a" )
   if query == "dump" then
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
      local nsFlag = true
      local snameFlag = true
      if target then
	 if string.find( target, "^:" ) then
	    snameFlag = false
	 else
	    nsFlag = false
	 end
      end
      if nsFlag then
	 db:mapNamespace(
	    target and string.format( "name like '%s%%'", target ),
	    function( item )
	       if item.name ~= "" then
		  print( item.name )
	       end
	       return true
	    end
	 )
      end
      if snameFlag then
	 db:mapSimpleName(
	    target and string.format( "name like '%s%%'", target ),
	    function( item )
	       if item.name ~= "" then
		  print( item.name )
	       end
	       return true
	    end
	 )
      end
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


function Query:outputRelation( target, depthLimit, relIf, outputFunc, ... )
   if not depthLimit then
      depthLimit = 4
   elseif string.find( depthLimit, "^[0-9]+$" ) then
      depthLimit = tonumber( depthLimit )
   end
   
   if not target then
      relIf:displayItems()
      os.exit( 0 )
   end


   local targetId
   if string.find( target, "^[0-9]+$" ) then
      targetId = tonumber( target )
   else
      targetId = relIf:getId( target )
      if not targetId then
	 log( 1, "not found", target )
	 os.exit( 1 )
      end
   end


   local id2BaseIdSetMap = {}
   local allIdList = {}
   local allIdSet = {}

   local newIdList = { { 1, targetId } }
   allIdSet[ targetId ] = 1
   repeat
      local workList = {}
      for index, info in ipairs( newIdList ) do
	 local dstId = info[ 2 ]
	 local depth = info[ 1 ]
	 if not allIdSet[ dstId ] then
	    table.insert( allIdList, dstId )
	    allIdSet[ dstId ] = 1
	 end
	 if depth < depthLimit then
	    relIf:mapBaseFor(
	       dstId, 
	       function( itemRaw )
		  local item = relIf:convItem( itemRaw )
		  local baseIdSet = id2BaseIdSetMap[ item.id ]
		  if not baseIdSet then
		     baseIdSet = {}
		     id2BaseIdSetMap[ item.id ] = baseIdSet
		  end
		  if not baseIdSet[ item.baseId ] then
		     baseIdSet[ item.baseId ] = 1
		     table.insert( workList, { depth + 1, item.baseId } )
		  end
		  return true
	       end
	    )
	 end
      end
      newIdList = workList
   until #workList == 0


   outputFunc( targetId, allIdList, id2BaseIdSetMap, relIf, ... )
end

function Query:outputCallRelation(
      dbPath, namespace, callerMode, depthLimit, outputFunc, ... )
   
   local db = dbPath and DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end

   local refIf = {
      reverseFlag = callerMode,
      displayItems = function( self )
	 local funcMap = {}
	 db:mapCall(
	    nil,
	    function( item )
	       if callerMode then
		  funcMap[ item.nsId ] = 1
	       else
		  funcMap[ item.belongNsId ] = 1
	       end
	       return true
	    end
	 )
	 for nsId in pairs( funcMap ) do
	    print( string.format(
		      "%d:%s", nsId, db:getNamespace( nsId ).name ) )
	 end
      end,
      getName = function( self, id )
	 return db:getNamespace( id ).name
      end,
      getTooltip = function( self, id )
	 return db:getNamespace( id ).name
      end,
      convItem = function( self, item )
	 if callerMode then
	    return { id = item.nsId, baseId = item.belongNsId }
	 end
	 return { id = item.belongNsId, baseId = item.nsId }
      end,
      getId = function( self, name )
	 return db:getNamespace( nil, name ).id
      end,
      mapBaseFor = function( self, dstId, func )
	 local condition = callerMode and "nsId = " or "belongNsId ="
	 db:mapCall( condition .. tostring( dstId ), func )
      end,
   }

   self:outputRelation( namespace, depthLimit, refIf, outputFunc, ... )
end


function Query:outputIncRelation(
      dbPath, incFilePath, incFlag, depthLimit, outputFunc, ... )
   local db = dbPath and DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end

   local refIf = {
      reverseFlag = not incFlag,
      displayItems = function( self )
	 db:mapFile(
	    incFlag and "incFlag = 0" or "incFlag = 1",
	    function( item )
	       print( item.id, db:getSystemPath( item.path ) )
	       return true
	    end
	 )
      end,
      getName = function( self, id )
	 return string.gsub( db:getFileInfo( id ).path,  ".*/", "" )
      end,
      getTooltip = function( self, id )
	 return db:getSystemPath( db:getFileInfo( id ).path )
      end,
      convItem = function( self, item )
	 if incFlag then
	    return { baseId = item.id, id = item.baseFileId }
	 end
	 return { id = item.id, baseId = item.baseFileId }
      end,
      getId = function( self, name )
	 return db:getFileInfo( nil, name ).id
      end,
      mapBaseFor = function( self, dstId, func )
	 if incFlag then
	    db:mapIncRefListFrom( dstId, func )
	 else
	    db:mapIncRefFor( dstId, func )
	 end
      end
   }

   self:outputRelation(
      incFilePath, depthLimit, refIf, outputFunc, ... )

end


function Query:outputSymbolRefRelation(
      dbPath, symbol, depthLimit, outputFunc, ... )
   local db = dbPath and DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end

   local refIf = {
      reverseFlag = true,
      displayItems = function( self )
	 db:mapNamespace(
	    nil,
	    function( item )
	       print( item.id, item.name )
	       return true
	    end
	 )
      end,
      getName = function( self, id )
	 return db:getNamespace( id ).name
      end,
      getTooltip = function( self, id )
	 return db:getNamespace( id ).name
      end,
      convItem = function( self, item )
	 return { id = item.nsId, baseId = item.belongNsId }
      end,
      getId = function( self, name )
	 return db:getNamespace( nil, name ).id
      end,
      mapBaseFor = function( self, dstId, func )
	 db:mapSymbolRef( dstId, func )
      end
   }

   self:outputRelation( symbol, depthLimit, refIf, outputFunc, ... )
end


return Query
