-- Copyright (C) 2017 ifritJP


-- 再帰になるので DBCtrl は require しない
--local DBCtrl = require( 'lctags.DBCtrl' )

local log = require( 'lctags.LogCtrl' )
local Util = require( 'lctags.Util' )

local Query = {}

local function getPattern( pattern, symbol )
   if symbol:sub( #symbol ) == "$" then
      symbol = symbol:sub( 1, #symbol - 1 )
      pattern = "%%%s"
   elseif symbol:sub( 1, 1 ) == "%" then
      symbol = symbol:sub( 2 )
      pattern = "%%%s%%"
   end
   return pattern, symbol:gsub( "_", "$_" )
end

function Query:execWithDb( db, query, target )
   local absFlag = query:find( "a" )
   if query == "dumpAll" then
      db:dump( 1, target )
   elseif query == "dumpTarget" then
      db:dumpTargetInfo( 1, target )
   elseif query == "dumpTargetList" then
      db:dumpTargetList( 1, target )
   elseif query == "dumpVersion" then
      db:dumpVersion( 1 )
   elseif query == "dumpProjDir" then
      db:dumpProjDir( 1 )
   elseif query == "dumpFile" then
      db:dumpFile( 1, target )
   elseif query == "dumpRef" then
      db:dumpSymbolRef( 1, target )
   elseif query == "dumpDef" then
      db:dumpSymbolDecl( 1, target )
   elseif query == "dumpCall" then
      db:dumpCall( 1, target )
   elseif query == "dumpInc" then
      db:dumpIncCache( 1, target )
   elseif query == "dumpIncSrc" then
      db:dumpIncSrc( 1, target )
   elseif query == "dumpBelong" then
      db:dumpIncBelong( 1, target )
   elseif query == "dumpDigest" then
      db:dumpTokenDigest( 1, target )
   elseif query == "dumpPrepro" then
      db:dumpPreproDigest( 1, target )
   elseif query:find( "P" ) then
      db:mapFile(
	 target and string.format( "path like '%%%s%%' escape '$'",
				   target:gsub( "_", "$_" ) ),
	 function( item )
	    Util:printLocate( db, "path", item.id, 1, absFlag, false )
	    return true
	 end
      )
   elseif query:find( "c" ) then
      local nsFlag = true
      local snameFlag = true
      local pattern = "%s%%"
      if target then
	 if string.find( target, "^:" ) then
	    snameFlag = false
	 else
	    nsFlag = false
	 end
	 pattern, target = getPattern( pattern, target )
	 pattern = string.format( "'%s'", pattern )
      end
      if nsFlag then
	 db:mapNamespace(
	    target and string.format( "name like " .. pattern .. "escape '$'", target ),
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
	    target and string.format( "name like " .. pattern .. " escape '$'", target ),
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
	    Util:printLocate( db, target, item.fileId, item.line, absFlag, true )
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
	    Util:printLocate( db, target, item.fileId, item.line, absFlag, true )
	    return true
	 end
      )
   elseif query:find( "T" ) then
      if not target then
	 return false
      end
      local pattern = "%s%%"
      pattern, target = getPattern( pattern, target )

      db:mapFuncDeclPattern(
	 string.format( pattern, target ),
	 function( item )
	    Util:printLocate( db, item.name, item.fileId, item.line, absFlag, true )
	    return true
	 end
      )
   elseif query:find( "C" ) then
      if not target then
	 return false
      end

      local nsInfo = db:getNamespace( nil, target )
      if not nsInfo then
	 return false
      end
      db:mapCall(
	 "nsId = " .. tostring( nsInfo.id ),
	 function( item )
	    Util:printLocate( db, target, item.fileId, item.line, absFlag, true )
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
	    Util:printLocate( db, target, item.fileId, item.line, absFlag, true )
	    return true
	 end
      )
   end
end

function Query:exec( db, query, target )
   self:execWithDb( db, query, target )
end


function Query:outputRelation( db, target, depthLimit, relIf, outputFunc, ... )
   local targetId, allIdList, id2BaseIdSetMap = 
      self:mapRelation( db, target, depthLimit, relIf, nil )

   outputFunc( db, targetId, allIdList, id2BaseIdSetMap, relIf, ... )
end


function Query:mapRelation( db, target, depthLimit, relIf, mapFunc )
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
   repeat
      local workList = {}
      for index, info in ipairs( newIdList ) do
	 local dstId = info[ 2 ]
	 local depth = info[ 1 ]
	 if not allIdSet[ dstId ] then
	    table.insert( allIdList, dstId )
	    allIdSet[ dstId ] = 1
	 end
	 if depthLimit == 0 or depth < depthLimit then
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
		     if mapFunc then
			return mapFunc( item.baseId, item.id )
		     end
		  end
		  return true
	       end
	    )
	 end
      end
      newIdList = workList
   until #workList == 0

   return targetId, allIdList, id2BaseIdSetMap
end




function Query:outputCallRelation(
      db, namespace, callerMode, depthLimit, outputFunc, ... )
   
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
      getFileId = function( self, id )
	 local fileId
	 db:mapDecl( id,
		     function( symbolDecl )
			fileId = symbolDecl.fileId
			if db:getFileInfo( symbolDecl.fileId ).incFlag == 0 then
			   return false
			end
			return true
		     end
	 )
	 return fileId
      end,
      mapBaseFor = function( self, dstId, func )
	 local condition = callerMode and "nsId = " or "belongNsId ="
	 db:mapCall( condition .. tostring( dstId ), func )
      end,
   }

   self:outputRelation( db, namespace, depthLimit, refIf, outputFunc, ... )
end

--- Include ファイルの参照関係を取得するインタフェース
-- @param incFlag true の場合、インクルードしているファイルを取得する。
--   false の場合、インクルードされているファイルを取得する
function Query:getIncIf( db, incFlag )
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
      getFileId = function( self, id )
	 return id
      end,
      mapBaseFor = function( self, dstId, func )
	 if incFlag then
	    db:mapIncRefListFrom( dstId, func )
	 else
	    db:mapIncRefFor( dstId, func )
	 end
      end
   }
   return refIf
end

function Query:outputIncRelation(
      db, incFilePath, incFlag, depthLimit, outputFunc, ... )
   self:outputRelation( db, incFilePath, depthLimit,
			self:getIncIf( db, incFlag ), outputFunc, ... )

end


function Query:outputSymbolRefRelation(
      db, symbol, depthLimit, outputFunc, ... )
   
   local refIf = {
      reverseFlag = true,
      id2fileIdMap = {},
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
      getFileId = function( self, id )
	 return self.id2fileIdMap[ id ]
      end,
      mapBaseFor = function( self, dstId, func )
	 db:mapSymbolRef(
	    dstId,
	    function (item)
	       self.id2fileIdMap[ item.belongNsId ] = item.fileId
	       if item.belongNsId ~= db.rootNsId then
		  return func( item )
	       end
	       return true
	    end
	 )
      end
   }

   self:outputRelation( db, symbol, depthLimit, refIf, outputFunc, ... )
end

function Query:outputIncSrcHeader( db, file, stream )
   local headerFileInfo = db:getFileInfo( nil, file )
   if not headerFileInfo then
      return
   end
   db:mapIncludeCacheForInc(
      headerFileInfo,
      function( item )
	 local fileInfo = db:getFileInfo( item.baseFileId )
	 Util:printLocateDirect(
	    stream, "path", db:getSystemPath( fileInfo.path ), 1, false )
	 return true
      end
   )
end


return Query
