local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )
local config = require( 'lctags.config' )
local idMap = require( 'lctags.idMap' )

local QueryParam = {}
QueryParam.id2Query = {}

function QueryParam:getQuery( name )
   return QueryParam.id2Query[ name ]
end

function QueryParam:getQueryId( name )
   local queryParam = QueryParam.id2Query[ name ]
   if not queryParam then
      return nil
   end
   return queryParam.name
end

local default = {}

function QueryParam:addQuery( obj, metatable )
   self.id2Query[ obj.name ] = obj
   if obj.alias then
      self.id2Query[ obj.alias ] = obj
   end
   setmetatable( obj, metatable or { __index = default } )
   return obj
end

------ default -----

function default:getNsInfo( db, target )
   if not self.nsInfo then
      if type( target ) == "number" or string.find( target, "^%d" ) then
	 self.nsInfo = db:getNamespace( tonumber( target ) )
      else
	 self.nsInfo = db:getNamespace( nil, target )
      end
   end
   return self.nsInfo
end


function default:getQueryParam( param )
   return { nil, param[ 1 ] }
end

function default:getQueryExecParam( db, nsInfo, mode, target, absFlag )
   return { mode, target }
end

function default:queryOutputHeader( writer, db, target )
end

function default:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   local list = {}
   for key, val in pairs( item ) do
      table.insert( list, key )
   end
   table.sort( list )
   for index, key in ipairs( list ) do
      writer:write( key, item[ key ] )
   end
   writer:endElement()
end

function default:queryOutput( db, isLimit, output, target )
end

------ callee ------

local callee = QueryParam:addQuery( { name = "callee" } )

function callee:queryOutputHeader( writer, db, target )
   local nsInfo = self:getNsInfo( db, target )
   if not nsInfo then
      log( 1, "not found nsInfo" )
      return nil
   end
   
   writer:startParent( "funcInfo" )

   writer:write( "nsId", nsInfo.id )
   writer:write( "name", nsInfo.name )

   db:mapDecl( nsInfo.id,
	       function( item )
		  writer:write( "type", idMap.cursorKind2NameMap[ item.type ] )
		  return false
	       end
   )
   writer:endElement()
end

function callee:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   writer:write( "nsId", item.nsId )
   writer:write( "name", db:getNamespace( item.nsId ).name )

   writer:write( "belongNsId", item.belongNsId )
   
   writer:endElement()
end

function callee:queryOutput( db, isLimit, output, target )
   local nsInfo = self:getNsInfo( db, target )
   
   if not nsInfo then
      log( 1, "not found nsInfo" )
      return nil
   end

   local matchFlag = false
   local cond = (self.name == "callee") and "belongNsId = " or "nsId = "
   db:mapCall(
      cond .. tostring( nsInfo.id ),
      function( item )
	 if isLimit() then return false; end
	 output( db, self.name, target, target, item )
	 matchFlag = true
	 return true
      end
   )
   if not matchFlag then
      log( 3, "no match" )
      if self.name == "callee" then
	 -- 関数呼び出しがない場合、動的呼び出しを探す
	 local indirectFlag = false
	 db:mapDecl( nsInfo.id,
		     function( item )
			if item.type == clang.core.CXCursor_TypedefDecl then
			   -- コール元が typedef の場合は動的呼び出しとする
			   indirectFlag = true
			   return false
			end
			return true
		     end
	 )
	 if indirectFlag then
	    log( 3, "no match: indirect" )
	    local indirectSet = {}
	    local indirectList = config:getIndirectFuncList( nsInfo.name )

	    if indirectList and #indirectList > 0 then
	       db:mapFuncDeclPattern(
		  indirectList,
		  function( item )
		     if not indirectSet[ item.nsId ] then
			indirectSet[ item.nsId ] = true
			output( db, self.name, target, target, item )
		     end
		     return true
		  end
	       )
	    end
	 end
      end
   end
end


------ caller ------


local caller = QueryParam:addQuery( { name = "caller" }, { __index = callee } )

function caller:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   writer:write( "nsId", item.belongNsId )
   writer:write( "name", db:getNamespace( item.belongNsId ).name )
   writer:endElement()
end


------ dumpDir ------

local dumpDir = QueryParam:addQuery( { name = "dumpDir", alias = "dir" } )

function dumpDir:queryOutputItem( writer, db, item )
   writer:write( "path", db:getSystemPath( item.path ) )
end

function dumpDir:queryOutput( db, isLimit, output, target )
   local dirSet = {}
   db:mapFile(
      nil,
      function( item )
	 local path = string.gsub( item.path, "/[^/]+$", "" )
	 if not dirSet[ path ] then
	    dirSet[ path ] = true
	    output( db, query, target, "path",
		    { path = path, fileId = item.id, line = 1 } )
	 end
	 return true
      end
   )
end

------ defAtFileId ------

local defAtFileId = QueryParam:addQuery( { name = "defAtFileId" } )

function defAtFileId:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   writer:write( "nsId", item.nsId )
   writer:write( "name", db:getNamespace( item.nsId ).name )
   writer:write( "type", idMap.cursorKind2NameMap[ item.type ] )
   writer:endElement()
end

function defAtFileId:queryOutput( db, isLimit, output, target )
   db:mapDeclAtFile(
      target,
      function( item )
	 output( db, query, target, target, item )
	 return true
      end
   )
end


------ matchFile ------

local matchFile = QueryParam:addQuery( { name = "matchFile" } )

function matchFile:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   writer:write( "fileId", item.id )
   writer:write( "path", db:getSystemPath( item.path ) )
   writer:endElement()
end

function matchFile:queryOutput( db, isLimit, output, target )
   local path = db:convPath( target )
   if not string.find( path, "/$" ) then
      path = path .. "/"
   end
   db:mapRowList(
      "filePath",
      string.format( "path like '%%%s%%' escape '$'", path ),
      nil, nil,
      function( item )
	 local basename = string.gsub( item.path, path, "" )
	 if not string.find( basename, "/" ) then
	    output( db, query, target, "file", item )
	 end
	 return true
      end
   )
end


------ defBody ------

local defBody = QueryParam:addQuery( { name = "defBody" } )

function defBody:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   writer:write( "fileId", item.fileId )
   writer:write( "line", item.line )
   writer:write( "column", item.column )
   writer:write( "path",
		 db:getSystemPath( db:getFileInfo( item.fileId ).path ) )
   writer:endElement()
end

function defBody:queryOutput( db, isLimit, output, target )
   db:mapDeclHasBody(
      target,
      function( item )
	 output( db, query, target, target, item )
	 return true
      end
   )
end


------ callPair ------

local callPair = QueryParam:addQuery( { name = "callPair" } )

function callPair:getQueryParam( param )
   return { nil, { param[ 1 ], param[ 2 ] } }
end

function callPair:queryOutputItem( writer, db, item )
   writer:startParent( "info" )
   writer:write( "fileId", item.fileId )
   writer:write( "line", item.line )
   writer:write( "column", item.column )
   writer:write( "path",
		 db:getSystemPath( db:getFileInfo( item.fileId ).path ) )
   writer:endElement()
end

function callPair:queryOutput( db, isLimit, output, target )
   db:mapCall(
      "nsId = " .. tostring( target[ 1 ] ) .. " AND " ..
	 "belongNsId = " .. tostring( target[ 2 ] ),
      function( item )
	 if isLimit() then return false; end
	 output( db, self.name, target[1], target[2], item )
	 return true
      end
   )
end


return QueryParam