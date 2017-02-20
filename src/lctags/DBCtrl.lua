-- Copyright (C) 2017 ifritJP


local clang = require( 'libclanglua.if' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local DBAccess = require( 'lctags.DBAccess' )

local function getFileLocation( cursor )
   local location = cursor:getCursorLocation()
   return clang.getFileLocation(
      location.__ptr, clang.core.clang_getFileLocation )
end

local rootNsId = 1
local enumNsId = 2
local structNsId = 3
local unionNsId = 4
local userNsId = 5
local systemFileId = 1
local projDirId = 2

local DBCtrl = {}

function convProjPath( projDir, currentDir )
   if not projDir then
      projDir = currentDir
   end
   if projDir ~= "/" and not string.find( projDir, "/$" ) then
      projDir = projDir .. "/"
   end
   return projDir
end

function DBCtrl:init( path, currentDir, projDir )
   os.remove( path )

   if not currentDir then
      log( 1, "open error. currentDir is nil" )
      return nil
   end

   projDir = convProjPath( projDir, currentDir )
   local db = DBAccess:open( path, false )
   if not db then
      log( 1, "open error." )
      return false
   end

   local obj = {
      db = db,
      currentDir = currentDir,
   }
   setmetatable( obj, { __index = DBCtrl } )

   obj:createTables( obj:convFullpath( projDir ) )

   obj:close()

   return true
end


function DBCtrl:forceUpdate( path )
   local db = DBAccess:open( path, false )
   if not db then
      log( 1, "open error." )
      return false
   end

   local obj = {
      db = db,
   }
   setmetatable( obj, { __index = DBCtrl } )

   obj:update( "filePath", "modTime = 0", "incFlag = 0" )

   obj:close()

   return true
end


function DBCtrl:calcFileDigest( path )
   local path = self:getSystemPath( path )
   if path == "" then
      return ""
   end
   local fileHandle = io.open( path, "r" )
   if not fileHandle then
      log( 1, "file open error", path )
      return nil
   end
   local digestObj = Helper.openDigest( "md5" )
   digestObj:write( fileHandle:read( "*a" ) )
   fileHandle:close()
   return digestObj:fix()
end

function DBCtrl:changeProjDir( path, currentDir, projDir )
   local obj = DBCtrl:open( path, false, currentDir )

   if not obj then
      return
   end

   projDir = obj:convFullpath( convProjPath( projDir, obj.currentDir ) )

   obj:update( "filePath", "path = '" ..  projDir .."'",
	       "id = " .. tostring( projDirId ) )

   obj:mapRowList(
      "filePath", nil, nil, nil,
      function( row )
	 if row.id == systemFileId or row.id == projDirId then
	    return true
	 end
	 
	 local filePath = obj:getSystemPath( row.path )
	 local fileDigest = obj:calcFileDigest( filePath )
	 if not fileDigest then
	    return false
	 end
	 -- DB の modTime を更新する
	 local modTime = 0
	 if row.digest == fileDigest then
	    -- digest が同じファイルは解析済みとし、
	    -- ローカルファイルの更新時間を DB に登録する
	    if filePath ~= "" then
	       modTime = Helper.getFileModTime( filePath )
	    end
	 else
	    log( 1, "detect modified", filePath )
	 end
	 obj:update(
	    "filePath",
	    string.format( "modTime = %d, digest = '%s'", modTime, fileDigest ),
	    "id = " .. tostring( row.id ) )
	 return true
      end
   )

   obj:close()

   return true
end



function DBCtrl:shrinkDB( path )
   local obj = DBCtrl:open( path, false, os.getenv( "PWD" ) )
   
   if not obj then
      return false
   end

   -- 存在しないファイルを削除
   local deleteFileList = {}
   obj:mapRowList(
      "filePath", nil, nil, nil,
      function( item )
	 if not Helper.getFileModTime( obj:getSystemPath( item.path ) ) then
	    table.insert( deleteFileList, item )
	 end
	 return true
      end
   )
   for index, fileInfo in ipairs( deleteFileList ) do
      obj:updateFile( fileInfo, true )
   end

   
   --- 存在しない名前空間、単純名を削除
   -- まずは全名前空間、全単純名の ID セットを取得
   
   local nsId2InfoMap = {}
   local snameIdSet = {}
   obj:mapRowList(
      "namespace", nil, nil, nil,
      function( item )
	 if item.id >= userNsId then
	    nsId2InfoMap[ item.id ] = item
	    snameIdSet[ item.snameId ] = 1
	 end
	 return true
      end
   )
   -- 定義されている名前空間の親 ID のセットを登録
   local usingNsIdSet = {}
   obj:mapRowList(
      "symbolDecl", nil, nil, nil,
      function( item )
	 usingNsIdSet[ item.nsId ] = 1
	 snameIdSet[ item.snameId ] = nil
	 return true
      end
   )

   -- 定義されている名前空間の親を辿って、親の定義状況を更新する
   local checkSet = usingNsIdSet
   repeat
      local noneFlag = true
      local workSet = {}
      for nsId in pairs( checkSet ) do
	 local nsInfo = nsId2InfoMap[ nsId ]
	 if nsInfo and nsId ~= rootNsId then
	    if not usingNsIdSet[ nsInfo.parentId ] then
	       usingNsIdSet[ nsInfo.parentId ] = 1
	       noneFlag = false
	       workSet[ nsInfo.parentId ] = 1
	    end
	 end
      end
      checkSet = workSet
   until noneFlag
   -- 定義されている名前空間 ID を usingNsIdSet から除外する
   for usingNsId in pairs( usingNsIdSet ) do
      nsId2InfoMap[ usingNsId ] = nil
   end
   
   -- 残りのセットは定義されていない名前空間、単純名なので削除
   for nsId, nsInfo in pairs( nsId2InfoMap ) do
      log( 2, "delete:", nsId, nsInfo.id, nsInfo.name )
      obj:deleteNamespace( nsId )
   end
   for snameId in pairs( snameIdSet ) do
      if snameId ~= 0 then
	 obj:delete( "simpleName", "id = " .. tostring( snameId ) )
      end
   end
   

   obj.db:commit()

   obj.db:exec( "VACUUM" )
   obj.db:close()
   return true
end


function DBCtrl:open( path, readonly, currentDir )
   if not currentDir then
      log( 1, "open error. currentDir is nil" )
      return nil
   end
   local db = DBAccess:open( path, readonly )
   if not db then
      return nil
   end
   
   local obj = {
      db = db,
      currentDir = currentDir,
      -- カーソルハッシュ -> fullname 情報 のマップ
      hashCursor2FullnameMap = {},
      hashCursor2NSMap = {},
      -- path -> info マップ
      path2fileInfoMap = {},
      fileId2fileInfoMap = {},
      -- sname -> sname マップ
      name2snameInfoMap = {},
      -- path -> converted path マップ
      convPathCache = {},
      targetFileInfo = nil,
   }
   setmetatable( obj, { __index = DBCtrl } )

   local item = obj:getRow( "etc", "keyName = 'version'" )
   if not item then
      log( 1, "unknown version" )
      db:close()
      return nil
   end
   if tonumber( item.val ) > 1 then
      log( 1, "not support version", item.val )
      db:close()
      return nil
   end

   
   if not readonly then
      db:begin()
   end

   local projDirInfo = obj:getFileInfo( projDirId )

   if not projDirInfo or projDirInfo.path == "" then
      log( 1, "db is not initialized" )
      obj:close()
      return nil
   end

   obj.projDir = projDirInfo.path

   return obj
end


function DBCtrl:close()
   self.db:commit()
   self.db:close()
end


function DBCtrl:createTables( projDir )
   self.db:createTables(
      string.format(
	 [[
BEGIN;
CREATE TABLE etc ( keyName VARCHAR UNIQUE COLLATE nocase PRIMARY KEY, val VARCHAR);
INSERT INTO etc VALUES( 'version', '1' );
CREATE TABLE namespace ( id INTEGER PRIMARY KEY, parentId INTEGER, snameId INTEGER, digest CHAR(32), name VARCHAR UNIQUE COLLATE nocase, otherName VARCHAR COLLATE nocase);
INSERT INTO namespace VALUES( NULL, 1, 0, '', '', '' );
INSERT INTO namespace VALUES( NULL, 1, 0, '', '::@enum', '::@enum' );
INSERT INTO namespace VALUES( NULL, 1, 0, '', '::@struct', '::@struct' );
INSERT INTO namespace VALUES( NULL, 1, 0, '', '::@union', '::@union' );

CREATE TABLE simpleName ( id INTEGER PRIMARY KEY, name VARCHAR UNIQUE COLLATE nocase);
CREATE TABLE filePath ( id INTEGER PRIMARY KEY, path VARCHAR UNIQUE COLLATE nocase, modTime INTEGER, incFlag INTEGER, digest CHAR(32), currentDir VARCHAR COLLATE nocase);
INSERT INTO filePath VALUES( NULL, '', 0, 0, '', '');
INSERT INTO filePath VALUES( NULL, '%s', 0, 0, '', '');

CREATE TABLE compileOp ( fileId INTEGER, target VARCHAR COLLATE nocase, compOp VARCHAR COLLATE nocase );
CREATE TABLE symbolDecl ( nsId INTEGER, parentId INTEGER, snameId INTEGER, type INTEGER, fileId INTEGER, line INTEGER, column INTEGER, endLine INTEGER, endColumn INTEGER, charSize INTEGER, comment VARCHAR COLLATE nocase, PRIMARY KEY( nsId, fileId, line ) );
CREATE TABLE symbolRef ( nsId INTEGER, snameId INTEGER, fileId INTEGER, line INTEGER, column INTEGER, endLine INTEGER, endColumn INTEGER, charSize INTEGER, belongNsId INTEGER, PRIMARY KEY( nsId, fileId, line ) );
CREATE TABLE incRef ( id INTEGER, baseFileId INTEGER, line INTEGER );
CREATE TABLE incBelong ( id INTEGER, baseFileId INTEGER, nsId INTEGER );
CREATE TABLE tokenDigest ( fileId INTEGER, digest CHAR(32), PRIMARY KEY( fileId, digest ) );
CREATE INDEX index_ns ON namespace ( id, parentId, snameId, name, otherName );
CREATE INDEX index_sName ON simpleName ( id, name );
CREATE INDEX index_filePath ON filePath ( id, path );
CREATE INDEX index_compOp ON compileOp ( fileId );
CREATE INDEX index_symDecl ON symbolDecl ( nsId, parentId, snameId, fileId );
CREATE INDEX index_symRef ON symbolRef ( nsId, snameId, fileId, belongNsId );
CREATE INDEX index_incRef ON incRef ( id, baseFileId );
CREATE INDEX index_incBelong ON incBelong ( id, baseFileId );
CREATE INDEX index_digest ON tokenDigest ( fileId, digest );
COMMIT;
]], projDir ) )
end

function DBCtrl:updateFile( fileInfo, removeFlag )
   if not fileInfo then
      return
   end
   local fileId = fileInfo.id
   if fileId == systemFileId then
      return
   end

   
   log( 1, "updateFile", fileInfo.path, fileId )
   self:delete( "symbolDecl", "fileId = " .. tostring( fileId ) )
   self:delete( "symbolRef", "fileId = " .. tostring( fileId ) )
   self:delete( "incRef", string.format( "baseFileId = %d", fileId ) )
   self:delete( "incBelong", string.format( "baseFileId = %d", fileId ) )
   self:delete( "tokenDigest", string.format( "fileId = %d", fileId ) )

   if removeFlag then
      self:delete( "filePath", string.format( "id = %d", fileId ) )
      self:delete( "incRef", string.format( "id = %d", fileId ) )
      self:delete( "incBelong", string.format( "id = %d", fileId ) )
   end
   
   self.fileId2fileInfoMap[ fileId ] = nil
   self.path2fileInfoMap[ fileInfo.path ] = nil
end


function DBCtrl:getFileIdLocation( cursor )
   local cxfile, line, column = getFileLocation( cursor )
   if not cxfile then
      return systemFileId, line, column
   end
   local fileInfo = self:getFileInfo( nil, cxfile:getFileName() )
   if not fileInfo then
      error( "not found file", cxfile:getFileName() )
   end
   return fileInfo.id, line, column
end


function DBCtrl:exec( stmt, errHandle )
   self.db:exec( stmt, errHandle )
end

function DBCtrl:exists( tableName, condition )
   local row = self:getRow( tableName, condition )
   return row ~= nil
end

function DBCtrl:delete( tableName, condition )
   self.db:delete( tableName, condition )
end

function DBCtrl:mapRowList( tableName, condition, limit, attrib, func, ... )
   self.db:mapRowList( tableName, condition, limit, attrib, func, ... )
end

function DBCtrl:getRowList( tableName, condition, limit, attrib )
   local rows = {}
   self:mapRowList( tableName, condition, limit, attrib,
		    function( item )
		       table.insert( rows, item )
		       return true
		    end
   )
   return rows
end

function DBCtrl:getRow( tableName, condition, attrib )
   return self:getRowList( tableName, condition, 1, attrib )[1]
end

function DBCtrl:getNamespace( id, canonicalName )
   if id then
      return self:getRow( "namespace", string.format( "id == %d", id ) )
   end
   return self:getRow( "namespace", "name == '" .. canonicalName .. "'" )
end

function DBCtrl:getSimpleName( id, name )
   if id then
      return self:getRow( "simpleName", string.format( "id == %d", id ) )
   end

   local item = self.name2snameInfoMap[ name ]
   if item then
      return item
   end
   item = self:getRow( "simpleName", "name == '" .. name .. "'" )
   self.name2snameInfoMap[ name ] = item
   return item
end


function DBCtrl:addFile(
      filePath, time, digest, compileOp, currentDir, isTarget, target )
   if not compileOp then
      compileOp = ""
   end
   if not currentDir then
      currentDir = ""
   end
   if not target then
      target = ""
   end
   local fileInfo, filePath = self:getFileInfo( nil, filePath )
   if fileInfo then
      if isTarget then
	 self.targetFileInfo = fileInfo
	 local targetInfo = self:getRow(
	    "compileOp",
	    string.format(
	       "fileId = %d and target = '%s'", fileInfo.id, target ) )
	 if targetInfo then
	    if targetInfo.compOp ~= compileOp then
	       self:update(
		  "compileOp", "compOp = '" .. compileOp .. "'",
		  string.format( "fileId = %d AND target = '%s'",
				 fileInfo.id, target ) )
	    end
	 end
      end
      if fileInfo.modTime == time then
	 if isTarget then
	    if self:existsIncWithDigest( fileInfo.id, digest ) then
	       fileInfo.uptodate = true
	       log( 3, "uptodate:", filePath )
	    else
	       log( 2, "detect mismatch digest", filePath, digest )
	    end
	 end
	 return
      end
      self:update(
	 "filePath", "modTime = " .. tostring( time ),
	 "id = " .. tostring( fileInfo.id ) )
      self:updateFile( fileInfo )
   end

   self:insert(
      "filePath",
      string.format(
	 "NULL, '%s', %d, %d, '%s', '%s'", filePath, time, isTarget and 0 or 1,
	 self:calcFileDigest( filePath ), self:convPath( currentDir ) ) )

   fileInfo = self:getFileInfo( nil, filePath )

   if isTarget then
      self:insert(
	 "compileOp",
	 string.format( "%d, '%s', '%s'", fileInfo.id, target, compileOp ) )
      self:addTokenDigest( fileInfo.id, digest )
   end
   
   if isTarget then
      self.targetFileInfo = fileInfo
   end
   return fileInfo
end

function DBCtrl:addSymbolDecl( cursor, fileId, nsInfo )
   self.hashCursor2FullnameMap[ cursor:hashCursor() ] = nsInfo.name
   self.hashCursor2NSMap[ cursor:hashCursor() ] = nsInfo
   local fileInfo = self:getFileInfo( fileId )
   log( 3, "addSymbolDecl", fileId,
	fileInfo.uptodate, nsInfo.name, cursor:hashCursor() )
   
   --local cxFile, line, column = getFileLocation( cursor )
   local startInfo, endInfo = self:getRangeFromCursor( cursor )
   local line, column =  startInfo[2], startInfo[3]

   local item = {}
   item.nsId = nsInfo.id
   item.parentId = nsInfo.parentId
   item.snameId = nsInfo.snameId
   item.type = cursor:getCursorKind()
   item.fileId = fileId
   item.line = line
   item.column = column
   item.endLine = endInfo[2]
   item.endColumn = endInfo[3]
   item.charSize = endInfo[4] - startInfo[4]
   item.comment = cursor:getRawCommentText()


   if not fileInfo.uptodate then
      self:insert(
	 "symbolDecl",
	 string.format( "%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, '%s'",
			item.nsId, item.parentId, item.snameId, item.type,
			item.fileId, item.line, item.column,
			item.endLine, item.endColumn, item.charSize,
			item.comment and "has" or "none" ) )
   else
      log( 3, "skip addSymbolDecl", nsInfo.name )
   end
   
   return item
end

function DBCtrl:addSimpleName( name )
   local item = self:getSimpleName( nil, name )
   if item then
      return item
   end
   self:insert( "simpleName", string.format( "NULL, '%s'", name ) )
   local item = self:getSimpleName( nil, name )
   return item
end


function DBCtrl:addNamespaceOne(
      cursor, digest, fileId, parentId, simpleName, namespace, otherName )
   local snameInfo = self:addSimpleName( simpleName )

   log( 3, "addNamespaceOne: ", namespace )

   if not otherName then
      otherName = namespace
   end
   
   local item = self:getNamespace( nil, namespace )
   if not item then
      self:insert(
	 "namespace",
	 string.format( "NULL, %d, %d, '%s', '%s', '%s'",
			parentId, snameInfo.id, digest, namespace, otherName ) )
      item = self:getNamespace( nil, namespace )
   end

   if not cursor then
      return item
   end
   
   return item, self:addSymbolDecl( cursor, fileId, item )
end

function DBCtrl:getFileFromCursor( cursor )
   local cxfile = getFileLocation( cursor )
   if not cxfile then
      return self:getFileInfo( systemFileId, nil )
   end
   local fileName = cxfile:getFileName()
   return self:getFileInfo( nil, fileName )
end

function DBCtrl:addNamespace( cursor, digest, anonymousName, typedefName )
   local fileInfo = self:getFileFromCursor( cursor )
   if fileInfo.id == systemFileId then
      log( 3, "addNamespace skip for system", cursor:getCursorSpelling() )
      return 
   end
   if fileInfo.uptodate then
      local fullname = self:getFullname(
	 cursor, fileInfo.id, anonymousName, typedefName )
      self.hashCursor2FullnameMap[ cursor:hashCursor() ] = fullname
      log( "update namespace:", fullname, cursor:hashCursor() )
      return
   end
   self:addNamespaceSub( cursor, fileInfo, digest, anonymousName, typedefName )
end

function DBCtrl:getFullname( cursor, fileId, anonymousName, typedefName )
   local spell = cursor:getCursorSpelling()
   local simpleName = spell
   local nsList = clang.getNamespaceList( cursor )
   if spell == "" then
      if typedefName and typedefName ~= "" then
	 simpleName = typedefName
      else
	 simpleName = anonymousName
      end
      spell = { anonymousName, simpleName }
   end
   table.insert( nsList, spell )


   local cursorKind = cursor:getCursorKind()
   if ( cursorKind ~= clang.core.CXCursor_FunctionDecl and
	   cursorKind ~= clang.core.CXCursor_CXXMethod and
	   cursorKind ~= clang.core.CXCursor_Namespace ) or
      cursor:getStorageClass() == clang.core.CX_SC_Static
   then
      table.insert( nsList, string.format( "%d", fileId ) )
   end

   local fullname = ""
   for index, name in ipairs( nsList ) do
      local workNS
      if type( name ) == 'table' then
	 workNS = fullname .. "::" .. name[2]
	 name = name[1]
	 fullname = fullname .. "::" .. name
      else
	 fullname = fullname .. "::" .. name
	 workNS = fullname
      end
      if index ~= #nsList then
	 fullname = workNS
      end
   end
   return fullname, nsList, simpleName
end

function DBCtrl:addNamespaceSub( cursor, fileInfo, digest, anonymousName, typedefName )
   local spell = cursor:getCursorSpelling()

   log( 3, "addNamespaceSub:", spell )
   
   local fileId = fileInfo.id
   local kind = cursor:getCursorKind()

   if not digest then
      digest = ""
   end


   -- if kind == clang.CXCursorKind.FunctionDecl.val or
   --    kind == clang.CXCursorKind.CXXMethod.val
   -- then
   --    cursor:visitChildren(
   -- 	 function( cursor, parent, exInfo )
   -- 	    if cursor:getCursorKind() == clang.CXCursorKind.CompoundStmt.val then
   -- 	       return clang.CXChildVisitResult.Break.val
   -- 	    end
   -- 	    return clang.CXChildVisitResult.Continue.val
   -- 	 end, nil )
   -- end

   local fullname, nsList, simpleName = self:getFullname(
      cursor, fileId, anonymousName, typedefName )

   local uptodate = false
   local nsInfo
   if digest ~= "" then
      nsInfo = self:getNamespace( nil, fullname )
      if nsInfo then
	 if nsInfo.digest == "" then
	    self:update(
	       "namespace", "digest = '" .. digest .. "'",
	       "id = " .. tostring( nsInfo.id ) )
	    nsInfo.digest = digest
	 elseif nsInfo.digest == digest then
	    uptodate = true
	    log( 3, "addNamespaceSub uptodate digest", fullname, digest, nsInfo.name )
	 else
	    log( 2, "addNamespaceSub mismatch digest", fullname, digest, nsInfo.digest )
	    -- self:deleteNamespace( nsInfo.id )
	 end
      end
   end

   local symbolDecl
   if uptodate then
      symbolDecl = self:addSymbolDecl( cursor, fileId, nsInfo )
   else
      local namespace = ""
      local parentNs = { id = 1 }
      for index, name in ipairs( nsList ) do
	 local workNS
	 if type( name ) == 'table' then
	    workNS = namespace .. "::" .. name[2]
	    name = name[1]
	    namespace = namespace .. "::" .. name
	 else
	    namespace = namespace .. "::" .. name
	    workNS = namespace
	 end
	 local isLastName = index == #nsList
	 nsInfo, symbolDecl = self:addNamespaceOne(
	    isLastName and cursor or nil, digest,
	    fileId, parentNs.id,
	    isLastName and simpleName or name, namespace, workNS )
	 namespace = workNS
	 parentNs = nsInfo
      end
   end

   return nsInfo, symbolDecl, uptodate
end

function DBCtrl:deleteNamespace( nsId, targetIdList )
   if nsId == rootNsId then
      return
   end
   local deleteFlag = false
   if not targetIdList then
      targetIdList = {}
      deleteFlag = true
   end
   table.insert( targetIdList, nsId )
   self:mapRowList( "namespace", "parentId == " .. tostring( nsId ), nil, nil,
		    function( item )
		       self:deleteNamespace( item.id, targetIdList )
		       return true
		    end
   )
   if deleteFlag then
      for index, id in ipairs( targetIdList ) do
	 self:delete( "namespace", "id == " .. tostring( id ) )
	 self:delete( "symbolDecl", "nsId == ".. tostring( id ) )
	 self:delete( "symbolRef",
		      "nsId == ".. tostring( id ) .. " or " ..
			 "belongNsId == ".. tostring( id ) )
	 log( "delete:", id )
      end
   end
end

function DBCtrl:calcEnumStructDigest( decl, kind, digest )
   local needFix = false
   if not digest then
      digest = Helper.openDigest( "md5" )
      needFix = true
   end

   local hasChild = false
   digest:write( decl:getCursorSpelling() )
   clang.visitChildrenFast(
      decl,
      function( cursor, parent, exInfo, changeFileFlag )
   	 local cursorKind = cursor:getCursorKind()
   	 if cursorKind == clang.core.CXCursor_StructDecl or
   	    cursorKind == clang.core.CXCursor_UnionDecl
   	 then
   	    hasChild = true
   	    self:calcEnumStructDigest( cursor, kind, digest )
   	 elseif cursorKind == kind then
   	    hasChild = true
   	    local cxtype = cursor:getCursorType()
   	    digest:write( cxtype:getTypeSpelling() )
   	    digest:write( cursor:getCursorSpelling() )
   	 end
      end,
      nil,
      { kind, clang.core.CXCursor_StructDecl,
   	clang.core.CXCursor_UnionDecl }, 1 )
	 

   if needFix then
      if not hasChild then
	 return ""
      end
      return digest:fix()
   end
end

function DBCtrl:addEnumStructDecl( decl, anonymousName, typedefName, kind )
   local fileInfo = self:getFileFromCursor( decl )

   local otherNameBase
   local fullnameBase
   local baseNsId

   local structUptodate = false
   
   if fileInfo.uptodate then
      fullnameBase = self:getFullname(
	 decl, fileInfo.id, anonymousName, typedefName )
      self.hashCursor2FullnameMap[ decl:hashCursor() ] = fullnameBase
   else
      log( 3, "addEnumStructDecl start",
	   decl:getCursorSpelling(), typedefName, os.clock(), os.date() )
      local digest = self:calcEnumStructDigest( decl, kind )

      local declNs, symbolDecl
      declNs, symbolDecl, structUptodate = self:addNamespaceSub(
	 decl, fileInfo, digest, anonymousName, typedefName )
      fullnameBase = declNs.name
      otherNameBase = declNs.otherName
      baseNsId = declNs.id

      if structUptodate then
	 log( 3, "addEnumStructDecl", "uptodate", fullnameBase )
      end
   end

   local hasIncFlag = self:hasInc( fileInfo, decl )
   local count = 0
   local workFileInfo = fileInfo
   clang.visitChildrenFast(
      decl,
      function( cursor, parent, exInfo, changeFileFlag )
   	 local cursorKind = cursor:getCursorKind()
   	 if hasIncFlag and changeFileFlag then
   	    workFileInfo = self:getFileFromCursor( cursor )
   	 end
	 
   	 if cursorKind == clang.core.CXCursor_StructDecl or
   	    cursorKind == clang.core.CXCursor_UnionDecl
   	 then
   	    self:addStructDecl(
   	       cursor, anonymousName .. "@" .. tostring( count ), "" )
   	    count = count + 1
   	 elseif cursorKind == kind then
   	    local name = cursor:getCursorSpelling()
   	    local fullname = fullnameBase .. "::" .. name
   	    log( 3, "addEnumStructDecl process", fullname, os.clock() )
	    
   	    if workFileInfo.uptodate or structUptodate then
   	       self.hashCursor2FullnameMap[ cursor:hashCursor() ] = fullname
   	    else
   	       local otherName = otherNameBase .. "::" .. name
   	       local nsInfo = self:addNamespaceOne(
   		  cursor, "", workFileInfo.id,
   		  baseNsId, name, fullname, otherName )
   	    end
   	 end
   	 return clang.CXChildVisitResult.Continue.val
      end,
      nil,
      { kind, clang.core.CXCursor_StructDecl,
   	clang.core.CXCursor_UnionDecl }, 1 )
   
   log( 3, "addEnumStructDecl end", os.clock() )
end

function DBCtrl:addEnumDecl( enumDecl, anonymousName, typedefName )
   self:addEnumStructDecl( enumDecl, anonymousName, typedefName,
			   clang.CXCursorKind.EnumConstantDecl.val )
end


function DBCtrl:addStructDecl( structDecl, anonymousName, typedefName )
   self:addEnumStructDecl( structDecl, anonymousName, typedefName,
			   clang.CXCursorKind.FieldDecl.val )
end

function DBCtrl:getNamespaceFromCursor( cursor )
   if not cursor or cursor:getCursorKind() == clang.core.CXCursor_InvalidFile then
      return self:getNamespace( rootNsId, nil )
   end
   local hash = cursor:hashCursor()
   local nsInfo = self.hashCursor2NSMap[ hash ]
   if nsInfo then
      return nsInfo
   end
   local fullname = self.hashCursor2FullnameMap[ hash ]
   if fullname then
      nsInfo = self:getNamespace( nil, fullname )
      self.hashCursor2NSMap[ hash ] = nsInfo
      return nsInfo
   end

   local fileId, line, column = self:getFileIdLocation( cursor )
   
   fullname = self:getFullname( cursor, fileId, "", "" )
   nsInfo = self:getNamespace( nil, fullname )
   self.hashCursor2FullnameMap[ hash ] = fullname
   self.hashCursor2NSMap[ hash ] = nsInfo
   return nsInfo
end


function DBCtrl:addReference( refInfo )
   local cursor = refInfo.cursor
   local declCursor = refInfo.declCursor
   -- local fileId, line = self:getFileIdLocation( cursor )
   local startInfo, endInfo = self:getRangeFromCursor( cursor )
   local line = startInfo and startInfo[ 2 ] or 0
   local fileInfo = startInfo and self:getFileInfo( nil, startInfo[ 1 ]:getFileName() )
   local fileId = fileInfo and fileInfo.id or systemFileId
   

   local parentNsInfo = self:getNamespaceFromCursor( refInfo.namespace )
   local nsInfo = self:getNamespaceFromCursor( declCursor )
   local kind = declCursor:getCursorKind()
   if not nsInfo then
      -- 宣言のないもの
      if kind == clang.core.CXCursor_VarDecl or
	 kind == clang.core.CXCursor_ParmDecl 
      then
	 local storageClass = declCursor:getStorageClass()
	 if storageClass ~= clang.core.CX_SC_Auto and
	    storageClass ~= clang.core.CX_SC_Register
	 then
	    -- ローカル変数, パラメータは参照には登録しない
	    return
	 end
      end
      -- ローカル変数, パラメータでなく、
      -- 宣言のないものはここでグローバルとして登録する

      local name = cursor:getCursorSpelling()
      log( 3, "regist none decl namespace",
	   name, declCursor:hashCursor(),
	   clang.getCursorKindSpelling( kind ),
	   declCursor:getCursorSpelling())
      nsInfo = self:addNamespaceOne(
	 declCursor, "", systemFileId, rootNsId, name, "::" .. name )
   end

   if fileInfo.uptodate then
      local declFileInfo = self:getFileFromCursor( declCursor )
      if declFileInfo.uptodate then
	 log( 3, "uptodate ref", nsInfo.name )
	 return
      end
   end


   self:insert(
      "symbolRef",
      string.format( "%d, %d, %d, %d, %d, %d, %d, %d, %d",
		     nsInfo.id, nsInfo.snameId, fileId, line,
		     startInfo and startInfo[ 3 ] or 0,
		     endInfo and endInfo[ 2 ] or 0,
		     endInfo and endInfo[ 3 ] or 0,
		     startInfo and endInfo and endInfo[ 4 ] - startInfo[ 4 ],
		     parentNsInfo and parentNsInfo.id or 0 ) )

   -- do
   --    local startInfo2, endInfo2 = self:getSpellRangeFromCursor( cursor, 0 )
   --    if startInfo2 and endInfo2 then
   -- 	 print( cursor:getCursorSpelling(),
   -- 		startInfo[ 2 ], startInfo[ 3 ], endInfo[ 2 ], endInfo[ 3 ],
   -- 		startInfo2[ 2 ], startInfo2[ 3 ], endInfo2[ 2 ], endInfo2[ 3 ])
   --    end
   -- end
end

function DBCtrl:mapSimpleName( condition, func )
   self.db:mapRowList( "simpleName", condition, nil, nil, func )
end

function DBCtrl:mapFile( condition, func )
   self.db:mapRowList( "filePath", condition, nil, nil, func )
end


function DBCtrl:getIncRef( parentFileId, includeFileId )
   return self:getRow(
      "incRef", string.format( "baseFileId = %d AND id = %d",
			       parentFileId, includeFileId ) )
end

function DBCtrl:mapIncRefListFrom( parentFileId, func )
   self:mapRowList(
      "incRef", string.format( "baseFileId = %d", parentFileId ), nil, nil, func )
end


function DBCtrl:existsIncWithDigest( includeFileId, digest )
   return self:exists(
      "tokenDigest", string.format( "fileId = %d AND digest = '%s'",
				    includeFileId, digest ) )
end

function DBCtrl:addTokenDigest( fileId, digest )
   self:insert( "tokenDigest", string.format( "%d, '%s'", fileId, digest ) )
end


function DBCtrl:addInclude( cursor, digest )
   local cxfile = cursor:getIncludedFile()
   local path = cxfile and cxfile:getFileName() or ""
   
   log( "inc:", path, digest )

   local fileInfo = self:getFileInfo( nil, path )

   local currentFile, line = getFileLocation( cursor )
   local currentFileInfo = self:getFileInfo( nil, currentFile:getFileName() )

   local incInfo = self:getIncRef( currentFileInfo.id, fileInfo.id )

   local existsIncWithDigestFlag = self:existsIncWithDigest( fileInfo.id, digest )

   table.insert( currentFileInfo.incPosList, line )

   if existsIncWithDigestFlag then
      if fileInfo.uptodate == nil then
	 fileInfo.uptodate = true
      end
      log( "uptodate some:", fileInfo.id, path, fileInfo.uptodate, digest )
   else
      log( 2, "detect new digest inc", fileInfo.path, digest )
      self:addTokenDigest( fileInfo.id, digest )
      fileInfo.uptodate = false
   end

   if incInfo then 
      return
   end

   self:insert( "incRef",
		string.format( "%d, %d, %d",
			       fileInfo.id, currentFileInfo.id, line ) )
end


function DBCtrl:addIncBelong( incBelong )
   if not incBelong.cxfile then
      return
   end
   local path = incBelong.cxfile:getFileName()
   log( 3, "incBelong:", path )

   local fileInfo = self:getFileInfo( nil, path )
   local belongNsId
   if not incBelong.namespace then
      return
   end
   belongNsId = self:getNamespaceFromCursor( incBelong.namespace ).id

   local baseFileId = self:getFileIdLocation( incBelong.namespace )
   if fileInfo.id == baseFileId then
      -- インクルードファイルと、所属名前空間のファイルが同じ場合、
      -- 正しく検出できていないので無視
      return
   end

   self:insert( "incBelong",
		string.format( "%d, %d, %d",
			       fileInfo.id, baseFileId, belongNsId ) )
end

function DBCtrl:getRangeFromCursor( cursor )
   local range = cursor:getCursorExtent()
   if not range then
      return
   end
   local startInfo = table.pack(
      clang.getFileLocation(
	 range:getRangeStart().__ptr, clang.core.clang_getFileLocation ) )
   local endInfo = table.pack(
      clang.getFileLocation(
	 range:getRangeEnd().__ptr, clang.core.clang_getFileLocation ) )
   return startInfo, endInfo
end

function DBCtrl:getSpellRangeFromCursor( cursor, index )
   local range = cursor:getSpellingNameRange( index, 0 )
   if not range then
      return
   end
   local startInfo = table.pack(
      clang.getFileLocation(
	 range:getRangeStart().__ptr, clang.core.clang_getFileLocation ) )
   local endInfo = table.pack(
      clang.getFileLocation(
	 range:getRangeEnd().__ptr, clang.core.clang_getFileLocation ) )
   return startInfo, endInfo
end


function DBCtrl:hasInc( fileInfo, cursor )
   local startInfo, endInfo = self:getRangeFromCursor( cursor )
   local startLine = startInfo[ 2 ]
   local endLine = endInfo[ 2 ]

   for index, line in ipairs( fileInfo.incPosList ) do
      if startLine < line and endLine > line then
	 log( 2, "hasInc:", cursor:getCursorSpelling() )
	 return true
      end
   end
   return false
end

function DBCtrl:mapSymbolInfoList( tableName, symbol, func, ... )
   local nsInfo = self:getRow( "namespace", "otherName = '" .. symbol .. "'" )
   if nsInfo then
      self:mapRowList(
	 tableName,
	 string.format( "nsId = %d", nsInfo.id ), nil, nil, func, ... )
      return
   end

   snameInfo = self:getRow( "simpleName", "name = '" .. symbol .. "'" )
   if not snameInfo then
      return {}
   end

   self:mapRowList(
      tableName,
      string.format( "snameId = %d", snameInfo.id ), nil, nil, func, ... )
end

function DBCtrl:mapCompInfo( condition, func )
   return self:mapRowList( "compileOp", condition, nil, nil, func )
end


function DBCtrl:mapDeclInfoList( symbol, func, ... )
   return self:mapSymbolInfoList( "symbolDecl", symbol, func, ... )
end

function DBCtrl:mapSymbolRefInfoList( symbol, func, ... )
   return self:mapSymbolInfoList( "symbolRef", symbol, func, ... )
end

function DBCtrl:SymbolRefInfoListForCursor( cursor, func, ... )
   local fileId, line = self:getFileIdLocation( cursor )
   local snameInfo = self:getSimpleName( nil, cursor:getCursorSpelling() )

   -- cursor の symbolDecl を次の条件で検索する。
   --    単純名ID, ファイルID、行番号、列が等しい
   -- 同じ単純名のメンバーを持つ構造体をマクロ内で複数宣言した場合、
   -- symbolDecl が複数ヒットしてしまうが、
   -- レアケースなのでここではすべて対象とする
   local symbolDeclInfoList = self:getRowList(
      "symbolDecl",
      string.format( "fileId = %d AND line = %d AND snameId = %d",
		     fileId, line, snameInfo.id ) )

   for index, symbolDecl in ipairs( symbolDeclInfoList ) do
      self:mapRowList(
	 "symbolRef",
	 "nsId = " .. tostring( symbolDecl.nsId ), nil, nil, func, ... )
   end
end

function DBCtrl:getSystemPath( path, baseDir )
   local cache = self.convPathCache[ path ]
   if cache then
      path = cache
   end
   if path:byte( 1 ) == 124 then  -- '|' 
      path = self.projDir .. path:sub( 2 )
   end

   if not baseDir or baseDir == "" then
      return path
   end

   if string.find( path, self.projDir, 1, true ) == 1 then
      path = "." .. path:sub( #self.projDir + 1 )
   end

   return path
end

function DBCtrl:convFullpath( path, currentDir )
   if path == "" then
      return ""
   end
   if not currentDir then
      currentDir = self.currentDir
   end
   path = string.gsub( path, ".*//", "/" )
   if string.find( path, "^[^/%.]" ) then
      path = currentDir .. "/" .. path
   else
      path = string.gsub( path, "^./", currentDir .. "/" )
      path = string.gsub( path, "^../", currentDir .. "/../" )
   end
   local nameList = {}
   for name in string.gmatch( path, "[^/]+" ) do
      if name == "." then
	 if #nameList == 0 then
	    table.insert( nameList, currentDir )
	 end
      elseif name == ".." then
	 if #nameList == 0 or nameList[ #nameList ] == ".." then
	    table.insert( nameList, ".." )
	 else
	    table.remove( nameList )
	 end
      else
	 table.insert( nameList, name )
      end
   end
   local path = ""
   for index, name in ipairs( nameList ) do
      path = path .. "/" .. name
   end
   return path
end

function DBCtrl:convPath( path )
   if path:byte( 1 ) == 124 then  -- '|' 
      return path
   end
   local cache = self.convPathCache[ path ] 
   if cache then
      return cache
   end

   local orgPath = path
   path = self:convFullpath( path )

   if string.find( path, self.projDir, 1, true ) == 1 then
      path = "|" .. path:sub( #self.projDir + 1 )
   end

   self.convPathCache[ orgPath ] = path
   self.convPathCache[ path ] = path
   return path
end


function DBCtrl:getFileInfo( id, path )
   local fileInfo
   if id then
      fileInfo = self.fileId2fileInfoMap[ id ]
      if fileInfo then
	 return fileInfo
      end
      fileInfo = self:getRow( "filePath", string.format( "id = %d", id ) )
      if not fileInfo then
	 return nil
      end
      self.path2fileInfoMap[ fileInfo.path ] = fileInfo
      self.fileId2fileInfoMap[ id ] = fileInfo
      fileInfo.incPosList = {}
      return fileInfo
   end

   if not path then
      path = ""
   else
      path = self:convPath( path )

      path = string.gsub( path, ".*//", "/" )
      if string.find( path, "^%." ) then
	 local nameList = {}
	 for name in string.gmatch( path, "[^/]" ) do
	    if name == "." then
	       if #nameList == 0 then
		  table.insert( nameList, self.currentDir )
	       end
	    elseif name == ".." then
	       if #nameList == 0 or nameList[ #nameList ] == ".." then
		  table.insert( nameList, ".." )
	       else
		  table.remove( nameList )
	       end
	    else
	       table.insert( nameList, name )
	    end
	 end
      end
   end

   fileInfo = self.path2fileInfoMap[ path ]
   if fileInfo then
      return fileInfo, path
   end
   fileInfo = self:getRow( "filePath", "path == '" .. path .. "'" )
   if not fileInfo then
      return nil, path
   end

   self.path2fileInfoMap[ path ] = fileInfo
   self.fileId2fileInfoMap[ fileInfo.id ] = fileInfo
   fileInfo.incPosList = {}
   return fileInfo, path
end

function DBCtrl:insert( tableName, values )
   self.db:insert( tableName, values )
end

function DBCtrl:update( tableName, set, condition )
   self.db:update( tableName, set, condition )
end

function DBCtrl:getFileOpt( filePath, target )
   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo = self:getFileInfo( nil, filePath )
   if not fileInfo then
      log( 1, "not regist file", filePath )
      os.exit( 1 )
   end
   if not target then
      target = ""
   end
   local compileOp = ""
   local compInfo = self:mapCompInfo(
      string.format( "target = '%s'", target ),
      function( item )
	 compileOp = item.compOp
	 return false
      end
   )

   -- コンパイルオプション文字列を、オプション配列に変換
   local optionList = {}
   for option in string.gmatch( compileOp, "([^ ]+)" ) do
      table.insert( optionList, option )
   end

   return fileInfo, optionList
end


function DBCtrl:infoAt( tableName, path, line, column )
   local info = nil
   local fileInfo = self:getFileInfo( nil, path )
   if not fileInfo then
      return nil
   end
   self:mapRowList(
      tableName,
      string.format( 
      	 "fileId = %d AND line <= %d AND endLine >= %d",
      	 fileInfo.id, line, line ),
      nil, nil,
      function( item )
	 if ( item.line < line and item.endLine > line ) or
	    ( item.line == line and item.column <= column and
		 item.endLine == line and item.endColumn >= column )
	 then
	    if not info then
	       info = item
	    elseif info.charSize > item.charSize then
	       info = item
	    end
	 end
	 return true
      end
   )
   return info
end

function DBCtrl:getNsInfoAt( path, line, column )
   local declInfo = self:infoAt( "symbolDecl", path, line, column )
   local refInfo = self:infoAt( "symbolRef", path, line, column )
   if not declInfo and not refInfo then
      return nil
   end
   local nsId
   if not declInfo and refInfo then
      nsId = refInfo.nsId
   elseif declInfo and not refInfo then
      nsId = declInfo.nsId
   elseif declInfo.charSize < refInfo.charSize then
      nsId = declInfo.nsId
   else
      nsId = refInfo.nsId
   end
   return self:getNamespace( nsId )
end

function DBCtrl:dump( level )
   if not level then
      level = 3
   end

   log( level, "-- comp db --", os.clock(), os.date()  )

   log( level, "-- table filePath -- " )
   log( level, "id", "incFlag", "mod.time", "digest" .. string.rep( ' ', 32 - 6 ),
	"path" )
   self:mapRowList(
      "filePath", nil, nil, nil,
      function( row ) 
	 log( level, row.id, row.incFlag,
	      row.modTime, row.digest, row.path,
	      row.currentDir, row.currentDir )
	 return true
      end
   )

   log( level, "-- table compileOp -- " )
   log( level, "fileId", "target", "path", "compOp" )
   self:mapRowList(
      "compileOp", nil, nil, nil,
      function( row )
	 local fileInfo = self:getFileInfo( row.id )
	 log( level, row.fileId, row.target, fileInfo.path, row.compOp )
	 return true
      end
   )
   
   log( level, "-- table incRef -- " )
   log( level, "incFile", "file", "line" )
   self:mapRowList(
      "incRef", nil, nil, nil,
      function( row )
	 local fileInfo = self:getFileInfo( row.id )
	 log( level, row.id, row.baseFileId, row.line, 
	      fileInfo and fileInfo.path or "<none>" )
	 return true
      end
   )

   log( level, "-- table namespace -- " )
   log( level, "id", "parent", "name" )
   self:mapRowList(
      "namespace", nil, nil, nil,
      function( row )
	 log( level,row.id, row.parentId, row.otherName,
	      row.otherName ~= row.name and row.name or "" )
	 return true
      end
   )

   log( level, "-- table simpleName -- " )
   log( level, "id", "name"  )
   self:mapRowList(
      "simpleName", nil, nil, nil,
      function( row ) 
	 log( level, row.id, row.name )
	 return true
      end
   )
   
   log( level, "-- table symbolDecl -- " )
   log( level, "nsId", "snameId", "type", "file", "line", "column", "eLine", "eColumn", "size", "comment", "name"  )
   self:mapRowList(
      "symbolDecl", nil, nil, nil,
      function( row ) 
	 local nsInfo = self:getNamespace( row.nsId )
	 log( level, row.nsId, row.snameId, row.type, row.fileId,
	      row.line, row.column, row.endLine, row.endColumn, row.charSize,
	      row.comment, nsInfo.otherName )
	 return true
      end
   )
   
   log( level, "-- table symbolRef -- " )
   log( level, "callee", "snameId", "fileId", "line", "colum",
	"eLine", "eColumn", "belong", "belong", "callee" )
   self:mapRowList(
      "symbolRef", nil, nil, nil,
      function( row ) 
	 local nsInfo = self:getNamespace( row.nsId )
	 local belongNsInfo = self:getNamespace( row.belongNsId )
	 log( level, row.nsId, row.snameId, row.fileId, row.line, row.column,
	      row.endLine, row.endColumn, row.charSize,
	      row.belongNsId, belongNsInfo and belongNsInfo.name or "<none>",
	      nsInfo and nsInfo.otherName )
	 return true
      end
   )

   log( level, "-- table incBelong -- " )
   log( level, "incFile", "baseFileId", "belong", "belongNs", "file" )
   self:mapRowList(
      "incBelong", nil, nil, nil,
      function( row ) 
	 local fileInfo = self:getFileInfo( row.id )
	 local belongNs = self:getNamespace( row.nsId )
	 log( level, row.id, row.baseFileId, row.nsId, belongNs.name, fileInfo.path )
	 return true
      end
   )

   log( level, "-- table tokenDigest -- " )
   log( level, "incFile", "digest" )
   self:mapRowList(
      "tokenDigest", nil, nil, nil,
      function( row ) 
	 local fileInfo = self:getFileInfo( row.fileId )
	 log( level, row.fileId, row.digest, fileInfo.path )
	 return true
      end
   )
   
   log( level, "-- table end -- " )
end


return DBCtrl
