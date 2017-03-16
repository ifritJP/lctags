-- Copyright (C) 2017 ifritJP


local clang = require( 'libclanglua.if' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local DBAccess = require( 'lctags.DBAccess' )
local Util = require( 'lctags.Util' )
local Query = require( 'lctags.Query' )

local function getFileLocation( cursor )
   local location = cursor:getCursorLocation()
   return clang.getFileLocation(
      location.__ptr, clang.core.clang_getFileLocation )
end

local rootNsId = 1
local userNsId = 2
local systemFileId = 1
local DB_VERSION = 5

local DBCtrl = {
   rootNsId = rootNsId,
   userNsId = userNsId,
   systemFileId = systemFileId,
}

function convProjPath( projDir, currentDir )
   if not projDir then
      projDir = currentDir
   end
   if projDir ~= "/" and not string.find( projDir, "/$" ) then
      projDir = projDir .. "/"
   end
   return projDir
end

function newObj( db, currentDir )
   local obj = {
      db = db,
      writeDb = db,
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
      -- true の場合、同名の構造体情報を個々に扱う
      individualStructFlag = false,
      -- true の場合、同名の型情報(enum,typedef,etc...)を個々に扱う
      individualTypeFlag = false,
      -- true の場合、同名のマクロシンボルを個々に扱う
      individualMacroFlag = false,
      getFileInfoFromCursor = function() return nil end
   }
   setmetatable( obj, { __index = DBCtrl } )
   
   return obj
end

function DBCtrl:init(
      path, currentDir, projDir,
      individualTypeFlag, individualStructFlag, individualMacroFlag )
   os.remove( path )

   if not currentDir then
      log( 1, "open error. currentDir is nil" )
      return nil
   end

   local db = DBAccess:open( path, false )
   if not db then
      log( 1, "open error." )
      return false
   end

   local obj = newObj( db, currentDir )

   obj:createTables()

   obj:setProjDir( path, projDir )
   
   obj:setEtc( "individualTypeFlag",
	       individualTypeFlag and "1" or "0" )
   obj:setEtc( "individualStructFlag",
	       individualStructFlag and "1" or "0" )
   obj:setEtc( "individualMacroFlag",
	       individualMacroFlag and "1" or "0" )

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

   obj:update( "filePath", "updateTime = 0", "incFlag = 0" )

   obj:close()

   return true
end

function DBCtrl:getEtc( key )
   return self:getRow( "etc", "keyName = '" .. key .. "'" )
end

function DBCtrl:setEtc( key, val )
   local keyTxt = "keyName = '" .. key .. "'"
   local valTxt = "val = '" .. val .. "'"
   if not self:getEtc( key ) then
      self:insert( "etc", keyTxt .. ", " .. valTxt )
   else
      self:update( "etc", valTxt, keyTxt )
   end
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
   local text = fileHandle:read( "*a" )
   fileHandle:close()
   return self:calcTextDigest( text )
end

function DBCtrl:calcTextDigest( text )
   local digestObj = Helper.openDigest( "md5" )
   digestObj:write( text )
   return digestObj:fix()
end


function DBCtrl:setProjDir( dbPath, projDir )
   projDir = self:convFullpath( convProjPath( projDir, self.currentDir ) )
   local dbFullPath = self:convFullpath( dbPath )
   local registProjDir = projDir
   if string.gsub( dbFullPath, '/[^/]+$', "" ) == projDir then
      registProjDir = "./"
   end
   self:setEtc( "projDir", registProjDir )
   self.projDir = projDir
end

function DBCtrl:getCountElement( tableName, condition )
   return self.db:getRowNumber( tableName, condition )
end

function DBCtrl:changeProjDir( path, currentDir, projDir )
   local obj = DBCtrl:open( path, false, currentDir )

   if not obj then
      return
   end

   obj:setProjDir( path, projDir )

   local currentTime = Helper.getCurrentTime()

   local maxNumber = obj:getCountElement( "filePath", nil )

   local index = 0
   obj:mapFile(
      nil,
      function( fileInfo )
	 index = index + 1
	 log( 1, string.format( "(%d/%d) %s", index, maxNumber, fileInfo.path ) )
	 if fileInfo.id ~= systemFileId then
	    local digest = obj:calcFileDigest( obj:getSystemPath( fileInfo.path ) )
	    if digest == fileInfo.digest then
	       obj:setUpdateTime( fileInfo.id, currentTime )
	    else
	       log( 1, "digest is difference", fileInfo.path )
	       obj:setUpdateTime( fileInfo.id, 0 )
	    end
	 end
	 return true
      end
   )
   obj:close()

   return true
end


function DBCtrl:checkRemovedFiles( dbPath )
   local obj = DBCtrl:open( dbPath, false, os.getenv( "PWD" ) )
   
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

   obj:close()
end



function DBCtrl:shrinkDB( path, full )
   DBCtrl:checkRemovedFiles( path )

   local obj = DBCtrl:open( path, false, os.getenv( "PWD" ) )
   
   if not obj then
      return false
   end

   if full then
      --- 存在しない名前空間、単純名を削除
      -- まずは全名前空間、全単純名の ID セットを取得

      log( 1, "step1" )
      local nsId2InfoMap = {}
      local snameIdSet = {}
      local count = 0
      obj:mapRowList(
	 "namespace", nil, nil, nil,
	 function( item )
	    if item.id >= userNsId then
	       nsId2InfoMap[ item.id ] = item
	       snameIdSet[ item.snameId ] = 1
	       count = count + 1
	    end
	    return true
	 end
      )
      log( 1, "namespace = ", count )
      
      -- 定義されている名前空間の親 ID のセットを登録
      log( 1, "step2" )
      local usingNsIdSet = {}
      count = 0
      obj:mapRowList(
	 "symbolDecl", nil, nil, nil,
	 function( item )
	    usingNsIdSet[ item.nsId ] = 1
	    snameIdSet[ item.snameId ] = nil
	    count = count + 1
	    return true
	 end
      )
      log( 1, "symbolDecl = ", count )
      
      -- 定義されている名前空間の親を辿って、親の定義状況を更新する
      log( 1, "step3" )
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
      log( 1, "step4" )
      for usingNsId in pairs( usingNsIdSet ) do
	 nsId2InfoMap[ usingNsId ] = nil
      end
      
      -- 残りのセットは定義されていない名前空間、単純名なので削除
      log( 1, "step5" )
      for nsId, nsInfo in pairs( nsId2InfoMap ) do
	 log( 2, "delete:", nsId, nsInfo.id, nsInfo.name )
	 obj:deleteNamespace( nsId )
      end
      for snameId in pairs( snameIdSet ) do
	 if snameId ~= 0 then
	    obj:delete( "simpleName", "id = " .. tostring( snameId ) )
	 end
      end
   end

   log( 1, "step6" )
   obj.db:commit()

   log( 1, "step7" )
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
   
   local obj = newObj( db, currentDir )

   local item = obj:getRow( "etc", "keyName = 'version'" )
   if not item then
      log( 1, "unknown version" )
      db:close()
      return nil
   end
   if tonumber( item.val ) ~= DB_VERSION then
      log( 1, "not support version.", item.val )
      db:close()
      return nil
   end

   if not readonly then
      db:begin()
   end

   local projDirInfo = obj:getEtc( "projDir" )

   obj.individualTypeFlag = obj:getEtc( "individualTypeFlag" ).val ~= '0'
   obj.individualStructFlag = obj:getEtc( "individualStructFlag" ).val ~= '0'
   obj.individualMacroFlag = obj:getEtc( "individualMacroFlag" ).val ~= '0'

   if not projDirInfo or projDirInfo.val == "" then
      log( 1, "db is not initialized" )
      obj:close()
      return nil
   end

   if projDirInfo.val == "./" then
      obj.projDir = string.gsub( obj:convFullpath( path ), '/[^/]+$', "" )
   else
      obj.projDir = projDirInfo.val
   end

   return obj
end


function DBCtrl:begin()
   self.db:begin()
end

function DBCtrl:beginForTemp()
   local obj = {
      insertList = {},
      nsInfoList = {},
   }
   function obj:exec( stmt, errHandle )
      log( -2, "not support" )
      os.exec( 1 )
   end
   function obj:delete( tableName, condition )
      log( -2, "not support" )
      os.exec( 1 )
   end
   function obj:insert( tableName, values )
      table.insert( self.insertList, { tableName, values } )
   end
   function obj:update( tableName, set, condition )
      log( -2, "not support" )
      os.exec( 1 )
   end

   self.writeDb = obj
end

function DBCtrl:commit()
   if self.writeDb ~= self.db then
      local writeDb = self.writeDb
      self.writeDb = self.db

      self.db:begin()
      log( 2, "merge begin:", os.clock(), os.date() )

      local tmpId2ActIdMap = {}
      for index, tmpInfo in ipairs( writeDb.nsInfoList ) do
	 self:insert( "simpleName",
		      string.format( "NULL, '%s'", tmpInfo.simpleName ) )
	 local snameInfo = self:getSimpleName( nil, tmpInfo.simpleName )
	 self:insert(
	    "namespace",
	    string.format(
	       "NULL, %d, %d, '%s', '%s', '%s'",
	       tmpInfo.parentId, tmpInfo.snameId, tmpInfo.digest,
	       tmpInfo.name, tmpInfo.otherName ) )
	 local nsInfo = self:getNamespace( nil, tmpInfo.name )

	 tmpId2ActIdMap[ tmpInfo.id ] = { nsInfo.id, snameInfo.id }
      end

      for index, insert in ipairs( writeDb.insertList ) do
	 local val = insert[ 2 ]
	 if string.find( val, "^%-" ) then
	    local index = string.find( val, ",", 1, true )
	    local tmpId = string.sub( val, 1, index - 1 )
	    local actIdList = tmpId2ActIdMap[ tonumber( tmpId ) ]
	    val = string.gsub(
	       val, string.format( "%%%s, %%%s", tmpId, tmpId ),
	       string.format( "%d, %d", actIdList[ 1 ], actIdList[ 2 ] ) )
	 end
	 self:insert( insert[ 1 ], val )
      end
      log( 2, "merge end:", os.clock(), os.date() )
      self.db:commit()
   else
      self.db:commit()
   end
end

function DBCtrl:close()
   self:commit()
   self.db:close()
end


function DBCtrl:createTables()
   self.writeDb:createTables(
      string.format(
	 [[
BEGIN;
CREATE TABLE etc ( keyName VARCHAR UNIQUE COLLATE binary PRIMARY KEY, val VARCHAR);
INSERT INTO etc VALUES( 'version', '%d' );
INSERT INTO etc VALUES( 'projDir', '' );
INSERT INTO etc VALUES( 'individualStructFlag', '0' );
INSERT INTO etc VALUES( 'individualTypeFlag', '0' );
INSERT INTO etc VALUES( 'individualMacroFlag', '0' );
CREATE TABLE namespace ( id INTEGER PRIMARY KEY, parentId INTEGER, snameId INTEGER, digest CHAR(32), name VARCHAR UNIQUE COLLATE binary, otherName VARCHAR COLLATE binary);
INSERT INTO namespace VALUES( NULL, 1, 0, '', '', '' );

CREATE TABLE simpleName ( id INTEGER PRIMARY KEY, name VARCHAR UNIQUE COLLATE binary);
CREATE TABLE filePath ( id INTEGER PRIMARY KEY, path VARCHAR UNIQUE COLLATE binary, updateTime INTEGER, incFlag INTEGER, digest CHAR(32), currentDir VARCHAR COLLATE binary, invalidSkip INTEGER);
INSERT INTO filePath VALUES( NULL, '', 0, 0, '', '', 1 );

CREATE TABLE compileOp ( fileId INTEGER, target VARCHAR COLLATE binary, compOp VARCHAR COLLATE binary );
CREATE TABLE symbolDecl ( nsId INTEGER, snameId INTEGER, parentId INTEGER, type INTEGER, fileId INTEGER, line INTEGER, column INTEGER, endLine INTEGER, endColumn INTEGER, charSize INTEGER, comment VARCHAR COLLATE binary, PRIMARY KEY( nsId, fileId, line ) );
INSERT INTO symbolDecl VALUES( 1, 1, 0, 1, 0, 0, 0, 0, 0, 0, '' );

CREATE TABLE symbolRef ( nsId INTEGER, snameId INTEGER, fileId INTEGER, line INTEGER, column INTEGER, endLine INTEGER, endColumn INTEGER, charSize INTEGER, belongNsId INTEGER, PRIMARY KEY( nsId, fileId, line, column ) );
CREATE TABLE funcCall ( nsId INTEGER, snameId INTEGER, belongNsId INTEGER, fileId INTEGER, line INTEGER, column INTEGER, endLine INTEGER, endColumn INTEGER, charSize, PRIMARY KEY( nsId, belongNsId ) );
CREATE TABLE incRef ( id INTEGER, baseFileId INTEGER, line INTEGER );
CREATE TABLE incCache ( id INTEGER, baseFileId INTEGER, incFlag INTEGER, PRIMARY KEY( id, baseFileId ) );
CREATE TABLE incBelong ( id INTEGER, baseFileId INTEGER, nsId INTEGER );
CREATE TABLE tokenDigest ( fileId INTEGER, digest CHAR(32), PRIMARY KEY( fileId, digest ) );
CREATE TABLE preproDigest ( fileId INTEGER, nsId INTEGER, digest CHAR(32), PRIMARY KEY( fileId, nsId, digest ) );
CREATE INDEX index_ns ON namespace ( id, parentId, snameId, name, otherName );
CREATE INDEX index_sName ON simpleName ( id, name );
CREATE INDEX index_filePath ON filePath ( id, path );
CREATE INDEX index_compOp ON compileOp ( fileId );
CREATE INDEX index_symDecl ON symbolDecl ( nsId, parentId, snameId, fileId );
CREATE INDEX index_symRef ON symbolRef ( nsId, snameId, fileId, belongNsId );
CREATE INDEX index_incRef ON incRef ( id, baseFileId );
CREATE INDEX index_incCache ON incCache ( id, baseFileId, incFlag );
CREATE INDEX index_incBelong ON incBelong ( id, baseFileId );
CREATE INDEX index_digest ON tokenDigest ( fileId, digest );
CREATE INDEX index_prepro ON preproDigest ( fileId, nsId, digest );
COMMIT;
]], DB_VERSION ) )
end

function DBCtrl:setFuncToGetFileInfoFromCursor( func )
   self.getFileInfoFromCursor = func
end

function DBCtrl:updateFile( fileInfo, removeFlag )
   if not fileInfo then
      return
   end
   local fileId = fileInfo.id
   if fileId == systemFileId then
      return
   end

   fileInfo.uptodate = false
   fileInfo.renew = true
   
   log( 1, "updateFile", fileInfo.path, fileId )
   self:delete( "symbolDecl", "fileId = " .. tostring( fileId ) )
   self:delete( "symbolRef", "fileId = " .. tostring( fileId ) )
   self:delete( "incRef", string.format( "baseFileId = %d", fileId ) )
   self:delete( "incCache", string.format( "baseFileId = %d", fileId ) )
   self:delete( "incBelong", string.format( "baseFileId = %d", fileId ) )
   self:delete( "tokenDigest", string.format( "fileId = %d", fileId ) )
   self:delete( "preproDigest", string.format( "fileId = %d", fileId ) )

   if removeFlag then
      self:delete( "filePath", string.format( "id = %d", fileId ) )
      self:delete( "incRef", string.format( "id = %d", fileId ) )
      self:delete( "incCache", string.format( "id = %d", fileId ) )
      self:delete( "incBelong", string.format( "id = %d", fileId ) )
      self.path2fileInfoMap[ fileInfo.path ] = nil
      self.fileId2fileInfoMap[ fileId ] = nil
   end
   
end


function DBCtrl:getFileIdLocation( cursor )
   local cxfile, line, column = getFileLocation( cursor )
   if not cxfile then
      return systemFileId, line, column
   end
   local fileInfo = self:getFileInfo( nil, cxfile:getFileName() )
   if not fileInfo then
      error( "not found file: " .. cxfile:getFileName() )
   end
   return fileInfo.id, line, column
end


function DBCtrl:exec( stmt, errHandle )
   self.writeDb:exec( stmt, errHandle )
end

function DBCtrl:delete( tableName, condition )
   self.writeDb:delete( tableName, condition )
end

function DBCtrl:insert( tableName, values )
   self.writeDb:insert( tableName, values )
end

function DBCtrl:update( tableName, set, condition )
   self.writeDb:update( tableName, set, condition )
end

function DBCtrl:exists( tableName, condition )
   local row = self:getRow( tableName, condition )
   return row ~= nil
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
   if not name then
      name = "@@@"
   end
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

function DBCtrl:equalsCompOp( fileInfo, compileOp, target )
   local condition
   if target then
      condition = string.format(
	 "fileId = %d and target = '%s'", fileInfo.id, target )
   else
      condition = string.format( "fileId = %d and compOp = '%s'",
				 fileInfo.id, compileOp )
   end
   
   local targetInfo = self:getRow( "compileOp", condition )
   if not targetInfo then
      log( 2, "not found compileOp:", fileInfo.id, target or "nil" )
      return nil
   end

   log( 3, "equalsCompOp:", compileOp, targetInfo.compOp )
   
   return targetInfo.compOp == compileOp
end

function DBCtrl:setUpdateTime( fileId, time )
   self:update(
      "filePath", "updateTime = " .. tostring( time ),
      "id = " .. tostring( fileId ) )
end

function DBCtrl:updateCompileOp( fileInfo, target, compileOp )
   self:update(
      "compileOp", "compOp = '" .. compileOp .. "'",
      string.format( "fileId = %d AND target = '%s'",
		     fileInfo.id, target ) )
end

function DBCtrl:insertCompileOP( fileId, target, compileOp )
   if not target then
      target = ""
   end
   self:insert(
      "compileOp",
      string.format( "%d, '%s', '%s'", fileId, target, compileOp ) )
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
   log( 3, "addFile:", filePath )
   if fileInfo then
      if fileInfo.id == systemFileId then
	 return fileInfo
      end
      if isTarget then
	 self.targetFileInfo = fileInfo
	 local equalsCompOpResult = self:equalsCompOp( fileInfo, compileOp, target )
	 if equalsCompOpResult == nil then
	    self:insertCompileOP( fileInfo.id, target, compileOp )
	    log( 2, "new compileOp" )
	 elseif not equalsCompOpResult then
	    self:updateCompileOp( fileInfo, target, compileOp )
	 else
	    -- コンパイルオプションが等しいので、
	    -- コンパイルオプションを含む systemFile は uptodate
	    local systemFIleInfo = self:getFileInfo( systemFileId, nil )
	    systemFIleInfo.uptodate = true
	    log( 2, "systemFile is uptodate" )
	 end
      end
      local modTime = Helper.getFileModTime( self:getSystemPath( fileInfo.path ) )
      if modTime > fileInfo.updateTime or
	 modTime > self.targetFileInfo.updateTime 
      then
	 -- ファイルの更新日時が違う
	 local fileDigest = self:calcFileDigest( filePath )
	 if fileDigest ~= fileInfo.digest then
	    -- ファイルの digest も違う場合は、登録情報を全て更新
	    log( 2, "detect mismatch digest", filePath, digest )
	    self:updateFile( fileInfo )
	    self:update(
	       "filePath", "digest = '" .. fileDigest .. "'",
	       string.format( "id = %d", fileInfo.id ) )
	 end
      end

      if self:existsFileWithTokenDigest( fileInfo.id, digest ) then
	 if fileInfo.uptodate == nil then
	    log( 3, "uptodate target", fileInfo.path )
	    fileInfo.uptodate = true
	 end
      else
	 log( 2, "detect new digest file", fileInfo.path, digest )
	 fileInfo.uptodate = false
	 self:addTokenDigest( fileInfo.id, digest )
      end
      
      self:setUpdateTime( fileInfo.id, time )
      return fileInfo
   end

   self:insert(
      "filePath",
      string.format(
	 "NULL, '%s', %d, %d, '%s', '%s', 0", filePath, time, isTarget and 0 or 1,
	 self:calcFileDigest( filePath ),
	 isTarget and self:convPath( currentDir ) or "" ) )

   fileInfo = self:getFileInfo( nil, filePath )

   if isTarget then
      self:insertCompileOP( fileInfo.id, target, compileOp )
      self:addTokenDigest( fileInfo.id, digest )
   end
   
   if isTarget then
      self.targetFileInfo = fileInfo
   end
   return fileInfo
end

function DBCtrl:makeNsInfo( nsId, snameId, parentId, digest, namespace, otherName )
   local nsInfo = {
      id = nsId,
      snameId = snameId,
      parentId = parentId,
      digest = digest,
      name = namespace, 
      otherName = otherName,
   }
   return nsInfo
end

function DBCtrl:makeSymbolDeclInfo( cursor, fileInfo, nsInfo )
   
   local startInfo, endInfo = self:getRangeFromCursor( cursor )
   local line, column =  startInfo[2], startInfo[3]

   local item = {}
   item.nsId = nsInfo.id
   item.parentId = nsInfo.parentId
   item.snameId = nsInfo.snameId
   item.type = cursor:getCursorKind()
   item.fileId = fileInfo.id
   item.line = line
   item.column = column
   item.endLine = endInfo[2]
   item.endColumn = endInfo[3]
   item.charSize = endInfo[4] - startInfo[4]
   item.comment = cursor:getRawCommentText()

   return item
end

function DBCtrl:addSymbolDecl( cursor, fileId, nsInfo )
   self.hashCursor2FullnameMap[ cursor:hashCursor() ] = nsInfo.name
   self.hashCursor2NSMap[ cursor:hashCursor() ] = nsInfo

   local fileInfo = self:getFileInfo( fileId )
   local item = self:makeSymbolDeclInfo( cursor, fileInfo, nsInfo )

   log( 3, "addSymbolDecl", fileInfo.id,
	fileInfo.uptodate, nsInfo.name, cursor:hashCursor() )

   if not fileInfo.uptodate then
      self:insert(
	 "symbolDecl",
	 string.format( "%d, %d, %d, %d, %d, %d, %d, %d, %d, %d, '%s'",
			item.nsId, item.snameId, item.parentId, item.type,
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
   if not otherName then
      otherName = namespace
   end
   
   if self.writeDb ~= self.db then
      local tmpId = -#self.writeDb.nsInfoList - 1
      local nsInfo = self:makeNsInfo( tmpId, tmpId, rootNsId, namespace, otherName )
      nsInfo.simpleName = simpleName
      table.insert( self.writeDb.nsInfoList, nsInfo )
      self:addSymbolDecl( cursor, fileId, nsInfo )
      return nsInfo
   end

      
   local snameInfo = self:addSimpleName( simpleName )

   log( 3, "addNamespaceOne: ", namespace )
   
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
   local fileInfo = self:getFileInfoFromCursor( cursor )
   if fileInfo then
      return fileInfo
   end
   
   local cxfile = getFileLocation( cursor )
   if not cxfile then
      return self:getFileInfo( systemFileId, nil )
   end
   local fileName = cxfile:getFileName()
   return self:getFileInfo( nil, fileName )
end

function DBCtrl:addNamespace( cursor )
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
   self:addNamespaceSub( cursor, fileInfo )
end

function DBCtrl:getFullname( cursor, fileId, anonymousName, typedefName )
   local spell = cursor:getCursorSpelling()
   local simpleName = spell
   local nsList = clang.getNamespaceList(
      cursor, nil,
      function( aCursor )
	 return self.hashCursor2FullnameMap[ aCursor:hashCursor() ]
      end
   )
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

   if ( cursor:getStorageClass() == clang.core.CX_SC_Static and
	   ( cursorKind == clang.core.CXCursor_FunctionDecl or
		cursorKind == clang.core.CXCursor_VarDecl ) ) or
      (self.individualMacroFlag and
	  cursorKind == clang.core.CXCursor_MacroDefinition ) or
      (self.individualStructFlag and
	  cursorKind == clang.core.CXCursor_StructDecl ) or
      (self.individualTypeFlag and
	  ( cursorKind == clang.core.CXCursor_EnumDecl or
	       cursorKind == clang.core.CXCursor_UnionDecl or
	       cursorKind == clang.core.CXCursor_UnionDecl or
	       cursorKind == clang.core.CXCursor_TypedefDecl ) )
   then
      -- 同名が存在する名前空間は、識別子としてファイル ID を付加する
      table.insert( nsList, string.format( "%d", fileId ) )
   end

   local fullname = ""
   for index, name in ipairs( nsList ) do
      local workNS
      if type( name ) == 'table' then
	 workNS = fullname .. "::" .. ( name[2] or "@@@" )
	 name = name[1]
	 fullname = fullname .. "::" .. ( name or "@@@" )
      else
	 fullname = fullname .. "::" .. ( name or "@@@" )
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
	    workNS = namespace .. "::" .. (name[2] or "@@@")
	    name = name[1]
	    namespace = namespace .. "::" .. (name or "@@@")
	 else
	    namespace = namespace .. "::" .. (name or "@@@")
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
      function( cursor, parent, exInfo, appendInfo )
   	 local cursorKind = cursor:getCursorKind()
   	 if cursorKind == clang.core.CXCursor_StructDecl or
   	    cursorKind == clang.core.CXCursor_UnionDecl
   	 then
   	    hasChild = true
   	    self:calcEnumStructDigest( cursor, kind, digest )
   	 elseif cursorKind == kind then
   	    hasChild = true
   	    local cxtype = cursor:getCursorType()
	    digest:write( tostring( appendInfo[ 2 ] ) )
   	    digest:write( cxtype:getTypeSpelling() )
   	    digest:write( cursor:getCursorSpelling() )
   	 end
      end,
      nil,
      { kind, clang.core.CXCursor_StructDecl,
   	clang.core.CXCursor_UnionDecl,
	clang.core.CXCursor_EnumDecl }, 1 )
	 

   if needFix then
      if not hasChild then
	 return ""
      end
      return digest:fix()
   end
end

function DBCtrl:addEnumStructDecl( decl, anonymousName, typedefName, kind, nsObj )
   local fileInfo = self:getFileFromCursor( decl )

   local otherNameBase
   local fullnameBase
   local baseNsId

   local structUptodate = false

   if not nsObj then
      log( 2, "addEnumStructDecl: nsObj is nil", decl:getCursorSpelling() )
   end

   local hasIncFlag = self:hasInc( fileInfo, decl )
   
   if fileInfo.uptodate and not hasIncFlag then
      -- ファイルが変更なしで、構造体内でインクルードしていない場合
      fullnameBase = self:getFullname(
	 decl, fileInfo.id, anonymousName, typedefName )
      self.hashCursor2FullnameMap[ decl:hashCursor() ] = fullnameBase
   else
      -- ファイルが変更ありか、構造体内でインクルードしている場合
      log( 3, "addEnumStructDecl start",
	   decl:getCursorSpelling(), typedefName, os.clock(), os.date() )
      -- local digest = self:calcEnumStructDigest( decl, kind )
      local digest = nsObj and nsObj.fixDigest or ""

      -- local parentKind = decl:getCursorSemanticParent():getCursorKind()
      -- if nsObj.parentNs and parentKind == clang.core.CXCursor_TranslationUnit then
      -- 	 -- 親の namespace があるはずが、 CXCursor_TranslationUnit のものは、
      -- 	 -- ソースコード中にバグがあるので登録しない
      -- 	 local cxfile, line, column = getFileLocation( decl )
      -- 	 log( 1, "addEnumStructDecl: illegal struct",
      -- 	      cxfile:getFileName(), line, column )
      -- 	 return
      -- end

      local declNs, symbolDecl
      declNs, symbolDecl, structUptodate = self:addNamespaceSub(
	 decl, fileInfo, digest, anonymousName, typedefName )
      fullnameBase = declNs.name
      otherNameBase = declNs.otherName
      baseNsId = declNs.id

      -- マクロ内で複数定義している場合、
      -- メンバー参照の nsInfo の解決が正常にできないので
      -- ヘッダ解析の skip は無効にする。
      local condition = string.format(
	 "nsId <> %d AND fileId = %d AND line = %d AND column = %d AND endLine = %d AND endColumn = %d AND type = %d",
	 symbolDecl.nsId, symbolDecl.fileId,
	 symbolDecl.line, symbolDecl.column,
	 symbolDecl.endLine, symbolDecl.endColumn, symbolDecl.type )
      local samePosDecl = self:getRow( "symbolDecl", condition )
      if samePosDecl then
	 local samePosDeclNsInfo = self:getNamespace( samePosDecl.nsId )
	 log( 1, "multiple decl in macro",
	      fullnameBase, samePosDeclNsInfo.name, fileInfo.path,
	      symbolDecl.line, symbolDecl.column )
	 self:update( "filePath", "invalidSkip = 1",
		      "id = " .. tostring( fileInfo.id ) )
      end
      
      if structUptodate then
	 log( 3, "addEnumStructDecl", "uptodate", fullnameBase )
      end
   end

   local count = 0
   local workFileInfo = fileInfo

   local prevFile = nil
   for index, info in ipairs( nsObj and nsObj.memberList or {} ) do
      local cursor = info[ 1 ]
      local cxfile = info[ 2 ]
      local cursorKind = cursor:getCursorKind()
      if not prevFile or not cxfile:isEqual( prevFile ) then
	 workFileInfo = self:getFileInfo( nil, cxfile:getFileName() )
      end
      prevFile = cxfile
      
      local name = cursor:getCursorSpelling()
      local fullname = fullnameBase .. "::" .. name
      log( 3, "addEnumStructDecl process", fullname, os.clock() )

      if not workFileInfo.renew and (workFileInfo.uptodate or structUptodate ) then
	 self.hashCursor2FullnameMap[ cursor:hashCursor() ] = fullname
      else
	 local otherName = otherNameBase .. "::" .. name
	 self:addNamespaceOne( cursor, "", workFileInfo.id,
			       baseNsId, name, fullname, otherName )
      end
   end
   
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

   if not nsInfo then
      symbolDeclInfo = self:infoAt(
	 "symbolDecl", fileId, line, column, cursor:getCursorKind() )
      if symbolDeclInfo then
	 nsInfo = self:getNamespace( symbolDeclInfo.nsId )
      end
   end
   self.hashCursor2FullnameMap[ hash ] = fullname
   self.hashCursor2NSMap[ hash ] = nsInfo
   return nsInfo
end


function DBCtrl:addReference( refInfo )
   local cursor = refInfo.cursor
   local declCursor = refInfo.declCursor

   local path = refInfo.cxfile and refInfo.cxfile:getFileName() or ""

   local srcFileInfo = self:getFileInfo( nil, path )
   if srcFileInfo then
      if srcFileInfo.uptodate then
	 log( 3, "uptodate ref", cursor:getCursorSpelling() )
	 return
      end
   else
      log( 3, "not found fileInfo",
	   cursor:getCursorSpelling(), cursor:hashCursor() )
   end

   -- local fileId, line = self:getFileIdLocation( cursor )
   local startInfo, endInfo = self:getRangeFromCursor( cursor )
   local line = startInfo and startInfo[ 2 ] or 0
   local filePath
   if startInfo and startInfo[ 1 ] then
      filePath = startInfo[ 1 ]:getFileName()
   end
   local fileInfo = startInfo and self:getFileInfo( nil, filePath )
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


   self:insert(
      "symbolRef",
      string.format( "%d, %d, %d, %d, %d, %d, %d, %d, %d",
		     nsInfo.id, nsInfo.snameId, fileId, line,
		     startInfo and startInfo[ 3 ] or 0,
		     endInfo and endInfo[ 2 ] or 0,
		     endInfo and endInfo[ 3 ] or 0,
		     startInfo and endInfo and endInfo[ 4 ] - startInfo[ 4 ],
		     parentNsInfo and parentNsInfo.id or 0 ) )
end



function DBCtrl:addCall( cursor, namespace )
   local declCursor = cursor:getCursorReferenced()
   if declCursor:isNull() then
      -- 演算で関数ポインタを取得しているような場合、
      -- getCursorReferenced() は無効になるので、
      -- 演算結果から関数ポインタの情報を取得する
      local result, childList = clang.getChildrenList( cursor, nil, 2 )
      for index, info in ipairs( childList ) do
	 local child = info[ 1 ]
	 declCursor = clang.getDeclCursorFromType( child:getCursorType() )
	 if declCursor then
	    local nsInfo = self:getNamespaceFromCursor( declCursor )
	    if nsInfo then
	       break
	    end
	 end
      end
   end
   
   -- local fileId, line = self:getFileIdLocation( cursor )
   local startInfo, endInfo = self:getRangeFromCursor( cursor )
   local line = startInfo and startInfo[ 2 ] or 0
   local fileInfo = startInfo and self:getFileInfo( nil, startInfo[ 1 ]:getFileName() )
   local fileId = fileInfo and fileInfo.id or systemFileId
   

   local parentNsInfo = self:getNamespaceFromCursor( namespace )
   local kind = declCursor:getCursorKind()

   local nsInfo = self:getNamespaceFromCursor( declCursor )

   log( 3, "addCallDecl:", cursor:getCursorSpelling(),
	declCursor:getCursorSpelling(),
	clang.getCursorKindSpelling( kind ) )

   if kind == clang.core.CXCursor_VarDecl or
      kind == clang.core.CXCursor_ParmDecl or
      kind == clang.core.CXCursor_FieldDecl
   then
      declCursor = clang.getDeclCursorFromType( declCursor:getCursorType() )
      kind = declCursor:getCursorKind()
      nsInfo = self:getNamespaceFromCursor( declCursor )
      if not nsInfo then
	 -- 関数ポインタ型の呼び出しで、
	 -- 名前空間の指定がないものは処理しない。
	 log( 3, "addCallDecl not found ns:" )
	 return
      end
   end

   log( 3, "addCall:", cursor:getCursorSpelling(),
	declCursor:getCursorSpelling(),
	clang.getCursorKindSpelling( kind ) )
   
   if not nsInfo then
      -- 宣言のないもの
      if kind == clang.core.CXCursor_Constructor then
	 -- コンストラクタで宣言がないものは、
	 -- デフォルトコンストラクタなので、あえて登録しない。
	 return
      end
      local name = cursor:getCursorSpelling()
      log( 3, "regist none call namespace",
	   name, declCursor:hashCursor(),
	   clang.getCursorKindSpelling( kind ),
	   declCursor:getCursorSpelling())
      nsInfo = self:addNamespaceOne(
	 declCursor, "", systemFileId, rootNsId, name, "::" .. name )
   end

   if fileInfo.uptodate then
      local declFileInfo = self:getFileFromCursor( declCursor )
      if declFileInfo.uptodate then
	 log( 3, "uptodate call", nsInfo.name )
	 return
      end
   end

   self:insert(
      "funcCall",
      string.format(
	 "%d, %d, %d, %d, %d, %d, %d, %d, %d",
	 nsInfo.id, nsInfo.snameId, parentNsInfo and parentNsInfo.id or 0,
	 fileId, line, startInfo and startInfo[ 3 ] or 0,
	 endInfo and endInfo[ 2 ] or 0,
	 endInfo and endInfo[ 3 ] or 0,
	 startInfo and endInfo and endInfo[ 4 ] - startInfo[ 4 ] ) )
end


function DBCtrl:mapSimpleName( condition, func )
   self.db:mapRowList( "simpleName", condition, nil, nil, func )
end

function DBCtrl:mapNamespace( condition, func )
   self.db:mapRowList( "namespace", condition, nil, nil, func )
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

function DBCtrl:mapIncRefFor( fileId, func )
   self:mapRowList(
      "incRef", string.format( "id = %d", fileId ), nil, nil, func )
end

function DBCtrl:existsPrepro( fileId, nsId, digest )
   return self:exists(
      "preproDigest",
      string.format( "fileId = %d AND digest = '%s' AND nsId = %d",
		     fileId, digest, nsId ) )
end

function DBCtrl:addPrepro( fullPath, belongNsName, digest )
   local fileInfo = self:getFileInfo( nil, fullPath )
   local nsInfo = self:getNamespace( nil, belongNsName )
   self:insert(
      "preproDigest", string.format(
	 "%d, %d, '%s'", fileInfo.id, nsInfo and nsInfo.id or rootNsId, digest ) )
end


function DBCtrl:existsFileWithTokenDigest( fileId, digest )
   return self:exists(
      "tokenDigest", string.format( "fileId = %d AND digest = '%s'",
				    fileId, digest ) )
end


function DBCtrl:addTokenDigest( fileId, digest )
   self:insert( "tokenDigest", string.format( "%d, '%s'", fileId, digest ) )
end


function DBCtrl:addInclude( cursor, digest, fileId2IncFileInfoListMap )
   local cxfile = cursor:getIncludedFile()
   local path = cxfile and cxfile:getFileName() or ""
   
   log( "inc:", path, digest )

   local fileInfo = self:getFileInfo( nil, path )

   local currentFile, line = getFileLocation( cursor )
   local currentFileInfo = self:getFileInfo( nil, currentFile:getFileName() )


   local incFileInfoList = fileId2IncFileInfoListMap[ currentFileInfo.id ]
   if not incFileInfoList then
      incFileInfoList = {}
      fileId2IncFileInfoListMap[ currentFileInfo.id ] = incFileInfoList
   end
   table.insert( incFileInfoList, fileInfo )
   

   local incInfo = self:getIncRef( currentFileInfo.id, fileInfo.id )

   table.insert( currentFileInfo.incPosList, line )

   if not self:existsFileWithTokenDigest( fileInfo.id, digest ) then
      log( 2, "detect new digest inc", fileInfo.path, digest )
      self:addTokenDigest( fileInfo.id, digest )
      fileInfo.uptodate = false
   elseif fileInfo.uptodate == nil then
      fileInfo.uptodate = true
   end

   if incInfo then 
      return fileInfo
   end

   self:insert( "incRef",
		string.format( "%d, %d, %d",
			       fileInfo.id, currentFileInfo.id, line ) )
   return fileInfo
end


function DBCtrl:addIncBelong( incBelong )
   if not incBelong.cxfile then
      return
   end
   local path = incBelong.cxfile:getFileName()
   log( 3, "incBelong:", incBelong.namespace,
	incBelong.namespace and incBelong.namespace:getCursorSpelling(), path )

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

function DBCtrl:mapDeclAtFile( fileId, func, ... )
   self:mapRowList(
      "symbolDecl", "fileId = " .. tostring( fileId ), nil, nil, func, ... )
end

function DBCtrl:mapDeclForParent( parentId, func, ... )
   self:mapRowList(
      "symbolDecl", "parentId = " .. tostring( parentId ), nil, nil, func, ... )
end

function DBCtrl:mapDecl( nsId, func, ... )
   self:mapRowList(
      "symbolDecl", "nsId = " .. tostring( nsId ), nil, nil, func, ... )
end

function DBCtrl:mapSymbolRefInfoList( symbol, func, ... )
   return self:mapSymbolInfoList( "symbolRef", symbol, func, ... )
end

function DBCtrl:mapSymbolRef( nsId, func, ... )
   return self:mapRowList(
      "symbolRef", "nsId = " .. tostring( nsId ), nil, nil, func, ... )
end

function DBCtrl:mapSymbolRefFrom( nsId, func, ... )
   return self:mapRowList(
      "symbolRef", "belongNsId = " .. tostring( nsId ), nil, nil, func, ... )
end

function DBCtrl:SymbolRefInfoListForCursor( cursor, func, ... )
   local fileId, line = self:getFileIdLocation( cursor )
   local snameInfo = self:getSimpleName( nil, cursor:getCursorSpelling() )

   if not snameInfo then
      return
   end

   -- cursor の symbolDecl を次の条件で検索する。
   --    単純名ID, ファイルID、行番号、列が等しい
   -- 同じ単純名のメンバーを持つ構造体をマクロ内で複数宣言した場合、
   -- symbolDecl が複数ヒットしてしまうが、
   -- レアケースなのでここではすべて対象とする
   local symbolDeclInfoList = self:getRowList(
      "symbolDecl",
      string.format( "fileId = %d AND (line = %d OR endLine = %d) AND snameId = %d",
		     fileId, line, line, snameInfo.id ) )

   for index, symbolDecl in ipairs( symbolDeclInfoList ) do
      self:mapRowList(
	 "symbolRef",
	 "nsId = " .. tostring( symbolDecl.nsId ), nil, nil, func, ... )
   end
end

function DBCtrl:SymbolDefInfoListForCursor( cursor, func, ... )
   local nsInfo = self:getNamespaceFromCursor( cursor )
   if nsInfo then
      self:mapRowList(
	 "symbolDecl",
	 string.format( "nsId = %d", nsInfo.id ), nil, nil, func, ... )
      return
   end

   local kind = cursor:getCursorKind()
   if kind == clang.core.CXCursor_FunctionDecl then
      self:mapDeclInfoList( cursor:getCursorSpelling(), func, ... )
      return
   elseif kind == clang.core.CXCursor_VarDecl or
      kind == clang.core.CXCursor_ParmDecl
   then
      local startInfo, endInfo = self:getRangeFromCursor( cursor )
      local fileInfo = self:getFileInfo( nil, startInfo[ 1 ]:getFileName() )
      local typeCursor = clang.getDeclCursorFromType( cursor:getCursorType() )
      local nsInfo = self:getNamespaceFromCursor( typeCursor )

      if not nsInfo then
	 nsInfo = self:makeNsInfo( rootNsId, rootNsId, rootNsId, "", "", "" )
	 log( 2, "type is unknown" )
      end

      if nsInfo then
	 func( self:makeSymbolDeclInfo( cursor, fileInfo, nsInfo ), ... )
      end
      return
   end

   
   local fileId, line = self:getFileIdLocation( cursor )
   local snameInfo = self:getSimpleName( nil, cursor:getCursorSpelling() )
   if snameInfo then
      log( 2, "SymbolDefInfoListForCursor", fileId, line, snameInfo.name, snameInfo.id )
      local findFlag = false
      local params = { ... }
      self:mapRowList(
	 "symbolDecl",
	 string.format( "fileId = %d AND (line = %d OR endLine = %d) AND snameId = %d",
			fileId, line, line, snameInfo.id ), nil, nil,
	 function( item )
	    findFlag = true
	    return func( item, table.unpack( params ) )
	 end
      )
      if findFlag then
	 return
      end
   end

   
   self:mapRowList(
      "symbolDecl",
      string.format( "fileId = %d AND (line = %d OR endLine = %d) AND type = %d",
		     fileId, line, line, cursor:getCursorKind() ),
      nil, nil, func, ... )
end


function DBCtrl:mapCallForCursor( cursor, func, ... )
   local kind = cursor:getCursorKind()
   if kind == clang.core.CXCursor_ParmDecl or
      kind == clang.core.CXCursor_VarDecl or
      kind == clang.core.CXCursor_FieldDecl
   then
      cursor = clang.getDeclCursorFromType( cursor:getCursorType() )
   end
   
   local fileId, line = self:getFileIdLocation( cursor )
   local snameInfo = self:getSimpleName( nil, cursor:getCursorSpelling() )

   -- cursor の symbolDecl を次の条件で検索する。
   --    単純名ID, ファイルID、行番号、列が等しい
   -- 同じ単純名のメンバーを持つ構造体をマクロ内で複数宣言した場合、
   -- symbolDecl が複数ヒットしてしまうが、
   -- レアケースなのでここではすべて対象とする
   local symbolDeclInfoList = self:getRowList(
      "symbolDecl",
      string.format( "fileId = %d AND (line = %d OR endLine = %d) AND snameId = %d",
		     fileId, line, line, snameInfo.id ) )

   for index, symbolDecl in ipairs( symbolDeclInfoList ) do
      self:mapRowList(
	 "funcCall",
	 "nsId = " .. tostring( symbolDecl.nsId ), nil, nil, func, ... )
   end
end


function DBCtrl:mapCall( condition, func, ... )
   self:mapRowList(
      "funcCall", condition, nil, nil, func, ... )
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

   if string.find( path, baseDir, 1, true ) == 1 then
      path = "." .. path:sub( #baseDir + 1 )
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
   -- foo//bar を foo/bar に変換
   path = string.gsub( path, "(.*)//", "%1/" )

   if string.find( path, "^%./" ) then
      -- ./foo -> PWD/foo
      path = string.gsub( path, "^%./", currentDir .. "/" )
   elseif string.find( path, "^%.%./" ) then
      -- ../foo -> PWD/../foo
      path = string.gsub( path, "^%.%./", currentDir .. "/../" )
   elseif string.find( path, "^[^/]" ) then
      -- / 以外で始まっているパスを、カレントからの絶対パスに変換
      path = currentDir .. "/" .. path
   end
   -- ./, ../ の除去
   local nameList = {}
   for name in string.gmatch( path, "[^/]+" ) do
      if name == "." then
	 if #nameList == 0 then
	    table.insert( nameList, currentDir )
	 end
      elseif name == ".." then
	 if #nameList == 0 or nameList[ #nameList ] == ".." then
	    log( 1, "illegal path", path )
	    os.exit( 1 )
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
	 return fileInfo, fileInfo.path
      end
      fileInfo = self:getRow( "filePath", string.format( "id = %d", id ) )
      if not fileInfo then
	 return nil
      end
      local work = self.path2fileInfoMap[ fileInfo.path ]
      if work then
	 return work
      end
      log( 3, "getFileInfo new", fileInfo.id, fileInfo.path )
      self.path2fileInfoMap[ fileInfo.path ] = fileInfo
      self.fileId2fileInfoMap[ id ] = fileInfo
      fileInfo.incPosList = {}
      return fileInfo, fileInfo.path
   end

   if not path then
      path = ""
   else
      path = self:convPath( path )
   end

   fileInfo = self.path2fileInfoMap[ path ]
   if fileInfo then
      return fileInfo, path
   end
   fileInfo = self:getRow( "filePath", "path == '" .. path .. "'" )
   if not fileInfo then
      return nil, path
   end

   local work = self.fileId2fileInfoMap[ fileInfo.id ]
   if work then
      return work
   end
   log( 3, "getFileInfo new", fileInfo.id, fileInfo.path )
   self.path2fileInfoMap[ path ] = fileInfo
   self.fileId2fileInfoMap[ fileInfo.id ] = fileInfo
   fileInfo.incPosList = {}
   return fileInfo, path
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
   local compileOp = nil
   local compInfo = self:mapCompInfo(
      string.format( "fileId = %d AND target = '%s'", fileInfo.id, target ),
      function( item )
	 compileOp = item.compOp
	 return false
      end
   )
   if not compileOp then
      return fileInfo, nil
   end

   -- コンパイルオプション文字列を、オプション配列に変換
   local optionList = {}
   for option in string.gmatch( compileOp, "([^ ]+)" ) do
      table.insert( optionList, option )
   end

   return fileInfo, optionList
end


function DBCtrl:infoAt( tableName, fileId, line, column, kind )
   local info = nil
   local condition = string.format( 
      "fileId = %d AND line <= %d AND endLine >= %d", fileId, line, line )
   if kind then
      condition = condition .. " AND type = " .. tostring( kind )
   end
   self:mapRowList(
      tableName, condition, nil, nil,
      function( item )
	 if ( item.line < line and item.endLine > line ) or
	    ( item.line == line and item.column <= column or
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

function DBCtrl:getNsInfoAt( fileInfo, line, column, fileContents )
   if not fileInfo then
      return nil
   end
   local declInfo = self:infoAt( "symbolDecl", fileInfo.id, line, column )
   local refInfo = self:infoAt( "symbolRef", fileInfo.id, line, column )
   log( 2, declInfo and declInfo.nsId, refInfo and refInfo.nsId )
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
   elseif declInfo.charSize > refInfo.charSize then
      nsId = refInfo.nsId
   else
      local token = Util:getToken(
	 self:getSystemPath( fileInfo.path ), line, column, fileContents )
      log( 2, "token", token )
      if self:getSimpleName( declInfo.snameId ).name == token then
	 nsId = declInfo.nsId
      else
	 nsId = refInfo.nsId
      end
   end
   return self:getNamespace( nsId )
end

function DBCtrl:dumpCompieOp( level, path )
   log( level, "-- table compileOp -- " )
   log( level, "fileId", "target", "path", "compOp" )

   local condition
   if path then
      local fileInfo = self:getFileInfo( nil, path )
      if not fileInfo then
	 return
      end
      condition = "fileId = " .. tostring( fileInfo.id )
      
   end
   self:mapRowList(
      "compileOp", condition, nil, nil,
      function( row )
	 local fileInfo = self:getFileInfo( row.fileId )
	 log( level, row.fileId, row.target, fileInfo.path, row.compOp )
	 return true
      end
   )
end

function DBCtrl:dumpFile( level, path )
   log( level, "-- table filePath -- " )
   log( level, "id", "incFlag", "skip", "upTime", "digest" .. string.rep( ' ', 32 - 6 ),
	"path" )

   local condition
   if path then
      local fileInfo = self:getFileInfo( nil, path )
      if not fileInfo then
	 return
      end
      condition = "id = " .. tostring( fileInfo.id )
   end
   self:mapRowList(
      "filePath", condition, nil, nil,
      function( row ) 
	 log( level, row.id, row.incFlag, row.invalidSkip == 0 and 'o' or 'x',
	      row.updateTime, row.digest, row.path, row.currentDir )
	 return true
      end
   )
end

function DBCtrl:dumpIncCache( level, path )
   log( level, "-- table incCache -- " )
   log( level, "beseFileId", "id", "incFlag", "basePath", "path" )

   local condition
   if path then
      local fileInfo = self:getFileInfo( nil, path )
      if not fileInfo then
	 return
      end
      condition = "baseFileId = " .. tostring( fileInfo.id )
   end
   self:mapRowList(
      "incCache", condition, nil, nil,
      function( row )
	 local baseFileInfo = self:getFileInfo( row.baseFileId )
	 local incFileInfo = self:getFileInfo( row.id )
	 log( level, row.baseFileId, row.id, row.incFlag, 
	      baseFileInfo.path, incFileInfo.path )
	 return true
      end
   )
end

function DBCtrl:dumpTokenDigest( level, path )
   log( level, "-- table tokenDigest -- " )
   log( level, "incFile", "digest" )

   local condition
   if path then
      local fileInfo = self:getFileInfo( nil, path )
      if not fileInfo then
	 return
      end
      condition = "fileId = " .. tostring( fileInfo.id )
   end
   
   self:mapRowList(
      "tokenDigest", condition, nil, nil,
      function( row ) 
	 local fileInfo = self:getFileInfo( row.fileId )
	 log( level, row.fileId, row.digest, fileInfo.path )
	 return true
      end
   )
end


function DBCtrl:dumpPreproDigest( level, path )
   log( level, "-- table preproDigest -- " )
   log( level, "incFile", "nsId", "digest", "path", "ns" )

   local condition
   if path then
      local fileInfo = self:getFileInfo( nil, path )
      if not fileInfo then
	 return
      end
      condition = "fileId = " .. tostring( fileInfo.id )
   end
   
   self:mapRowList(
      "preproDigest", condition, nil, nil,
      function( row )
	 local fileInfo = self:getFileInfo( row.fileId )
	 local nsInfo = self:getNamespace( row.nsId )
	 log( level, row.fileId, nsInfo.id, row.digest, fileInfo.path, nsInfo.name )
	 return true
      end
   )
end


function DBCtrl:dump( level )
   if not level then
      level = 3
   end

   log( level, "-- dump db --", os.clock(), os.date()  )

   self:mapRowList(
      "etc", nil, nil, nil,
      function( row ) 
	 log( level, row.keyName, row.val )
	 return true
      end
   )
   

   self:dumpFile( level, nil )

   self:dumpCompieOp( level, nil )
   
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

   self:dumpIncCache( level, nil )

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
	      row.comment, nsInfo and nsInfo.otherName )
	 return true
      end
   )
   
   log( level, "-- table symbolRef -- " )
   log( level, "refed", "snameId", "fileId", "line", "colum",
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


   log( level, "-- table funcCall -- " )
   log( level, "callee", "snameId", "caller", "fileId", "line", "CALLEE", "CALLER" )
   self:mapRowList(
      "funcCall", nil, nil, nil,
      function( row )
	 local nsInfo = self:getNamespace( row.nsId )
	 local belongNsInfo = self:getNamespace( row.belongNsId )
	 log( level, row.nsId, row.snameId, row.belongNsId,
	      row.fileId, row.line, nsInfo and nsInfo.otherName, 
	      belongNsInfo and belongNsInfo.name or "<none>" )
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

   self:dumpTokenDigest( level )

   self:dumpPreproDigest( level )
   
   log( level, "-- table end -- " )
end

function DBCtrl:removeTarget( dbPath, target )
   local obj = DBCtrl:open( dbPath, false, os.getenv( "PWD" ) )
   
   if not obj then
      return false
   end

   obj:delete( "compileOp", string.format( "target = '%s'", target or ""  ) )

   obj:close()
end

function DBCtrl:hasTarget( fileId, target )
   local condition = string.format( "target = '%s'", target )
   if fileId then
      condition = condition .. " AND fileId = " .. tostring( fileId )
   end
   return self:exists( "compileOp", condition )
end

-- fileInfo をインクルードしているソースファイルでターゲットが target のものを1つ返す。
-- fileInfo がソースファイルで、ターゲットが targetfileInfo ある場合 fileInfo を返す。
-- ない場合は nil を返す
function DBCtrl:getSrcForIncOne( fileInfo, target )
   if not target then
      target = ""
   end

   local srcFileInfo
   
   if fileInfo.incFlag ~= 0 then
      self:mapRowList(
	 "incCache",
	 string.format(  "id = %d AND incFlag = 0", fileInfo.id ), nil, nil,
	 function( item )
	    local baseId = item.baseFileId
	    if self:hasTarget( baseId, target ) then
	       srcFileInfo = workFileInfo
	       return false
	    end
	    return true
	 end
      )
   else
      if self:hasTarget( fileInfo.id, target ) then
	 srcFileInfo = fileInfo
	 return false
      end
   end

   if not srcFileInfo then
      log( 1, "getSrcForIncOne: not found souce", fileInfo.path )
   end
   return srcFileInfo
end


function DBCtrl:mapIncludeCache( fileInfo, func )
   self:mapRowList(
      "incCache", "baseFileId = " .. tostring( fileInfo.id ), nil, nil, func )
end

function DBCtrl:getIncludeCache( fileInfo )
   local incFileIdSet = {}
   self:mapIncludeCache(
      fileInfo,
      function( item )
	 incFileIdSet[ item.id ] = 1
	 return true
      end
   )
   return incFileIdSet
end

function DBCtrl:addIncludeCache( targetFileInfo, fileId2IncFileInfoListMap )

   -- fileId 毎の全 include 関係を辿ったインクルードファイルセットのマップ
   local fileId2IncCacheSetMap = {}
   -- キャッシュ先の incCacheSet のリスト
   local incCacheSetList = {}

   self:addIncludeCacheSub(
      targetFileInfo, fileId2IncFileInfoListMap,
      fileId2IncCacheSetMap, incCacheSetList )
   
   for fileId, incCacheSet in pairs( fileId2IncCacheSetMap ) do
      local workFileInfo = self:getFileInfo( fileId )
      if not workFileInfo.uptodate then
   	 for incFileInfo in pairs( incCacheSet ) do
   	    self:insert(
   	       "incCache",
	       string.format( "%d, %d, %d",
			      incFileInfo.id, fileId, workFileInfo.incFlag ) )
   	 end
      end
   end
end

function DBCtrl:addIncludeCacheSub(
      fileInfo, fileId2IncFileInfoListMap,
      fileId2IncCacheSetMap, incCacheSetList )

   local incFileInfoList = fileId2IncFileInfoListMap[ fileInfo.id ]
   local incCacheSet = {}
   fileId2IncCacheSetMap[ fileInfo.id ] = incCacheSet

   table.insert( incCacheSetList, incCacheSet )

   if incFileInfoList then
      for index, incFileInfo in ipairs( incFileInfoList ) do
	 if not fileId2IncCacheSetMap[ incFileInfo.id ] then
	    for fileId, incFileIdSet in pairs( incCacheSetList ) do
	       incFileIdSet[ incFileInfo ] = 1
	    end
	    self:addIncludeCacheSub(
	       incFileInfo, fileId2IncFileInfoListMap,
	       fileId2IncCacheSetMap, incCacheSetList )
	 end
      end
   end

   table.remove( incCacheSetList )
end



return DBCtrl
