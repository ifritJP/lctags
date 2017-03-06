-- Copyright (C) 2017 ifritJP

local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local Query = require( 'lctags.Query' )
local Util = require( 'lctags.Util' )
local OutputCtrl = require( 'lctags.OutputCtrl' )


local function dumpCursorInfo( cursor, depth, prefix )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()

   log( 4, string.format(
   	   "%s %s%s %s(%d) %d %s",
   	   string.rep( "  ", depth ),
   	   prefix and (prefix .. " ") or "", txt, 
   	   clang.getCursorKindSpelling( cursorKind ), cursorKind,
   	   cursor:hashCursor(), cursor:getRawCommentText() or "" ) )
end

local function getFileLoc( cursor )
   return clang.getFileLocation(
      cursor:getCursorLocation().__ptr, clang.core.clang_getFileLocation )
end

local function isFuncDecl( cursorKind )
   return cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_Destructor or
      cursorKind == clang.core.CXCursor_FunctionDecl
end

local function isNamespaceDecl( cursorKind )
   return cursorKind == clang.core.CXCursor_Namespace or
      cursorKind == clang.core.CXCursor_ClassDecl or
      cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_FunctionDecl or
      cursorKind == clang.core.CXCursor_Constructor or
      cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_EnumDecl
end

local function calcDigestTxt( txt, spInfo )
   if not spInfo.digest then
      return
   end
   spInfo.digest:write( txt )
   if spInfo.recordDigest then
      spInfo.recordDigest:write( txt .. "\n" )
   end
end

local function calcDigest( cursor, spInfo )
   if not cursor then
      return
   end
   calcDigestTxt( cursor:getCursorSpelling(), spInfo )
end

local targetKindList = {
   clang.core.CXCursor_MacroDefinition,
   clang.core.CXCursor_MacroExpansion,
   clang.core.CXCursor_UnexposedDecl,
   clang.core.CXCursor_InclusionDirective,
   clang.core.CXCursor_Namespace,
   clang.core.CXCursor_ClassDecl,
   clang.core.CXCursor_StructDecl,
   clang.core.CXCursor_UnionDecl,
   clang.core.CXCursor_EnumDecl,
   clang.core.CXCursor_EnumConstantDecl,
   clang.core.CXCursor_FieldDecl,
   clang.core.CXCursor_CXXMethod,
   clang.core.CXCursor_Destructor,
   clang.core.CXCursor_FunctionDecl,
   clang.core.CXCursor_Constructor,
   clang.core.CXCursor_DeclRefExpr,
   clang.core.CXCursor_MemberRefExpr,
   --clang.core.CXCursor_ParmDecl,
   clang.core.CXCursor_TypedefDecl,
   clang.core.CXCursor_TypeRef,
   clang.core.CXCursor_VarDecl,
   clang.core.CXCursor_CallExpr,
}

local function visitFuncMain( cursor, parent, analyzer, exInfo )
   local cursorKind = cursor:getCursorKind()

   dumpCursorInfo( cursor, analyzer.depth )

   local uptodateFlag
   local cursorOffset = tostring( exInfo[ 2 ] )
   
   if exInfo[ 1 ] then
      local cxfile, line = getFileLoc( cursor )
      if analyzer.currentFile ~= cxfile and
	 ( not analyzer.currentFile or not cxfile or
	      not analyzer.currentFile:isEqual( cxfile ) )
      then
	 local path = ""
	 if cxfile then
	    path = DBCtrl:convFullpath( cxfile:getFileName(), analyzer.currentDir )
	 end

	 table.insert(
	    analyzer.incBelongList,
	    { cxfile = cxfile, namespace = analyzer:getNowNs() } )

	 analyzer.currentFile = cxfile
	 local spInfo = analyzer.path2InfoMap[ path ]
	 if not spInfo then
	    spInfo = analyzer:createSpInfo( path, false )
	 end
	 analyzer.currentSpInfo = spInfo
	 uptodateFlag = spInfo.uptodateFlag
	 log( 3, "changeCurrentFile:", path, 
	      analyzer.currentSpInfo.digest, uptodateFlag )
      end
   end

   local endProcess = {}

   local recursiveFlag = analyzer.recursiveBaseKind ~= clang.core.CXCursor_InvalidFile
   if not recursiveFlag then
      if isNamespaceDecl( cursorKind ) then
	 analyzer:enterNs( cursor, cursorKind )
	 table.insert( endProcess,
		       function() analyzer:exitNs() end )
      end
   end

   if cursorKind == clang.core.CXCursor_Namespace then
      table.insert( analyzer.nsList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )
   end
   
   if cursorKind == clang.core.CXCursor_InclusionDirective then
      local cxfile = cursor:getIncludedFile()
      local path = ""
      if cxfile then
	 path = DBCtrl:convFullpath( cxfile:getFileName(),analyzer.currentDir )
      end
      local spInfo = analyzer.path2InfoMap[ path ]
      if not spInfo then
	 spInfo = analyzer:createSpInfo( path, true, cxfile )
      end
      
      table.insert( analyzer.incList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )
      return 1
   elseif cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_ClassDecl or
      cursorKind == clang.core.CXCursor_EnumDecl
   then
      if cursor:getCursorSpelling() == "" then
	 analyzer.currentSpInfo.anonymousCount =
	    analyzer.currentSpInfo.anonymousCount + 1
      end
      local list
      if cursorKind == clang.core.CXCursor_EnumDecl then
	 list = analyzer.enumList
      elseif cursorKind == clang.core.CXCursor_StructDecl then
	 list = analyzer.structList
      elseif cursorKind == clang.core.CXCursor_UnionDecl then
	 list = analyzer.unionList
      elseif cursorKind == clang.core.CXCursor_ClassDecl then
	 list = analyzer.classList
      end
	 
      table.insert( list, { cursor, analyzer.currentSpInfo } )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )
   elseif cursorKind == clang.core.CXCursor_FunctionDecl or
      cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_Constructor      
   then
      table.insert( analyzer.funcList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )

      local declCursor = clang.getDeclCursorFromType(
	 cursor:getCursorType():getResultType() )
      analyzer:addTypeRef( cursor, declCursor )
      
   elseif cursorKind == clang.core.CXCursor_MacroDefinition then
      table.insert( analyzer.macroDefList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )
   elseif cursorKind == clang.core.CXCursor_MacroExpansion then
      local declCursor = cursor:getCursorReferenced()
      table.insert( analyzer.macroRefList,
		    { cursor = cursor, declCursor = declCursor, namespace = nil } )
      -- マクロ参照はヘッダの多重 include 抑止に利用していることが多い。
      -- これを digest 計算に加えると、include 抑止の ifdef が差分で引っかかるので
      -- digest には加えずに、ハッシュだけ登録する
      -- analyzer:registCursor( cursor )
      analyzer.cursorHash2SpInfoMap[ cursor:hashCursor() ] = analyzer.currentSpInfo
   elseif clang.isReference( cursorKind ) then
      if cursorKind ~= clang.core.CXCursor_NamespaceRef then
	 local declCursor = cursor:getCursorReferenced()
	 local namespace = analyzer:getNowNs()
	 table.insert(
	    analyzer.refList,
	    { cursor = cursor, declCursor = declCursor,
	      namespace = namespace } )
	 calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
	 analyzer:registCursor( cursor )
	 calcDigest( declCursor, analyzer.currentSpInfo )
	 calcDigest( namespace, analyzer.currentSpInfo )
      end
   elseif cursorKind == clang.core.CXCursor_DeclRefExpr or
      cursorKind == clang.core.CXCursor_MemberRefExpr
   then
      local declCursor = cursor:getCursorReferenced()
      if declCursor:getCursorKind() ~= clang.core.CXCursor_ParmDecl then
	 local storageClass = declCursor:getStorageClass()
	 if storageClass ~= clang.core.CX_SC_Auto and
	    storageClass ~= clang.core.CX_SC_Register
	 then
	    local namespace = analyzer:getNowNs()
	    table.insert(
	       analyzer.refList,
	       { cursor = cursor, declCursor = declCursor,
		 namespace = namespace } )
	    calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
	    analyzer:registCursor( cursor )
	    calcDigest( declCursor, analyzer.currentSpInfo )
	    calcDigest( namespace, analyzer.currentSpInfo )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_CallExpr then
      local namespace = analyzer:getNowNs()
      table.insert( analyzer.callList,
		    { cursor = cursor, namespace = namespace } )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )
      calcDigest( namespace, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_TypedefDecl then
      table.insert( analyzer.typedefList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor )

      local isFuncFlag = false
      if cursor:getCursorResultType().__ptr.kind ~= clang.core.CXType_Invalid then
	 isFuncFlag = true
      end
      local srcCursor
      clang.visitChildrenFast(
	 cursor,
	 function ( aCursor, aParent, aExInfo, append )
	    if not srcCursor then
	       srcCursor = aCursor
	    end
	    if isFuncFlag then
	       visitFuncMain( aCursor, aParent, analyzer, append )
	    else
	       return clang.CXChildVisitResult.Break.val
	    end
	 end, analyzer, targetKindList, isFuncFlag and 2 or 1 )

      if srcCursor then
	 analyzer.hashCursor2TypedefMap[ srcCursor:hashCursor() ] =
	    { typedef = cursor, src = srcCursor }
      end
      
   elseif cursorKind == clang.core.CXCursor_VarDecl then
      if analyzer.currentFunc == nil then
	 table.insert( analyzer.wideAreaValList, cursor )
	 calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
	 analyzer:registCursor( cursor )
      end
   elseif cursorKind == clang.core.CXCursor_FieldDecl or
      cursorKind == clang.core.CXCursor_EnumConstantDecl
   then
      analyzer:addMember( cursor, cursorKind, cursorOffset )
   end

   if not recursiveFlag then
      if cursorKind == clang.core.CXCursor_Namespace or
	 cursorKind == clang.core.CXCursor_ClassDecl or
	 cursorKind == clang.core.CXCursor_StructDecl or
	 cursorKind == clang.core.CXCursor_EnumDecl or
	 cursorKind == clang.core.CXCursor_UnionDecl or
	 cursorKind == clang.core.CXCursor_UnexposedDecl or
	 cursorKind == clang.core.CXCursor_VarDecl or
	 (not uptodateFlag and
	     (isFuncDecl( cursorKind ) or
		 cursorKind == clang.core.CXCursor_CompoundStmt or
		 clang.core.clang_isStatement( cursorKind ) ~= 0 ) )
      then
	 analyzer.depth = analyzer.depth + 1

	 local switchFlag = false
	 if not recursiveFlag then
	    if isFuncDecl( cursorKind ) or
	       cursorKind == clang.core.CXCursor_EnumDecl or
	       cursorKind == clang.core.CXCursor_VarDecl
	    then
	       analyzer.recursiveBaseKind = cursorKind
	       switchFlag = true
	    end
	 end

	 clang.visitChildrenFast(
	    cursor, visitFuncMain, analyzer, targetKindList, switchFlag and 2 or 1 )

	 if switchFlag then
	    analyzer.recursiveBaseKind = clang.core.CXCursor_InvalidFile
	 end
	 
	 analyzer.depth = analyzer.depth - 1
      end
   end
   
   for index, process in ipairs( endProcess ) do
      process()
   end

   return 1
end



local Analyzer = {}

function Analyzer:newAs( recordDigestSrcFlag, displayDiagnostics )
   return Analyzer:new( self.dbPath, recordDigestSrcFlag, displayDiagnostics )
end

function Analyzer:new( dbPath, recordDigestSrcFlag, displayDiagnostics )
   local obj = {
      clangIndex = clang.createIndex( 1, displayDiagnostics and 1 or 0 ),
      currentDir = os.getenv( "PWD" ),

      dbPath = dbPath,
      recursiveBaseKind = clang.core.CXCursor_InvalidFile,

      recordDigestSrcFlag = recordDigestSrcFlag,
      displayDiagnostics = displayDiagnostics,
      
      depth = 0,
      targetFile = nil,
      currentFile = nil,
      nsList = {},
      
      nsLevelList = { { memberList = {} } },
      hash2NsObj = {},
      
      incList = {},
      incBelong = {},
      classList = {},
      funcList = {},
      refList = {},
      typedefList = {},
      enumList = {},
      structList = {},
      unionList = {},
      wideAreaValList = {},
      incBelongList = {},
      macroDefList = {},
      macroRefList = {},
      callList = {},

      -- 解析中関数のカーソル
      currentFunc = nil,

      -- typedef の元定義カーソル -> typedef 定義カーソル
      hashCursor2TypedefMap = {},

      -- anonymous struct, union, enum の元定義カーソル -> VarDecl, FieldDecl 定義カーソル
      anonymousHash2VarDeclMap = {},

      -- cursor -> spInfo のマップ
      cursorHash2SpInfoMap = {},

      -- ファイルパス -> ファイル毎の解析情報
      path2InfoMap = {},
      currentSpInfo = nil,
   }

   setmetatable( obj, { __index = Analyzer } )
   return obj
end

function Analyzer:getNowNs()
   return self.nsLevelList[ #self.nsLevelList ].cursor
end

function Analyzer:enterNs( cursor, cursorKind )
   local nsObj = { cursor = cursor, memberList = {} }
   if cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_EnumDecl
   then
      nsObj.digestObj = Helper.openDigest( "md5" )
   end
   table.insert( self.nsLevelList, nsObj )
   self.hash2NsObj[ cursor:hashCursor() ] = nsObj
end

function Analyzer:addTypeRef( cursor, declCursor )
   local declKind = declCursor:getCursorKind() 
   if declKind ~= clang.core.CXCursor_NoDeclFound then
      table.insert(
	 self.refList,
	 { cursor = cursor, declCursor = declCursor,
	   namespace = self:getNowNs() } )
      self:registCursor( cursor )
   end
end

function Analyzer:addMember( cursor, cursorKind, cursorOffset )
   local nsObj = self.nsLevelList[ #self.nsLevelList ]
   table.insert( nsObj.memberList, { cursor, self.currentFile } )
   local digestObj = nsObj.digestObj

   if digestObj then
      digestObj:write( tostring( cursorOffset ) )
      digestObj:write( cursor:getCursorSpelling() )
      digestObj:write( cursor:getCursorType():getTypeSpelling() )
   end

   local declCursor = clang.getDeclCursorFromType( cursor:getCursorType() )
   if cursorKind == clang.core.CXCursor_FieldDecl then
      self:addTypeRef( cursor, declCursor )
   end

   local declKind = declCursor:getCursorKind()
   if declKind == clang.core.CXCursor_StructDecl or
      declKind == clang.core.CXCursor_UnionDecl or
      declKind == clang.core.CXCursor_EnumDecl
   then
      if declCursor:getCursorSpelling() == "" then
	 self.anonymousHash2VarDeclMap[ declCursor:hashCursor() ] = cursor
	 log( 3, "anonymousHash2VarDeclMap:", declCursor:hashCursor() )
      end
   end
end

function Analyzer:exitNs()
   local nsObj = self.nsLevelList[ #self.nsLevelList ]
   table.remove( self.nsLevelList )
   if nsObj.digestObj then
      nsObj.fixDigest = nsObj.digestObj:fix()
   end
end


function Analyzer:createSpInfo( path, uptodateFlag, cxfile )
   local spInfo = {}

   self.path2InfoMap[ path ] = spInfo
   spInfo.path = path
   spInfo.digest = Helper.openDigest( "md5" )
   spInfo.anonymousCount = 0
   spInfo.uptodateFlag = uptodateFlag
   spInfo.cxfile = cxfile

   if self.recordDigestSrcFlag then
      local target = self.targetFilePath
      spInfo.recordDigest =
	 io.open( string.gsub( path, ".*/", "digest." .. target .. "." ), "w" )
   end

   return spInfo
end

function Analyzer:registCursor( cursor )
   self.cursorHash2SpInfoMap[ cursor:hashCursor() ] = self.currentSpInfo
   calcDigest( cursor, self.currentSpInfo )
end


--[[
   readonly で DB を開く。

   @param currentDir カレントディレクトリ。
     nil の場合は、 Analyzer の currentDir を使用。
   @return DBCtrl
--]]
function Analyzer:openDBForReadOnly( currentDir )
   return DBCtrl:open( self.dbPath, true, currentDir or self.currentDir )
end

function Analyzer:openDBForWrite()
   return DBCtrl:open( self.dbPath, false, self.currentDir )
end

function Analyzer:isUptodate( filePath, compileOp, target, unsavedFile )
   local db = self:openDBForReadOnly()
   if not db then
      log( 2, "db open error" )
      return false
   end

   local targetFileInfo
   local needUpdateFlag = false
   local needUpdateCompOp = false
   
   local success, result = pcall(
      function()
	 -- 解析対象のファイル情報リストを取得
	 targetFileInfo = db:getFileInfo( nil, filePath )
	 if not targetFileInfo then
	    log( 2, "not found target in db" )
	    return false
	 end

	 if compileOp and not db:equalsCompOp( targetFileInfo, compileOp, target ) then
	    if db:equalsCompOp( targetFileInfo, compileOp, nil ) then
	       -- コンパイルオプションが同じ他のターゲットがあれば、
	       -- 解析済みとして扱う。
	       -- ただし、コンパイルオプションを更新しておく必要がある。
	       needUpdateCompOp = true
	       log( 2, "exists same compile option" )
	    else
	       log( 2, "change compile option" )
	       return false
	    end
	 end

	 local targetSystemPath = db:getSystemPath( targetFileInfo.path )
	 if unsavedFile or
	    targetFileInfo.updateTime < Helper.getFileModTime( targetSystemPath )
	 then
	    local digest
	    if unsavedFile then
	       digest = db:calcTextDigest( unsavedFile.Contents )
	    else
	       digest = db:calcFileDigest( targetSystemPath )
	    end
	    if targetFileInfo.digest ~= digest then
	       log( 2, "target is modified" )
	       return false
	    else
	       log( 2, "target is modified but digest is equal" )
	       needUpdateFlag = true
	    end
	 end

	 local fileId2FlieInfoMap = {}
	 fileId2FlieInfoMap[ targetFileInfo.id ] = targetFileInfo

	 -- filePath からインクルードしている全ファイルの情報を
	 -- fileId2FlieInfoMap に設定
	 local newIncFileIdList = { targetFileInfo.id }
	 repeat
	    local incFileIdList = {}
	    for index, fileId in ipairs( newIncFileIdList ) do
	       local detectChange = false
	       db:mapIncRefListFrom(
		  fileId,
		  function( incRefInfo )
		     local incFileInfo = db:getFileInfo( incRefInfo.id )
		     if not incFileInfo then
			log( 2, "not found incFile in db", incRefInfo.id )
			detectChange = true
			return false
		     end
		     if not fileId2FlieInfoMap[ incFileInfo.id ] then
			fileId2FlieInfoMap[ incFileInfo.id ] = incFileInfo
			table.insert( incFileIdList, incFileInfo.id )
			log( 3, "inc file", incFileInfo.path )
		     end
		     return true
		  end
	       )
	       if detectChange then
		  return false
	       end
	    end
	    newIncFileIdList = incFileIdList
	 until #newIncFileIdList == 0

	 -- 取得した全ファイルの情報から、ファイルが更新されているかどうかチェック
	 local fileId2updateInfoMap = {}
	 local uptodateFlag = true
	 for fileId, fileInfo in pairs( fileId2FlieInfoMap ) do
	    local systemPath = db:getSystemPath( fileInfo.path )
	    local modTime = Helper.getFileModTime( systemPath )
	    if fileInfo.path ~= "" and
	       ( not modTime or targetFileInfo.updateTime < modTime )
	    then
	       uptodateFlag = false
	       log( 1, "detect modified", fileInfo.path,
		    targetFileInfo.updateTime, modTime )
	    end
	    fileId2updateInfoMap[ fileInfo.id ] = {
	       -- このファイルの宣言に影響を与えるファイルの Set
	       dependFileUpdateInfoSet = {},
	       path = db:convFullpath( db:getSystemPath( fileInfo.path ) ),
	    }

	    
	 end
	 
	 if not uptodateFlag then
	    -- 個別のファイルの更新が、他のファイルにも影響するかチェック
	    -- ここでは次をチェックしている
	    -- - 更新されているファイル内で宣言している名前空間が、
	    --   インクルード元に影響しているか？
	    --   例えば struct のメンバーを別ファイルをインクードして宣言している場合。
	    --    - struct のメンバー
	    --    - enum の値
	    --    - class のメンバー、メソッド

	    local changeFlag = false
	    for fileId, fileInfo in ipairs( fileId2FlieInfoMap ) do
	       local updateInfo = fileId2updateInfoMap[ fileInfo.id ]
	       local parentIdSet = {}
	       db:mapRowList(
		  "incBelong", "id = " .. tostring( fileInfo.id ), nil, "baseFileId",
		  function ( item )
		     local parentInfo = fileId2updateInfoMap[ item.baseFileId ]
		     updateInfo.dependFileUpdateInfoSet[ parentInfo ] =  1
		     if not updateInfo.uptodateFlag then
			if parentInfo.uptodateFlag then
			   parentInfo.uptodateFlag = false
			   changeFlag = true
			   log( 1, "depend", fileInfo.path, "->", parentInfo.path )
			end
		     end
		  end
	       )
	    end

	    while changeFlag do
	       changeFlag = false
	       for fileId, updateInfo in pairs( fileId2updateInfoMap ) do
		  if updateInfo.uptodateFlag then
		     for dependSpInfo in pairs( updateInfo.dependFileUpdateInfoSet ) do
			if not dependSpInfo.uptodateFlag then
			   updateInfo.uptodateFlag = false
			   log( 1, "depend", updateInfo.path, "->", parentInfo.path )
			   changeFlag = true
			end
		     end
		  end
	       end
	    end
	 end
	 
	 if uptodateFlag then
	    log( 2, "uptodate" )
	 end
	 return uptodateFlag
      end
   )
   db:close()

   if not success then
      log( 1, result )
   end

   if result and ( needUpdateFlag or needUpdateCompOp ) then
      -- uptodate で needUpdateFlag の場合、ファイルの更新日時だけ違う。
      -- 次回のチェック時間を短縮するため、updateTime を更新する。
      db = self:openDBForWrite()
      db:setUpdateTime( targetFileInfo.id, Helper.getCurrentTime() )
      db:updateCompileOp( targetFileInfo, target, compileOp )
      db:close()
   end
   
   return result
end


function Analyzer:analyzeUnit( transUnit, compileOp, target )
   local targetPath = transUnit:getTranslationUnitSpelling()
   log( -1, string.gsub( targetPath, ".*/", "" ) .. ":" )

   log( 3, "start", compileOp, os.clock(), os.date() )

   log( transUnit )

   local root = transUnit:getTranslationUnitCursor()
   log( root )


   self.targetFile =
      transUnit:getFile( transUnit:getTranslationUnitSpelling() )
   self.currentFile = self.targetFile
   
   log( 2, "visitChildren", os.clock(), os.date() )
   clang.visitChildrenFast( root, visitFuncMain, self, targetKindList, 1 )
   log( 2, "visitChildren end", os.clock(), os.date() )

   local db = self:openDBForWrite()
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end
   log( 2, "db open", os.clock(), os.date() )
   

   
   log( 2, "-- file --", os.clock(), os.date() )
   -- 最初にターゲットファイルを登録
   targetPath = DBCtrl:convFullpath(
      self.targetFile:getFileName(), self.currentDir )
   local targetSpInfo = self.path2InfoMap[ targetPath ]
   if not targetSpInfo then
      targetSpInfo = self:createSpInfo( targetPath, false, self.targetFile )
   end
   targetSpInfo.fixDigest = targetSpInfo.digest:fix()
   targetSpInfo.fileInfo = db:addFile(
      targetPath, Helper.getCurrentTime(), targetSpInfo.fixDigest,
      compileOp, self.currentDir, true, target )
   
   -- 残りのヘッダファイルを登録
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      log( filePath )
      local cxfile = transUnit:getFile( db:getSystemPath( filePath ) )
      if not cxfile or not cxfile:isEqual( self.targetFile ) then
	 spInfo.fixDigest = spInfo.digest:fix()
	 spInfo.fileInfo = db:addFile(
	    filePath, Helper.getCurrentTime(), spInfo.fixDigest,
	    nil, nil, false, target )
      end
   end
   
   log( 2, "-- nsList --", os.clock(), os.date() )
   for index, cursor in ipairs( self.nsList ) do
      db:addNamespace( cursor )
   end
   
   log( 2, "-- inc --", os.clock(), os.date() )
   for index, inclusion in ipairs( self.incList ) do
      local cxfile = inclusion:getIncludedFile()
      local path = cxfile and cxfile:getFileName() or ""
      path = db:convFullpath( path )
      local spInfo = self.path2InfoMap[ path ]
      spInfo.fileInfo = db:addInclude( inclusion, spInfo.fixDigest )
   end


   -- ファイル登録の後で、更新チェックをかける
   local uptodateFlag = true
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      if filePath ~= "" and not spInfo.fileInfo.uptodate then
	 log( 1, "need update", filePath )
	 uptodateFlag = false
	 break
      end
   end
   if uptodateFlag then
      log( 1, "uptodate all" )
   else
      self:registerToDB( db )
   end
   
   log( 2, "close", os.clock(), os.date()  )
   db:close()
   log( 2, "end", os.clock(), os.date()  )
end


function Analyzer:processStructEnum( db, info, anonymousForm, kind )
   local cursor = info[ 1 ]
   local name = cursor:getCursorSpelling()
   local hash = cursor:hashCursor()

   log( "processStructEnum:", name,
	clang.getCursorKindSpelling( cursor:getCursorKind() ), hash )

   local typedefInfo = self.hashCursor2TypedefMap[ hash ]
   local typedefName = typedefInfo and typedefInfo.typedef:getCursorSpelling() or ""

   local nsObj = self.hash2NsObj[ hash ]

   local anonymousId
   if name == "" then
      if typedefInfo then
	 anonymousId = typedefName
      else
	 local varCur = self.anonymousHash2VarDeclMap[ hash ]
	 if varCur then
	    anonymousId = varCur:getCursorSpelling()
	 else
	    anonymousId = "anonymous"
	 end
      end
   end

   db:addEnumStructDecl(
      cursor, string.format( anonymousForm, anonymousId ), typedefName, kind, nsObj )
end

function Analyzer:registerToDB( db )

   db:setFuncToGetFileInfoFromCursor(
      function( db, cursor )
	 local spInfo = self.cursorHash2SpInfoMap[ cursor:hashCursor() ]
	 if not spInfo then
	    log( 3, "notfound", cursor:getCursorSpelling(),
		 clang.getCursorKindSpelling( cursor:getCursorKind() ) )

	    local cxfile = getFileLoc( cursor )
	    local path = ""
	    if cxfile ~= nil then
	       path = DBCtrl:convFullpath( cxfile:getFileName(), self.currentDir )
	    end
	    spInfo = self.path2InfoMap[ path ]
	    self.cursorHash2SpInfoMap[ cursor:hashCursor() ] = spInfo
	 end
	 return spInfo.fileInfo
      end
   )
   
   log( 2, "-- macroDefList --", os.clock(), os.date()  )
   for index, macroDef in ipairs( self.macroDefList ) do
      db:addNamespace( macroDef )
   end

   -- typedef を先に処理する
   log( 2, "-- typedef -- ", os.clock(), os.date() )
   for index, info in ipairs( self.typedefList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      
      db:addNamespace( info )
   end
   
   log( 2, "-- classList --", os.clock(), os.date()  )
   for index, info in ipairs( self.classList ) do
      self:processStructEnum( db, info, "<class_%s>",
			      clang.CXCursorKind.FieldDecl.val )
   end

   log( 2, "-- funcList --", os.clock(), os.date()  )
   for index, funcDecl in ipairs( self.funcList ) do
      log( funcDecl:getCursorSpelling() )
      db:addNamespace( funcDecl )
   end
   

   log( 2, "-- enum -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.enumList ) do
      self:processStructEnum( db, info, "<enum_%s>",
			      clang.CXCursorKind.EnumConstantDecl.val )
   end
   
   log( 2, "-- struct -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.structList ) do
      self:processStructEnum( db, info, "<struct_%s>",
			      clang.CXCursorKind.FieldDecl.val )
   end

   log( 2, "-- union -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.unionList ) do
      self:processStructEnum( db, info, "<union_%s>",
			      clang.CXCursorKind.FieldDecl.val )
   end
   
   log( 2, "-- wideAreaVal -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.wideAreaValList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      db:addNamespace( info )
   end


   db:commit()


   --- DB のロック時間を少しでも削減するため、
   --- これ以降は、DB に直接記録しないでメモリ上に格納しておき、
   --- 最後に DB に反映する。
   
   db:beginForTemp()

   log( 2, "-- macroRefList --", os.clock(), os.date()  )
   for index, macroRef in ipairs( self.macroRefList ) do
      db:addReference( macroRef )
   end

   log( 2, "-- incBelong --", os.clock(), os.date() )
   for index, incBelong in ipairs( self.incBelongList ) do
      db:addIncBelong( incBelong )
   end

   log( 2, "-- refList --", os.clock(), os.date()  )
   for index, refInfo in ipairs( self.refList ) do
      log( refInfo.cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( refInfo.cursor:getCursorKind() ) )
      db:addReference( refInfo )
   end

   log( 2, "-- callList --", os.clock(), os.date()  )
   for index, info in ipairs( self.callList ) do
      log( info.cursor:getCursorSpelling() )
      db:addCall( info.cursor, info.namespace )
   end
end


function Analyzer:update( path, target )
   if not target then
      target = ""
   end
   
   if self:isUptodate( path, nil, target ) then
      return
   end

   local transUnit, compileOp = self:createUnit( path, target )

   self:analyzeUnit( transUnit, compileOp, target )
end

function Analyzer:createUnit( path, target )
   self.targetFilePath = path

   if not target then
      target = ""
   end
   
   local db = self:openDBForReadOnly()
   
   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo, optionList = db:getFileOpt( path, target )
   if fileInfo.incFlag ~= 0 then
      fileInfo = self:getSrcForIncOne( fileInfo )
      fileInfo, optionList = db:getFileOpt(
	 db:getSystemPath( fileInfo.path ), target )
   end
   
   if not optionList then
      log( 1, "This file doesn't has target" )
      os.exit( 1 )
   end

   local compileOp = ""
   for index, option in ipairs( optionList ) do
      compileOp = compileOp .. option .. " "
   end
   log( 3, "src:", fileInfo.path, "target:", target, "compOP:", compileOp )
   
   
   db:close()

   if not fileInfo then
      log( -2, "file not found" )
      os.exit( 0 )
   end
   
   if not optionList then
      log( -2, "skip this file since unmatch target" )
      os.exit( 0 )
   end

   local args = clang.mkCharArray( optionList )

   local compileOp = ""
   for index, option in ipairs( optionList ) do
      compileOp = compileOp .. option .. " "
   end

   local unit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(), 0, nil )

   return unit, compileOp
end



function Analyzer:analyzeSource( path, options, target, unsavedFileTable )
   self.targetFilePath = path

   if not target then
      target = ""
   end

   local compileOp = ""
   for index, option in ipairs( options ) do
      compileOp = compileOp .. option .. " "
   end
   
   if self:isUptodate( path, compileOp, target,
		       unsavedFileTable and unsavedFileTable[1] )
   then
      return
   end
   
   local args = clang.mkCharArray( options )
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(),
      unsavedFileArray:getLength(), unsavedFileArray:getPtr() )

   self:analyzeUnit( transUnit, compileOp, target )
end

function Analyzer:analyzeSourcePch( path )
   local transUnit = self.clangIndex:createTranslationUnit( path )

   
   self:analyzeUnit( transUnit, nil, nil )
end


function Analyzer:analyzeSourceAtWithFunc(
      currentDir, targetFullPath, line, column,
      optionList, target, fileContents, func )
   
   Helper.chdir( currentDir )
   log( 2, "currentDir", currentDir, targetFullPath )

   local args = clang.mkCharArray( optionList )
   local unsavedFileTable
   if fileContents then
      unsavedFileTable = {}
      local unsavedFile = clang.core.CXUnsavedFile()
      unsavedFile.Filename = targetFullPath
      unsavedFile.Contents = fileContents
      unsavedFile.Length = #unsavedFile.Contents
      table.insert( unsavedFileTable, unsavedFile )
   end

   
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      targetFullPath, args:getLength(), args:getPtr(),
      unsavedFileArray:getLength(), unsavedFileArray:getPtr() )

   local cxfile = transUnit:getFile( targetFullPath )
   local location = transUnit:getLocation( cxfile, line, column )
   local cursor = transUnit:getCursor( location )

   local declCursor = cursor:getCursorReferenced()
   if clang.isDeclaration( cursor:getCursorKind() ) then
      declCursor = cursor
   end

   log( 2, "cursor",
	cursor:getCursorSpelling(),
	clang.getCursorKindSpelling( cursor:getCursorKind() ),
	clang.getCursorKindSpelling( declCursor:getCursorKind() ))

   local db = self:openDBForReadOnly( currentDir )
   
   func( db, db:getFileInfo( nil, targetFullPath ).id,  nil, declCursor )
   
   db:close()
end


function Analyzer:queryAtFunc(
      filePath, line, column, target, fileContents, func )
   local db = self:openDBForReadOnly()

   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo, optionList = db:getFileOpt( filePath, target )
   
   if fileInfo.incFlag == 1 then
      -- ヘッダの場合は解析せずに DB 登録されている情報を使用する
      local nsInfo = db:getNsInfoAt( filePath, line, column, fileContents )
      if nsInfo then
	 func( db, fileInfo.id, nsInfo, nil )
      else
	 log( 1, "not found namespace" )
	 os.exit( 1 )
      end
      db:close()
      return
   end

   if not optionList then
      log( 1, "This file doesn't has target" )
      os.exit( 1 )
   end

   local compileOp = ""
   for index, option in ipairs( optionList ) do
      compileOp = compileOp .. option .. " "
   end
   log( 3, "src:", fileInfo.path, "target:", target, "compOP:", compileOp )
   
      
   local analyzer = Analyzer:new(
      db:getSystemPath( db:convFullpath( self.dbPath ) ),
      self.recordDigestSrcFlag, self.displayDiagnostics )

   local currentDir = db:getSystemPath( fileInfo.currentDir )
   local targetFilePath = db:getSystemPath( fileInfo.path )

   db:close()
   
   analyzer:analyzeSourceAtWithFunc(
      currentDir, targetFilePath, line, column,
      optionList, target, fileContents, func )
end




function Analyzer:queryAt(
      mode, filePath, line, column, absFlag, target, fileContents )
   self:queryAtFunc(
      filePath, line, column, target, fileContents,
      function( db, targetFileId, nsInfo, declCursor )
	 if nsInfo then
	    if mode == "ref-at" then
	       Query:execWithDb( db, "r" .. (absFlag and "a" or ""), nsInfo.name )
	    elseif mode == "def-at" then
	       Query:execWithDb( db, "t" .. (absFlag and "a" or ""), nsInfo.name )
	    elseif mode == "call-at" then
	       Query:execWithDb( db, "C" .. (absFlag and "a" or ""), nsInfo.name )
	    elseif mode == "call-at" then
	       print( nsInfo.id, nsInfo.name )
	    else
	       log( 1, "illegal mode", mode )
	       os.exit( 1 )
	    end
	 else
	    local kind = declCursor:getCursorKind()
	    if mode == "ref-at" then
	       db:SymbolRefInfoListForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Util:printLocate(
			db, nsInfo.name, item.fileId, item.line, absFlag, true,
			item.fileId == targetFileId and fileContents or nil )
		     return true
		  end	 
	       )
	    elseif mode == "def-at" then
	       db:SymbolDefInfoListForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Util:printLocate(
			db, nsInfo.name, item.fileId, item.line, absFlag, true,
			item.fileId == targetFileId and fileContents or nil )
		     return true
		  end	 
	       )
	    elseif mode == "call-at" then
	       db:mapCallForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Util:printLocate(
			db, nsInfo.name, item.fileId, item.line, absFlag, true,
			item.fileId == targetFileId and fileContents or nil )
		     return true
		  end	 
	       )
	    elseif mode == "ns-at" then
	       local nsIdSet = {}
	       db:SymbolDefInfoListForCursor(
		  declCursor,
		  function( item )
		     if not nsIdSet[ item.nsId ] then
			local nsInfo = db:getNamespace( item.nsId )
			nsIdSet[ nsInfo.id ] = 1
			print( nsInfo.id, nsInfo.name )
		     end
		     return true
		  end
	       )
	    else
	       log( 1, "illegal mode", mode )
	       os.exit( 1 )
	    end
	 end
      end
   )
end




function Analyzer:graphAt(
      graph, filePath, line, column, target,
      depthLimit, browseFlag, outputFile, imageFormat )

   self:queryAtFunc(
      filePath, line, column, target, nil,
      function( db, targetFileId, nsInfo, declCursor )
	 if nsInfo then
	    if graph == "caller" or graph == "callee" then
	       Query:outputCallRelation(
		  self.dbPath, nsInfo.name, graph == "caller", depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	    elseif graph == "systemPath" then
	       Query:outputSymbolRefRelation(
		  self.dbPath, nsInfo.name, depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	    else
	       log( 1, "illegal graph", graph )
	       os.exit( 1 )
	    end
	 else
	    if graph == "caller" or graph == "callee" then
	       local kind = declCursor:getCursorKind()
	       if kind == clang.core.CXCursor_FieldDecl or
		  kind == clang.core.CXCursor_ParmDecl or
		  kind == clang.core.CXCursor_VarDecl
	       then
		  local cxtype = declCursor:getCursorType()
		  declCursor = clang.getDeclCursorFromType( cxtype )
		  nsInfo = db:getNamespaceFromCursor( declCursor )
	       else
		  nsInfo = db:getNamespaceFromCursor( declCursor )
	       end

	       if not nsInfo then
		  log( 1, "not found namespace", declCursor:getCursorSpelling() )
		  os.exit( 1 )
	       end
	       
	       Query:outputCallRelation(
		  self.dbPath, nsInfo.name, graph == "caller", depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	    elseif graph == "symbol" then
	       db:SymbolRefInfoListForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Query:outputSymbolRefRelation(
			self.dbPath, nsInfo.name, depthLimit,
			OutputCtrl.dot, browseFlag, outputFile, imageFormat )
		     return false
		  end
	       )
	    else
	       log( 1, "illegal graph", mode )
	       os.exit( 1 )
	    end
	 end
      end
   )
end


return Analyzer
