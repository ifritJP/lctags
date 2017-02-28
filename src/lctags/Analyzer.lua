-- Copyright (C) 2017 ifritJP

local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local Query = require( 'lctags.Query' )


local function dumpCursorInfo( cursor, depth, prefix )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()

   log( 4, string.format(
   	   "%s %s%s %s(%d) %d %s",
   	   string.rep( " ", depth ),
   	   prefix and (prefix .. " ") or "", txt, 
   	   clang.getCursorKindSpelling( cursorKind ), cursorKind,
   	   cursor:hashCursor(), cursor:getRawCommentText() or "" ) )
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
   clang.core.CXCursor_CXXMethod,
   clang.core.CXCursor_Destructor,
   clang.core.CXCursor_FunctionDecl,
   clang.core.CXCursor_Constructor,
   clang.core.CXCursor_DeclRefExpr,
   clang.core.CXCursor_MemberRefExpr,
   clang.core.CXCursor_ParmDecl,
   clang.core.CXCursor_TypedefDecl,
   clang.core.CXCursor_VarDecl,
   clang.core.CXCursor_CallExpr,
}

local function visitFuncMain( cursor, parent, analyzer, exInfo )
   local cursorKind = cursor:getCursorKind()

   dumpCursorInfo( cursor, analyzer.depth )

   local uptodateFlag
   local cursorOffset = tostring( exInfo[ 2 ] )
   
   if exInfo[ 1 ] then
      local cxfile, line = clang.getFileLocation(
	 cursor:getCursorLocation().__ptr, clang.core.clang_getFileLocation )
      if analyzer.currentFile ~= cxfile and
	 ( not analyzer.currentFile or not cxfile or
	      not analyzer.currentFile:isEqual( cxfile ) )
      then
	 local path = ""
	 if cxfile then
	    path = DBCtrl:convFullpath( cxfile:getFileName(), analyzer.currentDir )
	 end

	 table.insert( analyzer.incBelongList,
		       { cxfile = cxfile,
			 namespace = analyzer.nsLevelList[ #analyzer.nsLevelList ] } )

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

   if isNamespaceDecl( cursorKind ) then
      table.insert( analyzer.nsLevelList, cursor )
      table.insert( endProcess,
		    function() table.remove( analyzer.nsLevelList ) end )
   end

   if cursorKind == clang.core.CXCursor_Namespace then
      table.insert( analyzer.nsList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
   end
   
   if cursorKind == clang.core.CXCursor_InclusionDirective then
      local cxfile = cursor:getIncludedFile()
      local path = cxfile and cxfile:getFileName() or ""
      local spInfo = analyzer.path2InfoMap[ path ]
      if not spInfo then
	 path = DBCtrl:convFullpath( path, analyzer.currentDir )	 
	 spInfo = analyzer:createSpInfo( path, true, cxfile )
      end
      
      table.insert( analyzer.incList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
      return 1
   elseif cursorKind == clang.core.CXCursor_ClassDecl then
      table.insert( analyzer.classList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
      
   elseif cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
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
      end
	 
      table.insert( list, { cursor, analyzer.currentSpInfo.anonymousCount } )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_FunctionDecl then
      table.insert( analyzer.funcList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
      
   elseif cursorKind == clang.core.CXCursor_MacroDefinition then
      table.insert( analyzer.macroDefList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )

   elseif cursorKind == clang.core.CXCursor_MacroExpansion then
      local declCursor = cursor:getCursorReferenced()
      table.insert( analyzer.macroRefList,
		    { cursor = cursor, declCursor = declCursor, namespace = nil } )
      -- マクロ参照はヘッダの多重 include 抑止に利用していることが多い。
      -- これを digest 計算に加えると、include 抑止
      -- calcDigest( cursor, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_Constructor
   then
      table.insert( analyzer.methodList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
      
   elseif clang.isReference( cursorKind ) then
      if cursorKind ~= clang.core.CXCursor_NamespaceRef then
	 local declCursor = cursor:getCursorReferenced()
	 local namespace = analyzer.nsLevelList[ #analyzer.nsLevelList ]
	 table.insert(
	    analyzer.refList,
	    { cursor = cursor, declCursor = declCursor,
	      namespace = namespace } )
	 calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
	 calcDigest( cursor, analyzer.currentSpInfo )
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
	    local namespace = analyzer.nsLevelList[ #analyzer.nsLevelList ]
	    table.insert(
	       analyzer.refList,
	       { cursor = cursor, declCursor = declCursor,
		 namespace = namespace } )
	    calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
	    calcDigest( cursor, analyzer.currentSpInfo )
	    calcDigest( declCursor, analyzer.currentSpInfo )
	    calcDigest( namespace, analyzer.currentSpInfo )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_CallExpr then
      local namespace = analyzer.nsLevelList[ #analyzer.nsLevelList ]
      table.insert( analyzer.callList,
		    { cursor = cursor, namespace = namespace } )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
      calcDigest( namespace, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_TypedefDecl then
      table.insert( analyzer.typedefList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      calcDigest( cursor, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_VarDecl then
      if analyzer.currentFunc == nil then
	 table.insert( analyzer.wideAreaValList, cursor )
	 calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
	 calcDigest( cursor, analyzer.currentSpInfo )
      end
   end

   if not analyzer.recursiveFlag then
      if cursorKind == clang.core.CXCursor_Namespace or
	 cursorKind == clang.core.CXCursor_ClassDecl or
	 cursorKind == clang.core.CXCursor_UnexposedDecl or
	 cursorKind == clang.core.CXCursor_VarDecl or
	 (not uptodateFlag and
	     (isFuncDecl( cursorKind ) or
		 cursorKind == clang.core.CXCursor_CompoundStmt or
		 clang.core.clang_isStatement( cursorKind ) ) )
      then
	 analyzer.depth = analyzer.depth + 1

	 local switchFlag = false
	 if not analyzer.recursiveFlag then
	    if isFuncDecl( cursorKind ) or
	       cursorKind == clang.core.CXCursor_VarDecl
	    then
	       analyzer.recursiveFlag = true
	       switchFlag = true
	    end
	 end

	 clang.visitChildrenFast(
	    cursor, visitFuncMain, analyzer, targetKindList,
	    analyzer.recursiveFlag and 2 or 1 )

	 if switchFlag then
	    analyzer.recursiveFlag = false
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

function Analyzer:new( dbPath, recordDigestSrcFlag, displayDiagnostics )
   local obj = {
      clangIndex = clang.createIndex( 1, displayDiagnostics and 1 or 0 ),
      currentDir = os.getenv( "PWD" ),

      dbPath = dbPath,
      recursiveFlag = false,

      recordDigestSrcFlag = recordDigestSrcFlag,
      displayDiagnostics = displayDiagnostics,
      
      depth = 0,
      targetFile = nil,
      currentFile = nil,
      nsList = {},
      nsLevelList = {},
      incList = {},
      incBelong = {},
      classList = {},
      funcList = {},
      methodList = {},
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

      -- ファイルパス -> ファイル毎の解析情報
      path2InfoMap = {},
      currentSpInfo = nil,
   }

   setmetatable( obj, { __index = Analyzer } )
   return obj
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

function Analyzer:isUptodate( filePath, compileOp, target )
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
	 if targetFileInfo.updateTime < Helper.getFileModTime( targetSystemPath )
	 then
	    local digest = db:calcFileDigest( targetSystemPath )
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
   clang.visitChildrenFast(
      root, visitFuncMain, self, targetKindList,
      self.recursiveFlag and 2 or 1 )
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
   db:addFile( targetPath, Helper.getCurrentTime(), targetSpInfo.fixDigest,
	       compileOp, self.currentDir, true, target )
   -- 残りのヘッダファイルを登録
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      log( filePath )
      local cxfile = transUnit:getFile( db:getSystemPath( filePath ) )
      if not cxfile or not cxfile:isEqual( self.targetFile ) then
	 spInfo.fixDigest = spInfo.digest:fix()
	 db:addFile( filePath, Helper.getCurrentTime(), spInfo.fixDigest,
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
      db:addInclude( inclusion, spInfo.fixDigest )
   end

   -- ファイル登録の後で、更新チェックをかける
   local uptodateFlag = true
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      if filePath ~= "" and not db:getFileInfo( nil, filePath ).uptodate then
	 log( 1, "need update", filePath )
	 uptodateFlag = false
	 break
      end
   end
   if uptodateFlag then
      log( 1, "uptodate all" )
   else
      self:retisterToDB( db )
   end
   
   log( 2, "close", os.clock(), os.date()  )
   db:close()
end


function Analyzer:retisterToDB( db )
   log( 2, "-- macroDefList --", os.clock(), os.date()  )
   for index, macroDef in ipairs( self.macroDefList ) do
      db:addNamespace( macroDef )
   end

   log( 2, "-- macroRefList --", os.clock(), os.date()  )
   for index, macroRef in ipairs( self.macroRefList ) do
      db:addReference( macroRef )
   end

   -- typedef を先に処理する
   log( 2, "-- typedef -- ", os.clock(), os.date() )
   for index, info in ipairs( self.typedefList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      
      info:visitChildren(
	 function( aCursor, aParent, aExInfo )
	    self.hashCursor2TypedefMap[ aCursor:hashCursor() ] =
	       { typedef = info, src = aCursor }
	    return clang.CXChildVisitResult.Break.val
	 end, {} )

      
      db:addNamespace( info )
   end
   
   log( 2, "-- classList --", os.clock(), os.date()  )
   for index, cursor in ipairs( self.classList ) do
      log( cursor:getCursorSpelling() )
      db:addNamespace( cursor )
   end

   log( 2, "-- funcList --", os.clock(), os.date()  )
   for index, funcDecl in ipairs( self.funcList ) do
      log( funcDecl:getCursorSpelling() )
      db:addNamespace( funcDecl )
   end
   
   log( 2, "-- methodList --", os.clock(), os.date()  )
   for index, methodDecl in ipairs( self.methodList ) do
      log( methodDecl:getCursorSpelling() )
      db:addNamespace( methodDecl )
   end
   

   log( 2, "-- enum -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.enumList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )
      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      local typedefName = typedefInfo and typedefInfo.typedef:getCursorSpelling() or ""
      local anonymousId = typedefInfo and typedefName or info[2]
      
      db:addStructDecl( cursor, string.format( "<anonymous_enum_%s>", anonymousId ),
			typedefName )
   end
   
   log( 2, "-- struct -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.structList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )

      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      local typedefName = typedefInfo and typedefInfo.typedef:getCursorSpelling() or ""
      local anonymousId = typedefInfo and typedefName or info[2]
      
      db:addStructDecl( cursor, string.format( "<anonymous_struct_%s>", anonymousId ),
			typedefName )
   end

   log( 2, "-- union -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.unionList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )

      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      db:addStructDecl( cursor, string.format( "<anonymous_union_%d>", info[2] ),
			typedefInfo and typedefInfo.typedef:getCursorSpelling() or "" )
   end
   
   log( 2, "-- wideAreaVal -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.wideAreaValList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      db:addNamespace( info )
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
   self.targetFilePath = path

   if not target then
      target = ""
   end
   
   if self:isUptodate( path, nil, target ) then
      return
   end

   local db = self:openDBForReadOnly()
   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo, optionList = db:getFileOpt( path, target )
   db:close()

   local args = clang.mkCharArray( optionList )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(), 0, nil )

   local compileOp = ""
   for index, option in ipairs( optionList ) do
      compileOp = compileOp .. option .. " "
   end

   self:analyzeUnit( transUnit, compileOp, target )
end



function Analyzer:analyzeSource( path, options, target )
   self.targetFilePath = path

   if not target then
      target = ""
   end

   local compileOp = ""
   for index, option in ipairs( options ) do
      compileOp = compileOp .. option .. " "
   end
   
   if self:isUptodate( path, compileOp, target ) then
      return
   end
   
   local args = clang.mkCharArray( options )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(), 0, nil )

   self:analyzeUnit( transUnit, compileOp, target )
end

function Analyzer:analyzeSourcePch( path )
   local transUnit = self.clangIndex:createTranslationUnit( path )

   
   self:analyzeUnit( transUnit, nil, nil )
end


function Analyzer:analyzeSourceAtWithFunc(
      currentDir, targetFullPath, line, column, optionList, func )
   
   Helper.chdir( currentDir )
   log( 2, "currentDir", currentDir, targetFullPath )

   local db = self:openDBForReadOnly( currentDir )

   local args = clang.mkCharArray( optionList )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      targetFullPath, args:getLength(), args:getPtr(), 0, nil )

   local location = transUnit:getLocation(
      transUnit:getFile( targetFullPath ), line, column )
   local cursor = transUnit:getCursor( location )

   local declCursor = cursor:getCursorReferenced()
   if clang.isDeclaration( cursor:getCursorKind() ) then
      declCursor = cursor
   end

   log( 2, "cursor",
	clang.getCursorKindSpelling( cursor:getCursorKind() ),
	clang.getCursorKindSpelling( declCursor:getCursorKind() ))

   func( db, nil, declCursor )
   
   db:close()
end


function Analyzer:queryAtFunc(
      filePath, line, column, target, func )
   local db = self:openDBForReadOnly()

   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo, optionList = db:getFileOpt( filePath, target )

   local compileOp = ""
   for index, option in ipairs( optionList ) do
      compileOp = compileOp .. option .. " "
   end
   log( 3, "src:", fileInfo.path, "target:", target, "compOP:", compileOp )
   

   if fileInfo.incFlag == 1 then
      -- ヘッダの場合は解析せずに DB 登録されている情報を使用する
      local nsInfo = db:getNsInfoAt( filePath, line, column )
      if nsInfo then
	 func( db, nsInfo, nil )
      else
	 log( 1, "not found namespace" )
	 os.exit( 1 )
      end
   else
      local analyzer = Analyzer:new(
	 db:getSystemPath( db:convFullpath( self.dbPath ) ),
	 self.recordDigestSrcFlag, self.displayDiagnostics )
      
      analyzer:analyzeSourceAtWithFunc(
	 db:getSystemPath( fileInfo.currentDir ),
	 db:getSystemPath( fileInfo.path ), line, column,
	 optionList, func )
   end

   db:close()
end




function Analyzer:queryAt( mode, filePath, line, column, absFlag, target )
   self:queryAtFunc(
      filePath, line, column, target,
      function( db, nsInfo, declCursor )
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
		     Query:printLocate(
			db, nsInfo.name, item.fileId, item.line, absFlag, true )
		     return true
		  end	 
	       )
	    elseif mode == "def-at" then
	       if kind == clang.core.CXCursor_VarDecl or
		  kind == clang.core.CXCursor_ParmDecl
	       then
		  local startInfo, endInfo = db:getRangeFromCursor( declCursor )
		  local fileInfo =
		     db:getFileInfo( nil, startInfo[ 1 ]:getFileName() )
		  Query:printLocate(
		     db, declCursor:getCursorSpelling(), fileInfo.id,
		     startInfo[ 2 ], absFlag, true )
		  return
	       end
	       
	       
	       db:SymbolDefInfoListForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Query:printLocate(
			db, nsInfo.name, item.fileId, item.line, absFlag, true )
		     return true
		  end	 
	       )
	    elseif mode == "call-at" then
	       db:mapCallForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Query:printLocate(
			db, nsInfo.name, item.fileId, item.line, absFlag, true )
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
      filePath, line, column, target,
      function( db, nsInfo, declCursor )
	 if nsInfo then
	    if graph == "caller" or graph == "callee" then
	       Query:outputCallRelation(
		  self.dbPath, nsInfo.name, graph == "caller",
		  depthLimit, browseFlag, outputFile, imageFormat )
	    elseif graph == "systemPath" then
	       Query:outputSymbolRefRelation(
		  self.dbPath, nsInfo.name, depthLimit,
		  browseFlag, outputFile, imageFormat )
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
		  declCursor = db:getDeclCursorFromType( cxtype )
		  nsInfo = db:getNamespaceFromCursor( declCursor )
	       else
		  nsInfo = db:getNamespaceFromCursor( declCursor )
	       end
	       
	       Query:outputCallRelation(
		  self.dbPath, nsInfo.name, graph == "caller",
		  depthLimit, browseFlag, outputFile, imageFormat )
	    elseif graph == "symbol" then
	       db:SymbolRefInfoListForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Query:outputSymbolRefRelation(
			self.dbPath, nsInfo.name,
			depthLimit, browseFlag, outputFile, imageFormat )
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
