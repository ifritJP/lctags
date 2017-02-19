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
      cursorKind == clang.core.CXCursor_ClassDecl
end

local function calcDigest( cursor, spInfo )
   if not spInfo.digest then
      return
   end
   spInfo.digest:write( cursor:getCursorSpelling() )
   if spInfo.recordDigest then
      spInfo.recordDigest:write( cursor:getCursorSpelling() .. "\n" )
   end
end

local function visitFuncMain( cursor, parent, analyzer )
   local cursorKind = cursor:getCursorKind()

   dumpCursorInfo( cursor, analyzer.depth )

   local uptodateFlag
   
   if not analyzer.currentFunc then
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
      table.insert( endProcess, function() table.remove( analyzer.nsLevelList ) end )
   end

   if cursorKind == clang.core.CXCursor_Namespace then
      table.insert( analyzer.nsList, cursor )
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
      calcDigest( cursor, analyzer.currentSpInfo )
      return 1
   elseif cursorKind == clang.core.CXCursor_ClassDecl then
      table.insert( analyzer.nsLevelList, cursor )
      table.insert( endProcess, function() table.remove( analyzer.nsLevelList ) end )

      table.insert( analyzer.classList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo )
      
   elseif cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_EnumDecl
   then
      table.insert( analyzer.nsLevelList, cursor )
      table.insert( endProcess, function() table.remove( analyzer.nsLevelList ) end )
      
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
      calcDigest( cursor, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_FunctionDecl then
      table.insert( analyzer.funcList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigest( cursor, analyzer.currentSpInfo )
      
   elseif cursorKind == clang.core.CXCursor_MacroDefinition then
      table.insert( analyzer.macroDefList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo )

   elseif cursorKind == clang.core.CXCursor_MacroExpansion then
      local declCursor = cursor:getCursorReferenced()
      table.insert( analyzer.macroRefList,
		    { cursor = cursor, declCursor = declCursor, namespace = nil } )
      -- マクロ参照はヘッダの多重 include 抑止に利用していることが多い。
      -- これを digest 計算に加えると、include 抑止
      -- calcDigest( cursor, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_CXXMethod then
      table.insert( analyzer.methodList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigest( cursor, analyzer.currentSpInfo )
      
   elseif clang.isReference( cursorKind ) then
      if cursorKind ~= clang.core.CXCursor_NamespaceRef then
	 local declCursor = cursor:getCursorReferenced()
	 table.insert(
	    analyzer.refList,
	    { cursor = cursor, declCursor = declCursor,
	      namespace = analyzer.nsLevelList[ #analyzer.nsLevelList ] } )
	 calcDigest( cursor, analyzer.currentSpInfo )
	 calcDigest( declCursor, analyzer.currentSpInfo )
      end
   elseif cursorKind == clang.core.CXCursor_DeclRefExpr or
      cursorKind == clang.core.CXCursor_MemberRefExpr
   then
      -- local declCursor = cursor:getCursorDefinition()
      local declCursor = cursor:getCursorReferenced()
      if declCursor:getCursorKind() ~= clang.core.CXCursor_ParmDecl then
	 local storageClass = declCursor:getStorageClass()
	 if storageClass ~= clang.core.CX_SC_Auto and
	    storageClass ~= clang.core.CX_SC_Register
	 then
	    table.insert(
	       analyzer.refList,
	       { cursor = cursor, declCursor = declCursor,
		 namespace = analyzer.nsLevelList[ #analyzer.nsLevelList ] } )
	    calcDigest( cursor, analyzer.currentSpInfo )
	    calcDigest( declCursor, analyzer.currentSpInfo )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_TypedefDecl then
      table.insert( analyzer.typedefList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_VarDecl then
      if analyzer.currentFunc == nil then
	 table.insert( analyzer.wideAreaValList, cursor )
	 calcDigest( cursor, analyzer.currentSpInfo )
      end
   end

   if not analyzer.recursiveFlag then
      if cursorKind == clang.core.CXCursor_Namespace or
	 cursorKind == clang.core.CXCursor_ClassDecl or
	 (not uptodateFlag and
	     (isFuncDecl( cursorKind ) or
		 cursorKind == clang.core.CXCursor_CompoundStmt or
		 clang.core.clang_isStatement( cursorKind ) ) )
      then
	 analyzer.depth = analyzer.depth + 1
	 if isFuncDecl( cursorKind ) then
	    analyzer.recursiveFlag = true
	 end
	 
	 local ret = cursor:visitChildren( visitFuncMain, analyzer )

	 if isFuncDecl( cursorKind ) then
	    analyzer.recursiveFlag = false
	 end
	 analyzer.depth = analyzer.depth - 1
      end
   end
   
   for index, process in ipairs( endProcess ) do
      process()
   end

   if analyzer.recursiveFlag  then
      return 2
   end
   return 1
end



local Analyzer = {}

function Analyzer:new( dbPath, recordDigestSrcFlag )
   local obj = {
      clangIndex = clang.createIndex( 1, 1 ),
      currentDir = os.getenv( "PWD" ),

      dbPath = dbPath,
      recursiveFlag = false,

      recordDigestSrcFlag = recordDigestSrcFlag,
      
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
   -- このファイルの宣言に影響を与えるファイルの spInfo の Set
   spInfo.dependFileSpInfoSet = {}

   if self.recordDigestSrcFlag then
      local target = self.targetFilePath
      spInfo.recordDigest =
	 io.open( string.gsub( path, ".*/", "digest." .. target .. "." ), "w" )
   end

   return spInfo
end

function Analyzer:openDBForReadOnly()
   return DBCtrl:open( self.dbPath, true, self.currentDir )
end

function Analyzer:openDBForWrite()
   return DBCtrl:open( self.dbPath, false, self.currentDir )
end

function Analyzer:isUptodate( filePath )
   local db = self:openDBForReadOnly()
   if not db then
      log( 2, "db open error" )
      return false
   end
   
   local success, result = pcall(
      function()
	 -- 解析対象のファイル情報リストを取得
	 local targetFileInfo = db:getFileInfo( nil, filePath )
	 if not targetFileInfo then
	    log( 2, "not found target in db" )
	    return false
	 end

	 local fileId2FlieInfoMap = {}
	 fileId2FlieInfoMap[ targetFileInfo.id ] = targetFileInfo

	 -- filePath からインクルードしている全ファイルの情報を
	 -- fileId2FlieInfoMap に設定
	 local newIncFileIdList = { targetFileInfo.id }
	 repeat
	    local incFileIdList = {}
	    for index, fileId in ipairs( newIncFileIdList ) do
	       db:mapIncRefListFrom(
		  fileId,
		  function( incRefInfo )
		     local incFileInfo = db:getFileInfo( incRefInfo.id )
		     if not incFileInfo then
			log( 2, "not found incFile in db", incRefInfo.id )
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
	    end
	    newIncFileIdList = incFileIdList
	 until #newIncFileIdList == 0

	 -- 取得した全ファイルの情報から、ファイルが更新されているかどうかチェック
	 local fileId2SpInfoMap = {}
	 local uptodateFlag = true
	 for fileId, fileInfo in pairs( fileId2FlieInfoMap ) do
	    local modTime = Helper.getFileModTime( db:getSystemPath( fileInfo.path ) )
	    if fileInfo.path ~= "" and fileInfo.modTime ~= modTime then
	       uptodateFlag = false
	       log( 1, "detect modified", fileInfo.path,
		    fileInfo.modTime, modTime )
	    end
	    fileId2SpInfoMap[ fileInfo.id ] =
	       self:createSpInfo(
		  db:convFullpath( db:getSystemPath( fileInfo.path ) ),
		  uptodateFlag, cxfile )
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
	       local spInfo = fileId2SpInfoMap[ fileInfo.id ]
	       local parentIdSet = {}
	       db:mapRowList(
		  "incBelong", "id = " .. tostring( fileInfo.id ), nil, "baseFileId",
		  function ( item )
		     local parentInfo = fileId2SpInfoMap[ item.baseFileId ]
		     spInfo.dependFileSpInfoSet[ parentInfo ] =  1
		     if not spInfo.uptodateFlag then
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
	       for fileId, spInfo in pairs( fileId2SpInfoMap ) do
		  if spInfo.uptodateFlag then
		     for dependSpInfo in pairs( spInfo.dependFileSpInfoSet ) do
			if not dependSpInfo.uptodateFlag then
			   spInfo.uptodateFlag = false
			   log( 1, "depend", spInfo.path, "->", parentInfo.path )
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
   return result
end


function Analyzer:analyzeUnit( transUnit, compileOp, target )
   local targetPath = transUnit:getTranslationUnitSpelling()
   log( -1, string.gsub( targetPath, ".*/", "" ) .. ":" )

   log( 2, "start", compileOp, os.clock(), os.date() )

   log( transUnit )

   local root = transUnit:getTranslationUnitCursor()
   log( root )


   self.targetFile =
      transUnit:getFile( transUnit:getTranslationUnitSpelling() )
   self.currentFile = self.targetFile

   log( 2, "visitChildren", os.clock(), os.date() )
   root:visitChildren( visitFuncMain, self )
   log( 2, "visitChildren end", os.clock(), os.date() )

   local db = self:openDBForWrite()
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end
   log( 2, "db open", os.clock(), os.date() )
   

   
   log( 2, "-- file --", os.clock(), os.date() )
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      log( filePath )
      local cxfile = transUnit:getFile( db:getSystemPath( filePath ) )
      local isTarget = cxfile and cxfile:isEqual( self.targetFile )
      spInfo.fixDigest = spInfo.digest:fix()
      db:addFile( filePath, cxfile and cxfile:getFileTime() or 0,
		  spInfo.fixDigest,
		  isTarget and compileOp, isTarget and self.currentDir,
		  isTarget, target )
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
      db:addEnumDecl( info[1], string.format( "<anonymous_enum_%d>", info[2] ),
		      typedefInfo and typedefInfo.typedef:getCursorSpelling() or "" )
   end
   
   log( 2, "-- struct -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.structList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )

      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      db:addStructDecl( cursor, string.format( "<anonymous_struct_%d>", info[2] ),
			typedefInfo and typedefInfo.typedef:getCursorSpelling() or "" )
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

   log( 2, "close", os.clock(), os.date()  )
   db:close()
end

function Analyzer:analyzeSource( path, options, target )
   self.targetFilePath = path
   
   if self:isUptodate( path ) then
      return
   end
   
   local args = clang.mkCharArray( options )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(), 0, nil )

   local compileOp = ""
   for index, option in ipairs( options ) do
      compileOp = compileOp .. option .. " "
   end

   self:analyzeUnit( transUnit, compileOp, target )
end

function Analyzer:analyzeSourcePch( path )
   local transUnit = self.clangIndex:createTranslationUnit( path )

   
   self:analyzeUnit( transUnit, nil, nil )
end


function Analyzer:queryAt( refFlag, filePath, line, column, absFlag, target )
   local db = self:openDBForReadOnly()

   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo = db:getFileInfo( nil, filePath )
   if not fileInfo then
      log( 1, "not regist file", filePath )
      os.exit( 1 )
   end
   if not target then
      target = ""
   end
   local compileOp = ""
   local compInfo = db:mapCompInfo(
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

   if fileInfo.incFlag then
      -- ヘッダの場合は解析せずに DB 登録されている情報を使用する
      local nsInfo = db:getNsInfoAt( filePath, line, column )
      if nsInfo then
	 if refFlag then
	    Query:execWithDb( db, "r" .. (absFlag and "a" or ""), nsInfo.name )
	 else
	    Query:execWithDb( db, "t" .. (absFlag and "a" or ""), nsInfo.name )
	 end
      end
   else
      -- ソースの場合は解析して、指定位置の情報を使用する
      Helper.chdir( db:getSystemPath( fileInfo.currentDir ) )
      
      local args = clang.mkCharArray( optionList )
      local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
	 filePath, args:getLength(), args:getPtr(), 0, nil )

      local location = transUnit:getLocation(
	 transUnit:getFile( db:getSystemPath( fileInfo.path ) ), line, column )
      local cursor = transUnit:getCursor( location )

      local declCursor = cursor:getCursorReferenced()
      if refFlag then
	 db:SymbolRefInfoListForCursor(
	    declCursor,
	    function( item )
	       local nsInfo = db:getNamespace( item.nsId )
	       Query:printLocate(
		  db, nsInfo.name, item.fileId, item.line, absFlag, true )
	       return true
	    end	 
	 )
      else
	 local fileId, line = db:getFileIdLocation( declCursor )
	 Query:printLocate( db, cursor:getCursorSpelling(), fileId, line, absFlag, true )
      end
   end

   db:close()
end


return Analyzer
