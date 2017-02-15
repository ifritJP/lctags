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
      isFuncDecl( cursorKind )
end

local function calcDigest( cursor, digest )
   if not digest then
      return
   end
   digest:write( cursor:getCursorSpelling() )
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
	    path = cxfile:getFileName()
	 end

	 table.insert( analyzer.incBelongList,
		       { cxfile = cxfile,
			 namespace = analyzer.nsList[ #analyzer.nsList ] } )

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
      table.insert( analyzer.nsList, cursor )
      table.insert( endProcess, function() table.remove( analyzer.nsList ) end )
      calcDigest( cursor, analyzer.currentSpInfo.digest )

   end

   
   if cursorKind == clang.core.CXCursor_InclusionDirective then
      local cxfile = cursor:getIncludedFile()
      local path = cxfile:getFileName()
      local spInfo = analyzer.path2InfoMap[ path ]
      if not spInfo then
	 spInfo = analyzer:createSpInfo( path, true, cxfile )
      end
      
      table.insert( analyzer.incList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo.digest )
      return 1
   elseif cursorKind == clang.core.CXCursor_ClassDecl then
      table.insert( analyzer.nsList, cursor )
      table.insert( endProcess, function() table.remove( analyzer.nsList ) end )

      table.insert( analyzer.classList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo.digest )
      
   elseif cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_EnumDecl
   then
      table.insert( analyzer.nsList, cursor )
      table.insert( endProcess, function() table.remove( analyzer.nsList ) end )
      
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
      calcDigest( cursor, analyzer.currentSpInfo.digest )
   elseif cursorKind == clang.core.CXCursor_FunctionDecl then
      table.insert( analyzer.funcList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigest( cursor, analyzer.currentSpInfo.digest )
      
   elseif cursorKind == clang.core.CXCursor_MacroDefinition then
      table.insert( analyzer.macroDefList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo.digest )

   elseif cursorKind == clang.core.CXCursor_MacroExpansion then
      local declCursor = cursor:getCursorReferenced()
      table.insert( analyzer.macroRefList,
		    { cursor = cursor, declCursor = declCursor, namespace = nil } )
      calcDigest( cursor, analyzer.currentSpInfo.digest )
      
   elseif cursorKind == clang.core.CXCursor_CXXMethod then
      table.insert( analyzer.methodList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigest( cursor, analyzer.currentSpInfo.digest )
      
   elseif clang.isReference( cursorKind ) then
      if cursorKind ~= clang.core.CXCursor_NamespaceRef then
	 --local declCursor = cursor:getCursorReferenced()
	 local declCursor = cursor:getCursorDefinition()
	 table.insert(
	    analyzer.refList,
	    { cursor = cursor, declCursor = declCursor,
	      namespace = analyzer.nsList[ #analyzer.nsList ] } )
	 calcDigest( cursor, analyzer.currentSpInfo.digest )
      end
   elseif cursorKind == clang.core.CXCursor_DeclRefExpr or
      cursorKind == clang.core.CXCursor_MemberRefExpr
   then
      local declCursor = cursor:getCursorDefinition()
      if declCursor:getCursorKind() ~= clang.core.CXCursor_ParmDecl then
	 local storageClass = declCursor:getStorageClass()
	 if storageClass ~= clang.core.CX_SC_Auto and
	    storageClass ~= clang.core.CX_SC_Register
	 then
	    table.insert(
	       analyzer.refList,
	       { cursor = cursor, declCursor = declCursor,
		 namespace = analyzer.nsList[ #analyzer.nsList ] } )
	    calcDigest( cursor, analyzer.currentSpInfo.digest )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_TypedefDecl then
      table.insert( analyzer.typedefList, cursor )
      calcDigest( cursor, analyzer.currentSpInfo.digest )
   elseif cursorKind == clang.core.CXCursor_VarDecl then
      if analyzer.currentFunc == nil then
	 table.insert( analyzer.wideAreaValList, cursor )
	 calcDigest( cursor, analyzer.currentSpInfo.digest )
      end
   end

   if cursorKind == clang.core.CXCursor_Namespace or
      cursorKind == clang.core.CXCursor_ClassDecl or
      (not uptodateFlag and
	  (isFuncDecl( cursorKind ) or
	      cursorKind == clang.core.CXCursor_CompoundStmt or
	      clang.core.clang_isStatement( cursorKind ) ) )
   then
      analyzer.depth = analyzer.depth + 1
      
      local ret = cursor:visitChildren( visitFuncMain, analyzer )
      
      analyzer.depth = analyzer.depth - 1
   end
   
   for index, process in ipairs( endProcess ) do
      process()
   end
   
   return 1
end



local Analyzer = {}

function Analyzer:new( dbPath )
   local obj = {
      clangIndex = clang.createIndex( 1, 1 ),
      currentDir = os.getenv( "PWD" ),

      dbPath = dbPath,
      
      depth = 0,
      targetFile = nil,
      currentFile = nil,
      nsList = {},
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

   return spInfo
end

function Analyzer:openDBForReadOnly()
   return DBCtrl:open( self.dbPath, true, self.currentDir )
end

function Analyzer:openDBForWrite()
   return DBCtrl:open( self.dbPath, false, self.currentDir )
end

function Analyzer:isUptodate( unit )
   local db = self:openDBForReadOnly()
   if not db then
      log( 2, "db open error" )
      return false
   end

   list = {}

   local success, result = pcall(
      function()
	 -- 解析対象のファイル情報リストを取得
	 local targetFileInfo =
	    db:getFileInfo( nil, unit:getTranslationUnitSpelling() )
	 if not targetFileInfo then
	    log( 2, "not found target in db" )
	    return false
	 end
	 table.insert( list, targetFileInfo )
	 local incFileList =
	    db:getRowList( "incRef", "baseFileId == " .. targetFileInfo.id )
	 for index, incFile in pairs( incFileList ) do
	    local incFileInfo = db:getFileInfo( incFile.id )
	    if not incFileInfo then
	       log( 2, "not found incFile in db", incFile.id )
	       return false
	    end
	    table.insert( list, incFileInfo )
	 end

	 -- ファイル情報リストから、ファイルが更新されているかどうかチェック
	 local fileId2InfoMap = {}
	 local uptodateFlag = true
	 for index, fileInfo in ipairs( list ) do
	    local cxfile = unit:getFile( db:getSystemPath( fileInfo.path ) )
	    if fileInfo.modTime ~= cxfile:getFileTime() then
	       uptodateFlag = false
	       log( 1, "detect modified", fileInfo.path,
		    fileInfo.modTime, cxfile:getFileTime() )
	    end
	    fileId2InfoMap[ fileInfo.id ] =
	       self:createSpInfo( fileInfo.path, uptodateFlag, cxfile )
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
	    for index, fileInfo in ipairs( list ) do
	       local spInfo = fileId2InfoMap[ fileInfo.id ]
	       local parentIdSet = {}
	       db:mapRowList(
		  "incBelong", "id = " .. tostring( fileInfo.id ), nil, "baseFileId",
		  function ( item )
		     local parentInfo = fileId2InfoMap[ item.baseFileId ]
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
	       for fileId, spInfo in pairs( fileId2InfoMap ) do
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


function Analyzer:analyzeUnit( transUnit, compileOp )
   log( 2, "start", os.clock(), os.date() )

   local target = transUnit:getTranslationUnitSpelling()
   log( -1, string.gsub( target, ".*/", "" ) )
   
   if self:isUptodate( transUnit ) then
      return
   end

   log( transUnit )

   local root = transUnit:getTranslationUnitCursor()
   log( root )


   self.targetFile =
      transUnit:getFile( transUnit:getTranslationUnitSpelling() )
   self.currentFile = self.targetFile

   log( 2, "visitChildren", os.clock(), os.date() )
   root:visitChildren( visitFuncMain, self )

   log( 2, "db open", os.clock(), os.date() )
   local db = self:openDBForWrite()
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end
   

   log( "-- file --", os.clock(), os.date() )
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      log( filePath )
      local cxfile = transUnit:getFile( db:getSystemPath( filePath ) )
      local isTarget = cxfile and cxfile:isEqual( self.targetFile )
      db:addFile( filePath, cxfile and cxfile:getFileTime() or 0,
		  spInfo.digest:fix(),
		  isTarget and compileOp, isTarget and self.currentDir )
   end
   log( "-- inc --", os.clock(), os.date() )
   for index, inclusion in ipairs( self.incList ) do
      db:addInclude( inclusion )
   end

   log( "-- macroDefList --", os.clock(), os.date()  )
   for index, macroDef in ipairs( self.macroDefList ) do
      db:addNamespace( macroDef )
   end

   log( "-- macroRefList --", os.clock(), os.date()  )
   for index, macroRef in ipairs( self.macroRefList ) do
      db:addReference( macroRef )
   end
   
   -- typedef を先に処理する
   log( "-- typedef -- ", os.clock(), os.date() )
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
   
   log( "-- classList --", os.clock(), os.date()  )
   for index, cursor in ipairs( self.classList ) do
      log( cursor:getCursorSpelling() )
      db:addNamespace( cursor )
   end

   log( "-- funcList --", os.clock(), os.date()  )
   for index, funcDecl in ipairs( self.funcList ) do
      log( funcDecl:getCursorSpelling() )
      db:addNamespace( funcDecl )
   end
   
   log( "-- methodList --", os.clock(), os.date()  )
   for index, methodDecl in ipairs( self.methodList ) do
      log( methodDecl:getCursorSpelling() )
      db:addNamespace( methodDecl )
   end
  

   log( "-- enum -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.enumList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )
      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      db:addEnumDecl( info[1], string.format( "<anonymous_enum_%d>", info[2] ),
		      typedefInfo and typedefInfo.typedef:getCursorSpelling() or "" )
   end
   
   log( "-- struct -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.structList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )

      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      db:addStructDecl( cursor, string.format( "<anonymous_struct_%d>", info[2] ),
			typedefInfo and typedefInfo.typedef:getCursorSpelling() or "" )
   end

   log( "-- union -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.unionList ) do
      local cursor = info[ 1 ]
      log( cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ) )

      local typedefInfo = self.hashCursor2TypedefMap[ cursor:hashCursor() ]
      db:addStructDecl( cursor, string.format( "<anonymous_union_%d>", info[2] ),
			typedefInfo and typedefInfo.typedef:getCursorSpelling() or "" )
   end
   
   log( "-- wideAreaVal -- ", os.clock(), os.date()  )
   for index, info in ipairs( self.wideAreaValList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      db:addNamespace( info )
   end

   log( "-- incBelong --", os.clock(), os.date() )
   for index, incBelong in ipairs( self.incBelongList ) do
      db:addIncBelong( incBelong )
   end

   log( "-- refList --", os.clock(), os.date()  )
   for index, refInfo in ipairs( self.refList ) do
      log( refInfo.cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( refInfo.cursor:getCursorKind() ) )
      db:addReference( refInfo )
   end

   log( 2, "close", os.clock(), os.date()  )
   db:close()
end

function Analyzer:analyzeSource( path, options )
   local args = clang.mkCharArray( options )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(), 0, nil )

   local compileOp = ""
   for index, option in ipairs( options ) do
      compileOp = compileOp .. option .. " "
   end

   self:analyzeUnit( transUnit, compileOp )
end

function Analyzer:analyzeSourcePch( path )
   local transUnit = self.clangIndex:createTranslationUnit( path )

   
   self:analyzeUnit( transUnit, nil )
end

function Analyzer:initDB( projDir )
   if not projDir then
      projDir = self.currentDir
   end
   if projDir ~= "/" and not string.find( projDir, "/$" ) then
      projDir = projDir .. "/"
   end
   
   
   os.remove( self.dbPath )
   DBCtrl:init( self.dbPath, os.getenv( "PWD" ), projDir )
end

function Analyzer:shrinkDB()
   DBCtrl:shrinkDB( self.dbPath )
end


function Analyzer:queryAt( refFlag, filePath, line, column, absFlag )
   local db = self:openDBForReadOnly()

   local fileInfo = db:getFileInfo( nil, filePath )
   local compileOp = ""

   local optionList = {}
   for option in string.gmatch( fileInfo.compOp, "([^ ]+)" ) do
      table.insert( optionList, option )
   end

   if compileOp == "" then
      local nsInfo = db:getNsInfoAt( filePath, line, column )
      if nsInfo then
	 if refFlag then
	    Query:execWithDb( db, "r" .. (absFlag and "a" or ""), nsInfo.name )
	 else
	    Query:execWithDb( db, "t" .. (absFlag and "a" or ""), nsInfo.name )
	 end
      end
   else
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
