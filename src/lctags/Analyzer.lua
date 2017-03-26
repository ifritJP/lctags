-- Copyright (C) 2017 ifritJP

local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )
local Query = require( 'lctags.Query' )
local Util = require( 'lctags.Util' )
local OutputCtrl = require( 'lctags.OutputCtrl' )
local Option = require( 'lctags.Option' )


local function dumpCursorInfo( cursor, depth, prefix, cursorOffset )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()

   log( 5,
	function()
	   return string.format(
	      "%s %s%s %s(%d) %d %s %s",
	      string.rep( "  ", depth ),
	      prefix and (prefix .. " ") or "", txt, 
	      clang.getCursorKindSpelling( cursorKind ), cursorKind,
	      cursor:hashCursor(), "",
	      cursorOffset or ""  )
	end
   )
end

local function getIncludeFileTable( path, compileOp )
   os.execute(
      string.format( "%s %s %s", arg[-1], arg[0], "" ))
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
      cursorKind == clang.core.CXCursor_ClassTemplate or
      cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_Constructor or
      cursorKind == clang.core.CXCursor_Destructor or
      cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_FunctionDecl or
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
   -- clang.core.CXCursor_MacroDefinition,
   -- clang.core.CXCursor_MacroExpansion,
   clang.core.CXCursor_InclusionDirective,
   clang.core.CXCursor_UnexposedDecl,
   clang.core.CXCursor_Namespace,
   clang.core.CXCursor_ClassDecl,
   clang.core.CXCursor_ClassTemplate,
   clang.core.CXCursor_TemplateTypeParameter,
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
   clang.core.CXCursor_NamespaceRef,
   clang.core.CXCursor_TemplateRef,
   clang.core.CXCursor_VarDecl,
   clang.core.CXCursor_CallExpr,
}


local targetKindNsList = {
   clang.core.CXCursor_Namespace,
   clang.core.CXCursor_ClassDecl,
   clang.core.CXCursor_ClassTemplate,
   clang.core.CXCursor_StructDecl,
   clang.core.CXCursor_UnionDecl,
   clang.core.CXCursor_Constructor,
   clang.core.CXCursor_Destructor,
   clang.core.CXCursor_CXXMethod,
   clang.core.CXCursor_FunctionDecl,
   clang.core.CXCursor_EnumDecl
}

local targetKindPreproList = {
   clang.core.CXCursor_MacroDefinition,
   clang.core.CXCursor_MacroExpansion,
   clang.core.CXCursor_InclusionDirective,
}


local function checkChangeFile( analyzer, cursor )
   local uptodateFlag
   
   local cxfile, line = getFileLoc( cursor )

   if analyzer.currentFile ~= cxfile and
      ( not analyzer.currentFile or not cxfile or
	   not analyzer.currentFile:isEqual( cxfile ) )
   then
      local path = ""
      if cxfile then
	 path = DBCtrl:convFullpath( cxfile:getFileName(), analyzer.currentDir )
      end

      if not analyzer.checkPreproFlag then
	 table.insert(
	    analyzer.incBelongList,
	    { cxfile = cxfile, namespace = analyzer:getNowNs() } )
      end

      analyzer.currentFile = cxfile
      local spInfo = analyzer.path2InfoMap[ path ]
      if not spInfo then
	 spInfo = analyzer:createSpInfo( path, false )
      else
	 if not spInfo.aleadyCheckedFlag and
	    analyzer.prevIncFile and analyzer.prevIncFile:isEqual( cxfile )
	 then
	    -- 読み込む箇所で定義が切り替わるインクルードファイルはプリコンパイル禁止
	    spInfo.inhibitToPrecomileFlag = true
	    log( 2, "inhibitToPrecomile", path )
	 end
      end
      analyzer.currentSpInfo = spInfo
      uptodateFlag = spInfo.uptodateFlag
      log( 3,
	   function()
	      return "changeCurrentFile:", path, analyzer.currentSpInfo.digest, uptodateFlag
	   end
      )
   end

   return uptodateFlag
end


--[[
   次を解析する。
   ・namespace 内での #include がないか？
   ・その namespace, #include は解析済みか？ 
]]
local function visitFuncNsInc( cursor, parent, analyzer, exInfo )
   local cursorKind = cursor:getCursorKind()
   local cursorOffset = tostring( exInfo[ 2 ] )

   dumpCursorInfo( cursor, analyzer.depth, "visitFuncNsInc:", cursorOffset )

   if exInfo[ 1 ] or analyzer.returnVisit then
      analyzer.returnVisit = false
      checkChangeFile( analyzer, cursor )
   end
   analyzer.prevIncFile = nil

   local currentSpInfo = analyzer.currentSpInfo

   if cursorKind == clang.core.CXCursor_InclusionDirective then
      -- local cxfile = cursor:getIncludedFile()
      -- local path = ""
      -- if cxfile then
      -- 	 path = DBCtrl:convFullpath( cxfile:getFileName(),analyzer.currentDir )
      -- end
      -- local spInfo = analyzer.path2InfoMap[ path ]
      -- if not spInfo then
      -- 	 spInfo = analyzer:createSpInfo( path, true, cxfile )
      -- end
      
      table.insert( analyzer.incList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
      return 1
   end
      
   if cursorKind == clang.core.CXCursor_MacroDefinition then
      table.insert( currentSpInfo.macroDefList, cursor )
      calcDigestTxt( cursorOffset, analyzer.currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
      return 1
   end

   if cursorKind == clang.core.CXCursor_MacroExpansion then
      local declCursor = cursor:getCursorReferenced()
      analyzer:addRef( analyzer.macroRefList, cursor, declCursor, nil )
      -- マクロ参照はヘッダの多重 include 抑止に利用していることが多い。
      -- これを digest 計算に加えると、include 抑止の ifdef が差分で引っかかるので
      -- digest には加えずに、ハッシュだけ登録する
      analyzer:registCursor( cursor, exInfo )
      -- analyzer.cursorHash2SpInfoMap[ cursor:hashCursor() ] = analyzer.currentSpInfo
      return 1
   end

   if isNamespaceDecl( cursorKind ) then
      analyzer.depth = analyzer.depth + 1

      table.insert( analyzer.namespaceList, cursor )
      
      clang.visitChildrenFast2(
	 cursor, visitFuncNsInc, analyzer,
	 targetKindPreproList, targetKindNsList, { analyzer.targetFile }, 1 )
      analyzer.returnVisit = true

      analyzer.depth = analyzer.depth - 1
   end

end


local function visitFuncMain( cursor, parent, analyzer, exInfo )
   local cursorKind = cursor:getCursorKind()

   local uptodateFlag
   local cursorOffset = tostring( exInfo[ 2 ] )

   dumpCursorInfo( cursor, analyzer.depth, nil, cursorOffset )

   if exInfo[ 1 ] or analyzer.returnVisit then
      analyzer.returnVisit = false
      uptodateFlag = checkChangeFile( analyzer, cursor )
   end
   analyzer.prevIncFile = nil

   local currentSpInfo = analyzer.currentSpInfo
   
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
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
   end

   if cursorKind == clang.core.CXCursor_InclusionDirective then
      analyzer.prevIncFile = cursor:getIncludedFile()
      return
   end
   
   --[[
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
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
      return 1
   end
   --]]
      
   if cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_ClassDecl or
      cursorKind == clang.core.CXCursor_ClassTemplate or
      cursorKind == clang.core.CXCursor_EnumDecl
   then
      if cursor:getCursorSpelling() == "" then
	 currentSpInfo.anonymousCount =
	    currentSpInfo.anonymousCount + 1
      end
      local list
      if cursorKind == clang.core.CXCursor_EnumDecl then
	 list = currentSpInfo.enumList
      elseif cursorKind == clang.core.CXCursor_StructDecl then
	 list = currentSpInfo.structList
      elseif cursorKind == clang.core.CXCursor_UnionDecl then
	 list = currentSpInfo.unionList
      elseif cursorKind == clang.core.CXCursor_ClassDecl or
	 cursorKind == clang.core.CXCursor_ClassTemplate
      then
	 list = currentSpInfo.classList
      end
	 
      table.insert( list, { cursor, currentSpInfo } )
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
   elseif cursorKind == clang.core.CXCursor_TemplateTypeParameter then
      local nsObj = analyzer.nsLevelList[ #analyzer.nsLevelList ]
      tmpTypeList = nsObj.tmpTypeList
      if not tmpTypeList then
	 tmpTypeList = {}
	 nsObj.tmpTypeList = tmpTypeList
      end
      table.insert( tmpTypeList, cursor )
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
   elseif cursorKind == clang.core.CXCursor_FunctionDecl or
      cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_Constructor      
   then
      table.insert( currentSpInfo.funcList, cursor )
      analyzer.currentFunc = cursor
      table.insert( endProcess, function() analyzer.currentFunc = nil end )
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )

      local declCursor = clang.getDeclCursorFromType(
	 cursor:getCursorType():getResultType() )
      analyzer:addTypeRef( cursor, declCursor, exInfo )
      --[[
   elseif cursorKind == clang.core.CXCursor_MacroDefinition then
      table.insert( analyzer.macroDefList, cursor )
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
   elseif cursorKind == clang.core.CXCursor_MacroExpansion then
      local declCursor = cursor:getCursorReferenced()
      analyzer:addRef( analyzer.macroRefList, cursor, declCursor, nil )
      -- マクロ参照はヘッダの多重 include 抑止に利用していることが多い。
      -- これを digest 計算に加えると、include 抑止の ifdef が差分で引っかかるので
      -- digest には加えずに、ハッシュだけ登録する
      -- analyzer:registCursor( cursor, exInfo )
      analyzer.cursorHash2SpInfoMap[ cursor:hashCursor() ] = currentSpInfo
      --]]
   elseif clang.isReference( cursorKind ) then
      if cursorKind ~= clang.core.CXCursor_NamespaceRef then
	 local declCursor = cursor:getCursorReferenced()
	 local namespace = analyzer:getNowNs()
	 analyzer:addRef( analyzer.refList, cursor, declCursor, namespace )
	 calcDigestTxt( cursorOffset, currentSpInfo )
	 analyzer:registCursor( cursor, exInfo )
	 calcDigest( declCursor, currentSpInfo )
	 calcDigest( namespace, currentSpInfo )
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
	    analyzer:addRef( analyzer.refList, cursor, declCursor, namespace )
	    calcDigestTxt( cursorOffset, currentSpInfo )
	    analyzer:registCursor( cursor, exInfo )
	    calcDigest( declCursor, currentSpInfo )
	    calcDigest( namespace, currentSpInfo )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_CallExpr then
      local namespace = analyzer:getNowNs()
      table.insert( analyzer.callList,
		    { cursor = cursor, namespace = namespace } )
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )
      calcDigest( namespace, currentSpInfo )
   elseif cursorKind == clang.core.CXCursor_TypedefDecl then
      table.insert( currentSpInfo.typedefList, cursor )
      calcDigestTxt( cursorOffset, currentSpInfo )
      analyzer:registCursor( cursor, exInfo )

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
	 table.insert( currentSpInfo.wideAreaValList, cursor )
	 calcDigestTxt( cursorOffset, currentSpInfo )
	 analyzer:registCursor( cursor, exInfo )
      end
   elseif cursorKind == clang.core.CXCursor_FieldDecl or
      cursorKind == clang.core.CXCursor_EnumConstantDecl
   then
      analyzer:addMember( cursor, cursorKind, cursorOffset, exInfo )
   end

   if not recursiveFlag then
      if cursorKind == clang.core.CXCursor_Namespace or
	 cursorKind == clang.core.CXCursor_ClassDecl or
	 cursorKind == clang.core.CXCursor_ClassTemplate or
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
	 analyzer.returnVisit = true

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

function Analyzer:newAs( recordDigestSrcFlag, displayDiagnostics, currentDir )
   if currentDir then
      Helper.chdir( currentDir )
   end
   return Analyzer:new(
      self.dbPath, recordDigestSrcFlag, displayDiagnostics, currentDir )
end

function Analyzer:new(
      dbPath, recordDigestSrcFlag, displayDiagnostics, currentDir )
   if not currentDir then
      currentDir = os.getenv( "PWD" )
   end
   local obj = {
      clangIndex = clang.createIndex( 0, displayDiagnostics and 1 or 0 ),
      currentDir = currentDir,

      dbPath = DBCtrl:convFullpath( dbPath, currentDir ),
      recursiveBaseKind = clang.core.CXCursor_InvalidFile,

      recordDigestSrcFlag = recordDigestSrcFlag,
      displayDiagnostics = displayDiagnostics,
      
      depth = 0,
      targetFile = nil,
      currentFile = nil,
      nsList = {},

      namespaceList = {},
      
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

      -- hash -> range
      hash2RangeMap = {},
      cursor2RangeMap = {},
   }

   setmetatable( obj, { __index = Analyzer } )
   return obj
end

function Analyzer:addRange( cursor, exInfo )
   local info = {
      cxfile = self.currentFile, range = exInfo
   }
   self.hash2RangeMap[ cursor:hashCursor() ] = info
   self.cursor2RangeMap[ cursor ] = info
end

function Analyzer:addRef( list, cursor, declCursor, namespace )
   table.insert(
      list,
      { cursor = cursor, cxfile = self.currentFile,
	declCursor = declCursor, namespace = namespace } )
end



function Analyzer:getNowNs()
   return self.nsLevelList[ #self.nsLevelList ].cursor
end

function Analyzer:enterNs( cursor, cursorKind )
   local nsObj = { cursor = cursor, memberList = {} }
   if #self.nsLevelList > 0 then
      nsObj.parentNs = self.nsLevelList[ #self.nsLevelList ].cursor
   end
   
   if cursorKind == clang.core.CXCursor_StructDecl or
      cursorKind == clang.core.CXCursor_UnionDecl or
      cursorKind == clang.core.CXCursor_EnumDecl
   then
      nsObj.digestObj = Helper.openDigest( "md5" )
   end
   self.hash2NsObj[ cursor:hashCursor() ] = nsObj
   table.insert( self.nsLevelList, nsObj )
end

function Analyzer:exitNs()
   local nsObj = self.nsLevelList[ #self.nsLevelList ]
   if nsObj.digestObj then
      nsObj.fixDigest = nsObj.digestObj:fix()
   end
   table.remove( self.nsLevelList )
end


function Analyzer:addTypeRef( cursor, declCursor, exInfo )
   local declKind = declCursor:getCursorKind() 
   if declKind ~= clang.core.CXCursor_NoDeclFound then
      table.insert(
	 self.refList,
	 { cursor = cursor, declCursor = declCursor,
	   cxfile = self.currentFile, namespace = self:getNowNs() } )
      self:registCursor( cursor, exInfo )
   end
end

function Analyzer:addMember( cursor, cursorKind, cursorOffset, exInfo )
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
      self:addTypeRef( cursor, declCursor, exInfo )
   end

   local declKind = declCursor:getCursorKind()
   if declKind == clang.core.CXCursor_StructDecl or
      declKind == clang.core.CXCursor_UnionDecl or
      declKind == clang.core.CXCursor_EnumDecl
   then
      if declCursor:getCursorSpelling() == "" then
	 self.anonymousHash2VarDeclMap[ declCursor:hashCursor() ] = cursor
	 log( 3,
	      function()
		 return "anonymousHash2VarDeclMap:", declCursor:hashCursor()
	      end
	 )
	 
      end
   end
end

function Analyzer:createSpInfo( path, uptodateFlag, cxfile )
   local spInfo = {
      classList = {},
      funcList = {},
      typedefList = {},
      enumList = {},
      structList = {},
      unionList = {},
      wideAreaValList = {},
      macroDefList = {},
   }

   self.path2InfoMap[ path ] = spInfo
   spInfo.path = path
   spInfo.digest = Helper.openDigest( "md5" )
   spInfo.anonymousCount = 0
   spInfo.uptodateFlag = uptodateFlag
   spInfo.cxfile = cxfile
   spInfo.infoCount = 0

   

   if self.recordDigestSrcFlag then
      local target = self.targetFilePath
      spInfo.recordDigest =
	 io.open( string.gsub( path, ".*/", "digest." .. target .. "." ), "w" )
   end

   return spInfo
end

function Analyzer:registCursor( cursor, exInfo )
   self.cursorHash2SpInfoMap[ cursor:hashCursor() ] = self.currentSpInfo
   calcDigest( cursor, self.currentSpInfo )
   self:addRange( cursor, exInfo )
   self.currentSpInfo.infoCount = self.currentSpInfo.infoCount + 1
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

function Analyzer:openDBForWrite( message )
   return DBCtrl:open( self.dbPath, false, self.currentDir, message )
end

function Analyzer:isUptodate( db, filePath, compileOp, target, unsavedFile )

   local targetFileInfo
   local needUpdateFlag = false
   local sameAsOtherTarget = false
   
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
	       sameAsOtherTarget = true
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
	       digest = Util:calcTextDigest( unsavedFile.Contents )
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

	 local incFileIdSet = db:getIncludeCache( targetFileInfo )
	 
	 -- 取得した全ファイルの情報から、ファイルが更新されているかどうかチェック
	 local fileId2updateInfoMap = {}
	 local uptodateFlag = true
	 for fileId in pairs( incFileIdSet ) do
	    local fileInfo = db:getFileInfo( fileId )
	    local systemPath = db:getSystemPath( fileInfo.path )
	    local modTime = Helper.getFileModTime( systemPath )
	    if fileInfo.path ~= "" and
	       ( not modTime or targetFileInfo.updateTime < modTime )
	    then
	       uptodateFlag = false
	       log( 1, "detect modified", fileInfo.path,
		    targetFileInfo.updateTime, systemPath, modTime )
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
	    for fileId in ipairs( incFileIdSet ) do
	       local fileInfo = db:getFileInfo( fileId )
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

   if not success then
      log( 1, result )
   end

   if result and ( needUpdateFlag or sameAsOtherTarget ) then
      -- uptodate で needUpdateFlag の場合、ファイルの更新日時だけ違う。
      -- 次回のチェック時間を短縮するため、updateTime を更新する。
      db = self:openDBForWrite( "update time" )
      db:setUpdateTime( targetFileInfo.id, Helper.getCurrentTime() )
      db:updateCompileOp( targetFileInfo, target, compileOp )
      db:close()
   end
   
   return result
end

function Analyzer:getRangeFromCursor( cursor )
   local rangeInfo = self.cursor2RangeMap[ cursor ]
   if not rangeInfo then
      rangeInfo = self.hash2RangeMap[ cursor:hashCursor() ]
      if not rangeInfo then
	 return nil
      end
   end
   
   local range = rangeInfo.range
   local startInfo = { rangeInfo.cxfile, range[ 3 ], range[ 4 ], range[ 2 ] }
   local endInfo = { rangeInfo.cxfile, range[ 5 ], range[ 6 ], range[ 7 ] }
   return startInfo, endInfo
end



function Analyzer:analyzeUnit( transUnit, compileOp, target )
   
   local targetPath = transUnit:getTranslationUnitSpelling()
   log( -1, string.gsub( targetPath, ".*/", "" ) .. ":" )

   log( 3,
	function()
	   return "start", compileOp, os.clock(), os.date()
	end
   )

   log( transUnit )

   local root = transUnit:getTranslationUnitCursor()
   log( root )


   self.targetFile =
      transUnit:getFile( transUnit:getTranslationUnitSpelling() )
   self.currentFile = self.targetFile
   local targetFullPath = DBCtrl:convFullpath( targetPath, self.currentDir )
   self.currentSpInfo = self:createSpInfo( targetFullPath, false, self.targetFile )


   log( 2, "checkPrepro", os.clock(), os.date() )
   self.depth = 0
   self.checkPreproFlag = true
   clang.visitChildrenFast2(
      root, visitFuncNsInc, self,
      targetKindPreproList, targetKindNsList, { self.targetFile }, 1 )
   self.checkPreproFlag = false


   -- 解析対象の cxfile リスト
   local targetFileList = { self.targetFile }
   
   local preproDigestList = {}
   local db = self:openDBForReadOnly()

   for path, spInfo in pairs( self.path2InfoMap ) do
      spInfo.fixPreproDigest = spInfo.digest:fix()
      spInfo.digest = Helper.openDigest( "md5" )
      spInfo.digest:write( spInfo.fixPreproDigest )
   end

   -- インクルードファイルの prepro digest を確認して、解析済みかどうか確認する。
   -- 解析済みでなければ targetFileList に入れて解析対象にする
   local incFilePathSet = {}
   for incIndex, inclusion in ipairs( self.incList ) do
      local incFile = inclusion:getIncludedFile()
      local incFullPath = ""
      if incFile then
	 incFullPath = DBCtrl:convFullpath( incFile:getFileName(), self.currentDir )
      end
      local fileInfo = db:getFileInfo( nil, incFullPath )
      if fileInfo and fileInfo.invalidSkip ~= 0 then
	 if not incFilePathSet[ incFullPath ] then
	    incFilePathSet[ incFullPath ] = 1
	    table.insert( targetFileList, incFile )
	    log( 2, "This file has invalidSkip", incFullPath )
	 end
      else
	 local cxfile, line, column, offset = clang.getCursorLocation( inclusion )

	 local spInfo = self.path2InfoMap[ incFullPath ]
	 if not spInfo then
	    spInfo = self:createSpInfo( incFullPath, false )
	 end
	 
	 local belongNsName = ""
	 for index, cursor in ipairs( self.namespaceList ) do
	    local nsList, fullname = clang.getNamespaceList( cursor, true )

	    local startInfo, endInfo = db:getRangeFromCursor( cursor )
	    if startInfo[ 1 ]:isEqual( cxfile ) and startInfo[ 4 ] <= offset and
	       endInfo[ 1 ]:isEqual( cxfile ) and endInfo[ 4 ] >= offset
	    then
	       log( 2, "find include in namespace", inclusion:getCursorSpelling() )
	       log( cursor:getCursorSpelling(), spInfo.fixPreproDigest,
		    fullname, nsInfo and nsInfo.id or "none" )

	       belongNsName = fullname
	       break
	    end
	 end

	 local nsInfo = db:getNamespace( nil, belongNsName )
	 local alreadyFlag = false
	 if nsInfo and fileInfo then
	    log( 3, "check prepro", nsInfo.name, fileInfo.path, spInfo.fixPreproDigest )
	    if db:existsPrepro( fileInfo.id, nsInfo.id, spInfo.fixPreproDigest ) and
	       fileInfo.id ~= systemFileId
	    then
	       local modTime = Helper.getFileModTime( incFullPath )
	       if fileInfo.updateTime >= modTime then
		  alreadyFlag = true
		  log( 3, "already analyzed", fileInfo.path )
	       end
	    end
	 end
	 if not alreadyFlag then
	    if not incFilePathSet[ incFullPath ] then
	       incFilePathSet[ incFullPath ] = 1
	       table.insert( targetFileList, incFile )
	       if fileInfo then
		  log( 2, "new prepro", fileInfo.path )
	       end
	    end
	    table.insert( preproDigestList,
			  { fullPath = incFullPath, nsName = belongNsName,
			    digest = spInfo.fixPreproDigest } )
	 end
      end
   end

   db:close()

   for path, spInfo in pairs( self.path2InfoMap ) do
      spInfo.aleadyCheckedFlag = true
   end

   log( 2, "visitChildren start", #targetFileList, #self.incList, os.clock(), os.date() )
   self.depth = 0
   clang.visitChildrenFast2(
      root, visitFuncMain, self,
      { clang.core.CXCursor_Namespace }, targetKindList, targetFileList,  1 )
   log( 2, "visitChildren end", os.clock(), os.date() )

   local db = self:openDBForWrite( "analyze" )
   if not db then
      log( 1, "db open error" )
      os.exit( 1 )
   end
   log( 2, "db open", os.clock(), os.date() )

   db:setFuncToGetRangeCursor(
      function( db, cursor )
	 return self:getRangeFromCursor( cursor )
      end
   )

   log( 2, "-- precompile --", os.clock(), os.date() )

   -- コンパイルオプション文字列を、オプション配列に変換
   local optionList = {}   
   for option in string.gmatch( compileOp, "([^%s]+)" ) do
      table.insert( optionList, option )
   end
   local stdMode = self:getStdMode( optionList, targetPath )
   table.insert( optionList, "-x" )
   if string.find( stdMode, "c++", 1, true ) == 1 then
      table.insert( optionList, "c++-header" )
   else
      table.insert( optionList, "c-header" )
   end

   -- pch ファイルの作成は現時点であまり効果が見込めないので生成しない
   log( 2, "not support to generate pch" )
   -- local checkedSet = {}
   -- for index, inclusion in ipairs( self.incList ) do
   --    local startInfo, endInfo = self:getRangeFromCursor( inclusion )
   --    local fullPath = db:convFullpath( startInfo[ 1 ]:getFileName() )
   --    local incfile = inclusion:getIncludedFile()
   --    if incfile and not self.targetFile:isEqual( incfile ) and
   -- 	 db:isInProjFile( fullPath )
   --    then
   -- 	 -- プロジェクト内のファイルからインクルードしているファイルを
   -- 	 -- プリコンパイル対象とする
   -- 	 local incFilePath = db:convFullpath( incfile:getFileName() )
   -- 	 if not checkedSet[ incFilePath ] then
   -- 	    checkedSet[ incFilePath ] = 1
   -- 	    local spInfo = self.path2InfoMap[ incFilePath ]
   -- 	    if not spInfo.inhibitToPrecomileFlag and spInfo.infoCount > 100 then
   -- 	       self:createPrecompileFile(
   -- 		  incFilePath,
   -- 		  db:getPchPath( incFilePath, target, stdMode ), optionList )
   -- 	       spInfo.hasPch = true
   -- 	    end
   -- 	 end
   --    end
   -- end
   
   
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
      compileOp, self.currentDir, true, target, 0 )
   
   -- 残りのヘッダファイルを登録
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      local cxfile = transUnit:getFile( db:getSystemPath( filePath ) )
      if not cxfile or not cxfile:isEqual( self.targetFile ) then
	 spInfo.fixDigest = spInfo.digest:fix()
	 spInfo.fileInfo = db:addFile(
	    filePath, Helper.getCurrentTime(), spInfo.fixDigest,
	    stdMode, nil, false, target, spInfo.hasPch )
      end
   end
   
   log( 2, "-- nsList --", os.clock(), os.date() )
   for index, cursor in ipairs( self.nsList ) do
      db:addNamespace( cursor )
   end

   local fileId2IncFileInfoListMap = {}
   
   log( 2, "-- inc --", os.clock(), os.date() )
   for index, inclusion in ipairs( self.incList ) do
      local cxfile = inclusion:getIncludedFile()
      local path = cxfile and cxfile:getFileName() or ""
      path = db:convFullpath( path )
      local spInfo = self.path2InfoMap[ path ]
      spInfo.fileInfo = db:addInclude(
	 inclusion, spInfo.fixDigest, fileId2IncFileInfoListMap )
   end

   log( 2, "-- prepro -- ", os.clock(), os.date() )
   for index, prepro in ipairs( preproDigestList ) do
      db:addPrepro( prepro.fullPath, prepro.nsName, prepro.digest )
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
      Util:profile(
	 function()
	    self:registerToDB( db, fileId2IncFileInfoListMap, targetSpInfo )
	 end, "profi." .. string.gsub( targetPath, ".*/", "" ) )
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

   local anonymousId = "anonymous"
   if name == "" then
      if typedefInfo then
	 anonymousId = typedefName
      else
	 local varCur = self.anonymousHash2VarDeclMap[ hash ]
	 if varCur then
	    anonymousId = varCur:getCursorSpelling()
	 end
      end
   end

   db:addEnumStructDecl(
      cursor, string.format( anonymousForm, anonymousId ), typedefName, kind, nsObj )

   -- local tmpTypeList = info[ 3 ]
   -- if tmpTypeList then
   --    log( 3, "-- tmpTypeList --", os.clock(), os.date()  )
   --    for index, cursor in ipairs( tmpTypeList ) do
   -- 	 db:addNamespace( cursor )
   --    end
   -- end
end

function Analyzer:registerSpInfo( db, spInfo )
   if spInfo.fileInfo.id ~= db.systemFileId then
      local targetName = string.gsub( self.targetFilePath, ".*/", "" )
      local currentName = string.gsub( spInfo.fileInfo.path, ".*/", "" )
      if targetName ~= currentName then
	 log( -1, string.format( "%s:%s:", targetName, currentName ) )
      else
	 log( -1, string.format( "%s:", targetName ) )
      end
   end
   
   log( 2, "-- macroDefList --", os.clock(), os.date()  )
   for index, macroDef in ipairs( spInfo.macroDefList ) do
      db:addNamespace( macroDef )
   end

   -- typedef を先に処理する
   log( 2, "-- typedef -- ", os.clock(), os.date() )
   for index, info in ipairs( spInfo.typedefList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      
      db:addNamespace( info )
   end
   
   log( 2, "-- classList --", os.clock(), os.date()  )
   for index, info in ipairs( spInfo.classList ) do
      self:processStructEnum( db, info, "<class_%s>",
			      clang.CXCursorKind.FieldDecl.val )
   end
   
   log( 2, "-- funcList --", os.clock(), os.date()  )
   for index, funcDecl in ipairs( spInfo.funcList ) do
      log( funcDecl:getCursorSpelling() )
      db:addNamespace( funcDecl )
   end
   

   log( 2, "-- enum -- ", os.clock(), os.date()  )
   for index, info in ipairs( spInfo.enumList ) do
      self:processStructEnum( db, info, "<enum_%s>",
			      clang.CXCursorKind.EnumConstantDecl.val )
   end
   
   log( 2, "-- struct -- ", os.clock(), os.date()  )
   for index, info in ipairs( spInfo.structList ) do
      self:processStructEnum( db, info, "<struct_%s>",
			      clang.CXCursorKind.FieldDecl.val )
   end

   log( 2, "-- union -- ", os.clock(), os.date()  )
   for index, info in ipairs( spInfo.unionList ) do
      self:processStructEnum( db, info, "<union_%s>",
			      clang.CXCursorKind.FieldDecl.val )
   end
   
   log( 2, "-- wideAreaVal -- ", os.clock(), os.date()  )
   for index, info in ipairs( spInfo.wideAreaValList ) do
      log( info:getCursorSpelling(),
	   clang.getCursorKindSpelling( info:getCursorKind() ) )
      db:addNamespace( info )
   end
end

function Analyzer:registerToDB( db, fileId2IncFileInfoListMap, targetSpInfo )

   db:setFuncToGetFileInfoFromCursor(
      function( db, cursor )
	 local spInfo = self.cursorHash2SpInfoMap[ cursor:hashCursor() ]
	 if not spInfo then
	    log( 3,
		 function()
		    return "notfound", cursor:getCursorSpelling(),
		    clang.getCursorKindSpelling( cursor:getCursorKind() )
		 end
	    )

	    local cxfile = getFileLoc( cursor )
	    local path = ""
	    if cxfile ~= nil then
	       path = DBCtrl:convFullpath( cxfile:getFileName(), self.currentDir )
	    end
	    spInfo = self.path2InfoMap[ path ]
	    self.cursorHash2SpInfoMap[ cursor:hashCursor() ] = spInfo
	 end
	 if not spInfo then
	    return nil
	 end
	 return spInfo.fileInfo
      end
   )

   for filePath, spInfo in pairs( self.path2InfoMap ) do
      -- ヘッダの情報を登録する。
      -- 影響の少ないソースファイル内の定義は、
      -- ロックをかけずに処理し後で整合性を取る。
      -- uptodate のヘッダ情報は変更がないので登録処理しない。
      
      local fileInfo = spInfo.fileInfo

      if not fileInfo.uptodate and fileInfo.incFlag ~= 0 then
	 self:registerSpInfo( db, spInfo )
      end
   end

   db:commit()


   --- DB のロック時間を少しでも削減するため、
   --- これ以降は、DB に直接記録しないでメモリ上に格納しておき、
   --- 最後に DB に反映する。
   
   db:beginForTemp()

   self:registerSpInfo( db, targetSpInfo )

   log( 2, "-- macroRefList --", os.clock(), os.date()  )
   for index, macroRef in ipairs( self.macroRefList ) do
      db:addReference( macroRef )
   end

   log( 2, "-- incBelong --", os.clock(), os.date() )
   for index, incBelong in ipairs( self.incBelongList ) do
      db:addIncBelong( incBelong )
   end

   log( 2, "-- incCache --", os.clock(),  os.date() )
   db:addIncludeCache(
      db:getFileInfo( nil, self.targetFile:getFileName() ),
      fileId2IncFileInfoListMap )

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

   log( 2, "-- update token digest -- ", os.clock(), os.date()  )
   for filePath, spInfo in pairs( self.path2InfoMap ) do
      db:addTokenDigest( spInfo.fileInfo.id, spInfo.fixDigest )
   end
end


function Analyzer:update( path, target )
   if not target then
      target = ""
   end

   -- これが呼ばれるときは更新が必要な時だけなので、uptodate チェックしない。
   local transUnit, compileOp, analyzer = self:createUnit( path, target, false )

   if transUnit == "uptodate" then
      return
   end

   analyzer:analyzeUnit( transUnit, compileOp, target )
end

function Analyzer:createUnit( path, target, checkUptodateFlag, fileContents )
   self.targetFilePath = path

   if not target then
      target = ""
   end
   
   local db = self:openDBForReadOnly()

   local targetFullPath = db:getSystemPath( path )
   
   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo, optionList = db:getFileOpt( path, target )
   if fileInfo.incFlag ~= 0 then
      fileInfo = db:getSrcForIncOne( fileInfo, target )
      fileInfo, optionList = db:getFileOpt( fileInfo.path, target )
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

   local compDir = db:getSystemPath( fileInfo.currentDir )

   if checkUptodateFlag then
      if analyzer:isUptodate( db, targetFullPath, nil, target ) then
	 return "uptodate"
      end
   end

   local incFileIdSet = db:getIncludeCache( fileInfo )
   local includeList = {}
   for incId in pairs( incFileIdSet ) do
      table.insert(
	 includeList, db:getSystemPath( db:getFileInfo( incId ).path ) )
   end
   
   db:close()

   if not fileInfo then
      log( -2, "file not found" )
      os.exit( 0 )
   end
   
   if not optionList then
      log( -2, "skip this file since unmatch target" )
      os.exit( 0 )
   end


   local analyzer = self:newAs(
      self.recordDigestSrcFlag, self.displayDiagnostics, compDir )
   analyzer.targetFilePath = targetFullPath


   local stdMode = self:getStdMode( optionList, targetFullPath )
   
   local newOptList = { table.unpack( optionList )}
   -- for index, incPath in ipairs( includeList ) do
   --    local pchPath = db:getPchPath( incPath, target, stdMode )
   --    local modTime = Helper.getFileModTime( pchPath )
   --    if modTime and modTime > Helper.getFileModTime( incPath ) then
   -- 	 --table.insert( newOptList, "-Xclang" )
   -- 	 table.insert( newOptList, "-include-pch" )
   -- 	 table.insert( newOptList, pchPath )
   --    end
   -- end

   local args = clang.mkCharArray( newOptList )
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
   local unit = analyzer.clangIndex:createTranslationUnitFromSourceFile(
      targetFullPath, args:getLength(), args:getPtr(),
      unsavedFileArray:getLength(), unsavedFileArray:getPtr() )
   log( 2, "end createTrans", os.clock(), os.date() )

   local compileOp = ""
   for index, option in ipairs( optionList ) do
      compileOp = compileOp .. option .. " "
   end

   return unit, compileOp, analyzer, stdMode
end


function Analyzer:getStdMode( optionList, targetPath )
   local stdMode = "c89"
   if not string.find( targetPath, "%.c$" ) then
      stdMode = "c++98"
   end
   for index, option in ipairs( optionList ) do
      if string.find( option, "-std=", 1, true ) == 1 then
	 stdMode = option:sub( #"-std=" + 1 )
	 break
      end
   end
   return stdMode
end

function Analyzer:onlyRegister( path, options, target )
   log( -1, string.gsub( path, ".*/", "" ) .. ":" )
   
   if not target then
      target = ""
   end

   local compileOp = ""
   for index, option in ipairs( options ) do
      compileOp = compileOp .. option .. " "
   end

   local includeList = self:getIncludeList( path, compileOp )

   local db = self:openDBForReadOnly()
   local dependFilePath = db:getMiscPath( "depend", target, path )
   db:close()
   Util:mkdirWithParent( string.gsub( dependFilePath, "/[^/]+$", "" ) )

   local fileHandle = io.open( dependFilePath, "w" )
   fileHandle:write( db:convFullpath( path ) .. "\n" )
   fileHandle:write( self.currentDir .. "\n" )
   fileHandle:write( compileOp .. "\n" )
   for index, incPath in ipairs( includeList ) do
      fileHandle:write( incPath .. "\n" )
   end
   
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

   
   local stdMode = self:getStdMode( options, path )

   local db = self:openDBForReadOnly()
   
   local newOptList = { table.unpack( options ) }
   -- local includeList = self:getIncludeList( path, compileOp )
   -- for index, incPath in ipairs( includeList ) do
   --    local pchPath = db:getPchPath( incPath, target, stdMode )
   --    local modTime = Helper.getFileModTime( pchPath )
   --    if modTime and modTime > Helper.getFileModTime( incPath ) then
   -- 	 table.insert( newOptList, "-include-pch" )
   -- 	 table.insert( newOptList, pchPath )
   -- 	 log( 3, "pch", pchPath )
   --    end
   -- end

   local uptodate = self:isUptodate(
      db, path, compileOp, target, unsavedFileTable and unsavedFileTable[1] )
   db:close()
   
   if uptodate then
      return
   end
   
   local args = clang.mkCharArray( newOptList )
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )


   ---[[
   local transUnit = self.clangIndex:parseTranslationUnit(
      path, args:getPtr(), args:getLength(), 
      unsavedFileArray:getPtr(), unsavedFileArray:getLength(),
      clang.core.CXTranslationUnit_DetailedPreprocessingRecord )
   --]]
   
   --[[
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(),
      unsavedFileArray:getLength(), unsavedFileArray:getPtr() )
   --]]

   self:analyzeUnit( transUnit, compileOp, target )
end

function Analyzer:analyzeSourceAtWithFunc(
      targetFullPath, line, column,
      optionList, target, fileContents, func )
   
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


   log( 2, "analyzeSourceAtWithFunc:", self.currentDir,
	targetFullPath, line, column )
   
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )
   local transUnit = self.clangIndex:parseTranslationUnit(
      targetFullPath, args:getPtr(), args:getLength(), 
      unsavedFileArray:getPtr(), unsavedFileArray:getLength(),
      clang.core.CXTranslationUnit_DetailedPreprocessingRecord +
	 clang.core.CXTranslationUnit_Incomplete )

   log( 2, "createTranslationUnitFromSourceFile: end" )
   
   local cxfile = transUnit:getFile( targetFullPath )
   local location = transUnit:getLocation( cxfile, line, column )
   local cursor = transUnit:getCursor( location )

   log( 2, "getCursor: end" )

   local declCursor = cursor:getCursorReferenced()
   if clang.isDeclaration( cursor:getCursorKind() ) then
      declCursor = cursor
   end
   
   log( 2,
	function()
	   local cxfile, line = getFileLoc( declCursor )
	   return "cursor", cursor:getCursorSpelling(), clang.getCursorKindSpelling( cursor:getCursorKind() ), clang.getCursorKindSpelling( declCursor:getCursorKind() ),
	   cxfile and cxfile:getFileName(), line
	end
   )

   local db = self:openDBForReadOnly( self.currentDir )
   
   func( db, db:getFileInfo( nil, targetFullPath ).id,  nil, declCursor, cursor )
   
   db:close()
end


function Analyzer:queryAtFunc(
      filePath, line, column, target, funcFlag, fileContents, func )
   local db = self:openDBForReadOnly()

   -- filePath の target に対応するコンパイルオプションを取得
   local fileInfo, optionList = db:getFileOpt( filePath, target )

   local equalDigestFlag
   if fileInfo.incFlag == 0 and
      fileInfo.updateTime >= Helper.getFileModTime( filePath )
   then
      -- ソースで解析後に編集していない
      if fileContents then
	 if fileInfo.digest == Util:calcTextDigest( fileContents ) then
	    -- コンテンツ指定されている場合は、ソースと変更されていない時
	    equalDigestFlag = true
	 end
      else
	 equalDigestFlag = true
      end
   end
   
   if fileInfo.incFlag ~= 0 or equalDigestFlag then
      -- 解析せずに DB 登録されている情報を使用する
      local nsInfo = db:getNsInfoAt(
	 fileInfo, line, column, fileContents, true )
      if nsInfo then
	 local needAnalyzeFlag = false
	 if funcFlag then
	    db:mapDecl(
	       nsInfo.id,
	       function( item )
		  if item.type == clang.core.CXCursor_FieldDecl then
		     needAnalyzeFlag = true
		  end
		  return false
	       end
	    )
	 end

	 if not needAnalyzeFlag then
	    log( 2, "queryAtFunc", nsInfo.id, nsInfo.name )
	    func( db, fileInfo.id, nsInfo, nil )
	    db:close()
	    return
	 end
      else
	 if fileInfo.incFlag ~= 0 then
	    log( 1, "not found namespace" )
	    os.exit( 1 )
	 end
      end
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
   
      
   local currentDir = db:getSystemPath( fileInfo.currentDir )
   local targetFilePath = db:getSystemPath( fileInfo.path )

   local analyzer = self:newAs(
      self.recordDigestSrcFlag, self.displayDiagnostics, currentDir )
   
   db:close()

   analyzer:analyzeSourceAtWithFunc(
      targetFilePath, line, column,
      optionList, target, fileContents, func )
end




function Analyzer:queryAt(
      mode, filePath, line, column, absFlag, target, fileContents )
   self:queryAtFunc(
      filePath, line, column, target, mode == "call-at", fileContents,
      function( db, targetFileId, nsInfo, declCursor, cursor )
	 if nsInfo then
	    if mode == "ref-at" then
	       Query:execWithDb( db, "r" .. (absFlag and "a" or ""), nsInfo.name )
	    elseif mode == "def-at" then
	       Query:execWithDb( db, "t" .. (absFlag and "a" or ""), nsInfo.name )
	    elseif mode == "call-at" then
	       Query:execWithDb( db, "C" .. (absFlag and "a" or ""), nsInfo.name )
	    elseif mode == "ns-at" then
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
      filePath, line, column, target,
      graph == "caller" or graph == "callee", nil,
      function( db, targetFileId, nsInfo, declCursor )
	 if nsInfo then
	    if graph == "caller" or graph == "callee" then
	       Query:outputCallRelation(
		  db, nsInfo.name, graph == "caller", depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	    elseif graph == "systemPath" then
	       Query:outputSymbolRefRelation(
		  db, nsInfo.name, depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	    elseif graph == "symbol" then
	       Query:outputSymbolRefRelation(
		  db, nsInfo.name, depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	       return false
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
		  db, nsInfo.name, graph == "caller", depthLimit,
		  OutputCtrl.dot, browseFlag, outputFile, imageFormat )
	    elseif graph == "symbol" then
	       db:SymbolRefInfoListForCursor(
		  declCursor,
		  function( item )
		     local nsInfo = db:getNamespace( item.nsId )
		     Query:outputSymbolRefRelation(
			db, nsInfo.name, depthLimit,
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

function Analyzer:createPrecompileFile(
      inputPath, outputPath, optionList, fileContents )

   local modTime = Helper.getFileModTime( outputPath )
   if modTime then
      if Helper.getFileModTime( inputPath ) < modTime then
	 return
      end
   end

   log( 2, "createPrecompileFile", inputPath, outputPath )
   
   Util:mkdirWithParent( string.gsub( outputPath, "/[^/]+$", "" ) )
   

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


   local unit = self.clangIndex:parseTranslationUnit(
      inputPath, args:getPtr(), args:getLength(), 
      unsavedFileArray:getPtr(), unsavedFileArray:getLength(),
      clang.core.CXTranslationUnit_DetailedPreprocessingRecord +
      clang.core.CXTranslationUnit_ForSerialization )

   unit:saveTranslationUnit(
      outputPath, unit:defaultSaveOptions() )
end

function Analyzer:getIncludeList( path, compileOp )
   local newCompileOp = ""
   for token in string.gmatch( compileOp, "[^%s]+" ) do
      newCompileOp = newCompileOp .. string.format( '"%s" ', token )
   end
   compileOp = newCompileOp

   local command = string.format(
      "%s %s depIncs %s %s", arg[-1], arg[0], compileOp, path )
   local pipe = io.popen( command )
   log( 3, "getIncludeList", command )

   local basename = string.gsub( path, ".*/([^/]+)$", "%1" )
   
   local incList = {}
   while true do
      local txt = pipe:read( '*l' )
      if not txt then
	 break
      end
      for incPath in string.gmatch( txt, "[^%s\\]+" ) do
	 if not string.find( incPath, ":$" ) then
	    if string.gsub( incPath, ".*/([^/]+)$", "%1" ) ~= basename then
	       incPath = DBCtrl:convFullpath( incPath, self.currentDir )
	       table.insert( incList, incPath )
	       log( 3, "getIncludeList", incPath )
	    end
	 end
      end
   end
   return incList
end

function Analyzer:dumpIncludeList( path, options, unsavedFileTable )
   options = { "-M", table.unpack( options ) }

   local args = clang.mkCharArray( options )
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )
   local transUnit = self.clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(),
      unsavedFileArray:getLength(), unsavedFileArray:getPtr() )
end

function Analyzer:convFullpath( path )
   return DBCtrl:convFullpath( path, self.currentDir )
end

return Analyzer
