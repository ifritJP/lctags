local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )



local SrcInfo = {
   -- 関数ポインタに設定している関数
   srcFunc = "func",
   -- 関数ポインタを保持するメンバ
   srcMember = "member",
   -- 関数ポインタを返す関数
   srcResult = "result",
   -- 関数ポインタの引数
   srcParam = "param",
}
function SrcInfo:new( db, cursor )
   local obj = {}
   local kind = cursor:getCursorKind()
   local nsInfo

   if kind == clang.core.CXCursor_DeclRefExpr then
      local declCursor = cursor:getCursorReferenced()
      nsInfo = db:getNamespaceFromCursor( declCursor )
      obj.src = self.srcFunc
   else
      if kind == clang.core.CXCursor_FunctionDecl then
	 nsInfo = db:getNamespaceFromCursor( cursor )
	 obj.src = self.srcResult
      elseif kind == clang.core.CXCursor_ParmDecl then
	 local funcCursor = cursor:getCursorSemanticParent()
	 nsInfo = db:getNamespaceFromCursor( funcCursor )
	 obj.src = self.srcParam

	 for index = 0, funcCursor:getNumArguments() do
	    local argCursor = funcCursor:getArgument( index )
	    if argCursor:hashCursor() == cursor:hashCursor() then
	       obj.paramIndex = index
	       break
	    end
	 end
      elseif kind == clang.core.CXCursor_FieldDecl then
	 nsInfo = db:getNamespaceFromCursor( cursor )
	 obj.src = self.srcMember
      else
	 log( 2, "SrcInfo:new fail",
	      cursor:getCursorSpelling(), clang.getCursorKindSpelling( kind ) )
	 return nil
      end
   end

   if not nsInfo then
      log( 2, "SrcInfo:new fail not nsInfo",
	   cursor:getCursorSpelling(), clang.getCursorKindSpelling( kind ) )
      return nil
   end
   obj.nsInfo = nsInfo

   setmetatable( obj,
		 { __index = SrcInfo, __tostring = self.__tostring } )

   return obj
end

function SrcInfo:equals( obj )
   return self.src == obj.src and self.nsInfo.id == obj.nsInfo.id and
      self.paramIndex == obj.paramIndex
end

function SrcInfo:__tostring()
   return string.format(
      "{ %s, %s, %s }", self.src, self.nsInfo.name, self.paramIndex )
end



local DynamicCall = {}

function DynamicCall:equalKind( cursor, kindList )
   local cursorKind = cursor:getCursorKind()
   for index, kind in ipairs( kindList ) do
      if cursorKind == kind then
	 return true
      end
   end
   return false
end

-- exprCursor 以降の kindList を探す
function DynamicCall:searchExpr( exprCursor, kindList, checkFunc )
   if self:equalKind( exprCursor, kindList ) then
      if not checkFunc or checkFunc( exprCursor ) then
	 log( 3, "searchExpr find", exprCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( exprCursor:getCursorKind() ) )
	 return exprCursor
      end
   end

   local result, list = clang.getChildrenList( exprCursor, nil, 2 )
   for index, info in ipairs( list ) do
      local cursor, parent, append = info[ 1 ], info[ 2 ], info[ 3 ]

      append = clang.getVisitAppendInfo( append )

      if self:equalKind( cursor, kindList ) then
	 if not checkFunc or checkFunc( cursor, list, index ) then
	    log( 3, "searchExpr find", cursor:getCursorSpelling(),
		 clang.getCursorKindSpelling( cursor:getCursorKind() ),
		 append.line, append.column )
	    return cursor
	 end
      end

      log( 4, "visit", cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ),
	   append.line, append.column )
   end

   return nil
end

-- exprCursor 以降の call cursor を探す。
-- 関数ポインタではない通常の call cursor は除外する。
function DynamicCall:searchCallExprWithOutNormal( exprCursor )
   local callExpr = self:searchExpr(
      exprCursor, { clang.core.CXCursor_CallExpr },
      function( cursor )
	 local declCursor = cursor:getCursorReferenced()
	 local kind = declCursor:getCursorKind()
	 log( 3, "searchCallExprWithOutNormal",
	      clang.getCursorKindSpelling( kind ) )
	 if kind == clang.core.CXCursor_FunctionDecl or
	    kind == clang.core.CXCursor_CXXMethod
	 then
	    return false
	 end
	 return true
      end
   )
   return callExpr
end

-- exprCursor 以降の参照 cursor を探す
function DynamicCall:searchRefExpr( exprCursor, checkFunc )
   local kindList = {
      clang.core.CXCursor_MemberRefExpr,
      clang.core.CXCursor_DeclRefExpr
   }
   return self:searchExpr( exprCursor, kindList, checkFunc )
end

-- exprCursor 以降の参照、あるいは Call cursor を探す
function DynamicCall:searchCallRefExpr( exprCursor, checkFunc )
   local kindList = {
      clang.core.CXCursor_CallExpr,
      clang.core.CXCursor_MemberRefExpr,
      clang.core.CXCursor_DeclRefExpr
   }
   return self:searchExpr( exprCursor, kindList, checkFunc )
end


-- compCursor の中から declCursor に設定している情報を探す
function DynamicCall:searchDecSet( compCursor, declCursor, checkFunc )
   log( 3, "searchDecSet", declCursor:getCursorSpelling() )

   local targetCursorList = {}
   
   if declCursor:getCursorKind() == clang.core.CXCursor_ParmDecl then
      table.insert( targetCursorList, declCursor )
   end


   local setRefFunc = function( targetCursor )
      local declCursor = targetCursor:getCursorReferenced()
      if targetCursor:getCursorKind() == clang.core.CXCursor_CallExpr then
	 table.insert( targetCursorList, declCursor )
      else
	 if declCursor:getCursorKind() == clang.core.CXCursor_FunctionDecl then
	    -- 関数コールでない関数の参照は、その関数ポインタを設定している
	    table.insert( targetCursorList, targetCursor )
	 else
	    table.insert( targetCursorList, declCursor )
	 end
      end
   end
   
   -- 変数の初期値取得
   local initCurosr
   clang.visitChildrenFast(
      declCursor,
      function( cursor, parent, exinfo, append )
	 initCurosr = cursor
      end, nil, nil, 1 )

  
   if initCurosr then
      log( 3, "searchDecSet init", initCurosr:getCursorSpelling(),
	   clang.getCursorKindSpelling( initCurosr:getCursorKind() ) )

      local targetCursor = self:searchCallRefExpr( initCurosr, checkFunc )
      if targetCursor then
	 log( 3, "searchDecSet init ", targetCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( targetCursor:getCursorKind() ) )
	 setRefFunc( targetCursor )
      end
   end

   local declHash = declCursor:hashCursor()
   local visitExprFunc
   local visitBinFunc
   local validFlag = false
   visitBinFunc = function( cursor, parent, exinfo, append )
      local kind = cursor:getCursorKind()
      if exinfo.leftFlag then
   	 exinfo.leftFlag = false
   	 if exinfo.op == "=" then
   	    if kind == clang.core.CXCursor_DeclRefExpr or
	       kind == clang.core.CXCursor_MemberRefExpr
	    then
   	       local work = cursor:getCursorReferenced()
   	       if declHash == work:hashCursor() then
   		  validFlag = true
		  append = clang.getVisitAppendInfo( append )
   		  log( 3, "visitBinFunc find",
		       cursor:getCursorSpelling(), append.line, append.column )
   	       end
   	    end
   	 end
      else
   	 if validFlag then
	    validFlag = false
	    append = clang.getVisitAppendInfo( append )
   	    local targetCursor = self:searchCallRefExpr( cursor, checkFunc )
   	    if targetCursor then
   	       log( 3, "searchDecSet find", targetCursor:getCursorSpelling(),
   		    clang.getCursorKindSpelling( targetCursor:getCursorKind() ),
		    append.line, append.column )
	       setRefFunc( targetCursor )
   	    end
   	    return 0
	 else
	    return visitExprFunc( cursor, parent, exinfo, append )
   	 end
      end

      return 1
   end

   visitExprFunc = function( cursor, parent, exinfo, append )
      append = clang.getVisitAppendInfo( append )
      local kind = cursor:getCursorKind()

      if kind == clang.core.CXCursor_BinaryOperator then
	 local opTxt = ""
	 local range = cursor:getCursorReferenceNameRange(
	    clang.core.CXNameRange_WantQualifier, 0 )
	 if range then
	    local unit = cursor:getTranslationUnit()

	    clang.mapRangePlainText(
	       unit.__ptr, range.__ptr,
	       function( token )
		  opTxt = token
	       end
	    )
	 end
	 if exinfo then
	    -- 最初の BinaryOperator は = として処理する
	    opTxt = "="
	 end

	 if opTxt == "=" then
	    validFlag = false
	    clang.visitChildrenFast( cursor, visitBinFunc,
				     { op = opTxt, leftFlag = true }, nil, 1 )
	 else
	    clang.visitChildrenFast( cursor, visitExprFunc, nil, nil, 1 )
	 end
      else
	 clang.visitChildrenFast( cursor, visitExprFunc, nil, nil, 1 )
      end
      -- if validFlag then
      -- 	 return 0
      -- end
      return 1
   end


   self:searchExprInComp( compCursor, visitExprFunc )

   return #targetCursorList ~= 0, targetCursorList
end


function DynamicCall:searchExprInComp( compCursor, visitExprFunc )
   local visitStmFunc
   visitStmFunc = function( cursor, parent, exinfo, append )
      local kind = cursor:getCursorKind()
      if clang.isExpression( kind ) then
	 return visitExprFunc( cursor, parent, true, append )
      elseif clang.isStatement( kind ) then
   	 clang.visitChildrenFast( cursor, visitStmFunc, nil, nil, 1 )
      elseif kind == clang.core.CXCursor_VarDecl then
	 -- 変数の初期化式の処理
	 return visitExprFunc( cursor, parent, true, append )
      end
      return 1
   end

   clang.visitChildrenFast( compCursor, visitStmFunc , nil, nil, 1 )
end


function DynamicCall:searchStmtInComp( compCursor, callback )
   local visitStmFunc
   visitStmFunc = function( cursor, parent, exinfo, append )
      local kind = cursor:getCursorKind()
      if clang.isExpression( kind ) then
	 return 1
      elseif clang.isStatement( kind ) then
	 if callback( cursor, parent, exinfo, append ) then
	    clang.visitChildrenFast( cursor, visitStmFunc, nil, nil, 1 )
	 end
      end
      return 1
   end

   clang.visitChildrenFast( compCursor, visitStmFunc , nil, nil, 1 )
end



-- compCursor 内で targetCursor に設定しているカーソルを探す
function DynamicCall:searchSrc( compCursor, targetCursor, checkFunc )
   if not compCursor then
      return nil
   end
   local findFlag = true
   while findFlag do
      local nsInfo
      local kind = targetCursor:getCursorKind()
      if kind == clang.core.CXCursor_DeclRefExpr then
	 local declCursor = targetCursor:getCursorReferenced()
	 local declKind = declCursor:getCursorKind()
	 log( 3, "searchSrc", clang.getCursorKindSpelling( declKind ) )
	 if declKind == clang.core.CXCursor_ParmDecl or
	    declKind == clang.core.CXCursor_VarDecl
	 then
	    findFlag, targetCursorList =
	       self:searchDecSet( compCursor, declCursor, checkFunc )
	    if declKind == clang.core.CXCursor_ParmDecl then
	       table.insert( targetCursorList, declCursor )
	    end
	    return targetCursorList
	 elseif declKind == clang.core.CXCursor_FunctionDecl then
	    return { targetCursor }
	 else
	    break
	 end
      elseif kind == clang.core.CXCursor_MemberRefExpr then
	 local declCursor = targetCursor:getCursorReferenced()
	 findFlag, targetCursorList =
	    self:searchDecSet( compCursor, declCursor, checkFunc )
	 return targetCursorList
      else
	 break
      end
   end
   return nil
end


-- メンバー名 funcpNsInfo に設定している処理を見つける。
-- 
-- @return 設定している SrcInfo のリスト
function DynamicCall:analyzeFuncPSetForNs( db, analyzer, funcpNsInfo )
   local refList = {}
   
   db:mapSymbolRef(
      funcpNsInfo.id,
      function( item )
	 table.insert( refList, item )
	 return true
      end
   )

   local srcInfoList = {}
   for index, item in ipairs( refList ) do
      local fileInfo = db:getFileInfo( item.fileId )
      log( 2,
	   funcpNsInfo.name, fileInfo.path, item.line, item.column )

      local path = db:getSystemPath( fileInfo.path )
      local unit, compileOp, newAnalyzer = analyzer:createUnit(
	 path, target, false, nil )

      local workSrcCursorList = self:analyzeFuncPSet(
	 db, unit, funcpNsInfo, item.line, item.column )
      for index, srcCursor in ipairs( workSrcCursorList ) do
	 local srcInfo = SrcInfo:new( db, srcCursor )
	 if srcInfo then
	    table.insert( srcInfoList, srcInfo )
	 end
      end
   end
   return srcInfoList
end

-- 名前 nsInfo を参照している中で fileInfo, line, column のものを探し、
-- その参照が属する名前空間のカーソルを取得する。
function DynamicCall:getBelongCompCursorForNs( db, unit, nsInfo, fileInfo, line, column )
   local refInfo
   db:mapSymbolRef(
      nsInfo.id,
      function( item )
	 if item.fileId == fileInfo.id and
	    ( item.line < line or
		 item.line == line and item.column <= column ) and
	    ( item.endLine > line or
		 item.endLine == line and item.endColumn >= column )
	 then
	    refInfo = item
	    return false
	 end
	 return true
      end
   )

   if not refInfo then
      log( 2, "getBelongCompCursorForNs: not found", nsInfo.name )
      return nil
   end

   local declInfo
   db:mapDecl(
      refInfo.belongNsId,
      function( item )
	 if item.fileId == fileInfo.id and
	    ( item.line < line or
		 item.line == line and item.column <= column ) and
	    ( item.endLine > line or
		 item.endLine == line and item.endColumn >= column )
	 then
	    declInfo = item
	    log( 2, "getBelongCompCursorForNs: find", item.endLine, item.endColumn - 1)
	    return false
	 end
	 return true
      end
   )

   local cxfile = unit:getFile( db:getSystemPath( fileInfo.path ) )
   local location = unit:getLocation(
      cxfile, declInfo.endLine, declInfo.endColumn - 1 )
   return unit:getCursor( location )
end

-- funcIdSet で指定した関数を解析するための Cursor を生成し、 callback を呼び出す。
function DynamicCall:analyzeInFunc( analyzer, db, funcIdSet, callback )
   -- 呼び出し元関数の定義情報取得
   local declList = {}
   for funcId in pairs( funcIdSet ) do
      db:mapDecl(
	 funcId,
	 function( item )
	    if item.hasBodyFlag == 1 then
	       table.insert( declList, item )
	       return false
	    end
	    return true
	 end
      )
   end

   -- 呼び出し元関数の定義カーソル取得
   for index, declInfo in ipairs( declList ) do
      local fileInfo = db:getFileInfo( declInfo.fileId )

      local unit, compileOp, newAnalyzer = analyzer:createUnit(
	 db:getSystemPath( fileInfo.path ), target, false, nil )

      
      local cxfile = unit:getFile( db:getSystemPath( fileInfo.path ) )
      local location = unit:getLocation(
	 cxfile, declInfo.endLine, declInfo.endColumn - 1 )
      local cursor = unit:getCursor( location )
      log( 2, "analyzeInFunc",
	   clang.getCursorKindSpelling( cursor:getCursorKind() ),
	   declInfo.endLine, declInfo.endColumn - 1 )

      callback( newAnalyzer, cursor )
   end
end

-- 関数 nsInfo が返している関数ポインタの SrcInfo のリストを探す。
function DynamicCall:getReturnSrcListForNs( analyzer, db, target, nsInfo )
   funcIdSet = {}
   funcIdSet[ nsInfo.id ] = 1

   log( 2, "getReturnSrcListForNs", nsInfo.name )
   local srcInfoList = {}
   self:analyzeInFunc(
      analyzer, db, funcIdSet,
      function( newAnalyzer, cursor )
	 local workSrcInfoList = self:searchReturnSrc( db, newAnalyzer, cursor )
	 self:addSrcInfo( srcInfoList, workSrcInfoList )
      end
   )
   return srcInfoList
end

-- compCursor 内の return している SrcInfo リストを返す
-- paramIndex 番目の引数を検索する
function DynamicCall:searchReturnSrc( db, analyzer, compCursor )
   local srcInfoList = {}

   self:searchStmtInComp(
      compCursor,
      function( cursor, parent, exinfo, append )
	 local kind = cursor:getCursorKind()
	 if kind ~= clang.core.CXCursor_ReturnStmt then
	    return true
	 end

	 append = clang.getVisitAppendInfo( append )

	 local refExpr = self:searchCallRefExpr( cursor )
	 if not refExpr then
	    log( 2, "searchReturnSrc not found ref", append.line, append.column )
	    return true
	 end

	 log( 2, "searchReturnSrc", append.line, append.column )

	 self:searchSrcInfo( db, analyzer, compCursor, refExpr, srcInfoList )
	 return true
      end
   )

   log( 2, "searchReturnSrc end", #srcInfoList )

   return srcInfoList
end


-- 関数 nsInfo をコールしているものを探し、
-- 関数のパラメータに設定している SrcInfo のリストを探す。
function DynamicCall:getBelongCompCursorForCallNs(
      analyzer, db, target, nsInfo, paramIndex )

   local count = 0
   local callerIdSet = {}
   db:mapCall(
      "nsId = " .. tostring( nsInfo.id ),
      function( item )
	 callerIdSet[ item.belongNsId ] = 1
	 count = count + 1
	 return true
      end
   )

   log( 2, "getBelongCompCursorForCallNs", count, nsInfo.name, paramIndex )

   local srcInfoList = {}
   self:analyzeInFunc(
      analyzer, db, callerIdSet,
      function( newAnalyzer, cursor )
	 local workSrcInfoList = self:searchParamSrc(
	    db, newAnalyzer, cursor, nsInfo, paramIndex )
	 self:addSrcInfo( srcInfoList, workSrcInfoList )
      end
   )
   return srcInfoList
end

-- compCursor 内の関数名 funcNsInfo を呼び出しているものの、
-- paramIndex 番目の引数を検索する
function DynamicCall:searchParamSrc( db, analyzer, compCursor, funcNsInfo, paramIndex )
   local srcInfoList = {}
   self:searchExprInComp(
      compCursor,
      function( cursor, parent, exinfo, append )
	 append = clang.getVisitAppendInfo( append )

	 -- funcNsInfo をコールしているカーソルを見つける
	 local callExpr = self:searchExpr(
	    cursor, { clang.core.CXCursor_CallExpr },
	    function( aCursor )
	       local declCursor = aCursor:getCursorReferenced()
	       local nsInfo = db:getNamespaceFromCursor( declCursor )
	       if not nsInfo or nsInfo.id ~= funcNsInfo.id then
		  return false
	       end
	       return true
	    end
	 )
	 if not callExpr then
	    return 1
	 end
	 local paramCursor = callExpr:getArgument( paramIndex )
	 if not paramCursor then
	    log( 2, "call param not found",
		 funcNsInfo.name, paramIndex, append.line, append.column )
	    return 1
	 end
	 paramCursor = self:searchCallRefExpr( paramCursor )
	 if not paramCursor then
	    log( 2, "call param not found ref", funcNsInfo.name, paramIndex,
		 append.line, append.column )
	    return 1
	 end

	 self:searchSrcInfo( db, analyzer, compCursor, paramCursor, srcInfoList )
	 return 1
      end
   )

   return srcInfoList
end



function DynamicCall:analyzeFuncPSet( db, unit, funcpNsInfo, line, column )
   local fileInfo = db:getFileInfo( nil, unit:getTranslationUnitSpelling() )
   local compCursor = self:getBelongCompCursorForNs(
      db, unit, funcpNsInfo, fileInfo, line, column )
   log( 2, "analyzeFuncPSet", compCursor:getCursorSpelling(), line, column )

   local refCursor = self:searchRefForNs( db, compCursor, funcpNsInfo, line, column )
   if not refCursor then
      return {}
   end
   
   local srcCursorList = self:searchSrc( compCursor, refCursor )

   if srcCursorList then
      local validSrcCursorList = {}
      for index, srcCursor in ipairs( srcCursorList ) do
	 local nsInfo = db:getNamespaceFromCursor( srcCursor )
	 if not nsInfo or nsInfo.id ~= funcpNsInfo.id then
	    log( 2, "call field src", srcCursor:getCursorSpelling(),
		 clang.getCursorKindSpelling( srcCursor:getCursorKind() ) )
	    table.insert( validSrcCursorList, srcCursor )
	 end
      end
      srcCursorList = validSrcCursorList
   end
   return srcCursorList
end

-- compCursor 以降で funcpNsInfo への参照するカーソルを探す
-- 
function DynamicCall:searchRefForNs( db, compCursor, funcpNsInfo, line, column )
   local refCursor
   local inRang = false

   self:searchExprInComp(
      compCursor,
      function( cursor, parent, exinfo, append )
	 local kind = cursor:getCursorKind()
	 append = clang.getVisitAppendInfo( append )
	 if ( append.line < line or
		 append.line == line and append.column <= column ) and
	    ( append.endLine > line or
		 append.endLine == line and append.endColumn >= column )
	 then
	    inRang = true
	    refCursor = self:searchRefExpr(
	       cursor,
	       function( aCursor, list, index )
		  if not refCursor then
		     local declCursor = aCursor:getCursorReferenced()
		     local nsInfo = db:getNamespaceFromCursor( declCursor )
		     if nsInfo and nsInfo.id == funcpNsInfo.id then
			log( 2, "searchRefForNs find", declCursor:getCursorSpelling() )
			return true
		     end
		  end
		  return false
	       end
	    )
	    if refCursor then
	       return 0
	    end
	    return 1
	 elseif inRang then
	    return 0
	 end
      end
   )
   log( 2, "searchRefForNs", funcpNsInfo.name, compCursor:getCursorSpelling(),
	clang.getCursorKindSpelling( compCursor:getCursorKind() ),
	line, column )
   return refCursor
end

function DynamicCall:addSrcInfo( srcInfoList, workSrcInfoList )
   if workSrcInfoList then
      for subIndex, workSrcInfo in ipairs( workSrcInfoList ) do
	 local existFlag = false
	 for infoIndex, srcInfo in ipairs( srcInfoList ) do
	    if workSrcInfo:equals( srcInfo ) then
	       existFlag = true
	       break
	    end
	 end
	 if not existFlag then
	    table.insert( srcInfoList, workSrcInfo )
	 end
      end
   end
end

-- compCursor 内で targetCursor の SrcInfo リストを検索し、 srcInfoList に追加
function DynamicCall:searchSrcInfo(
      db, analyzer, compCursor, targetCursor, srcInfoList )
   
   local srcCursorList = self:searchSrc( compCursor, targetCursor )
   local targetKind = targetCursor:getCursorKind()
   if targetKind == clang.core.CXCursor_MemberRefExpr then
      local declCursor = targetCursor:getCursorReferenced()
      table.insert( srcCursorList, declCursor )
   end
   
   for index, srcCursor in ipairs( srcCursorList ) do
      local srcKind = srcCursor:getCursorKind()
      log( 2, "srcKind", clang.getCursorKindSpelling( srcKind ))

      local funcpNsInfo
      local workSrcInfoList
      if srcKind == clang.core.CXCursor_FieldDecl then
	 funcpNsInfo = db:getNamespaceFromCursor( srcCursor )
	 if funcpNsInfo then
	    workSrcInfoList = self:analyzeFuncPSetForNs(
	       db, analyzer, funcpNsInfo )
	 end
      else
	 local srcInfo = SrcInfo:new( db, srcCursor )
	 if srcInfo then
	    workSrcInfoList = { srcInfo }
	 end
      end

      self:addSrcInfo( srcInfoList, workSrcInfoList )
   end
end

function DynamicCall:analyzeCaller(
      db, unit, analyzer, targetFullPath, indirectList, target )
   local cxfile = unit:getFile( targetFullPath )
   
   for index, item in ipairs( indirectList ) do
      local callee = db:getNamespace( item.nsId )
      local caller = db:getNamespace( item.belongNsId )
      log( 2, index, #indirectList, item.line, caller.name, "->", callee.name )

      local location = unit:getLocation( cxfile, item.line, item.column )
      local callCursor = unit:getCursor( location )
      local callKind = callCursor:getCursorKind()

      log( 2, "cursor", cxfile:getFileName(), item.line,
	   callCursor:getCursorSpelling(), clang.getCursorKindSpelling( callKind ) )

      local srcInfoList = {}
      local compCursor = callCursor:getCursorSemanticParent()
      local visitFunc

      self:searchExprInComp(
	 compCursor,
	 function( cursor, parent, exinfo, append )
	    append = clang.getVisitAppendInfo( append )
	    if ( append.line < item.line or append.line == item.line and
		    append.column <= item.column ) and
	       ( append.endLine > item.line or append.endLine == item.line and
		    append.endColumn >= item.column )
	    then
	       ;
	    else
	       return 1
	    end

	    while true do
	       -- callee をコールしているカーソルを見つける
	       local callExpr = self:searchCallExprWithOutNormal( cursor )
	       if not callExpr then
		  return 1
	       end

	       -- 関数コールしている関数ポインタの参照取得
	       targetCursor = self:searchRefExpr( callExpr )
	       if not targetCursor then
		  return 1
	       end
	       local declCursor = targetCursor:getCursorReferenced()
	       local typeCursor = clang.getDeclCursorFromType(
		  declCursor:getCursorType() )
	       local nsInfo = db:getNamespaceFromCursor( typeCursor )
	       if not nsInfo or nsInfo.id ~= callee.id then
		  cursor = targetCursor
	       else
		  self:searchSrcInfo(
		     db, analyzer, compCursor, targetCursor, srcInfoList )
		  return 0
	       end
	    end
	    return 1
	 end
      )

      local srcInfo2SrcInfoListMap = {}

      for loop = 1, 3 do 
	 local newSrcInfoList = {}
	 local stopLoopFlag = true
	 for infoIndex, srcInfo in ipairs( srcInfoList ) do
	    if srcInfo.src == SrcInfo.srcParam or srcInfo.src == SrcInfo.srcResult
	    then
	       stopLoopFlag = false
	       if srcInfo.src == SrcInfo.srcParam then
		  workSrcInfoList = self:getBelongCompCursorForCallNs(
		     analyzer, db, target, srcInfo.nsInfo, srcInfo.paramIndex )
	       else
		  workSrcInfoList = self:getReturnSrcListForNs(
		     analyzer, db, target, srcInfo.nsInfo )
	       end
	       srcInfo2SrcInfoListMap[ tostring( srcInfo ) ] = workSrcInfoList
	       for index, workSrcInfo in ipairs( workSrcInfoList ) do
		  table.insert( newSrcInfoList, workSrcInfo )
	       end
	    else
	       table.insert( newSrcInfoList, srcInfo )
	    end
	 end
	 srcInfoList = newSrcInfoList
	 if stopLoopFlag then
	    break
	 end
	 log( 2, "loop", loop )
      end
      if #srcInfoList > 0 then
	 for infoIndex, srcInfo in ipairs( srcInfoList ) do
	    log( 2, "call src", caller.name, callee.name, srcInfo )
	 end
      else
	 log( 2, "call src not found", caller.name, callee.name )
      end
   end
end

function DynamicCall:dumpInfo( dbPath, target )
   local db = DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   dbPath = db:convFullpath( dbPath )

   local indirectList = {}
   local fileId2IndirectListMap = {}
   
   db:mapJoin(
      "funcCall", "symbolDecl", "funcCall.nsId = symbolDecl.nsId",
      "symbolDecl.type = 20", nil,
      "funcCall.nsId, funcCall.belongNsId, " ..
	 "funcCall.fileId, funcCall.line, funcCall.column", 
      function( item )
	 local indirectList = fileId2IndirectListMap[ item.fileId ]
	 if not indirectList then
	    indirectList = {}
	    fileId2IndirectListMap[ item.fileId ] = indirectList
	 end
	 table.insert( indirectList, item )
	 return true
      end
   )

   db:close()
   db = nil

   if not target then
      target = ""
   end
   local analyzer = Analyzer:new( dbPath, false, false )

   for fileId, indirectList in pairs( fileId2IndirectListMap ) do
      db = DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
      local fileInfo = db:getFileInfo( fileId )
      local path = db:getSystemPath( fileInfo.path )
      local unit, compileOp, newAnalyzer = analyzer:createUnit(
	 path, target, false, nil )

      log( 2, "--- fileInfo ---", fileInfo.path )
      self:analyzeCaller( db, unit, newAnalyzer, path, indirectList, target )
      db:close()
   end
  
end

return DynamicCall
