local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )


local StackCalc = {}



function StackCalc:equalKind( cursor, kindList )
   local cursorKind = cursor:getCursorKind()
   for index, kind in ipairs( kindList ) do
      if cursorKind == kind then
	 return true
      end
   end
   return false
end

-- exprCursor 以降の kindList を探す
function StackCalc:searchExpr( exprCursor, kindList, checkFunc )
   if self:equalKind( exprCursor, kindList ) then
      if not checkFunc or checkFunc( exprCursor ) then
	 log( 3, "searchExpr find", exprCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( exprCursor:getCursorKind() ) )
      end
      return exprCursor
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

      log( 3, "visit", cursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( cursor:getCursorKind() ),
	   append.line, append.column )
   end

   return nil
end

-- exprCursor 以降の call cursor を探す。
-- 関数ポインタではない通常の call cursor は除外する。
function StackCalc:searchCallExpr( exprCursor )
   local callExpr = self:searchExpr(
      exprCursor, { clang.core.CXCursor_CallExpr },
      function( cursor )
	 local declCursor = cursor:getCursorReferenced()
	 local kind = declCursor:getCursorKind()
	 log( 3, "searchCallExpr", clang.getCursorKindSpelling( kind ) )
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
function StackCalc:searchRefExpr( exprCursor, checkFunc )
   local kindList = {
      clang.core.CXCursor_MemberRefExpr,
      clang.core.CXCursor_DeclRefExpr
   }
   return self:searchExpr( exprCursor, kindList, checkFunc )
end


-- compCursor の中から declCursor に設定している情報を探す
function StackCalc:searchDecSet( compCursor, declCursor, checkFunc )
   log( 3, "searchDecSet", declCursor:getCursorSpelling() )

   local targetCursorList = {}
   
   if declCursor:getCursorKind() == clang.core.CXCursor_ParmDecl then
      table.insert( targetCursorList, declCursor )
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

      local targetCursor = self:searchRefExpr( initCurosr, checkFunc )
      if targetCursor then
	 log( 3, "searchDecSet init ", targetCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( targetCursor:getCursorKind() ) )
	 table.insert( targetCursorList, targetCursor:getCursorReferenced() )
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
	    append = clang.getVisitAppendInfo( append )
   	    local targetCursor = self:searchRefExpr( cursor, checkFunc )
   	    if targetCursor then
   	       table.insert( targetCursorList, targetCursor:getCursorReferenced() )
   	       log( 3, "searchDecSet find", targetCursor:getCursorSpelling(),
   		    clang.getCursorKindSpelling( targetCursor:getCursorKind() ),
		    append.line, append.column )
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
      if validFlag then
	 return 0
      end
      return 1
   end

   local visitStmFunc
   visitStmFunc = function( cursor, parent, exinfo, append )
      local kind = cursor:getCursorKind()
      if clang.isExpression( kind ) then
	 return visitExprFunc( cursor, parent, true, append )
      elseif clang.isStatement( kind ) then
   	 clang.visitChildrenFast( cursor, visitStmFunc, nil, nil, 1 )
      end
      return 1
   end

   clang.visitChildrenFast( compCursor, visitStmFunc , nil, nil, 1 )

   return #targetCursorList ~= 0, targetCursorList
end

function StackCalc:searchSrc( compCursor, targetCursor, checkFunc )
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
	    return { declCursor }
	 else
	    break
	 end
      elseif kind == clang.core.CXCursor_MemberRefExpr then
	 local declCursor = targetCursor:getCursorReferenced()
	 findFlag, targetCursorList =
	    self:searchDecSet( compCursor, declCursor, checkFunc )
	 table.insert( targetCursorList, declCursor )
	 return targetCursorList
      else
	 break
      end
   end
   return nil
end


function StackCalc:analyzeFuncPSetForNs( db, analyzer, funcpNsInfo )
   local refList = {}
   
   db:mapSymbolRef(
      funcpNsInfo.id,
      function( item )
	 table.insert( refList, item )
	 return true
      end
   )

   for index, item in ipairs( refList ) do
      local fileInfo = db:getFileInfo( item.fileId )
      log( 2,
	   funcpNsInfo.name, fileInfo.path, item.line, item.column )

      local path = db:getSystemPath( fileInfo.path )
      local unit, compileOp, newAnalyzer = analyzer:createUnit(
	 path, target, false, nil )

      self:analyzeFuncPSet( db, unit, funcpNsInfo, item.line, item.column )
   end
end

function StackCalc:getDeclCursorForNs( db, unit, nsInfo, fileInfo, line, column )
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
      log( 2, "getDeclCursorForNs: not found", nsInfo.name )
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
	    log( 2, "getDeclCursorForNs: find", item.endLine, item.endColumn - 1)
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


function StackCalc:analyzeFuncPSet( db, unit, funcpNsInfo, line, column )
   local fileInfo = db:getFileInfo( nil, unit:getTranslationUnitSpelling() )
   local compCursor = self:getDeclCursorForNs(
      db, unit, funcpNsInfo, fileInfo, line, column )
   log( 2, "analyzeFuncPSet", compCursor:getCursorSpelling(), line, column )

   local refCursor = self:searchRefForNs( db, compCursor, funcpNsInfo, line, column )
   if not refCursor then
      return
   end
   
   local srcCursorList = self:searchSrc( compCursor, refCursor )

   if srcCursorList then
      for index, srcCursor in ipairs( srcCursorList ) do
	 log( 2, "call field src", srcCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( srcCursor:getCursorKind() ) )
      end
   end
end

-- compCursor 以降で funcpNsInfo への参照を探す
-- 
function StackCalc:searchRefForNs( db, compCursor, funcpNsInfo, line, column )
   local refCursor
   local visitFunc
   local inRang = false
   visitFunc = function( cursor, parent, exinfo, append )
      local kind = cursor:getCursorKind()
      if clang.isStatement( kind ) then
	 clang.visitChildrenFast( cursor, visitFunc, nil, nil, 1 )
      elseif clang.isExpression( kind ) then
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
      return 1
   end
   log( 2, "searchRefForNs", funcpNsInfo.name, compCursor:getCursorSpelling(),
	clang.getCursorKindSpelling( compCursor:getCursorKind() ),
	line, column )
   clang.visitChildrenFast( compCursor, visitFunc, nil, nil, 1 )
   return refCursor
end


function StackCalc:analyzeCaller( db, unit, analyzer, targetFullPath, indirectList )
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

      local compCursor = callCursor:getCursorSemanticParent()
      local visitFunc
      visitFunc = function( cursor, parent, exinfo, append )
	 local kind = cursor:getCursorKind()
	 if clang.isStatement( kind ) then
	    clang.visitChildrenFast( cursor, visitFunc, nil, nil, 1 )
	 elseif clang.isExpression( kind ) then
	    append = clang.getVisitAppendInfo( append )
	    if ( append.line < item.line or
		    append.line == item.line and
		    append.column <= item.column ) and
	       ( append.endLine > item.line or
		    append.endLine == item.line and
		    append.endColumn >= item.column )
	    then
	       local callExpr = self:searchCallExpr( cursor )
	       if not callExpr then
		  return 1
	       end
	       -- 関数コールしている関数ポインタの参照取得
	       targetCursor = self:searchRefExpr( callExpr )
	       if targetCursor then
		  -- 関数ポインタに代入している箇所を取得
		  local srcCursorList = self:searchSrc( compCursor, targetCursor )
		  for index, srcCursor in ipairs( srcCursorList ) do
		     local srcKind = srcCursor:getCursorKind()

		     local funcpNsInfo
		     if srcKind == clang.core.CXCursor_FieldDecl then
			funcpNsInfo = db:getNamespaceFromCursor( srcCursor )
			if funcpNsInfo then
			   self:analyzeFuncPSetForNs( db, analyzer, funcpNsInfo )
			end
		     end

		     log( 2, "call src", srcCursor:getCursorSpelling(),
			  clang.getCursorKindSpelling( srcCursor:getCursorKind() ),
			  funcpNsInfo and funcpNsInfo.name,
			  funcpNsInfo and funcpNsInfo.id )
		  end
		  return 0
	       end
	    end
	 end
	 return 1
      end
      clang.visitChildrenFast( compCursor, visitFunc, nil, nil, 1 )
   end
end

function StackCalc:dumpInfo( dbPath, target )
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
      self:analyzeCaller( db, unit, newAnalyzer, path, indirectList )
      db:close()
   end
  
end

return StackCalc
