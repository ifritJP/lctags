local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )
local Split = {}

function visit( cursor, parent, info, addInfo )
   local symbol2DeclMap = info.symbol2DeclMap
   local cursorKind = cursor:getCursorKind()

   Util:dumpCursorInfo( cursor, 1, "", 0 )
   local cxtype = cursor:getCursorType()
   log( 2, cxtype:getTypeSpelling() )

   if cursorKind == clang.core.CXCursor_DeclRefExpr then
      local declCursor = cursor:getCursorDefinition()
      local declCursorKind = declCursor:getCursorKind()
      if declCursorKind == clang.core.CXCursor_ParmDecl or
	 declCursorKind == clang.core.CXCursor_VarDecl
      then
	 local pointerFlag = false
	 local symbol = declCursor:getCursorSpelling()
	 if symbol == "" then
	    error( "illegal symbol" )
	 end
	 symbol2DeclMap[ symbol ] = {
	    cursor = declCursor,
	    pointerAccess = false,
	    binOpAccess = false,
	    arrayAccess = false,	    
	 }
	 table.insert( info.refList, cursor )
	 local range = cursor:getCursorExtent()
      end
   elseif cursorKind == clang.core.CXCursor_UnaryOperator then
      table.insert( info.unaryList, cursor )
   elseif cursorKind == clang.core.CXCursor_BinaryOperator then
      table.insert( info.binOpList, cursor )
   elseif cursorKind == clang.core.CXCursor_ArraySubscriptExpr then
      table.insert( info.arrayList, cursor )
   end

   return 1
end

function getDeclTxt( declCursorInfo )
   local pointerFlag = isNeedPassAddress( declCursorInfo )
   local cursor = declCursorInfo.cursor
   
   local cxtype = cursor:getCursorType()
   local typeTxt = cxtype:getTypeSpelling()
   local prefix = string.gsub( typeTxt, "%[.*", "", 1 )
   local suffix = string.gsub( typeTxt, "[^%[]+%[", "[", 1 )
   if prefix == suffix then
      suffix = ""
   end
   local name = cursor:getCursorSpelling()
   local callArg = name
   if pointerFlag then
      callArg = "&" .. name
      name = "p" .. name
      prefix = prefix .. "* "
   else
      prefix = prefix .. " "
   end
   local txt = prefix .. name .. suffix
   txt = string.gsub( txt, "  +", " " )
   return txt, callArg
end

function isNeedPassAddress( info )
   return not info.arrayAccess and (info.pointerAccess or info.binOpAccess)
end

function outputCode(
      stream, fileContents, startOffset, endOffset, refList, symbol2DeclMap )
   for index, ref in ipairs( refList ) do
      local sym = ref:getCursorSpelling()
      local declCursorInfo = symbol2DeclMap[ sym ]
      if declCursorInfo and isNeedPassAddress( declCursorInfo )
      then
	 local range = ref:getCursorExtent()
	 local frontFile, frontLine, frontColmn, frontOffset =
	    clang.getLocation( range:getRangeStart() )
	 local tailFile, tailLine, tailColmn, tailOffset =
	    clang.getLocation( range:getRangeEnd() )

	 if tailOffset - frontOffset ~= #sym or
	    fileContents:sub( frontOffset + 1, tailOffset ) ~= sym 
	 then
	    error( string.format( "sym %s, %s, %d:%d-%d",
				  fileContents:sub( frontOffset, tailOffset ),
				  sym, frontLine, frontColmn, tailColmn ) )
	 end

	 stream:write(
	    fileContents:sub( startOffset, frontOffset ) )
	 startOffset = tailOffset + 1
	 local refTxt = string.format( "(*p%s)", ref:getCursorSpelling() )
	 stream:write( refTxt )
      end
   end
   stream:write( fileContents:sub( startOffset, endOffset ) )
   stream:write( "\n" )
end

function Split:at( analyzer, path, line, column, target, fileContents )
   if not target then
      target = ""
   end

   path = analyzer:convFullpath( path )
   
   local analyzerForTokenize = analyzer:newAs(
      log( -4 ) >= 2 and true or false, false )
   local unit, compileOp, newAnalyzer =
      analyzerForTokenize:createUnit( path, target, false, fileContents )

   if not fileContents then
      fileContents = io.open( path ):read( '*a' )
   end

   local cxfile = unit:getFile( path )
   local currentLoc = unit:getLocation( cxfile, line, column + 1 )

   local cursor = unit:getCursor( currentLoc )

   if cursor:getCursorKind() ~= clang.core.CXCursor_CompoundStmt then
      return
   end

   info = {}
   info.symbol2DeclMap = {}
   info.refList = {}
   info.unaryList = {}
   info.binOpList = {}
   info.arrayList = {}
   clang.visitChildrenFast( cursor, visit, info, {}, 2 )

   local range = cursor:getCursorExtent()
   local startLoc = range:getRangeStart()
   local endLoc = range:getRangeEnd()

   local startFile, startLine, startColmn, startOffset = clang.getLocation( startLoc )
   local endFile, endLine, endColmn, endOffset = clang.getLocation( endLoc )


   -- 変数のアドレスアクセスしているかどうかのチェック
   for key, unary in pairs( info.unaryList ) do
      local opType = unary:getCursorType()
      clang.visitChildrenFast(
	 unary,
	 function( cursor, parent, aInfo, addInfo )
	    Util:dumpCursorInfo( cursor, 1, "unary", 0 )
	    
	    if cursor:getCursorKind() == clang.core.CXCursor_DeclRefExpr then
	       local declCursor = cursor:getCursorDefinition()
	       local declCursorKind = declCursor:getCursorKind()
	       if declCursorKind == clang.core.CXCursor_ParmDecl or
		  declCursorKind == clang.core.CXCursor_VarDecl
	       then
		  pointerFlag = opType:equalTypes( cursor:getCursorType() ) == 0

		  local declCursorInfo =
		     info.symbol2DeclMap[ declCursor:getCursorSpelling() ]
		  declCursorInfo.pointerAccess = pointerFlag
	       end
	    end
	    return 1
	 end,
	 nil, {}, 2 )
   end

   for key, array in pairs( info.arrayList ) do
      clang.visitChildrenFast(
	 array,
	 function( cursor, parent, aInfo, addInfo )
	    Util:dumpCursorInfo( cursor, 1, "array", 0 )
	    
	    if cursor:getCursorKind() == clang.core.CXCursor_DeclRefExpr then
	       local name = cursor:getCursorSpelling()
	       local declCursorInfo = info.symbol2DeclMap[ name ]
	       if declCursorInfo then
		  declCursorInfo.arrayAccess = true
	       end
	    end
	    return 1
	 end,
	 nil, {}, 2 )
   end

--[[
   int hoge;
   {
      hoge = 1;
   }

   →
   func( int * pHoge ) {
      *pHoge = 1;
   }

   func( &hoge );
]]

   -- BinaryOperator の左辺の変数チェック。
   -- BinaryOperator が "=" (代入) の場合は、左辺の変数を呼び出し元に反映する必要がある
   local symbol2LeftMap = {}
   for key, binOp in pairs( info.binOpList ) do
      local leftCursor
      local declCursor
      clang.visitChildrenFast(
	 binOp,
	 function( cursor, parent, aInfo )
	    leftCursor = cursor
	    return 0
	 end, nil, {}, 1 )

      log( 2, "binOp", clang.getCursorKindSpelling( leftCursor:getCursorKind() ) )
      if leftCursor:getCursorKind() == clang.core.CXCursor_DeclRefExpr then
	 declCursor = leftCursor:getCursorDefinition()
      else
	 clang.visitChildrenFast(
	    leftCursor,
	    function( cursor, parent, aInfo )
	       if cursor:getCursorKind() == clang.core.CXCursor_DeclRefExpr then
		  declCursor = cursor:getCursorDefinition()
	       end
	       return 1
	    end,
	    nil, {}, 2 )
      end

      if declCursor then
	 local declCursorKind = declCursor:getCursorKind()
	 if declCursorKind == clang.core.CXCursor_ParmDecl or
	    declCursorKind == clang.core.CXCursor_VarDecl
	 then
	    local declCursorInfo =
	       info.symbol2DeclMap[ declCursor:getCursorSpelling() ]
	    declCursorInfo.binOpAccess = true
	 end
      end
   end

   
   
   -- ブロック文内の変数は除外
   local newSymbol2DeclMap = {}
   local symbolList = {}
   for key, declCursorInfo in pairs( info.symbol2DeclMap ) do
      local declCursor = declCursorInfo.cursor
      
      local cxtype = declCursor:getCursorType()

      local file, line, colmn, offset =
	 clang.getLocation( declCursor:getCursorLocation() )

      local declParent = declCursor:getCursorSemanticParent()
      local declParentKind = declParent:getCursorKind()
      
      log( 2, "storage", declCursor:getCursorSpelling(),
	   declCursor:getStorageClass() )

      if ( offset < startOffset or offset > endOffset ) and
	 ( declParentKind == clang.core.CXCursor_CXXMethod or
	      declParentKind == clang.core.CXCursor_FunctionDecl )
      then
	 -- 変数の宣言場所が次の変数は、サブルーチン化の際に引数として渡す必要がある
	 -- - ブロックの外でかつ、
	 -- - メソッド内部か関数内部
	 newSymbol2DeclMap[ key ] = declCursorInfo
	 table.insert( symbolList, key )
      end
   end
   symbol2DeclMap = newSymbol2DeclMap

   local callArgs = ""
   local declArgs = ""

   
   table.sort( symbolList )
   for index, symbol in ipairs( symbolList ) do
      local declCursorInfo = symbol2DeclMap[ symbol ] 
      local declCursor = declCursorInfo.cursor
      local cxtype = declCursor:getCursorType()

      local declArgTxt, callArgTxt = getDeclTxt( declCursorInfo )
      log( 2, declArgTxt, isNeedPassAddress( declCursorInfo ),
	   declCursorInfo.pointerAccess,
	   declCursorInfo.binOpAccess, declCursorInfo.arrayAccess )

      declArgs = declArgs .. ", " .. declArgTxt
      callArgs = callArgs .. ", " .. callArgTxt
   end

   print( string.format( "func( %s );", string.gsub( callArgs, "^, ", "" ) ) )
   print( string.format( "func( %s )", string.gsub( declArgs, "^, ", "" ) ) )


  
   outputCode( io.stdout, fileContents,
	       startOffset, endOffset, info.refList, symbol2DeclMap )
end

return Split
