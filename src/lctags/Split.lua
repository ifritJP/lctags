local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )
local Split = {}

local function isNeedPassAddress( info )
   if info.ignoreFlag then
      return false
   end
   return not info.arrayAccess and (info.addressAccess or info.binOpAccess)
end

local function visit( cursor, parent, info, addInfo )
   local symbol2DeclMap = info.symbol2DeclMap
   local cursorKind = cursor:getCursorKind()
   local appendInfo = clang.getVisitAppendInfo( addInfo )

   Util:dumpCursorInfo( cursor, 1, "", 0 )
   local cxtype = cursor:getCursorType()

   if cursorKind == clang.core.CXCursor_DeclRefExpr then
      local declCursor = cursor:getCursorDefinition()
      local declCursorKind = declCursor:getCursorKind()
      if declCursorKind == clang.core.CXCursor_ParmDecl or
	 declCursorKind == clang.core.CXCursor_VarDecl
      then
	 local symbol = declCursor:getCursorSpelling()
	 if symbol == "" then
	    error( "illegal symbol" )
	 end
	 symbol2DeclMap[ symbol ] = {
	    cursor = declCursor,
	    addressAccess = false,
	    binOpAccess = false,
	    arrayAccess = false,
	    ignoreFlag = false,
	 }
	 table.insert( info.refList, cursor )
	 table.insert( info.repList,
		       { cursor = cursor, offset = appendInfo.offset } )
	 local range = cursor:getCursorExtent()
      end
   elseif cursorKind == clang.core.CXCursor_UnaryOperator then
      table.insert( info.unaryList, cursor )
   elseif cursorKind == clang.core.CXCursor_BinaryOperator then
      table.insert( info.binOpList, cursor )
   elseif cursorKind == clang.core.CXCursor_ArraySubscriptExpr then
      table.insert( info.arrayList, cursor )
   elseif cursorKind == clang.core.CXCursor_ReturnStmt then
      local valCursor = clang.getNthCursor( cursor, 1 )

      local valOffset = clang.getOffset( valCursor:getCursorLocation() )
      
      if appendInfo.offset == valOffset then
	 error( "return is in macro.", appendInfo.line, appendInfo.column )
      end
      
      table.insert( info.returnList, cursor )
      table.insert( info.repList,
		    { cursor = cursor, offset = appendInfo.offset } )
   end

   if cursorKind >= clang.core.CXCursor_FirstStmt and
      cursorKind <= clang.core.CXCursor_LastStmt then
	 info.lastStmtInfo = { cursor = cursor, parent = parent }
   end

   return 1
end

local function getDeclTxt( declCursorInfo )
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

local function outputCode(
      stream, fileContents, startOffset, endOffset, info, symbol2DeclMap )

   table.sort( info.repList,
	       function( repInfo1, repInfo2 )
		  return repInfo1.offset < repInfo2.offset
	       end )


   local retEndOffset = nil

   local function writeSubstr( frontOffset )
      if retEndOffset and retEndOffset <= frontOffset then
	 stream:write( fileContents:sub( startOffset, retEndOffset ) )
	 stream:write( ", 1" )
	 startOffset = retEndOffset + 1
	 retEndOffset = nil
      end

      stream:write( fileContents:sub( startOffset, frontOffset ) )
   end
   
   startOffset = startOffset + 1
   for index, repInfo in ipairs( info.repList ) do
      local target = repInfo.cursor
      if target:getCursorKind() == clang.core.CXCursor_ReturnStmt then
	 local valCursor = clang.getNthCursor( target, 1, nil )
	 local range = valCursor:getCursorExtent()

	 local frontFile, frontLine, frontColmn, frontOffset =
	    clang.getLocation( range:getRangeStart() )
	 local tailFile, tailLine, tailColmn, tailOffset =
	    clang.getLocation( range:getRangeEnd() )

	 writeSubstr( frontOffset )
	 startOffset = frontOffset

	 retEndOffset = tailOffset

	 stream:write( "*pFuncRet__=" )
      else
	 local ref = target
	 local sym = ref:getCursorSpelling()
	 local declCursorInfo = symbol2DeclMap[ sym ]
	 if declCursorInfo and isNeedPassAddress( declCursorInfo )
	 then
	    -- 参照しているシンボルがアドレスアクセス必要なシンボルだった場合
	    -- * 演算子を加える。
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

	    writeSubstr( frontOffset )
	    
	    startOffset = tailOffset + 1
	    local refTxt = string.format( "(*p%s)", ref:getCursorSpelling() )
	    stream:write( refTxt )
	 end
      end
   end

   writeSubstr( endOffset - 1 )

   if #info.returnList > 0 then
      if not info.lastStmtInfo or
	 not info.compoundCursor:getCursorExtent():equalRanges(
	    info.lastStmtInfo.parent:getCursorExtent() ) or
	 info.lastStmtInfo.cursor:getCursorKind() ~= clang.core.CXCursor_ReturnStmt
      then
	 stream:write( "    return 0;\n" )
      else
      end
   end
   stream:write( "}\n" )
end

function Split:at( analyzer, path, line, column, ignoreSymMap, target, fileContents )
   if not target then
      target = ""
   end

   path = analyzer:convFullpath( path )
   
   local analyzerForTokenize = analyzer:newAs(
      log( -4 ) >= 2 and true or false, false )
   local unit, compileOp, newAnalyzer =
      analyzerForTokenize:createUnit( path, target, false, fileContents )

   local diagList, errorLevel = newAnalyzer:getDiagList( unit )
   if errorLevel >= clang.core.CXDiagnostic_Error then
      Util:outputResult( clang.core.CXDiagnostic_Error, nil, diagList )
      return
   end

   if not fileContents then
      fileContents = io.open( path ):read( '*a' )
   end

   local cxfile = unit:getFile( path )
   local currentLoc = unit:getLocation( cxfile, line, column + 1 )

   local cursor = unit:getCursor( currentLoc )

   if cursor:getCursorKind() ~= clang.core.CXCursor_CompoundStmt then
      return
   end


   local resultType
   local parentCursor = cursor:getCursorSemanticParent()
   local parentKind = parentCursor:getCursorKind()
   if parentKind == clang.core.CXCursor_FunctionDecl or
      parentKind == clang.core.CXCursor_CXXMethod or
      parentKind == clang.core.CXCursor_Constructor or
      parentKind == clang.core.CXCursor_Destructor
   then
      resultType = parentCursor:getCursorResultType()
      log( 2, "resultType", resultType:getTypeSpelling() )
   end

   info = {}
   info.fileContents = fileContents
   info.compoundCursor = cursor
   info.symbol2DeclMap = {}
   info.refList = {}
   info.unaryList = {}
   info.binOpList = {}
   info.arrayList = {}
   info.returnList = {}
   info.repList = {}
   clang.visitChildrenFast( cursor, visit, info, {}, 2 )

   local range = cursor:getCursorExtent()
   local startLoc = range:getRangeStart()
   local endLoc = range:getRangeEnd()

   local startFile, startLine, startColmn, startOffset = clang.getLocation( startLoc )
   local endFile, endLine, endColmn, endOffset = clang.getLocation( endLoc )


   -- 変数のアドレスアクセスしているかどうかのチェック
   for key, unary in pairs( info.unaryList ) do
      local opType = unary:getCursorType()
      local opPointeeType = opType:getPointeeType()
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
		  local addressAccess = false
		  local cursorType = cursor:getCursorType()
		  if not opType:equalTypes( cursorType ) and 
		     cursorType:equalTypes( opPointeeType )
		  then
		     addressAccess = true
		  end
		  local declCursorInfo =
		     info.symbol2DeclMap[ declCursor:getCursorSpelling() ]
		  declCursorInfo.addressAccess = addressAccess
	       end
	    end
	    return 1
	 end,
	 nil, {}, 2 )
   end

   if ignoreSymMap then
      for sym in pairs( ignoreSymMap ) do
	 info.symbol2DeclMap[ sym ].ignoreFlag = true      
      end
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

   -- BinaryOperator の左辺の変数チェック。
   -- BinaryOperator が "=" (代入) の場合は、左辺の変数を呼び出し元に反映する必要がある
   local symbol2LeftMap = {}
   for key, binOp in pairs( info.binOpList ) do
      local refCursor = clang.getNthCursor(
	 binOp, 1, { clang.core.CXCursor_DeclRefExpr } )

      if refCursor then
	 local declCursor = refCursor:getCursorDefinition()
	 local declCursorKind = declCursor:getCursorKind()
	 log( 2, "declCursor", declCursor and declCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( declCursorKind ) )
	 
	 if declCursorKind == clang.core.CXCursor_ParmDecl or
	    declCursorKind == clang.core.CXCursor_VarDecl
	 then
	    local binOpTxt = clang.getBinOperatorTxt( binOp )
	    log( 2, "opTxt", binOpTxt )
	    if not binOpTxt or string.find( binOpTxt, "=", 1, true ) then
	       local declCursorInfo =
		  info.symbol2DeclMap[ declCursor:getCursorSpelling() ]
	       declCursorInfo.binOpAccess = true
	    end
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


   local stream = {
      write = function( self, txt )
	 io.stdout:write( Util:convertXmlTxt( txt ) )
      end
   }

   print( "<lctags_result><refactoring_split>" )
   print( "<args>" )

   if #info.returnList > 0 then
      declArgs = declArgs .. ", " .. resultType:getTypeSpelling() .. "* pFuncRet__"
      callArgs = callArgs .. ", &funcRet__"
   end
   table.sort( symbolList )
   for index, symbol in ipairs( symbolList ) do
      local declCursorInfo = symbol2DeclMap[ symbol ] 
      local declCursor = declCursorInfo.cursor
      local cxtype = declCursor:getCursorType()

      local declArgTxt, callArgTxt = getDeclTxt( declCursorInfo )
      log( 2, declArgTxt, isNeedPassAddress( declCursorInfo ),
	   declCursorInfo.addressAccess,
	   declCursorInfo.binOpAccess, declCursorInfo.arrayAccess )

      declArgs = declArgs .. ", " .. declArgTxt
      callArgs = callArgs .. ", " .. callArgTxt

      print( string.format(
		"<arg><addressAccess>%s</addressAccess><name>%s</name></arg>",
		isNeedPassAddress( declCursorInfo ), declCursor:getCursorSpelling() ) )
   end

   print( "</args>" )
   print( "<call>" )
   if #info.returnList > 0 then
      stream:write( string.format( [[
{
    %s funcRet__ = 0;
    if ( func( %s ) ) {
       return funcRet__;
    }
}]], resultType:getTypeSpelling(), string.gsub( callArgs, "^, ", "" ) ) )
   else
      stream:write( string.format( "func( %s );", string.gsub( callArgs, "^, ", "" ) ) )
   end
   print( "</call>" )
   print( "<sub_routine>" )
   stream:write( string.format(
		    "static %s func( %s )\n",
		    #info.returnList > 0 and "int" or "void",
		    string.gsub( declArgs, "^, ", "" ) ) )

  
   outputCode( stream, fileContents,
	       startOffset, endOffset, info, symbol2DeclMap )

   print( "</sub_routine>" )
   print( "</refactoring_split></lctags_result>" )
end

return Split
