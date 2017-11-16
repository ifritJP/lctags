local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )
local Split = {}

local function isNeedPassAddress( info )
   if info.directPassFlag then
      return false
   end
   return not info.arrayAccess and
      (info.addressAccess or info.setAccess or info.structFlag)
end

local function visit( cursor, parent, info, addInfo )
   local cursorKind = cursor:getCursorKind()
   local appendInfo = clang.getVisitAppendInfo( addInfo )

   Util:dumpCursorInfo( cursor, 1, "", 0 )
   local cxtype = cursor:getCursorType()

   if cursorKind == clang.core.CXCursor_DeclRefExpr then
      info.refSymbolSet[ cursor:getCursorSpelling() ] = true
      
      local declCursor = cursor:getCursorDefinition()
      local declCursorKind = declCursor:getCursorKind()
      if declCursorKind == clang.core.CXCursor_ParmDecl or
	 declCursorKind == clang.core.CXCursor_VarDecl
      then
	 local declFile, declLine, declColun, declOffset =
	    clang.getLocation( declCursor:getCursorLocation() )
	 if info.compoundRangeOffset.startPos > declOffset or
	    info.compoundRangeOffset.endPos < declOffset
	 then
	    -- サブルーチン化する領域外の変数を参照している場合、
	    -- その参照情報を登録する。
	    local symbol = declCursor:getCursorSpelling()
	    if symbol == "" then
	       error( "illegal symbol" )
	    end
	    local declCursorInfo = {
	       cursor = declCursor,
	       structFlag = false,
	       addressAccess = false,
	       setAccess = false,
	       arrayAccess = false,
	       directPassFlag = false,
	    }
	    log( "refSymbol:", symbol )
	    info.symbol2DeclMap[ symbol ] = declCursorInfo
	    info.hash2DeclMap[ declCursor:hashCursor() ] = declCursorInfo
	    local baseCursor = Util:getRootTypeCursor(
	       declCursor:getCursorType():getTypeDeclaration() )
	    if baseCursor:getCursorKind() == clang.core.CXCursor_StructDecl or
	       baseCursor:getCursorKind() == clang.core.CXCursor_UnionDecl or
	       baseCursor:getCursorKind() == clang.core.CXCursor_ClassDecl
	    then
	       declCursorInfo.structFlag = true
	    end
	    
	    table.insert( info.repList,
			  { cursor = cursor, offset = appendInfo.offset } )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_ParmDecl or
      cursorKind == clang.core.CXCursor_VarDecl
   then
      info.refSymbolSet[ cursor:getCursorSpelling() ] = true
   elseif cursorKind == clang.core.CXCursor_UnaryOperator then
      table.insert( info.unaryList, cursor )
      if clang.getUnaryOperatorTxt( cursor ) == "&" then
	 table.insert( info.repList,
		       { cursor = cursor, offset = appendInfo.offset } )
      end
   elseif cursorKind == clang.core.CXCursor_BinaryOperator then
      table.insert( info.binOpList, cursor )
   elseif cursorKind == clang.core.CXCursor_ArraySubscriptExpr then
      table.insert( info.arrayList, cursor )
   elseif cursorKind == clang.core.CXCursor_ReturnStmt then
      local valCursor = clang.getNthCursor( cursor, 1 )

      if valCursor then
	 local valOffset = clang.getOffset( valCursor:getCursorLocation() )
	 
	 if appendInfo.offset == valOffset then
	    error( "return is in macro.", appendInfo.line, appendInfo.column )
	 end
      end
      
      table.insert( info.returnList, cursor )
      table.insert( info.repList,
		    { cursor = cursor, offset = appendInfo.offset } )
   elseif cursorKind == clang.core.CXCursor_GotoStmt then
      error( "lctags is not support goto statement." )
   elseif cursorKind == clang.core.CXCursor_ForStmt or
      cursorKind == clang.core.CXCursor_WhileStmt or
      cursorKind == clang.core.CXCursor_DoStmt
   then
      table.insert( info.loopList, { cursor = cursor,
				     startOffset = appendInfo.offset,
				     endOffset = appendInfo.endOffset } )
   elseif cursorKind == clang.core.CXCursor_BreakStmt or
      cursorKind == clang.core.CXCursor_ContinueStmt
   then
      -- break, continue がサブルーチン化する処理内のループ内のものか、
      -- それともサブルーチン化する処理外のループのものか調べる。
      -- サブルーチン化する処理外であれば、
      -- そのままではサブルーチン化できないのでリストに追加する。
      local outerFlag = true
      for index, loopInfo in ipairs( info.loopList ) do
	 if loopInfo.startOffset <= appendInfo.offset or
	    loopInfo.endOffset >= appendInfo.offset
	 then
	    -- サブルーチン化する処理内
	    outerFlag = false
	    break
	 end
      end
      if outerFlag then
	 table.insert( info.repList,
		       { cursor = cursor, offset = appendInfo.offset } )
	 if cursorKind == clang.core.CXCursor_BreakStmt then
	    table.insert( info.breakList, cursor )
	 else
	    table.insert( info.continueList, cursor )
	 end
      end
   elseif cursorKind == clang.core.CXCursor_MemberRefExpr then
      table.insert( info.memberRefList,
		    { cursor = cursor, appendInfo = appendInfo } )
   end

   if cursorKind >= clang.core.CXCursor_FirstStmt and
      cursorKind <= clang.core.CXCursor_LastStmt then
	 info.lastStmtInfo = { cursor = cursor, parent = parent }
   end

   return 1
end

local function makeAddressSymbol( symbol )
   return "p" .. symbol:sub( 1, 1 ):upper() .. symbol:sub( 2 )
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
      name = declCursorInfo.addressSymbol
      prefix = prefix .. "* "
   else
      prefix = prefix .. " "
   end
   local txt = prefix .. name .. suffix
   txt = string.gsub( txt, "  +", " " )
   return txt, callArg
end

local function outputCode(
      stream, unit, fileContents, startOffset, endOffset, endPos,
      info, boolTypeInfo, hash2MemberAccessRef, resultTypeTxt,
      onlyReturnFlag )

   table.sort( info.repList,
	       function( repInfo1, repInfo2 )
		  return repInfo1.offset < repInfo2.offset
	       end )


   local retEndOffset = nil

   local function writeSubstr( frontOffset )
      if retEndOffset and retEndOffset <= frontOffset then
	 stream:write( fileContents:sub( startOffset, retEndOffset ) )
	 stream:write( ", " .. boolTypeInfo.ret )
	 startOffset = retEndOffset + 1
	 retEndOffset = nil
      end

      stream:write( fileContents:sub( startOffset, frontOffset ) )
   end

   local function replaceSymbol( target, addressAccessFlag )
      local ref = target
      local sym = ref:getCursorSpelling()
      local declCursorInfo =
	 info.hash2DeclMap[ target:getCursorDefinition():hashCursor() ]
      if declCursorInfo and isNeedPassAddress( declCursorInfo ) then
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

	 local addressRefName = declCursorInfo.addressSymbol
	 
	 local refTxt
	 if addressAccessFlag then
	    refTxt = addressRefName
	 else
	    if hash2MemberAccessRef[ ref:hashCursor() ] then
	       -- . を見つける
	       local dotOffset
	       clang.mapRangeToken(
		  unit, range:getRangeEnd():getRange( endPos ),
		  function( token )
		     local tokenTxt = unit:getTokenSpelling( token )
		     log( 3, "token", tokenTxt,
			  Util:getTokenKindSpelling( token:getTokenKind() ) )
		     local tokenKind = token:getTokenKind()
		     if tokenKind == clang.core.CXToken_Comment then
			return true
		     end
		     if tokenKind == clang.core.CXToken_Punctuation and
			tokenTxt == "."
		     then
			local tokenOffset =
			   clang.getRangeOffset( unit:getTokenExtent( token ) )
			dotOffset = tokenOffset.endPos
		     end
		     return false
		  end
	       )

	       if dotOffset then
		  stream:write( addressRefName )
		  writeSubstr( dotOffset - 1 )
		  
		  refTxt = "->"
		  startOffset = dotOffset + 1
	       end
	    end
	    if not refTxt then
	       refTxt = string.format( "(*%s)", addressRefName )
	    end
	 end
	 stream:write( refTxt )
      end
   end


   startOffset = startOffset + 1
   local skipFlag = false
   for index, repInfo in ipairs( info.repList ) do
      if skipFlag then
	 skipFlag = false
      else
	 local target = repInfo.cursor
	 local kind = target:getCursorKind()
	 if not onlyReturnFlag and kind == clang.core.CXCursor_ReturnStmt then
	    local valCursor = clang.getNthCursor( target, 1, nil )

	    if valCursor then
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
	       local range = target:getCursorExtent()
	       local tailFile, tailLine, tailColmn, tailOffset =
		  clang.getLocation( range:getRangeEnd() )
	       writeSubstr( tailOffset )
	       startOffset = tailOffset + 1

	       stream:write( " " .. boolTypeInfo.ret )
	    end
	 elseif kind == clang.core.CXCursor_BreakStmt or
	    kind == clang.core.CXCursor_ContinueStmt
	 then
	    local range = target:getCursorExtent()
	    local frontFile, frontLine, frontColmn, frontOffset =
	       clang.getLocation( range:getRangeStart() )
	    local tailFile, tailLine, tailColmn, tailOffset =
	       clang.getLocation( range:getRangeEnd() )
	    writeSubstr( frontOffset )
	    startOffset = tailOffset + 1
	    stream:write( "return " )
	    if kind == clang.core.CXCursor_BreakStmt then
	       stream:write( boolTypeInfo.brk )
	    else
	       stream:write( boolTypeInfo.cnt )
	    end
	 elseif kind == clang.core.CXCursor_UnaryOperator then
	    -- ここの UnaryOperator は、& に限定される。
	    -- &val を &(*pval) ではなく、 pval に変換する。
	    local valCursor = clang.getNthCursor(
	       target, 1, { clang.core.CXCursor_DeclRefExpr } )
	    if valCursor then
	       local declCursor = valCursor:getCursorDefinition()
	       local declCursorInfo = info.hash2DeclMap[ declCursor:hashCursor() ]
	       if declCursorInfo and isNeedPassAddress( declCursorInfo ) then
		  skipFlag = true
		  local offset = clang.getRangeOffset( target:getCursorExtent() )
		  writeSubstr( offset.startPos )
		  startOffset = offset.endPos + 1
		  replaceSymbol( valCursor, true )
	       end
	    end
	 else
	    -- val を (*pval) に変換する
	    replaceSymbol( target, false )
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
	 stream:write( string.format( "    return %s;\n", boolTypeInfo.non ) )
      else
      end
   end
   stream:write( "}\n" )
end

function Split:at( analyzer, path, line, column, directPassMap,
		   subRetTypeInfo, directRet, target, fileContents )
   if not subRetTypeInfo then
      subRetTypeInfo = {}
   end
   if not subRetTypeInfo.type then
      subRetTypeInfo.type = "int"
   end
   if not subRetTypeInfo.non then
      subRetTypeInfo.non = "0"
   end
   if not subRetTypeInfo.ret then
      subRetTypeInfo.ret = "1"
   end
   if not subRetTypeInfo.brk then
      subRetTypeInfo.brk = "2"
   end
   if not subRetTypeInfo.cnt then
      subRetTypeInfo.cnt = "3"
   end
   
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


   local resultTypeTxt
   local parentCursor = cursor:getCursorSemanticParent()
   local parentKind = parentCursor:getCursorKind()
   if parentKind == clang.core.CXCursor_FunctionDecl or
      parentKind == clang.core.CXCursor_CXXMethod or
      parentKind == clang.core.CXCursor_Constructor or
      parentKind == clang.core.CXCursor_Destructor
   then
      resultTypeTxt = parentCursor:getCursorResultType():getTypeSpelling()
      log( 2, "resultType", resultTypeTxt )
      if resultTypeTxt == "void" then
	 resultTypeTxt = nil
      end
   end
   local functionName = parentCursor:getCursorSpelling()
   local subroutineName = functionName .. "__sub"

   info = {}
   info.fileContents = fileContents
   info.compoundCursor = cursor
   info.compoundRangeOffset = clang.getRangeOffset( cursor:getCursorExtent() )
   info.symbol2DeclMap = {}
   info.hash2DeclMap = {}
   info.unaryList = {}
   info.binOpList = {}
   info.arrayList = {}
   info.returnList = {}
   info.repList = {}
   info.loopList = {}
   info.breakList = {}
   info.continueList = {}
   info.memberRefList = {}
   info.refSymbolSet = {}
   clang.visitChildrenFast( cursor, visit, info, {}, 2 )

   local range = cursor:getCursorExtent()
   local startLoc = range:getRangeStart()
   local endLoc = range:getRangeEnd()

   local startFile, startLine, startColmn, startOffset = clang.getLocation( startLoc )
   local endFile, endLine, endColmn, endOffset = clang.getLocation( endLoc )


   local hash2MemberAccessRef = {}
   -- 参照がメンバーアクセスしているか
   for key, memberRefInfo in ipairs( info.memberRefList ) do
      local objCursor = clang.getNthCursor(
	 memberRefInfo.cursor, 1, { clang.core.CXCursor_DeclRefExpr } )
      if objCursor then
	 local declCursor = objCursor:getCursorDefinition()
	 if info.hash2DeclMap[ declCursor:hashCursor() ] then
	    local hash = objCursor:hashCursor()
	    hash2MemberAccessRef[ hash ] = true
	    log( 2, "memberAccessRef", hash )
	 end
      end
   end

   
   -- 変数のアドレスアクセスしているかどうかのチェック
   for key, unary in pairs( info.unaryList ) do
      local opType = unary:getCursorType()
      local opPointeeType = opType:getPointeeType()
      local checked = false
      
      clang.visitChildrenFast(
	 unary,
	 function( cursor, parent, aInfo, addInfo )
	    Util:dumpCursorInfo( cursor, 1, "unary", 0 )

	    local cursorKind = cursor:getCursorKind()
	    if clang.isExprKind( cursorKind ) then
	       checked = true
	    end
	    
	    if cursorKind == clang.core.CXCursor_DeclRefExpr then
	       local declCursor = cursor:getCursorDefinition()
	       local declCursorInfo =
		  info.hash2DeclMap[ declCursor:hashCursor() ]

	       if declCursorInfo then
		  local cursorType = cursor:getCursorType()
		  if not opType:equalTypes( cursorType ) and 
		     cursorType:equalTypes( opPointeeType )
		  then
		     declCursorInfo.addressAccess = true
		  end

		  local unaryOpTxt = clang.getUnaryOperatorTxt( unary )
		  log( 2, "unaryOpTxt", unaryOpTxt, declCursor:getCursorSpelling() )
		  if not unaryOpTxt or string.find( unaryOpTxt, "[%+%-]", 2 )
		  then
		     local appendInfo = clang.getVisitAppendInfo( addInfo )
		     
		     log( 2, "unaryOpTxt: setAccess", appendInfo.line,
			  appendInfo.column )
		     declCursorInfo.setAccess = true
		  end
	       end
	    end
	    if checked then
	       return 0
	    end
	    return 1
	 end,
	 nil, {}, 2 )
   end

   if directPassMap then
      for sym in pairs( directPassMap ) do
	 info.symbol2DeclMap[ sym ].directPassFlag = true      
      end
   end

   for key, array in pairs( info.arrayList ) do
      clang.visitChildrenFast(
	 array,
	 function( cursor, parent, aInfo, addInfo )
	    Util:dumpCursorInfo( cursor, 1, "array", 0 )
	    
	    if cursor:getCursorKind() == clang.core.CXCursor_DeclRefExpr then
	       local declCursor = cursor:getCursorDefinition()
	       local declCursorInfo = info.hash2DeclMap[ declCursor:hashCursor() ]
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
	 binOp, 1, { clang.core.CXCursor_DeclRefExpr,
		     clang.core.CXCursor_MemberRefExpr } )

      if refCursor then
	 if refCursor:getCursorKind() == clang.core.CXCursor_MemberRefExpr then
	    log( 2, "member", refCursor:getCursorSpelling(), refCursor )
	    refCursor = clang.getNthCursor( refCursor, 1, nil )
	    log( 2, "member2", refCursor:getCursorSpelling() )
	 end
	 
	 local declCursor = refCursor:getCursorDefinition()
	 local declCursorKind = declCursor:getCursorKind()
	 log( 2, "declCursor", declCursor and declCursor:getCursorSpelling(),
	      clang.getCursorKindSpelling( declCursorKind ) )
	 
	 if declCursorKind == clang.core.CXCursor_ParmDecl or
	    declCursorKind == clang.core.CXCursor_VarDecl
	 then
	    local binOpTxt = clang.getBinOperatorTxt( binOp )
	    log( 2, "opTxt", binOpTxt )
	    if not binOpTxt or string.find( binOpTxt, "=", 1, true )
	    then
	       local declCursorInfo =
		  info.hash2DeclMap[ declCursor:hashCursor() ]
	       if declCursorInfo then
		  declCursorInfo.setAccess = true
	       end
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
   info.symbol2DeclMap = newSymbol2DeclMap

   -- アドレス渡し用変数名の決定
   for key, declCursorInfo in pairs( info.symbol2DeclMap ) do
      local declCursor = declCursorInfo.cursor
      if isNeedPassAddress( declCursorInfo ) then
	 local symbol = makeAddressSymbol( declCursor:getCursorSpelling() )
	 while info.refSymbolSet[ symbol ] do
	    symbol = symbol .. "_";
	 end
	 declCursorInfo.addressSymbol = symbol
      end
   end
   
   

   local callArgs = ""
   local declArgs = ""


   local stream = {
      write = function( self, txt )
	 io.stdout:write( Util:convertXmlTxt( txt ) )
      end
   }

   print( "<lctags_result><refactoring_split>" )

   local onlyReturnFlag = false
   if (#info.breakList == 0) and (#info.continueList == 0) and
      (#info.returnList > 0) and resultTypeTxt
   then
      onlyReturnFlag = directRet
      print( string.format( "<directRet>%s</directRet>",
			    directRet and "true" or "false" ) )
   end

   print( "<args>" )
   
   if resultTypeTxt and not onlyReturnFlag then
      declArgs = declArgs .. ", " .. resultTypeTxt .. "* pFuncRet__"
      callArgs = callArgs .. ", &funcRet__"
   end
   table.sort( symbolList )
   for index, symbol in ipairs( symbolList ) do
      local declCursorInfo = info.symbol2DeclMap[ symbol ] 
      local declCursor = declCursorInfo.cursor
      local cxtype = declCursor:getCursorType()

      local declArgTxt, callArgTxt = getDeclTxt( declCursorInfo )
      log( 2, "arg:", declArgTxt, isNeedPassAddress( declCursorInfo ),
	   declCursorInfo.addressAccess,
	   declCursorInfo.setAccess, declCursorInfo.arrayAccess )

      declArgs = declArgs .. ", " .. declArgTxt
      callArgs = callArgs .. ", " .. callArgTxt

      print( string.format(
		"<arg><addressAccess>%s</addressAccess><name>%s</name></arg>",
		isNeedPassAddress( declCursorInfo ), declCursor:getCursorSpelling() ) )
   end

   print( "</args>" )
   print( "<call>" )

   local retValDecl = ""
   if resultTypeTxt and not onlyReturnFlag then
      retValDecl = string.format( "%s funcRet__ = 0;\n", resultTypeTxt )
   end
   local callTxt = string.format(
      "%s( %s )", subroutineName, string.gsub( callArgs, "^, ", "" ) )
   local subModType = subRetTypeInfo.type
   if #info.returnList == 0 and #info.breakList == 0 and #info.continueList == 0 then
      subModType = "void"
      stream:write( string.format( "%s( %s );", subroutineName,
				   string.gsub( callArgs, "^, ", "" ) ) )
   elseif #info.returnList > 0 and #info.breakList == 0 and #info.continueList == 0 then
      if not onlyReturnFlag then
	 stream:write(
	    string.format( [[
{
    %sif ( %s ) {
       return funcRet__;
    }
}]],
	       retValDecl, callTxt ) )
      else
	 subModType = resultTypeTxt
	 stream:write( callTxt .. ";" )
      end
   elseif #info.returnList == 0 and #info.breakList > 0 and #info.continueList == 0 then
      stream:write(
	 string.format( [[
if ( %s ) {
  break;
}
]], callTxt ) )
   elseif #info.returnList == 0 and #info.breakList == 0 and #info.continueList > 0 then
      stream:write(
	 string.format( [[
if ( %s ) {
  continue;
}
]], callTxt ) )
   else
      stream:write( "{\n" )
      local stmtList = {}
      if #info.returnList > 0 then
	 stream:write( retValDecl )
	 table.insert( stmtList,
		       { val = subRetTypeInfo.ret, stmt = "return funcRet__;" } )
      end
      if #info.breakList > 0 then
	 table.insert( stmtList, { val = subRetTypeInfo.brk, stmt = "break;" } )
      end
      if #info.continueList > 0 then
	 table.insert( stmtList, { val = subRetTypeInfo.cnt, stmt = "continue;" } )
      end

      stream:write( string.format( "%s result__ = %s;\n",
				   subRetTypeInfo.type or "int", callTxt ) )

      for index, stmt in ipairs( stmtList ) do
	 local elseTxt = ""
	 if index ~= 1 then
	    elseTxt = " else"
	 end
	 stream:write( string.format( [[
%s if ( result__ == %s ) { %s }
]], elseTxt, stmt.val, stmt.stmt ) )
      end
      stream:write( "}\n" )
   end
   print( "</call>" )
   print( "<sub_routine>" )
   stream:write( string.format(
		    "static %s %s( %s )\n", subModType,
		    subroutineName, string.gsub( declArgs, "^, ", "" ) ) )

   
   outputCode( stream, unit, fileContents, startOffset, endOffset, endLoc,
	       info, subRetTypeInfo, hash2MemberAccessRef, resultTypeTxt,
	       onlyReturnFlag )

   print( "</sub_routine>" )
   print( "</refactoring_split></lctags_result>" )
end

return Split
