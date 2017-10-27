-- Copyright (C) 2017 ifritJP

local libs = {}
local libclangcore = require( 'libclanglua.core' )
libs.core = libclangcore

if _VERSION == "Lua 5.1" then
   table.pack = function( ... )
      return { ... }
   end
   table.unpack = function( ... )
      return unpack( ... )
   end
end

-- convert from CXString to String of Lua.
libs.cx2string = function( cxstr )
   local str = libclangcore.clang_getCString( cxstr )
   libclangcore.clang_disposeString( cxstr )
   return str
end


-- visit cursor children with low level accessing.
libs.visitChildrenLow = function( cursor, func, exInfo, cxfileList )
   if not __libclang_visit then
      __libclang_visit = {}
   end

   local list = {}
   if cxfileList then
      for index, cxfile in ipairs( cxfileList ) do
	 table.insert( list, cxfile.__ptr )
      end
   end
   local fileArray = cxfileList and libs.mkCXFileArray( list )
   
   table.insert( __libclang_visit, { func, exInfo, fileArray and fileArray.__ptr } )
   local result = libclangcore.clang_visitChildren( cursor, nil )

   table.remove( __libclang_visit )
   
   return result
end

-- visit cursor children.
libs.clang_visitChildren = function( cxCursor, func, exInfo )
   local wrapFunc = function( aCursor, aParent, aExInfo )
      return func( libs.CXCursor:new( aCursor ),
		   libs.CXCursor:new( aParent ), aExInfo )
   end
   return libs.visitChildrenLow( cxCursor, wrapFunc, exInfo )
end

-- obtain infomation from append of visit callback.
libs.getVisitAppendInfo = function( append )
   return {
      fileChangeFlag = append[ 1 ],
      offset = append[ 2 ],
      line = append[ 3 ],
      column = append[ 4 ],
      endLine = append[ 5 ],
      endColumn = append[ 6 ],
      endOffset = append[ 7 ],
   }
end

-- fast visit cursor children.
libs.visitChildrenFast = function( cursor, func, exInfo, kindList, callbackResult )
   local result, list = libs.getChildrenList( cursor, kindList, callbackResult )
   
   for index, info in ipairs( list ) do
      if func( info[ 1 ], info[ 2 ], exInfo, info[ 3 ] ) == 0 then
	 break
      end
   end
   return result
end

-- fast visit cursor children.
libs.visitChildrenFast2 = function(
      cursor, func, exInfo, kindList, kindList2,
      cxfileList, callbackResult )
   local result, list = libs.getChildrenList(
      cursor, kindList, callbackResult, kindList2, cxfileList )
   
   for index, info in ipairs( list ) do
      if func( info[ 1 ], info[ 2 ], exInfo, info[ 3 ] ) == 0 then
	 break
      end
   end
   return result
end

-- object children list for cursor
libs.getChildrenList = function( cursor, kindList, callbackResult, kindList2, cxfileList )
   if not kindList then
      kindList = {}
   end
   if not kindList2 then
      kindList2 = {}
   end
   local kindArray = libclangcore.new_intArray( #kindList + #kindList2 + 4 )
   
   libclangcore.intArray_setitem( kindArray, 0, callbackResult )
   libclangcore.intArray_setitem( kindArray, 1, cxfileList and #cxfileList or 0 )
   for index, kind in ipairs( kindList ) do
      libclangcore.intArray_setitem( kindArray, index + 1, kind )
   end
   libclangcore.intArray_setitem(
      kindArray, #kindList + 2, libclangcore.CXCursor_InvalidFile )
   
   for index, kind in ipairs( kindList2 ) do
      libclangcore.intArray_setitem( kindArray, #kindList + 2 + index, kind )
   end
   libclangcore.intArray_setitem(
      kindArray, #kindList + #kindList2 + 3, libclangcore.CXCursor_InvalidFile )

   local list = {}
   local result = libs.visitChildrenLow( cursor.__ptr, list, kindArray, cxfileList )
   libclangcore.delete_intArray( kindArray )



   -- local parentDepth = {}
   -- local hash2Parent = {}

   local cursorList = {}
   for index, info in ipairs( list ) do
      local cursor = libs.CXCursor:new( info[ 1 ] )
      local parent = libs.CXCursor:new( info[ 2 ] )

      -- if callbackResult == 2 then
      --         -- 深さを確認する
      -- 	 local parentHash = parent:hashCursor()
      -- 	 if not hash2Parent[ parentHash ] then
      -- 	    table.insert( parentDepth, parent )
      -- 	    hash2Parent[ parentHash ] = #parentDepth
      -- 	 else
      -- 	    local depth = hash2Parent[ parentHash ]
      -- 	    if depth ~= #parentDepth then
      -- 	       for index = depth + 1, #parentDepth do
      -- 		  local depthParent = parentDepth[ depth + 1 ]
      -- 		  hash2Parent[ depthParent:hashCursor() ] = nil
      -- 		  table.remove( parentDepth, depth + 1 )
      -- 	       end
      -- 	    end
      -- 	 end
      -- end
      table.insert(
	 cursorList,
	 { cursor, parent, table.pack( index == 1 or info[ 3 ],
				       table.unpack( info, 4 ) ) } )
   end
   return result, cursorList
end


-- obtain inclusions with low level accessing.
libs.getInclusionsLow = function( unit, func, exInfo )
   if not __libclang_visit then
      __libclang_visit = {}
   end
   table.insert( __libclang_visit, { func, exInfo } )
   libclangcore.clang_getInclusions( unit, nil )

   table.remove( __libclang_visit )
end

-- object inclusions.
libs.clang_getInclusions = function( unit, func, exInfo )
   local wrapFunc = function( included_file, inclusion_stack,
			      include_len, client_data )
      return func( libs.CXFile:new( included_file ),
		   libs.CXCXSourceLocation:new( inclusion_stack ),
		   inclusion_len, client_data )
   end
   return libs.getInclusionsLow( cursor, wrapFunc, exInfo )
end

-- visit field with low level accessing.
libs.visitFieldsLow = function( cxtype, func, exInfo )
   if not __libclang_visit then
      __libclang_visit = {}
   end
   table.insert( __libclang_visit, { func, exInfo } )
   libclangcore.clang_Type_visitFields( cxtype, nil )

   table.remove( __libclang_visit )
end

-- visit field.
libs.visitFields = function( cxtype, func, exInfo )
   local wrapFunc = function( cursor, client_data )
      return func( libs.CXCursor:new( cursor ), exInfo )
   end
   return libs.visitFieldsLow( cxtype.__ptr, wrapFunc, exInfo )
end

-- make char * Array.
libs.mkcharPArray = function( strArray )
   local array = {
      __length = #strArray,
      __ptr = libclangcore.new_charPArray( #strArray ),
      
      getLength = function( self )
	 return self.__length
      end,
      
      getPtr = function( self )
	 return self.__ptr
      end,
   }
   
   for key, str in ipairs( strArray ) do
      libclangcore.charPArray_setitem( array.__ptr, key - 1, str )
   end
   return array
end

-- 
libs.isBuiltInTypeKind = function( typeKind )
   return typeKind >= libclangcore.CXType_FirstBuiltin and
      typeKind <= libclangcore.CXType_LastBuiltin
end

libs.getNamespaceList = function( cursor, includeCurrent, cursorHash2NSFunc )
   local nsList = {}
   local target = cursor

   local firstFlag = true
   while true do
      local cursorKind = target:getCursorKind()
      if cursorKind == libclangcore.CXCursor_InvalidFile then
	 break
      end
      if cursorKind == libclangcore.CXCursor_ClassDecl or
	 cursorKind == libclangcore.CXCursor_ClassTemplate or
	 cursorKind == libclangcore.CXCursor_StructDecl or
	 cursorKind == libclangcore.CXCursor_UnionDecl or
	 cursorKind == libclangcore.CXCursor_Namespace or
	 firstFlag
      then
	 if not firstFlag or includeCurrent then
	    if cursorHash2NSFunc then
	       local fullname = cursorHash2NSFunc( target )
	       if fullname then
		  local index = 1
		  for name in string.gmatch( fullname, "[^:]+" ) do
		     table.insert( nsList, index, name )
		     index = index + 1
		  end
		  break
	       end
	    end
	    table.insert( nsList, 1, target:getCursorSpelling() )
	 end
	 firstFlag = false
	 
	 if target:getCursorKind() == libclangcore.CXCursor_StructDecl then
	    table.insert( nsList, 1, "@struct" )
	 elseif target:getCursorKind() == libclangcore.CXCursor_EnumDecl then
	    table.insert( nsList, 1, "@enum" )
	 elseif target:getCursorKind() == libclangcore.CXCursor_UnionDecl then
	    table.insert( nsList, 1, "@union" )
	 end
      end
      target = target:getCursorSemanticParent()
   end


   local namespace = ""
   for index, name in ipairs( nsList ) do
      namespace = namespace .. "::" .. name
   end
   
   return nsList, namespace
end

local cxFileArray
if libclangcore.new_CXFileArray then
   cxFileArray = libclangcore.new_CXFileArray( 1 )
end
--   libclangcore.delete_CXFileArray( cxFileArray )
libs.getFileLocation = function( obj, func, ... )
   local params = table.pack( ... )
   table.insert( params, cxFileArray )
   local result = table.pack( func( obj, table.unpack( params ) ) )
   local cxFile = libs.CXFile:new( libclangcore.CXFileArray_getitem( cxFileArray, 0 ) )
   return cxFile, table.unpack( result )
end

libs.getOffset = function( location )
   local file, line, column, offset = libs.getLocation( location )
   return offset
end

-- obtain file location from location.
libs.getLocation = function( location )
   return libs.getFileLocation( location.__ptr, libclangcore.clang_getFileLocation )
end

-- obtain cursor location.
libs.getCursorLocation = function( cursor )
   local location = cursor:getCursorLocation()
   return libs.getFileLocation(
      location.__ptr, libclangcore.clang_getFileLocation )
end

-- obtain cursor plain text.
libs.getCurosrPlainText = function( cursor )
   local txt = ""
   libs.mapCurosrPlainText(
      cursor,
      function( tokenTxt )
	 if txt ~= "" and tokenTxt ~= "}" then
	    txt = txt .. " " .. tokenTxt
	 else
	    txt = txt .. tokenTxt
	 end
	 if tokenTxt == ";" or tokenTxt == "{" or
	    string.find( tokenTxt, "*/$" ) or string.find( tokenTxt, "^//" )
	 then
	    txt = txt .. "\n"
	 end
      end
   )
   return txt
end

libs.mapCurosrPlainText = function( cursor, func, ... )
   local srcRange = libclangcore.clang_getCursorExtent( cursor )
   local unit = libclangcore.clang_Cursor_getTranslationUnit( cursor )
   libs.mapRangePlainText( unit, srcRange, func, ... )
end

libs.mapRangePlainText = function( cxUnit, cxRange, func, ... )
   local unit = cxUnit
   local tokenPBuf = libclangcore.new_CXTokenPArray( 1 )
   local tokenNum = libclangcore.clang_tokenize( unit, cxRange, tokenPBuf )
   local tokenArray = libclangcore.CXTokenPArray_getitem( tokenPBuf, 0 )

   for index = 0, tokenNum - 1 do
      local token = libclangcore.CXTokenArray_getitem( tokenArray, index )
      func( libs.cx2string( libclangcore.clang_getTokenSpelling( unit, token ) ), ... )
   end
   libclangcore.clang_disposeTokens( unit, tokenArray, tokenNum )
   libclangcore.delete_CXTokenPArray( tokenPBuf )
end

libs.mapRangeToken = function( unit, range, func )
   local tokenP = libs.mkCXTokenPArray( nil, 1 )
   local num = unit:tokenize( range, tokenP:getPtr() )
   local cxtokenArray = libs.mkCXTokenArray( tokenP:getItem( 0 ), num )

   for index = 0, num - 1 do
      if not func( libs.CXToken:new( cxtokenArray:getItem( index ) ) ) then
	 break
      end
   end
   unit:disposeTokens( cxtokenArray:getPtr(), cxtokenArray:getLength() )
end


libs.isPointerType = function( cxtype )
   local baseType = cxtype and cxtype:getPointeeType()
   return baseType.__ptr.kind ~= libclangcore.CXType_Invalid
end

libs.isArrayType = function( cxtype )
   local baseType = cxtype and cxtype:getElementType()
   return baseType.__ptr.kind ~= libclangcore.CXType_Invalid
end

libs.isExprKind = function( cursorKind )
   return cursorKind >= libclangcore.CXCursor_FirstExpr and
      cursorKind <= libclangcore.CXCursor_LastExpr
end


libs.getDeclCursorFromType = function( cxtype )
   while true do
      local baseType = cxtype and cxtype:getPointeeType()
      if baseType.__ptr.kind == libclangcore.CXType_Invalid then
	 if cxtype then
	    baseType = cxtype:getElementType()
	 end
	 if baseType.__ptr.kind == libclangcore.CXType_Invalid then
	    break
	 end
      end
      cxtype = baseType
   end
   if not cxtype then
      return nil
   end
   return cxtype:getTypeDeclaration()
end

libs.findChildCursor = function( cursor, targetKindList )
   local targetKindSet = {}
   for index, kind in ipairs( targetKindSet ) do
      targetKindSet[ kind ] = 1
   end

   local firstCursor
   local rangeInfo = {}

   local visit;
   visit = function( aCursor, parent, info, exInfo )
      if not targetKindList or targetKindSet[ aCursor:getCursorKind() ] then
	 firstCursor = aCursor
	 rangeInfo.startLine = exInfo[ 3 ]
	 rangeInfo.startColumn = exInfo[ 4 ]
	 rangeInfo.startOffset = exInfo[ 2 ]
	 rangeInfo.endLine = exInfo[ 5 ]
	 rangeInfo.endColumn = exInfo[ 6 ]
	 rangeInfo.endOffset = exInfo[ 7 ]
	 return 0
      end
      libs.visitChildrenFast( aCursor, visit, nil, {}, 1 )
      if firstCursor then
	 return 0
      end
      return 1
   end

   libs.visitChildrenFast( cursor, visit, nil, {}, 1 )
   return firstCursor, rangeInfo
end

libs.getNthCursor = function( cursor, nth, targetKindList )
   local targetKindSet = {}
   if targetKindList then
      for index, kind in ipairs( targetKindList ) do
	 targetKindSet[ kind ] = 1
      end
   end

   local nthCursor
   local rangeInfo = {}
   local index = 1

   local nthVisit = function( aCursor, parent, info, exInfo )
      if index == nth then
	 nthCursor = aCursor
	 rangeInfo.startLine = exInfo[ 3 ]
	 rangeInfo.startColumn = exInfo[ 4 ]
	 rangeInfo.startOffset = exInfo[ 2 ]
	 rangeInfo.endLine = exInfo[ 5 ]
	 rangeInfo.endColumn = exInfo[ 6 ]
	 rangeInfo.endOffset = exInfo[ 7 ]
	 return 0
      end
      index = index + 1
      return 1
   end

   libs.visitChildrenFast( cursor, nthVisit, nil, {}, 1 )
   if not nthCursor then
      return nil
   end

   if not targetKindList or targetKindSet[ nthCursor:getCursorKind() ] then
      return nthCursor, rangeInfo
   end

   return libs.findChildCursor( nthCursor, targetKindList )
end

libs.getOperatorTxt = function( cursor )
   local cursorKind = cursor:getCursorKind()
   if cursorKind == libclangcore.CXCursor_BinaryOperator or
      cursorKind == libclangcore.CXCursor_CompoundAssignOperator
   then
      return libs.getBinOperatorTxt( cursor )
   elseif cursorKind == libclangcore.CXCursor_UnaryOperator then
      return libs.getUnaryOperatorTxt( cursor )
   end
   return nil
end

libs.getBinOperatorTxt = function( cursor )
   local cursorKind = cursor:getCursorKind()
   if cursorKind ~= libclangcore.CXCursor_BinaryOperator and
      cursorKind ~= libclangcore.CXCursor_CompoundAssignOperator
   then
      return nil
   end
   local leftCursor, leftRangeInfo = libs.getNthCursor( cursor, 1 )

   local range = cursor:getCursorExtent()

   local file = libs.getLocation( range:getRangeEnd() )

   local unit = cursor:getTranslationUnit()
   local opLoc = unit:getLocationForOffset( file, leftRangeInfo.endOffset )
   local tokenRange = opLoc:getRange( range:getRangeEnd() )

   local opTxt
   libs.mapRangePlainText(
      unit.__ptr, tokenRange.__ptr,
      function( token )
	 if not opTxt then
	    opTxt = token
	 end
      end
   )

   if string.find( opTxt, "^[%=%|%^%-%+/%*%&]", 1, false ) then
      return opTxt
   end

   return nil
end

libs.getUnaryOperatorTxt = function( cursor )
   local cursorKind = cursor:getCursorKind()
   if cursorKind ~= libclangcore.CXCursor_UnaryOperator then
      return nil
   end
   local valCursor = libs.getNthCursor( cursor, 1 )
   local valRange = valCursor:getCursorExtent()
   
   local range = cursor:getCursorExtent()
   local unit = cursor:getTranslationUnit()

   local opRange

   local pos = ""
   if not range:getRangeStart():equalLocations( valRange:getRangeStart() ) then
      -- 単項演算子カーソルの先頭位置と、演算対象の先頭位置が異なる場合、
      -- 演算子は前方にある。
      opRange = range:getRangeStart():getRange( valRange:getRangeStart() )
      pos = "front"
   else
      -- 演算子は後方にある。
      opRange = valRange:getRangeEnd():getRange( range:getRangeEnd() )
      pos = "tail"
   end
   
   local opTxt
   libs.mapRangePlainText(
      unit.__ptr, opRange.__ptr,
      function( token )
	 if not opTxt then
	    opTxt = token
	 end
      end
   )
   
   if opTxt == "++" or opTxt == "--" or opTxt == "*" or opTxt == "&" then
      return opTxt, pos
   end
      
   return nil
end

libs.getRangeOffset = function( range )
   local startFile, startLine, startColmn, startOffset =
      libs.getLocation( range:getRangeStart() )
   local endFile, endLine, endColmn, endOffset =
      libs.getLocation( range:getRangeEnd() )

   return { startPos = startOffset, endPos = endOffset }
end
