-- Copyright (C) 2017 ifritJP

local libs = {}
local libclangcore = require( 'libclanglua.core' )
libs.core = libclangcore

if _VERSION == "Lua 5.1" then
   table.pack = function( ... )
      return { ... }
   end
   table.unpack = function( list )
      return unpack( list )
   end
end

libs.cx2string = function( cxstr )
   local str = libclangcore.clang_getCString( cxstr )
   libclangcore.clang_disposeString( cxstr )
   return str
end

libs.visitChildrenLow = function( cursor, func, exInfo )
   if not __libclang_visit then
      __libclang_visit = {}
   end
   table.insert( __libclang_visit, { func, exInfo } )
   local result = libclangcore.clang_visitChildren( cursor, nil )

   table.remove( __libclang_visit )
   
   return result
end

libs.clang_visitChildren = function( cxCursor, func, exInfo )
   local wrapFunc = function( aCursor, aParent, aExInfo )
      return func( libs.CXCursor:new( aCursor ),
		   libs.CXCursor:new( aParent ), aExInfo )
   end
   return libs.visitChildrenLow( cxCursor, wrapFunc, exInfo )
end


libs.visitChildrenFast = function( cursor, func, exInfo, kindList, callbackResult )
   local result, list = libs.getChildrenList( cursor, kindList, callbackResult )
   
   for index, info in ipairs( list ) do
      func( info[ 1 ], info[ 2 ], exInfo, info[ 3 ] )
   end
   return result
end

libs.getChildrenList = function( cursor, kindList, callbackResult )
   if not kindList then
      kindList = {}
   end
   local kindArray = libclangcore.new_intArray( #kindList + 2 )
   
   libclangcore.intArray_setitem( kindArray, 0, callbackResult )
   for index, kind in ipairs( kindList ) do
      libclangcore.intArray_setitem( kindArray, index, kind )
   end
   libclangcore.intArray_setitem(
      kindArray, #kindList + 1, libclangcore.CXCursor_InvalidFile )

   local list = {}
   local result = libs.visitChildrenLow( cursor.__ptr, list, kindArray )
   libclangcore.delete_intArray( kindArray )

   local cursorList = {}
   for index, info in ipairs( list ) do
      table.insert(
	 cursorList,
	 { libs.CXCursor:new( info[ 1 ] ),
	   libs.CXCursor:new( info[ 2 ] ),
	   table.pack( index == 1 or info[ 3 ],
		       table.unpack( info, 4 ) ) } )
   end
   return result, cursorList
end



libs.getInclusionsLow = function( unit, func, exInfo )
   if not __libclang_visit then
      __libclang_visit = {}
   end
   table.insert( __libclang_visit, { func, exInfo } )
   libclangcore.clang_getInclusions( unit, nil )

   table.remove( __libclang_visit )
end

libs.clang_getInclusions = function( unit, func, exInfo )
   local wrapFunc = function( included_file, inclusion_stack,
			      include_len, client_data )
      return func( libs.CXFile:new( included_file ),
		   libs.CXCXSourceLocation:new( inclusion_stack ),
		   inclusion_len, client_data )
   end
   return libs.getInclusionsLow( cursor, wrapFunc, exInfo )
end

libs.mkCharArray = function( strArray )
   local array = {
      __length = #strArray,
      __ptr = libclangcore.new_charArray( #strArray ),
      
      getLength = function( self )
	 return self.__length
      end,
      
      getPtr = function( self )
	 return self.__ptr
      end,
   }
   
   for key, str in ipairs( strArray ) do
      libclangcore.charArray_setitem( array.__ptr, key - 1, str )
   end
   return array
end

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
if libs.core.new_CXFileArray then
   cxFileArray = libs.core.new_CXFileArray( 1 )
end
--   libs.core.delete_CXFileArray( cxFileArray )
libs.getFileLocation = function( obj, func, ... )
   local params = table.pack( ... )
   table.insert( params, cxFileArray )
   local result = table.pack( func( obj, table.unpack( params ) ) )
   local cxFile = libs.CXFile:new( libs.core.CXFileArray_getitem( cxFileArray, 0 ) )
   return cxFile, table.unpack( result )
end

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
   local tokenPBuf = libclangcore.new_CXTokenPArray( 1 )
   local tokenNum = libclangcore.clang_tokenize( unit, srcRange, tokenPBuf )
   local tokenArray = libclangcore.CXTokenPArray_getitem( tokenPBuf, 0 )
   local txt = ""
   for index = 0, tokenNum - 1 do
      local token = libclangcore.CXTokenArray_getitem( tokenArray, index )
      func( libs.cx2string( libclangcore.clang_getTokenSpelling( unit, token ) ), ... )
   end
   libclangcore.clang_disposeTokens( unit, tokenArray, tokenNum )
   libclangcore.delete_CXTokenPArray( tokenPBuf )
end

libs.getDeclCursorFromType = function( cxtype )
   while true do
      local baseType = cxtype and cxtype:getPointeeType()
      if baseType.__ptr.kind == libs.core.CXType_Invalid then
	 if cxtype then
	    baseType = cxtype:getElementType()
	 end
	 if baseType.__ptr.kind == libs.core.CXType_Invalid then
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
