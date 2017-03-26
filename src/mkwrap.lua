-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP


local baseScrit = io.open( "_base.lua" ):read( '*a' ) .. "\nreturn libs"
local baseMode = false
if arg[1] == "base" then
   baseMode = true
   baseScrit = string.gsub( baseScrit, "'libclanglua.core'",
			    "'libclanglua.coreBase'" )
end

local clang
if not loadstring then
   clang = load( baseScrit )()
else
   clang = loadstring( baseScrit )()
end

-- gc 時に dispose を実行許可するオブジェクト名の set。
-- この set のオブジェクト名に登録されていないオブジェクトから生成した場合は、
-- gc 時に dispose を実行許可しない。
local isValidDisposeNameSetMap = {
   CXTranslationUnit = {
      CXIndex = 1,
   }
}

print( "clang= ", clang )

---- EnumInfo
-- enum 情報

local EnumInfo = {}

function EnumInfo:new( name )
   
   local obj = {
      name = name,
      itemList = {}
   }
   
   setmetatable( obj, { __index = EnumInfo } )
   return obj
end

function EnumInfo:addItem( name )
   table.insert( self.itemList, name )
end


---- FuncInfo
-- 関数情報

local FuncInfo = {}

function FuncInfo:new( name, cursor )
   local typeInfo = clang.core.clang_getCursorType( cursor )
   local resultInfo = clang.core.clang_getResultType( typeInfo )
   local comment = clang.cx2string( clang.core.clang_Cursor_getRawCommentText( cursor ) )
   
   
   local obj = {
      hasBody = false,
      blockInfo = nil,
      name = name,
      paramList = {},
      result = clang.cx2string( clang.core.clang_getTypeSpelling( resultInfo ) ),
      description = comment or "",
   }
   
   setmetatable( obj, { __index = FuncInfo } )
   return obj
end

function FuncInfo:getStackSize()
   return self.blockInfo:getSize()
end

function FuncInfo:getName()
   return self.name
end

function FuncInfo:addParam( cursor )
   local typeInfo = clang.core.clang_getCursorType( cursor )
   local name = clang.cx2string( clang.core.clang_getCursorSpelling( cursor ) )
   if name == "" then
      name = string.format( "_arg%d", #self.paramList )
   end
   table.insert( self.paramList,
		 { typeName = clang.cx2string( clang.core.clang_getTypeSpelling( typeInfo ) ),
		   name = name } )
end

---- AnalyzeInfo
-- 解析情報

local AnalyzeInfo = {}

function AnalyzeInfo:new()
   local obj = {
      depth = 0,
      funcMap = {},
      targetFuncList = {},
      nsList = {},
      typedefName = "",
      enumList = {},
      name2swigDataMap = {},
      pointerParamTypeSet = {},
      resultTypeSet = {},
      targetClassSet = {},
      name2StructInfo = {},
   }

   setmetatable( obj, { __index = AnalyzeInfo } )
   AnalyzeInfo.__instance = obj
   return obj
end

function AnalyzeInfo:getTargetFunc()
   return self.targetFuncList[ #self.targetFuncList ]
end

function AnalyzeInfo:getFuncMap()
   return self.funcMap
end

function AnalyzeInfo:startFuncDecl( name, cursor )
   local funcInfo = FuncInfo:new( name, cursor )
   table.insert( self.targetFuncList, funcInfo )
end

function AnalyzeInfo:endFuncDecl()
   local funcInfo = self:getTargetFunc()
   self.funcMap[ funcInfo.name ] = funcInfo
   table.remove( self.targetFuncList )
end

function AnalyzeInfo:getFuncDepth()
   return #self.targetFuncList
end

function AnalyzeInfo:startTypedef( name )
   self.typedefName = name
end

function AnalyzeInfo:endTypedef()
   self.typedefName = ""
end

function AnalyzeInfo:getTypedefName()
   return self.typedefName
end

function AnalyzeInfo:startEnum( name )
   table.insert( self.enumList, EnumInfo:new( name ) )
end

function AnalyzeInfo:endEnum()
end

function AnalyzeInfo:addEnumItem( name )
   self.enumList[ #self.enumList ]:addItem( name )
end

function AnalyzeInfo:addSwigData( data )
   self.name2swigDataMap[ data.className ] = data
end


local debugInfo =  debug.getinfo( 1 )
for key, value in pairs( debugInfo ) do
   print( key, value )
end

local scriptPath = os.getenv( "PWD" ) .. "/" .. debugInfo.short_src
print( scriptPath )

local function isFuncDecl( cursorKind )
   return cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_Destructor or
      cursorKind == clang.core.CXCursor_FunctionDecl
end

local function isCharType( typeKind )
   return typeKind == clang.core.CXType_Char_U or 
      typeKind == clang.core.CXType_UChar or
      typeKind == clang.core.CXType_Char_S or
      typeKind == clang.core.CXType_SChar
end

local function calVarSize( cursor )
   local cursorType = clang.core.clang_getCursorType( cursor )
   return clang.core.clang_Type_getSizeOf( cursorType )
end

local function visitFuncMain( cursor, parent, exInfo )
   local cursorKind = clang.core.clang_getCursorKind( cursor )
   local txt = clang.cx2string( clang.core.clang_getCursorSpelling( cursor ) )

   print( string.format(
	     "%s %s %s(%d)",
	     string.rep( " ", exInfo.depth ), txt, 
	     clang.cx2string( clang.core.clang_getCursorKindSpelling(
			   clang.core.clang_getCursorKind( cursor ) ) ),
	     cursorKind ) )
   
   local endProcess = {}
   local cursorKind = clang.core.clang_getCursorKind( cursor )

   if cursorKind == clang.core.CXCursor_EnumConstantDecl then
      exInfo:addEnumItem( txt )
   end

   if cursorKind == clang.core.CXCursor_EnumDecl then
      local name = txt
      if name == "" then
	 name = exInfo:getTypedefName()
      end
      if name == "" then
	 return 1
      end
      if name ~= "" then
	 exInfo:startEnum( name )
	 table.insert( endProcess, function() exInfo:endEnum() end )
      end
   end

   if cursorKind == clang.core.CXCursor_StructDecl then
      if txt == "" then
	 txt = exInfo.typedefName
      end
      if txt ~= "" then
	 exInfo.name2StructInfo[ txt ] = cursor
      end
   end

   
   if cursorKind ~= clang.core.CXCursor_Namespace and
      cursorKind ~= clang.core.CXCursor_ClassDecl and
      cursorKind ~= clang.core.CXCursor_TypedefDecl and
      cursorKind ~= clang.core.CXCursor_EnumDecl and
      not isFuncDecl( cursorKind ) and
      not ( exInfo:getFuncDepth() > 0 and
	       ( cursorKind == clang.core.CXCursor_CompoundStmt or
		    clang.core.clang_isStatement( cursorKind ) ) )
   then
      return 1
   end

   if cursorKind == clang.core.CXCursor_TypedefDecl then
      exInfo:startTypedef( txt )
      table.insert( endProcess, function() exInfo:endTypedef() end )
   end

   if cursorKind == clang.core.CXCursor_ParmDecl then
      local targetFunc = exInfo:getTargetFunc()
      targetFunc:addParam( cursor )

      if targetFunc:getName():find( "^clang_" ) then
	 local cursorType = clang.core.clang_getCursorType( cursor )
	 local pointeeType = clang.core.clang_getPointeeType( cursorType )
	 local baseType = pointeeType
	 local typeStr = clang.cx2string( clang.core.clang_getTypeSpelling( cursorType ) )

	 typeStr = string.gsub( typeStr, "const ", "" )
	 
	 if pointeeType.kind ~= clang.core.CXType_Invalid and
	    not exInfo.pointerParamTypeSet[ typeStr ]
	 then
	    -- cursorType is pointer
	    while true do
	       local parentType = clang.core.clang_getPointeeType( baseType )
	       if parentType.kind == clang.core.CXType_Invalid then
		  break
	       end
	       baseType = parentType
	    end
	    local pointeeTxt = clang.cx2string(
	       clang.core.clang_getTypeSpelling( pointeeType ) )
	    local baseTxt = clang.cx2string(
	       clang.core.clang_getTypeSpelling( baseType ) )
	    local resultType = clang.core.clang_getResultType( baseType )
	    if resultType.kind == clang.core.CXType_Invalid and
	       pointeeType.kind ~= clang.core.CXType_Void and
	       ( not isCharType( baseType.kind ) or ( pointeeTxt ~= baseTxt ) )
	    then
	       exInfo.pointerParamTypeSet[ typeStr ] = {
		  string.gsub( clang.cx2string(
				  clang.core.clang_getTypeSpelling( pointeeType ) ),
			       "const ", "" ),
		  string.gsub( clang.cx2string(
				  clang.core.clang_getTypeSpelling( baseType ) ),
			       "const ", "" ),
		  baseType.kind }
	    end
	 end

	 if #targetFunc.paramList == 1 and
	    pointeeType.kind == clang.core.CXType_Invalid and
	    not clang.isBuiltInTypeKind( cursorType.kind ) and
	    cursorType.kind ~= clang.core.CXType_Enum and
	    not exInfo.targetClassSet[ typeStr ]
	 then
	    local declCursor = clang.core.clang_getTypeDeclaration( cursorType )
	    if declCursor.kind ~= clang.core.CXCursor_EnumDecl then
	       -- 次のパラメータを登録
	       -- - 先頭のパラメータ
	       -- - ポインタでない
	       -- - プリミティブ
	       exInfo.targetClassSet[ typeStr ] = 1
	       print( "targetClassSet: ", typeStr, declCursor.kind )
	    end
	 end
      end
   end

   if isFuncDecl( cursorKind ) then
      exInfo:startFuncDecl( txt, cursor )
      table.insert(
	 endProcess,
	 function()
	    exInfo:endFuncDecl()
      end )
   end
   exInfo.depth = exInfo.depth + 1
   
   local ret = clang.visitChildrenLow( cursor, visitFuncMain, exInfo )
   
   exInfo.depth = exInfo.depth - 1
   for index, process in ipairs( endProcess ) do
      process()
   end
   
   return 1
end

local function dumpEnum( classFile, enumList )
   for index, enumInfo in pairs( enumList ) do
      classFile:write( string.format( "libs.%s = {\n", enumInfo.name ) )
      for eIndex, item in pairs( enumInfo.itemList ) do
	 classFile:write(
	    string.format(
	       [[
  %s = {
    name = "%s",
    val = libclangcore.%s
  },
]],
	       string.gsub( item, "%w+_", "", 1 ), item, item ) )
      end
      classFile:write( string.format( "}\n", enumInfo.name ) )
   end
end

local function createNew( targetName, objName, stmt )
   local validFlag = false
   local set = isValidDisposeNameSetMap[ objName ] 
   if (not targetName) or (not set) or set[ targetName ] then
      validFlag = true
   end
   
   return string.format( "libs.%s:new( %s, %s )", objName, stmt, not validFlag )
end

local function dumpMethod( classFile, funcMap, targetClassSet )
   
   local class2methodLitMap = {}
   
   for index, funcInfo in pairs( funcMap ) do
      if funcInfo:getName():find( "clang_" ) == 1 then
	 print( "----" )
	 print( funcInfo:getName(), funcInfo.result )


	 local declParamTxt = ""
	 local callParamTxt = ""
	 local paramDescript = ""
	 local retDesript = " @return "
	 if funcInfo.result ~= "void" then
	    retDesript = retDesript .. funcInfo.result
	 end
	 for pindex, param in ipairs( funcInfo.paramList ) do
	    print( "param =", pindex, param.typeName )
	    local callParam = param.name
	    if targetClassSet[ param.typeName ] then
	       callParam = callParam .. ".__ptr"
	    end
	    if param.typeName ~= "unsigned int *" then
	       if declParamTxt == "" then
		  declParamTxt = "_" .. param.name
		  callParamTxt = "_" .. callParam
	       else
		  declParamTxt = declParamTxt .. ", _" .. param.name
		  callParamTxt = callParamTxt .. ", _" .. callParam
	       end
	       paramDescript = paramDescript .. "\n @param _" .. param.name .. " " .. param.typeName
	    else
	       retDesript = string.format( "%s%s<%s>, ",
					   retDesript, param.name, "unsigned int" )
	    end
	    
	 end

	 if funcInfo.description ~= "" then
	    classFile:write(
	       string.format( "--[==[\n%s\n%s\n%s\n]==]\n",
			      funcInfo.description, paramDescript, retDesript ) )
	 end
	 
	 --[[
	 if funcInfo.name == "clang_visitChildren" then
	    declParamTxt = string.gsub( declParamTxt, ",? _*[%w%._]+$", "", 1 )
	    callParamTxt = string.gsub( callParamTxt, ",? _*[%w%._]+$", "", 1 )
	 end
	 --]]

	 
	 local method = false
	 if #funcInfo.paramList >= 1 then
	    local workDeclParamTxt = string.gsub( declParamTxt, "^_*[%w%._]+,?", "", 1 )
	    local workCallParamTxt = string.gsub( callParamTxt, "^_*[%w%._]+,?", "", 1 )

	    
	    print( "result = ", funcInfo.result )
	    if targetClassSet[ funcInfo.paramList[1].typeName ] and
	       clang.core[ funcInfo.name ]
	    then
	       local targetName = funcInfo.paramList[1].typeName
	       local libFuncName = "libclangcore." .. funcInfo:getName()
	       if clang[ funcInfo:getName() ] then
		  libFuncName = "libs." .. funcInfo:getName()
	       end
	       
	       local callStmt = string.format(
		  "%s( %s )", libFuncName,
		  "self.__ptr" .. ((workCallParamTxt == "") and "" or ", ") .. workCallParamTxt )
       
	       if funcInfo.result == "CXString" then
		  callStmt = "libs.cx2string( " .. callStmt .. " )"
	       elseif targetClassSet[ funcInfo.result ] then
		  callStmt = createNew( targetName, funcInfo.result, callStmt )
	       end
	       if string.find( callStmt, "_is%u" ) then
		  callStmt = string.format( callStmt .. " ~= 0" )
	       end
	       

	       local methodName = string.gsub( funcInfo:getName(), "clang_", "" )
	       methodName = string.gsub( methodName, "^" .. targetName .. "_", "" )
	       methodName = string.gsub(
		  methodName, "^" .. string.gsub( targetName, "^CX", "", 1 ) .. "_", "" )
	       
	       classFile:write(
		  string.format( [[
function libs.%s:%s( %s )
  return %s
end
]],
		     targetName, methodName, workDeclParamTxt, callStmt ) )
	       method = true

	       local methodList = class2methodLitMap[ targetName ]
	       if not methodList then
		  methodList = {}
		  class2methodLitMap[ targetName ] = methodList
	       end
	       table.insert( methodList, methodName )
	    else
	       print( "not method",
		      funcInfo.name,
		      funcInfo.paramList[1].typeName,
		      targetClassSet[ funcInfo.paramList[1].typeName ],
		      clang.core[ funcInfo.name ] )
	    end
	 end
	 if not method then
	    local callStmt = string.format(
	       "libclangcore.%s( %s )", funcInfo:getName(), callParamTxt )
	    if funcInfo.result == "CXString" then
	       callStmt = "libs.cx2string( " .. callStmt .. " )"
	    elseif targetClassSet[ funcInfo.result ] then
	       callStmt = createNew( nil, funcInfo.result, callStmt )
	    end
	    if string.find( callStmt, "_is%u" ) then
	       callStmt = string.format( callStmt .. " ~= 0" )
	    end

	    local methodName = string.gsub( funcInfo:getName(), "clang_", "" )
	    classFile:write(
	       string.format( [[
function libs.%s( %s )
  return %s
end
]],
		  methodName, declParamTxt, callStmt ) )
	 end
	 classFile:write( "\n" )
      end
   end
   return class2methodLitMap
end

local function dumpClasses( classFile, targetClassSet )
   for className in pairs( targetClassSet ) do
      classFile:write( string.format( "libs.%s = {}\n", className ) )
   end
end

local function getSwigDataTxt( swigData )
   local attribsTxt = ""
   for index, attrib in ipairs( swigData.attribList ) do
      attribsTxt = attribsTxt .. "\n--      " .. attrib
   end
   return attribsTxt
end

local function dumpConstructer(
      classFile, class2methodLitMap, disposeFuncMap,
      completionMode, name2swigDataMap, targetClassSet, name2StructInfo )
   for className in pairs( targetClassSet ) do
      local gcFunc = disposeFuncMap[ className ]
      if gcFunc then
	 --gcFunc = string.format( "function( val ) %s( val.__ptr ) end", gcFunc )
	 gcFunc = string.format( "%s( self.__ptr )", gcFunc )
      end
      local methodTxt = ""
      if completionMode then
	 local methodList = class2methodLitMap[ className ]
	 if methodList then
	    for index, method in pairs( methodList ) do
	       methodTxt = string.format(
		  "%s,\n      %s = libs.%s.%s", methodTxt, method, className, method )
	    end
	 end
      end
      local swigData = name2swigDataMap[ className ]
      
      local attribsTxt = ""
      if swigData then
	 attribsTxt = getSwigDataTxt( swigData )
      end
      if name2StructInfo[ className ] then
	 attribsTxt = string.format( [[
%s--[==[
%s
]==]
]], "\n", clang.getCurosrPlainText( name2StructInfo[ className ] ) )
      end
      
      methodTxt = "\n--    __ptr is next struct." .. attribsTxt .. methodTxt
      
      
      classFile:write( string.format( [[
function libs.%s:new( ptr, invalidDisposeFlag )
   if not ptr then
      return nil
   end
   local obj = {
      __needDisposeFlag = not invalidDisposeFlag,
      __ptr = ptr,%s
      disableDispose = function( self )
         self.__needDisposeFlag = false
      end
   }
   setmetatable(
      obj,
      {
         __index = libs.%s,
         __gc = function( self )
            if self.__needDisposeFlag then
               %s
            end
         end
      }
)
   return obj
end

]], className, methodTxt, className, gcFunc or ";" ) )
   end
end


local function mapPointerParam( pointerParamTypeSet, func )
   for typeName, typeInfo in pairs( pointerParamTypeSet ) do
      local pointeeTxt = typeInfo[ 1 ]
      local baseTxt = typeInfo[ 2 ]
      local baseTypeKind = typeInfo[ 3 ]
      local arrayName = string.gsub( pointeeTxt, " (%*+)$", "%1" )
      arrayName = string.gsub( arrayName, "%*", "P" )
      arrayName = string.gsub( arrayName, "signed ", "signed" )
      arrayName = string.gsub( arrayName, ".* ", "" )
      arrayName = arrayName .. "Array"
      func( typeName, typeInfo, arrayName )
   end
end


local function dumpSwigData(
      classFile, swigDataMap, pointerParamTypeSet,
      targetClassSet, name2StructInfo )
   for className, swigData in pairs( swigDataMap ) do
      if not targetClassSet[ className ] then
	 local attribsTxt = getSwigDataTxt( swigData )
	 if name2StructInfo[ className ] then
	    attribsTxt = clang.getCurosrPlainText( name2StructInfo[ className ] )
	 end
	 
	 classFile:write(
	    string.format( [[
--[==[
 For generating instance of libs.core.%s, call libs.core.%s().
 libs.core.%s instance has next attributes.

%s;
]==]


]], swigData.className, swigData.className, swigData.className, attribsTxt ) )
      end
   end

   mapPointerParam(
      pointerParamTypeSet,
      function( typeName, typeInfo, arrayName )
	 local pointeeTxt = typeInfo[ 1 ]
	 local baseTxt = typeInfo[ 2 ]
	 local baseTypeKind = typeInfo[ 3 ]
	 local swigTxt = string.format( [[
libs.mk%s = function( tbl, length )
   local len = tbl
   local typeId = type( tbl )
   local freeFlag = true
   local __ptr = tbl
   if typeId == "table" then
      len = #tbl
      __ptr = libclangcore.new_%s( len )
   else
      if length == nil then
         len = 0
      else
         len = length
      end
      if typeId == "userdata" then
         freeFlag = false
      else
         __ptr = libclangcore.new_%s( len )
      end
   end
   local array = {
      __length = len,
      __ptr = __ptr,
      __freeFlag = freeFlag,
      getLength = function( self )
	 return self.__length
      end,
      getPtr = function( self )
	 return self.__ptr
      end,
      getItem = function( self, index )
         return libclangcore.%s_getitem( self.__ptr, index )
      end,
      setItem = function( self, index, val )
         libclangcore.%s_setitem( self.__ptr, index, val )
      end,
   }
   setmetatable( array, { __gc = function( self )
                                   if self.__freeFlag then
                                      libclangcore.delete_%s( self.__ptr )
                                   end
                                 end } )

   if typeId == "table" then
      for key, val in ipairs( tbl ) do
         libclangcore.%s_setitem( array.__ptr, key - 1, val )
      end
   end
   return array
end
]], arrayName, arrayName, arrayName, arrayName, arrayName, arrayName, arrayName )
	 classFile:write( swigTxt )
      end
   )
end


local function createIfFile( classFile, info, completionMode )

   local baseFile = io.open( "_base.lua", "r" )
   classFile:write( baseFile:read( '*a' ) )
   baseFile:close()
   
   local disposeFuncMap = {}
   for name, funcInfo in pairs( info:getFuncMap() ) do
      if name:find( "dispose" ) and #funcInfo.paramList == 1
      then
	 local targetName = funcInfo.paramList[1].typeName
	 if info.targetClassSet[ targetName ] then
	    disposeFuncMap[ targetName ] = "libclangcore." .. name
	 end
      end
   end
   
   dumpClasses( classFile, info.targetClassSet )
   local class2methodLitMap = dumpMethod(
      classFile, info:getFuncMap(), info.targetClassSet )
   dumpConstructer(
      classFile, class2methodLitMap, disposeFuncMap,
      completionMode, info.name2swigDataMap, info.targetClassSet,
      info.name2StructInfo )
   dumpEnum( classFile, info.enumList )
   dumpSwigData( classFile, info.name2swigDataMap,
		 info.pointerParamTypeSet, info.targetClassSet,
		 info.name2StructInfo )

   classFile:write( "return libs\n" )
end


local function visitFuncMain2( cursor, parent, exInfo )
   local cursorKind = clang.core.clang_getCursorKind( cursor )
   local txt = clang.cx2string( clang.core.clang_getCursorSpelling( cursor ) )

   print( string.format(
	     "%s %s %s(%d)",
	     string.rep( " ", exInfo.depth ), txt, 
	     clang.cx2string( clang.core.clang_getCursorKindSpelling(
			   clang.core.clang_getCursorKind( cursor ) ) ),
	     cursorKind ) )

   if cursorKind == clang.core.CXCursor_VarDecl then
      local declType = clang.core.clang_getCursorType( cursor )
      local declTyepTxt =
	 clang.cx2string( clang.core.clang_getTypeSpelling( declType ) )
      if declTyepTxt:find( "swig_lua_attribute" ) and
	 declTyepTxt ~= "swig_lua_attribute [1]" 
      then
	 local className = string.gsub( txt, "swig_(%w+)_attributes$", "%1" )
	 print( "has attrib", className )

	 local swigData = { className = className, attribList = {}, }
	 exInfo:addSwigData( swigData )
	 exInfo.depth = exInfo.depth + 1
	 clang.visitChildrenLow(
	    cursor,
	    function( aCursor, aParent, exInfo )
	       local cursorKind = clang.core.clang_getCursorKind( aCursor )
	       local txt =
		  clang.cx2string( clang.core.clang_getCursorSpelling( aCursor ) )
	       print( string.format(
			 "%s %s %s(%d)",
			 string.rep( " ", exInfo.depth ), txt, 
			 clang.cx2string(
			    clang.core.clang_getCursorKindSpelling(
			       clang.core.clang_getCursorKind( aCursor ) ) ),
			 cursorKind ) )

	       if cursorKind == clang.core.CXCursor_StringLiteral then
		  local attribName = string.gsub( txt, '^"(.+)"$', "%1" )
		  print( string.format( "attrib = %s.%s", className, attribName ) )
		  table.insert( swigData.attribList, attribName )
	       end
		  
	       return 2
	    end,
	    exInfo
	 )
	 exInfo.depth = exInfo.depth - 1
      end
   end

   return 1
end

local function dumpCursorTU( transUnit, visitFunc, exInfo )
   print( transUnit )

   print( clang.cx2string( clang.core.clang_getTranslationUnitSpelling( transUnit ) ) )

   local root = clang.core.clang_getTranslationUnitCursor( transUnit )
   print( root )

   clang.visitChildrenLow( root, visitFunc, exInfo )
end



local function dumpCursor( clangIndex, path, options, dumpFunc, exInfo )
   local args = clang.mkCharArray( options )
   local transUnit = clang.core.clang_createTranslationUnitFromSourceFile(
      clangIndex, path, args:getLength(), args:getPtr(), 0, nil )

   dumpCursorTU( transUnit, dumpFunc, exInfo )

   return transUnit
end

local function dumpCursorPch( clangIndex, path, dumpFunc, exInfo )
   local transUnit = clang.core.clang_createTranslationUnit( clangIndex, path )

   dumpCursorTU( transUnit, dumpFunc, exInfo )

   return transUnit
end


keys = {}
for key, value in pairs( clang.core ) do
   table.insert( keys, key )
end

table.sort( keys )
for index, key in ipairs( keys ) do
   print( key, clang.core[ key ], type( clang.core[ key ] ) )
end

local baseDir = string.gsub( scriptPath, "(.*)/.*", "%1/../external" )
local clangDir = baseDir .. "/clang/r390"

local clangIndex = clang.core.clang_createIndex( 1, 1 )


local incDir = clangDir .. "/build/lib/clang/3.9.0/include"
if arg[2] then
   incDir = arg[2]
end

local clangIncDir = clangDir .. "/llvm/tools/clang/include"
if arg[3] then
   clangIncDir = arg[3]
end

local luaIncDir = baseDir .. "/lua/lua-5.3.4/src"
if arg[4] then
   luaIncDir = arg[4]
end


local analyzeInfo = AnalyzeInfo.new()

---[[
local transUnit = dumpCursor(
   clangIndex, "swig/wrap.c",
   { "-I" .. incDir, "-I" .. clangIncDir, "-I." },
   visitFuncMain, analyzeInfo )
--]]

local transUnit2 = dumpCursor(
   clangIndex, "swig/libClangLuaBase_wrap.c",
   { "-I" .. incDir, "-I" .. clangIncDir, "-I.", "-I" .. luaIncDir },
   visitFuncMain2, analyzeInfo )


if baseMode then
   local swigArrays = ""
   mapPointerParam(
      analyzeInfo.pointerParamTypeSet,
      function( typeName, typeInfo, arrayName )
	 local pointeeTxt = string.gsub( typeInfo[ 1 ], "const", "" )
	 local baseTxt = typeInfo[ 2 ]
	 local baseTypeKind = typeInfo[ 3 ]
	 local swigTxt = string.format(
	    "%%%%array_functions(%s, %s);", pointeeTxt, arrayName )
	 swigArrays = swigArrays .. "\n" .. swigTxt
	 print( "paramType2:", typeName, pointeeTxt, baseTxt, arrayName, swigTxt )
      end
   )
   local swigSrcFile = io.open( "swig/libClangLuaBase.i", "r" )
   swigSrc = swigSrcFile:read( '*a' )
   swigSrc = string.gsub( swigSrc, "//@array@", swigArrays )
   swigSrc = string.gsub( swigSrc, "%%module libclanglua_coreBase",
			  "%%module libclanglua_core" )
   swigSrcFile:close()

   local swigDstFile = io.open( "swig/libClangLua.i", "w" )
   swigDstFile:write( swigSrc )
   swigDstFile:close()
else
   createIfFile( io.open( "libclanglua/if.lua", "w" ), analyzeInfo, false )

   createIfFile( io.open( "libclanglua/ifc.lua", "w" ), analyzeInfo, true )
end
