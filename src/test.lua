-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local clang = require( 'libclanglua.if' )

local function getFileLocation( cursor )
   local location = cursor:getCursorLocation()
   return clang.getFileLocation(
      location.__ptr, clang.core.clang_getFileLocation )
end


local useFastFlag = false
local prevFile = nil
local function visitFuncMain( cursor, parent, exInfo )
   local cxfile, line, column, offset = getFileLocation( cursor )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()
   print( string.format(
   	     "%s %s %s(%d)",
   	     string.rep( " ", exInfo.depth ), txt, 
   	     clang.getCursorKindSpelling( cursorKind ), cursorKind ) )
   return 2
   
   -- local cursorKind = cursor:getCursorKind()
   -- local txt = cursor:getCursorSpelling()

   -- print( string.format(
   -- 	     "%s %s %s(%d)",
   -- 	     string.rep( " ", exInfo.depth ), txt, 
   -- 	     clang.getCursorKindSpelling( cursorKind ), cursorKind ) )

   -- if not exInfo.curFunc then
   --    if changeFileFlag ~= nil then
   -- 	 if changeFileFlag then
   -- 	    local cxfile, line = getFileLocation( cursor )
   -- 	    print( "change file 2:", cxfile and cxfile:getFileName() or "", line )
   -- 	 end
   --    else
   -- 	 local cxfile, line = getFileLocation( cursor )
   -- 	 if (cxfile and prevFile and not prevFile:isEqual( cxfile ) ) or
   -- 	    (cxfile ~= prevFile and (not cxfile or not prevFile))
   -- 	 then
   -- 	    print( "change file:", cxfile and cxfile:getFileName() or "", line )
   -- 	    prevFile = cxfile
   -- 	 end
   --    end
   -- end

   -- if cursorKind == clang.CXCursorKind.FunctionDecl.val or
   --    cursorKind == clang.CXCursorKind.CXXMethod.val
   -- then
   --    exInfo.curFunc = cursor
   --    exInfo.depth = exInfo.depth + 1
   --    if useFastFlag then
   -- 	 clang.visitChildrenFast( cursor, visitFuncMain, exInfo, nil )
   --    else
   -- 	 cursor:visitChildren( visitFuncMain, exInfo )
   --    end
   --    exInfo.depth = exInfo.depth - 1
   --    exInfo.curFunc = nil
   -- elseif cursorKind == clang.CXCursorKind.Namespace.val or
   --    cursorKind == clang.CXCursorKind.ClassDecl.val or
   --    cursorKind == clang.CXCursorKind.CompoundStmt.val or
   --    clang.isStatement( cursorKind )
   -- then
   --    exInfo.depth = exInfo.depth + 1
   --    if useFastFlag then
   -- 	 clang.visitChildrenFast( cursor, visitFuncMain, exInfo, nil )
   --    else
   -- 	 cursor:visitChildren( visitFuncMain, exInfo )
   --    end
   --    exInfo.depth = exInfo.depth - 1
   -- end
   -- return 1
end


local function visitFuncMainFast( cursor, parent, exInfo, appendInfo )
   local changeFileFlag = appendInfo[ 1 ]
   -- local cursorKind = cursor:getCursorKind()
   -- local txt = cursor:getCursorSpelling()
   -- print( string.format(
   -- 	     "%s %s %s(%d)",
   -- 	     string.rep( " ", exInfo.depth ), txt, 
   -- 	     clang.getCursorKindSpelling( cursorKind ), cursorKind ) )
   -- return 2
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()

   print( string.format(
   	     "%s %s %s(%d)",
   	     string.rep( " ", exInfo.depth ), txt, 
   	     clang.getCursorKindSpelling( cursorKind ), cursorKind ) )

   if not exInfo.curFunc then
      if changeFileFlag then
   	 local cxfile, line = getFileLocation( cursor )
   	 print( "change file 3:", cxfile and cxfile:getFileName() or "", line )
      end
   end

   if cursorKind == clang.CXCursorKind.FunctionDecl.val or
      cursorKind == clang.CXCursorKind.CXXMethod.val
   then
      exInfo.curFunc = cursor
      exInfo.depth = exInfo.depth + 1

      local result, list = clang.getChildrenList( cursor, nil, 1 )
      for index, info in ipairs( list ) do
   	 visitFuncMainFast( info[ 1 ], info[ 2 ], exInfo, info[ 3 ] )
      end
      
      exInfo.depth = exInfo.depth - 1
      exInfo.curFunc = nil
   elseif cursorKind == clang.CXCursorKind.Namespace.val or
      cursorKind == clang.CXCursorKind.ClassDecl.val or
      cursorKind == clang.CXCursorKind.CompoundStmt.val or
      clang.isStatement( cursorKind )
   then
      exInfo.depth = exInfo.depth + 1

      local result, list = clang.getChildrenList( cursor, nil, 1 )
      for index, info in ipairs( list ) do
   	 visitFuncMainFast( info[ 1 ], info[ 2 ], exInfo, info[ 3 ] )
      end
      
      exInfo.depth = exInfo.depth - 1
   end
   return 1
end

	 
local function dumpCursorTU( transUnit )
   print( transUnit )

   print( transUnit:getTranslationUnitSpelling() )

   local root = transUnit:getTranslationUnitCursor()
   print( root )

   local cursor = transUnit:getCursor(
      transUnit:getLocation(
	 transUnit:getFile( transUnit:getTranslationUnitSpelling() ), 29, 1 ) )
   print( "pos = ", cursor:getCursorSpelling() )
   print( "parent = ", clang.getCursorKindSpelling(
	     cursor:getCursorSemanticParent():getCursorKind() ) )
   

   if useFastFlag then
      --clang.visitChildrenFast( root, visitFuncMain, { depth = 0 }, nil )

      local result, list = clang.getChildrenList( root, nil, 1 )
      for index, info in ipairs( list ) do
      	 visitFuncMainFast( info[ 1 ], info[ 2 ], { depth = 0 }, info[ 3 ] )
      end
   else
      root:visitChildren( visitFuncMain, { depth = 0 } )
   end
end

local function dumpCursor( clangIndex, path, options, unsavedFileTable )
   local args = clang.mkcharPArray( options )
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )
   local transUnit = clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(),
      unsavedFileArray:getLength(), unsavedFileArray:getPtr() );

   dumpCursorTU( transUnit )
end

local clangIndex = clang.createIndex( 0, 1 )

print( "start", os.clock() )
dumpCursor( clangIndex, "test/hoge.cpp", { "-Itest" } )
print( "end", os.clock() )

useFastFlag = true
print( "start", os.clock() )
dumpCursor( clangIndex, "test/hoge.cpp", { "-Itest" } )
print( "end", os.clock() )

-- print( "start", os.clock() )
-- dumpCursor( clangIndex, "swig/libClangLua_wrap.c",
-- 	    { "-I/usr/lib/llvm-3.8/include", "-I/usr/include/lua5.3",
-- 	      "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include" } )
-- print( "end", os.clock() )

local unsavedFile = clang.core.CXUnsavedFile()
unsavedFile.Filename = "test/hoge.cpp"
unsavedFile.Contents = [[
int func() {
  return 1;
}
]]
unsavedFile.Length = #unsavedFile.Contents

print( "start", os.clock() )
dumpCursor( clangIndex, "test/hoge.cpp", { "-Itest" }, { unsavedFile } )
print( "end", os.clock() )
   
