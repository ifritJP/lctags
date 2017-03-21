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
   	     clang.getCursorKindSpelling( cursorKind ), cursorKind ),
	  cursor:hashCursor(),
	  cxfile and cxfile:getFileName(), line, column, os.clock() )
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
   

   if useFastFlag == 1 then
      clang.visitChildrenFast( root, visitFuncMain, { depth = 0 }, nil, 2 )
   elseif useFastFlag == 2 then
      clang.visitChildrenFast2(
	 root, visitFuncMain, { depth = 0 },
	 { clang.core.CXCursor_MacroDefinition },
	 { clang.core.CXCursor_FieldDecl },
	 { transUnit:getFile( transUnit:getTranslationUnitSpelling() ) }, 2 )
   else
      root:visitChildren( visitFuncMain, { depth = 0 } )
   end
end

local function dumpCursor( clangIndex, path, options, unsavedFileTable )
   local args = clang.mkcharPArray( options )
   local unsavedFileArray = clang.mkCXUnsavedFileArray( unsavedFileTable )
   print( "createTranslationUnitFromSourceFile start", os.clock() )

   local unitArray = clang.mkCXTranslationUnitArray( nil, 1 )
   local code = clangIndex:parseTranslationUnit2(
      path, args:getPtr(), args:getLength(),
      unsavedFileArray:getPtr(), unsavedFileArray:getLength(),
      clang.core.CXTranslationUnit_ForSerialization +
	 clang.core.CXTranslationUnit_DetailedPreprocessingRecord,
      unitArray:getPtr() )

   if code ~= clang.core.CXError_Success then
      print( "failed to parseTranslationUnit2", code, clang.core.CXError_Success )
      os.exit( 1 )
   end

   local transUnit = clang.CXTranslationUnit:new( unitArray:getItem( 0 ) )
   
   print( "createTranslationUnitFromSourceFile end", os.clock() )

   dumpCursorTU( transUnit )
end

-- for key, val in pairs( clang.core ) do
--    print( key, val )
-- end

local clangIndex = clang.createIndex( 1, 1 )

-- if arg[ 1 ] then
--    local optList = { "-M" }
--    for index, opt in ipairs( arg ) do
--       table.insert( optList, opt )
--    end
--    dumpCursor( clangIndex, "test/hoge.cpp", optList, nil )

--    os.exit( 1 )
-- end

-- print( string.format( "%s %s %s %s", arg[-1], arg[0], "-Itest",
-- 		      "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include" ) )

-- local pipe = io.popen(
--    string.format( "%s %s %s %s", arg[-1], arg[0], "-Itest",
-- 		  "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include" ) )
-- while true do
--    local txt = pipe:read( '*l' )
--    if not txt then
--       break
--    end
--    for path in string.gmatch( txt, "[^%s\\]+" ) do
--       if not string.find( path, ":$" ) then
-- 	 print( "file", path )
--       end
--    end
-- end

useFastFlag = 1
-- print( "start", os.clock() )
-- dumpCursor( clangIndex, "../external/luasqlite3/lsqlite3_fsl09x/sqlite3.c", { "-Itest" } )
-- print( "end", os.clock() )

print( "start", os.clock() )
-- dumpCursor( clangIndex, "test/inc1.cpp", { "-Itest", "-std=c++11", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include", "-include-pch", "field.pch" } )
-- dumpCursor( clangIndex, "test/inc1.cpp", { "-Itest", "-std=c++11", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include", "-include-pch", "inc1.pch" } )
-- dumpCursor( clangIndex, "test/inc1.cpp",
-- 	    { "-Itest", "-std=c++1z", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include",
-- 	      --"-include-pch", "inc1.h.pch", "-include-pch", "inc2.h.pch" } )
-- 	      --"-include-pch", "inc2.h.pch" } )
-- 	      "-include-pch", ".lctags/pch/@/proj/test/inc1.h.pch" } )
-- 	    --})
print( "end", os.clock() )


useFastFlag = 2
print( "start", os.clock() )
dumpCursor( clangIndex, "test/class.cpp",
	    { "-Itest", "-std=c++11", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include",
	      "-include-pch", ".lctags/pch/@/proj/test/class.h.pch.c++11"
} )
print( "end", os.clock() )

-- useFastFlag = 1
-- print( "start", os.clock() )
-- dumpCursor( clangIndex, "test/hoge.cpp",
-- 	    { "-Itest", "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include" } )
-- print( "end", os.clock() )

-- -- print( "start", os.clock() )
-- -- dumpCursor( clangIndex, "swig/libClangLua_wrap.c",
-- -- 	    { "-I/usr/lib/llvm-3.8/include", "-I/usr/include/lua5.3",
-- -- 	      "-I/usr/lib/llvm-3.8/lib/clang/3.8.0/include" } )
-- -- print( "end", os.clock() )

-- local unsavedFile = clang.core.CXUnsavedFile()
-- unsavedFile.Filename = "test/hoge.cpp"
-- unsavedFile.Contents = [[
-- int func() {
--   return 1;
-- }
-- ]]
-- unsavedFile.Length = #unsavedFile.Contents

-- print( "start", os.clock() )
-- dumpCursor( clangIndex, "test/hoge.cpp", { "-Itest" }, { unsavedFile } )
-- print( "end", os.clock() )
   
