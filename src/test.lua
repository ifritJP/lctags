-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local clang = require( 'libclanglua.if' )

local function getFileLocation( cursor )
   local location = cursor:getCursorLocation()
   return clang.getFileLocation(
      location.__ptr, clang.core.clang_getFileLocation )
end


local prevFile = nil
local function visitFuncMain( cursor, parent, exInfo )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()

   print( string.format(
   	     "%s %s %s(%d)",
   	     string.rep( " ", exInfo.depth ), txt, 
   	     clang.getCursorKindSpelling( cursorKind ), cursorKind ) )

   local cxfile, line = getFileLocation( cursor )
   if (cxfile and prevFile and not prevFile:isEqual( cxfile ) ) or
      (cxfile ~= prevFile and (not cxfile or not prevFile))
   then
      print( "change file:", cxfile and cxfile:getFileName() or "", line )
      prevFile = cxfile
   end

   if cursorKind == clang.CXCursorKind.FunctionDecl.val or
      cursorKind == clang.CXCursorKind.CXXMethod.val
   then
      exInfo.curFunc = cursor
      exInfo.depth = exInfo.depth + 1
      cursor:visitChildren( visitFuncMain, exInfo )
      exInfo.depth = exInfo.depth - 1
      exInfo.curFunc = nil
   elseif cursorKind == clang.CXCursorKind.Namespace.val or
      cursorKind == clang.CXCursorKind.ClassDecl.val or
      cursorKind == clang.CXCursorKind.CompoundStmt.val or
      clang.isStatement( cursorKind )
   then
      exInfo.depth = exInfo.depth + 1
      cursor:visitChildren( visitFuncMain, exInfo )
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
   

   root:visitChildren( visitFuncMain, { depth = 0 } )
end

local function dumpCursor( clangIndex, path, options )
   local args = clang.mkcharPArray( options )
   local transUnit = clangIndex:createTranslationUnitFromSourceFile(
      path, args:getLength(), args:getPtr(), 0, nil );

   dumpCursorTU( transUnit )
end

local clangIndex = clang.createIndex( 0, 1 )

---[[
dumpCursor( clangIndex, "test/hoge.cpp", { "-Itest" } )
--]]

--[[
dumpCursor( clangIndex, "swig/libClangLua_wrap.c",
	    { "-I/usr/include/lua5.3", "-I/usr/lib/llvm-3.8/include" } )
--]]
