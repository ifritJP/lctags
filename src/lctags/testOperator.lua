local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )

local TestOpe = {}

local function visit( cursor, parent, info, addInfo )
   local cursorKind = cursor:getCursorKind()

   Util:dumpCursorInfo( cursor, 1, "", 0 )

   if cursorKind == clang.core.CXCursor_BinaryOperator or
      cursorKind == clang.core.CXCursor_CompoundAssignOperator or
      cursorKind == clang.core.CXCursor_UnaryOperator
   then
      log( 1, clang.getOperatorTxt( cursor ) )
   end

   return 1
end


function TestOpe:at( analyzer, path, target )
   if not target then
      target = ""
   end

   path = analyzer:convFullpath( path )
   
   local analyzerForTokenize = analyzer:newAs(
      log( -4 ) >= 2 and true or false, false )
   local unit, compileOp, newAnalyzer =
      analyzerForTokenize:createUnit( path, target, false )

   clang.visitChildrenFast( unit:getTranslationUnitCursor(), visit, info, {}, 2 )
end

return TestOpe