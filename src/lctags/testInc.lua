local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )

local TestInc = {}

function TestInc:run( analyzer, path, target )
   if not target then
      target = ""
   end

   path = analyzer:convFullpath( path )
   
   local unit, compileOp, newAnalyzer =
      analyzer:createUnit( path, target, false )


   local incList = clang.getInclusionList( unit )
   for index, incFile in ipairs( incList ) do
      log( 1, incFile:getIncludedFile():getFileName() )
   end
end

return TestInc
