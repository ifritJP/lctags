local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )

local Scan = {}

function Scan:outputIncSrcHeader(
      scanMode, analyzer, src, target, fileContents )

   local unit, compileOp, newAnalyzer =
      analyzer:createUnit( src, target, false, fileContents )

   local incList = clang.getInclusionList( unit )
   for index, incFile in ipairs( incList ) do
      log( 1, incFile:getIncludedFile():getFileName() )
   end
end

return Scan
