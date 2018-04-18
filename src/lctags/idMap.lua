local idMap = {}
local cursorKind2NameMap = {}
local clang = require( 'libclanglua.if' )

idMap.cursorKind2NameMap = cursorKind2NameMap

for key, info in pairs( clang.CXCursorKind ) do
   cursorKind2NameMap[ info.val ] = key
end

return idMap
