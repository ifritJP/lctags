local Json = require( 'lctags.Json' )

function printJson( keyName, obj )
   if type( obj ) == "table" then
      if #obj ~= 0 then
	 for key, val in ipairs( obj ) do
	    printJson( string.format( "%s[%d]", keyName, key ), val )
	 end
      else
	 for key, val in pairs( obj ) do
	    printJson( string.format( "%s.%s", keyName, key ), val )
	 end
      end
   else
      if keyName ~= "" then
	 print( keyName .. ":", obj )
      else
	 print( obj )
      end
   end
end

local obj = Json:convertFrom( [[
{
  "directory": "/home/hoge/work/libclanglua/external/clang/r390/build/tools/llvm-config",
  "command": "/usr/bin/c++   -DCMAKE_CFG_INTDIR=\\\".\\\" -DGTEST_HAS_RTTI=0 -D_GNU_SOURCE -D__STDC_CONSTANT_MACROS -D__STDC_FORMAT_MACROS -D__STDC_LIMIT_MACROS -I/home/hoge/work/libclanglua/external/clang/r390/build/tools/llvm-config -I/home/hoge/work/libclanglua/external/clang/r390/llvm/tools/llvm-config -I/home/hoge/work/libclanglua/external/clang/r390/build/include -I/home/hoge/work/libclanglua/external/clang/r390/llvm/include   -fPIC -fvisibility-inlines-hidden -Wall -W -Wno-unused-parameter -Wwrite-strings -Wcast-qual -Wno-missing-field-initializers -pedantic -Wno-long-long -Wno-maybe-uninitialized -Wdelete-non-virtual-dtor -Wno-comment -Werror=date-time -std=c++11 -ffunction-sections -fdata-sections -O3 -DNDEBUG    -fno-exceptions -fno-rtti -o CMakeFiles/llvm-config.dir/llvm-config.cpp.o -c /home/hoge/work/libclanglua/external/clang/r390/llvm/tools/llvm-config/llvm-config.cpp",
  "file": "/home/hoge/work/libclanglua/external/clang/r390/llvm/tools/llvm-config/llvm-config.cpp"
}
]]
)

printJson( "", obj )



obj = Json:convertFrom( io.open( arg[1], "r" ):read( '*a' ) )

printJson( "", obj )
