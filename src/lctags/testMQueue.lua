local Helper = require( 'lctags.Helper' )
local Json = require( 'lctags.Json' )

local mqueue = Helper.createMQueue( "test" )

print( os.clock(), os.date() )
for index = 1, 10000 do
   mqueue:put( "123", true )
   mqueue:get()
end
print( os.clock(), os.date() )
for index = 1, 10000 do
   mqueue:put( "123", true )
   Json:convertFrom( mqueue:get() )
end
print( os.clock(), os.date() )
