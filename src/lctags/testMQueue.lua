local Helper = require( 'lctags.Helper' )
local Json = require( 'lctags.Json' )

local lock = Helper.createLock( "test" )
local mqueue = Helper.createMQueue( "test", true, lock )

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

Helper.deleteLock( "test" )
