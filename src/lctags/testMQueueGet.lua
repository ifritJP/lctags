local Helper = require( 'lctags.Helper' )

local mqueue = Helper.createMQueue( "test" )

print( mqueue:get() )
