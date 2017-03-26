local Helper = require( 'lctags.Helper' )


local mqueue = Helper.createMQueue( "test" )

mqueue:put( "123", true )

