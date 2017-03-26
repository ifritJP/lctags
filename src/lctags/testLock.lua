local Helper = require( 'lctags.Helper' )

local lock = Helper.createLock( "act" )

print( lock )

print( lock:isLocking() )

lock:begin()

print( lock:isLocking() )

Helper.msleep( 1000 * 5 )

lock:fin()
