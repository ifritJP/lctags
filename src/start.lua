local command = string.format(
   'python -c "import os; print( os.path.dirname( os.path.abspath( \'%s\' )));"', arg[0] )
local obj = io.popen( command )
local dir = obj:read( '*l' )

package.path = string.format( "%s/?.lua;%s", dir, package.path )
package.cpath = string.format( "%s/?.so;%s", dir, package.cpath )

local func, mess = loadfile( dir .. "/lctags/lctags.lua" )
if mess then
   print( mess )
end
if func then
   func()
end
