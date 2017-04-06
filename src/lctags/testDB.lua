local log = require( 'lctags.LogCtrl' )
local DBCtrl = require( 'lctags.DBCtrl' )
local Helper = require( 'lctags.Helper' )


local path = "test.sqlite3"
local currentDir = os.getenv( "PWD" )

local dataCount = 100000

if not arg[ 1 ] then
   DBCtrl:init( path, currentDir, 0, 0, 0 )
   log( 1, "init", os.clock() )

   Helper.msleep( 3000 )

   local db = DBCtrl:open( path, false, currentDir )

   log( 1, "open writeDb", os.clock() )

   for index = 1, dataCount do
      db:insert(
	 "namespace",
	 string.format( "NULL, %d, %d, '%s', '%s', '%s', 1",
			1, index, "",
			string.format( "name_eiaohap_%d", index ),
			string.format( "name_eiaohap_%d", index ) ) )
   end

   log( 1, "insert", os.clock() )

   db:close()

   db = DBCtrl:open( path, false, currentDir )

   for index = 1, dataCount do
      local name = string.format( "name_fiaojo_%d", index )
      db:insert(
	 "namespace",
	 string.format( "NULL, %d, %d, '%s', '%s', '%s', 1", 1, index, "", name, name ) )
      if not db:getRow( "namespace",
			string.format( "name = '%s'", name ) ) then
	 log( 1, "not found id" )
	 os.exit( 1 )
      end
   end

   log( 1, "insert and select", os.clock() )

   db:close()
end


db = DBCtrl:open( path, true, currentDir )

log( 1, "open read", os.clock() )

while true do
   for index = 1, dataCount do
      if not db:getRow( "namespace",
			string.format( "name = '%s'", 
				       string.format( "name_eiaohap_%d", index ) ) ) then
	 if not arg[ 1 ] then
	    log( 1, "not found name" )
	    os.exit( 1 )
	 end
      end
   end

   log( 1, "select name", os.clock() )

   for index = 1, dataCount do
      if not db:getRow( "namespace",
			string.format( "id = %d", index ) ) then
	 if not arg[ 1 ] then
	    log( 1, "not found id" )
	    os.exit( 1 )
	 end
      end
   end

   log( 1, "select id", os.clock() )
end

db:close()
