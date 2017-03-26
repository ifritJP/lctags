local Server = require( 'lctags.Server' )
local DBCtrl = require( 'lctags.DBCtrl' )

Server:connect( "test" )

local result = Server:requestExec(
   "INSERT INTO filePath VALUES ( NULL, '|/test/hoge.cpp', 1490273968, 0, '17c3aa6506bb39fb5d1ba63a3fc33c01', '|', 0 );" )

local query = "SELECT * FROM filePath WHERE id < 10 LIMIT 10"
local item = Server:requestInq(
   query,
   function( item )
      for key, val in pairs( item ) do
	 print( key, val )
      end
      return true
   end
)

local db = DBCtrl:open( 'lctags.sqlite3', true, os.getenv( "PWD" ) )

local fileInfo = db:getFileInfo( 2 )
print( "fileInfo 2", fileInfo )

fileInfo = db:getFileInfo( 1 )
print( "fileInfo 1", fileInfo )



print( 0, os.clock(), os.date() )

for index = 1, 10000 do
   Server:requestExec(
      string.format(
	 "INSERT INTO filePath VALUES ( NULL, '%d', 0, 0, '17c3aa6506bb39fb5d1ba63a3fc33c01', '|', 0 );", index ) )
end

print( 1, os.clock(), os.date() )

for index = 1, 1 do
   local item = Server:requestInq(
      "SELECT * FROM etc WHERE keyName = 'projDir' LIMIT 1",
      function( item )
      end
   )
end

print( 1, os.clock(), os.date() )
Server:requestTest()
print( 1, os.clock(), os.date() )

Server:requestEnd()

