local Server = require( 'lctags.Server' )
local DBCtrl = require( 'lctags.DBCtrl' )

local currentDir = Util:getcwd()
DBCtrl:init( 'lctags.sqlite3', currentDir, '.', false, false, false )
local db = DBCtrl:open( 'lctags.sqlite3', false, currentDir )

print( 0, os.clock(), os.date() )

-- for index = 1, 10000 do
--    db.db.db:exec(
--       string.format(
-- 	 "INSERT INTO filePath VALUES ( NULL, '%d', 0, 0, '17c3aa6506bb39fb5d1ba63a3fc33c01', '|', 0 );", index ) )
-- end

print( 1, os.clock(), os.date() )

Server:new( "test", db )
