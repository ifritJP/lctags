local Helper = require( 'lctags.Helper' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )

local Make = {}


function Make:updateFor( dbPath, target, jobs, src )
   local db = DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   local fileInfo = db:getFileInfo( nil, src )
   
   local list = {}
   if not fileInfo then
      local condition = string.format(
	 "path like '%s%%' AND incFlag = 0", db:convPath( src ) )
      db:mapFile(
	 condition,
	 function( item )
	    table.insert( list, item )
	    return true
	 end
      )
   else
      fileInfo = DBCtrl:getSrcForIncOne( fileInfo )
      table.insert( list, fileInfo )
   end
   if #list == 0 then
      log( 1, 'not match' )
      os.exit( 1 )
   end

   local tmpName = Helper.getTempFilename( "lcmake" )
   local fileHandle = io.open( tmpName, "w" )
   if not fileHandle then
      log( 1, 'failed to open', tmpName )
      os.exit( 1 )
   end

   for index, fileInfo in ipairs( list ) do
      fileHandle:write(
	 string.format( "SRCS += %d/%d%s\n", index, #list,
			db:getSystemPath( fileInfo.path ) ) )
   end
   db:close()

   fileHandle:write(
      string.format( 
	 [[
SRCS := $(addsuffix .lc, $(SRCS))

all: $(SRCS)

%%.lc:
	@echo $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@[\1]  /@'))
	@%s %s updateForMake %s $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@/@')) --lctags-log 2
]],
	 arg[-1], arg[0],
	 target and ("--lctags-target " .. target ) or "" ) )

   fileHandle:close()

   log( 1, "-j:", jobs )
   os.execute( "make -f " .. tmpName .. " " ..
		  (jobs and ("-j" .. tostring( jobs )) or "" ))
   os.remove( tmpName )
  
end

return Make
