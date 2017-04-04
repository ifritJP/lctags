-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )
local Query = require( 'lctags.Query' )
local DBCtrl = require( 'lctags.DBCtrl' )
local OutputCtrl = require( 'lctags.OutputCtrl' )
local Make = require( 'lctags.Make' )
local Complete = require( 'lctags.Complete' )
local Option = require( 'lctags.Option' )
local Json = require( 'lctags.Json' )
local Server = require( 'lctags.Server' )
local StatusServer = require( 'lctags.StatusServer' )
local DynamicCall = require( 'lctags.DynamicCall' )
local Helper = require( 'lctags.Helper' )

if not arg[1] then
   Option:printUsage( "" )
end

local lockObj = Helper.createLock()
if not lockObj then
   Helper.deleteLock()
else
   lockObj = nil
end

local srcList, optList, lctagOptMap = Option:analyzeOption( arg )
if lctagOptMap.conf and lctagOptMap.conf.getDefaultOptionList then
   local list = lctagOptMap.conf:getDefaultOptionList( lctagOptMap.cc )
   for index, opt in ipairs( list ) do
      table.insert( optList, opt )
   end
end

local projDir = os.getenv( "PWD" )
if lctagOptMap.projDir then
   projDir = lctagOptMap.projDir
end

if not lctagOptMap.mode then
   Option:printUsage( "mode is none" )
end

if not lctagOptMap.dbPath then
   local dir = os.getenv( "PWD" )
   repeat
      local dbPath = dir .. "/" .. "lctags.sqlite3"
      local dbFile = io.open( dbPath, "r" )
      if dbFile then
	 dbFile:close()
	 lctagOptMap.dbPath = DBCtrl:convFullpath( dbPath, os.getenv( "PWD" ) )
	 break
      end
      dir = string.gsub( dir, "/[^/]*$", "" )
   until dir == ""
   if not lctagOptMap.dbPath then
      if lctagOptMap.mode == "init" then
         lctagOptMap.dbPath = os.getenv( "PWD" ) .. "/" .. "lctags.sqlite3"
      else
	 Option:printUsage( "not found lctags.sqlite3" )
      end
   end
end

if lctagOptMap.mode == "server" then
   if srcList[ 1 ] == "stop" then
      Server:connect( lctagOptMap.dbPath )
      Server:requestEnd()
   elseif srcList[ 1 ] == "start" then
      Server:new( lctagOptMap.dbPath,
		  DBCtrl:open( lctagOptMap.dbPath, false, os.getenv( "PWD" ) ) )
   else
      Option:printUsage( "stop or start" )
   end
   os.exit( 0 )
end

if lctagOptMap.mode == "statusServer" then
   if srcList[ 1 ] == "stop" then
      StatusServer:connect( lctagOptMap.dbPath )
      StatusServer:requestEnd()
   elseif srcList[ 1 ] == "start" then
      StatusServer:new( lctagOptMap.dbPath )
   elseif srcList[ 1 ] == "wait" then
      StatusServer:connect( lctagOptMap.dbPath )
   else
      Option:printUsage( "stop or start" )
   end
   os.exit( 0 )
end

if lctagOptMap.mode == "status" then
   StatusServer:connect( lctagOptMap.dbPath )


   local TermCtrl = require( 'lctags.TermCtrl' )
   
   while true do
      local statusList = StatusServer:requestGetStatus()
      if not statusList or #statusList == 0 then
	 break
      end
      TermCtrl:clr()
      for index, status in ipairs( statusList ) do
	 TermCtrl:gotoAt( 1, index )
	 TermCtrl:clrLine()
	 print( status.name, status.state )
      end
      Helper.msleep( 500 )
   end
   os.exit( 0 )
end


if lctagOptMap.mode == "init" then
   DBCtrl:init(
      lctagOptMap.dbPath, os.getenv( "PWD" ), projDir,
      lctagOptMap.individualTypeFlag, lctagOptMap.individualStructFlag,
      lctagOptMap.individualMacroFlag )
   os.exit( 0 )
end


if lctagOptMap.mode == "shrink" then
   DBCtrl:shrinkDB( lctagOptMap.dbPath, false )
   os.exit( 0 )
end

if lctagOptMap.mode == "shrinkFull" then
   DBCtrl:shrinkDB( lctagOptMap.dbPath, true )
   os.exit( 0 )
end

if lctagOptMap.mode == "forceUpdate" then
   DBCtrl:forceUpdate( lctagOptMap.dbPath )
   os.exit( 0 )
end

if lctagOptMap.mode == "chg-proj" then
   DBCtrl:changeProjDir(
      lctagOptMap.dbPath, os.getenv( "PWD" ), projDir )
   os.exit( 0 )
end

if lctagOptMap.mode == "query" then
   local db = lctagOptMap.dbPath and DBCtrl:open( lctagOptMap.dbPath,
						  true, os.getenv( "PWD" ) )

   if not db then
      if not lctagOptMap.useGlobalFlag then
	 log( 1, "db open error" )
	 os.exit( 1 )
      end
      local commad = string.format( "global %s %s", lctagOptMap.query, srcList[1] or "" )
      local success, endType, code = os.execute( commad )
      if not success then
	 os.exit( code )
      end
      os.exit( 0 )
   end
   
   Query:exec( db, lctagOptMap.query, srcList[ 1 ] )

   db:close()
   os.exit( 0 )
end

if lctagOptMap.mode == "list" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, os.getenv( "PWD" ) )
   
   if lctagOptMap.query == "inc" or lctagOptMap.query == "incSrc" then
      Query:outputIncRelation(
	 db, srcList[ 1 ], lctagOptMap.query == "inc",
	 lctagOptMap.depth, OutputCtrl.txt, io.stdout )
   end

   db:close()
   os.exit( 0 )
end

if lctagOptMap.mode == "graph" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, os.getenv( "PWD" ) )
   
   if lctagOptMap.graph == "inc" or lctagOptMap.graph == "incSrc" then
      Query:outputIncRelation(
	 db, srcList[ 1 ], lctagOptMap.graph == "inc",
	 lctagOptMap.depth, OutputCtrl.dot,
	 lctagOptMap.browse, lctagOptMap.outputFile, lctagOptMap.imageFormat )
   elseif lctagOptMap.graph == "caller" or lctagOptMap.graph == "callee" then
      Query:outputCallRelation(
	 db, srcList[ 1 ], lctagOptMap.graph == "caller",
	 lctagOptMap.depth, OutputCtrl.dot,
	 lctagOptMap.browse, lctagOptMap.outputFile, lctagOptMap.imageFormat )
   elseif lctagOptMap.graph == "symbol" then
      Query:outputSymbolRefRelation(
	 db, srcList[ 1 ], lctagOptMap.depth, OutputCtrl.dot,
	 lctagOptMap.browse, lctagOptMap.outputFile, lctagOptMap.imageFormat )
   else
      Option:printUsage( "unknown graph" )
   end

   db:close()
   os.exit( 0 )
end


if not lctagOptMap.dbPath then
   Option:printUsage( "db is not found." )
end


if lctagOptMap.mode == "update" then
   local src = srcList[1]
   if not src then
      Option:printUsage( "" )
   end

   Make:updateFor( lctagOptMap.dbPath, lctagOptMap.target, lctagOptMap.jobs, src )
   os.exit( 0 )
end

if lctagOptMap.mode == "chkFiles" then
   DBCtrl:checkRemovedFiles( lctagOptMap.dbPath )
   os.exit( 0 )
end

if lctagOptMap.mode == "rm" then
   DBCtrl:remove( lctagOptMap.dbPath, lctagOptMap.rm, srcList[ 1 ] )
   os.exit( 0 )
end

if lctagOptMap.mode == "register" then
   if lctagOptMap.registerFromInfo then
      DBCtrl:registerFromInfo(
	 lctagOptMap.dbPath, lctagOptMap.target, srcList[ 1 ] )
   else
      DBCtrl:registerFromJson(
	 lctagOptMap.dbPath, lctagOptMap.target,
	 Json:fromStream( io.open( srcList[ 1 ], "r" ) ) )
   end
   os.exit( 0 )
end

if lctagOptMap.mode == "dcall" then
   DynamicCall:dumpInfo( lctagOptMap.dbPath )
   os.exit( 0 )
end


local analyzer = Analyzer:new(
   lctagOptMap.dbPath, lctagOptMap.recordDigestSrcFlag, not lctagOptMap.quiet )

if lctagOptMap.mode == "depIncs" then
   analyzer:dumpIncludeList( srcList[ 1 ], optList, nil )
   os.exit( 0 )
end

if lctagOptMap.mode == "build" then
   local src = srcList[1]
   if not src then
      log( 1, "src is nil" )
      os.exit( 1 )
   end

   local option = ""
   for index, opt in ipairs( optList ) do
      option = option .. opt .. " "
   end
   log( 2, "src:", src, "target:", lctagOptMap.target )
   log( 3, "opt:", option )
   
   
   if lctagOptMap.conf then
      for index, info in ipairs( lctagOptMap.conf:getIgnorePattern() ) do
	 local fullpath = DBCtrl:convFullpath( src, os.getenv( "PWD" ) )
	 if info[ 1 ] == "simple" then
	    if string.find( fullpath, info[ 2 ], 1, true ) then
	       log( 1, "ignore:", fullpath )
	       os.exit( 0 )
	    end
	 elseif info[ 1 ] == "lua" then
	    if string.find( fullpath, info[ 2 ] ) then
	       log( 1, "ignore:", fullpath )
	       os.exit( 0 )
	    end
	 end
      end
   end

   if lctagOptMap.onlyReg then
      analyzer:onlyRegister( src, optList, lctagOptMap.target )
   else
      analyzer:analyzeSource( src, optList, lctagOptMap.target )
   end
   os.exit( 0 )
end

if lctagOptMap.mode == "updateForMake" then
   local src = srcList[1]
   if not src then
      Option:printUsage( "" )
   end

   log( 3, "src:", src, "target:", lctagOptMap.target )
   StatusServer:connect( lctagOptMap.dbPath )
   local statusName = string.gsub( src, ".*/", "" )
   log:setStatusServer( StatusServer, statusName )
   
   analyzer:update( src, lctagOptMap.target )

   StatusServer:requestEndStatus( statusName )
   os.exit( 0 )
end

if lctagOptMap.mode == "ref-at" or lctagOptMap.mode == "def-at" or
   lctagOptMap.mode == "call-at" or lctagOptMap.mode == "ns-at"
then
   local filePath = srcList[ 1 ]
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   analyzer:queryAt( lctagOptMap.mode, filePath, tonumber( srcList[ 2 ] ),
		     tonumber( srcList[ 3 ] ), lctagOptMap.abs,
		     lctagOptMap.target, fileContents )
   os.exit( 0 )
end

if lctagOptMap.mode == "graph-at" then
   analyzer:graphAt(
      lctagOptMap.graph, srcList[ 1 ], tonumber( srcList[ 2 ] ),
      tonumber( srcList[ 3 ] ), lctagOptMap.target,
      lctagOptMap.depth, lctagOptMap.browse,
      lctagOptMap.outputFile, lctagOptMap.imageFormat )
   os.exit( 0 )
end

if lctagOptMap.mode == "comp-at" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Complete:at( analyzer, srcList[ 1 ],
		tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		lctagOptMap.target, fileContents )
   os.exit( 0 )
end

if lctagOptMap.mode == "inq-at" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Complete:inqAt( analyzer, srcList[ 1 ],
		   tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		   lctagOptMap.target, fileContents )
   os.exit( 0 )
end
