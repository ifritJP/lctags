-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )
local Query = require( 'lctags.Query' )
local DBCtrl = require( 'lctags.DBCtrl' )
local DBAccess = require( 'lctags.DBAccess' )
local OutputCtrl = require( 'lctags.OutputCtrl' )
local Make = require( 'lctags.Make' )
local Complete = require( 'lctags.Complete' )
local Option = require( 'lctags.Option' )
local Json = require( 'lctags.Json' )
local Server = require( 'lctags.Server' )
local StatusServer = require( 'lctags.StatusServer' )
local DynamicCall = require( 'lctags.DynamicCall' )
local Helper = require( 'lctags.Helper' )
local StackCalc = require( 'lctags.StackCalc' )
local Split = require( 'lctags.Split' )

local startTime = Helper.getTime( true )

local function finish( code )
   log( 2, "finish", Helper.getTime( true ) - startTime )
   os.exit( code )
end


if not arg[1] then
   Option:printUsage( "" )
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

if lctagOptMap.mode == "clang-ver" then
   print( require( 'libclanglua.if' ).getClangVersion() )
   finish( 0 )
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
      elseif lctagOptMap.compatibleGlobal then
	 ;
      else
	 Option:printUsage( "not found lctags.sqlite3" )
      end
   end
end

local lockObj = Helper.createLock( DBAccess:getLockName( lctagOptMap.dbPath ) )
if not lockObj then
   Helper.deleteLock()
else
   lockObj = nil
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
   finish( 0 )
end

if lctagOptMap.mode == "statusServer" then
   if srcList[ 1 ] == "stop" then
      StatusServer:connect( lctagOptMap.dbPath )
      StatusServer:requestEnd()
   elseif srcList[ 1 ] == "start" then
      StatusServer:new( lctagOptMap.dbPath )
   elseif srcList[ 1 ] == "wait" then
      StatusServer:connect( lctagOptMap.dbPath, true )
   else
      Option:printUsage( "stop or start" )
   end
   finish( 0 )
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
      local runCount = 0
      for index, status in ipairs( statusList ) do
	 TermCtrl:gotoAt( 1, index )
	 TermCtrl:clrLine()
	 local waitFlag = string.find( status.info.state, "db is busy", 1, true )
	 if not waitFlag then
	    runCount = runCount + 1
	 end
	 print( status.info.time - status.basetime,
		waitFlag and "x" or "o",
		status.name, status.info.state )
      end
      TermCtrl:gotoAt( 1, #statusList + 1 )
      TermCtrl:clrLine()
      print( "run:", runCount )
      TermCtrl:gotoAt( 1, #statusList + 2 )
      TermCtrl:clrLine()
      Helper.msleep( 500 )
   end
   finish( 0 )
end


if lctagOptMap.mode == "init" then
   DBCtrl:init(
      lctagOptMap.dbPath, os.getenv( "PWD" ), projDir,
      lctagOptMap.individualTypeFlag, lctagOptMap.individualStructFlag,
      lctagOptMap.individualMacroFlag )
   finish( 0 )
end


if lctagOptMap.mode == "shrink" then
   DBCtrl:shrinkDB( lctagOptMap.dbPath, false )
   finish( 0 )
end

if lctagOptMap.mode == "shrinkFull" then
   DBCtrl:shrinkDB( lctagOptMap.dbPath, true )
   finish( 0 )
end

if lctagOptMap.mode == "forceUpdate" then
   DBCtrl:forceUpdate( lctagOptMap.dbPath )
   finish( 0 )
end

if lctagOptMap.mode == "chg-proj" or lctagOptMap.mode == "set-projDir" then

   chgDirMap = {}
   for index, chgDir in ipairs( srcList ) do
      local splitIndex = string.find( chgDir, "@", 1, true )
      if not splitIndex then
	 log( 1, "pattern error. need delimiter with @", chgDir )
	 finish( 1 )
      end
      local srcDir = string.sub( chgDir, 1, splitIndex - 1 )
      local dstDir = string.sub( chgDir, splitIndex + 1 )
      chgDirMap[ string.gsub( srcDir, "/$", "" ) ] = string.gsub( dstDir, "/$", "" )
   end
   
   DBCtrl:changeProjDir(
      lctagOptMap.dbPath, os.getenv( "PWD" ), projDir,
      lctagOptMap.mode == "set-projDir", chgDirMap )
   finish( 0 )
end

if lctagOptMap.mode == "query" then
   local db = lctagOptMap.dbPath and DBCtrl:open( lctagOptMap.dbPath,
						  true, os.getenv( "PWD" ) )

   if not db then
      if not lctagOptMap.useGlobalFlag then
	 log( 1, "db open error" )
	 finish( 1 )
      end
      local commad = string.format( "global %s %s", lctagOptMap.query, srcList[1] or "" )
      local success, endType, code = os.execute( commad )
      if not success then
	 finish( code )
      end
      finish( 0 )
   end

   if #srcList > 0 then
      for index, pattern in ipairs( srcList ) do
	 Query:exec( db, lctagOptMap.query, pattern )
      end
   else
      Query:exec( db, lctagOptMap.query, nil )
   end

   db:close()
   finish( 0 )
end

if lctagOptMap.mode == "list" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, os.getenv( "PWD" ) )
   
   if lctagOptMap.query == "inc" or lctagOptMap.query == "incSrc" then
      Query:outputIncRelation(
	 db, srcList[ 1 ], lctagOptMap.query == "inc",
	 lctagOptMap.depth, OutputCtrl.txt, io.stdout )
   end

   db:close()
   finish( 0 )
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
   finish( 0 )
end


if not lctagOptMap.dbPath then
   Option:printUsage( "db is not found." )
end

if lctagOptMap.mode == "kill" then
   DBCtrl:setKill( lctagOptMap.dbPath, os.getenv( "PWD" ), true )
   finish( 0 )
end

if lctagOptMap.mode == "cancel-kill" then
   DBCtrl:setKill( lctagOptMap.dbPath, os.getenv( "PWD" ), false )
   finish( 0 )
end

if lctagOptMap.mode == "update" then
   local src = srcList[1]
   if not src then
      Option:printUsage( "" )
   end

   Make:updateFor( lctagOptMap.dbPath, lctagOptMap.target, lctagOptMap.jobs, src )
   finish( 0 )
end

if lctagOptMap.mode == "chkFiles" then
   DBCtrl:checkRemovedFiles( lctagOptMap.dbPath )
   finish( 0 )
end

if lctagOptMap.mode == "rm" then
   DBCtrl:remove( lctagOptMap.dbPath, "file", srcList[ 1 ] )
   finish( 0 )
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
   finish( 0 )
end

if lctagOptMap.mode == "dcall" then
   DynamicCall:dumpInfo( lctagOptMap.dbPath )
   finish( 0 )
end

local analyzer = Analyzer:new(
   lctagOptMap.dbPath, lctagOptMap.recordDigestSrcFlag, not lctagOptMap.quiet )

local db = analyzer:openDBForReadOnly( os.getenv( "PWD" ) )

local modOptList = {}
for index, opt in ipairs( optList ) do
   if string.find( opt, "-I", 1, true ) then
      local path = opt:sub( 3 )
      if not string.find( path, "|", 1, true ) then
	 path = db:convRelativePath( path, os.getenv( "PWD" ) )
      end
      opt = "-I" .. path
   end
   table.insert( modOptList, opt )
end
optList = modOptList

db:close()

if lctagOptMap.mode == "depIncs" then
   analyzer:dumpIncludeList( srcList[ 1 ], optList, nil )
   finish( 0 )
end

if lctagOptMap.mode == "build" then
   local src = srcList[1]
   if not src then
      log( 1, "src is nil" )
      finish( 1 )
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
	       finish( 0 )
	    end
	 elseif info[ 1 ] == "lua" then
	    if string.find( fullpath, info[ 2 ] ) then
	       log( 1, "ignore:", fullpath )
	       finish( 0 )
	    end
	 end
      end
   end

   if lctagOptMap.onlyReg then
      analyzer:onlyRegister( src, optList, lctagOptMap.target )
   else
      analyzer:analyzeSource( src, optList, lctagOptMap.target )
   end
   finish( 0 )
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

   log:setStatusServer( nil, nil )
   StatusServer:requestEndStatus( statusName )
   finish( 0 )
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
   finish( 0 )
end

if lctagOptMap.mode == "graph-at" then
   analyzer:graphAt(
      lctagOptMap.graph, srcList[ 1 ], tonumber( srcList[ 2 ] ),
      tonumber( srcList[ 3 ] ), lctagOptMap.target,
      lctagOptMap.depth, lctagOptMap.browse,
      lctagOptMap.outputFile, lctagOptMap.imageFormat )
   finish( 0 )
end

if lctagOptMap.mode == "split-at" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Split:at( analyzer, srcList[ 1 ],
	     tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
	     lctagOptMap.ignoreSymMap, lctagOptMap.subRetTypeInfo,
	     lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "comp-at" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Complete:at( analyzer, srcList[ 1 ],
		tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "inq-at" or lctagOptMap.mode == "expand" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Complete:inqAt( analyzer, srcList[ 1 ],
		   tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		   lctagOptMap.target, fileContents, lctagOptMap.mode )
   finish( 0 )
end

if lctagOptMap.mode == "diag" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Complete:analyzeDiagnostic(
      analyzer, srcList[ 1 ], lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "stack" then
   StackCalc:analyze( srcList[ 1 ], lctagOptMap.target, analyzer )
   finish( 0 )
end

if lctagOptMap.mode == "testOpe" then
   require( 'lctags.testOperator' ):at( analyzer, srcList[ 1 ], lctagOptMap.target )
   finish( 0 )
end
