-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )
local Query = require( 'lctags.Query' )
local DBCtrl = require( 'lctags.DBCtrl' )
local DBAccess = require( 'lctags.DBAccess' )
local OutputCtrl = require( 'lctags.OutputCtrl' )
local Make = require( 'lctags.Make' )
local Completion = require( 'lctags.Completion' )
local Option = require( 'lctags.Option' )
local Json = require( 'lctags.Json' )
local Server = require( 'lctags.Server' )
local StatusServer = require( 'lctags.StatusServer' )
local DynamicCall = require( 'lctags.DynamicCall' )
local Helper = require( 'lctags.Helper' )
local StackCalc = require( 'lctags.StackCalc' )
local Split = require( 'lctags.Split' )
local Scan = require( 'lctags.Scan' )
local Util = require( 'lctags.Util' )
local QueryParam = require( 'lctags.QueryParam' )
local config = require( 'lctags.config' )

local startTime = Helper.getTime( true )

local function finish( code )
   log( 2, "finish", Helper.getTime( true ) - startTime )
   os.exit( code )
end


if not arg[1] then
   Option:printUsage( "" )
end

local srcList, optList, lctagOptMap = Option:analyzeOption( arg )
local list = lctagOptMap.conf:getDefaultOptionList( lctagOptMap.cc )
for index, opt in ipairs( list ) do
   table.insert( optList, opt )
end

local projDir = Util:getcwd()
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
   local dir = Util:getcwd()
   repeat
      local dbPath = dir .. "/" .. "lctags.sqlite3"
      local dbFile = io.open( dbPath, "r" )
      if dbFile then
	 dbFile:close()
	 lctagOptMap.dbPath = DBCtrl:convFullpath( dbPath, Util:getcwd() )
	 break
      end
      dir = string.gsub( dir, "/[^/]*$", "" )
   until dir == ""
   if not lctagOptMap.dbPath then
      if lctagOptMap.mode == "init" then
         lctagOptMap.dbPath = Util:getcwd() .. "/" .. "lctags.sqlite3"
      elseif lctagOptMap.compatibleGlobal then
	 ;
      else
	 Option:printUsage( "not found lctags.sqlite3" )
      end
   end
end

if not lctagOptMap.confPath then
   lctagOptMap.confPath = Option:getConfPathFromDbPath( lctagOptMap.dbPath )
end

package.path = package.path .. ";" ..
   string.gsub( lctagOptMap.confPath, "^(.*)/[^/]+$", "%1/?.lua" )

-- local lockObj = Helper.createLock( DBAccess:getLockName( lctagOptMap.dbPath ) )
-- if not lockObj then
--    Helper.deleteLock()
-- else
--    lockObj = nil
-- end


if lctagOptMap.mode == "server" then
   if srcList[ 1 ] == "stop" then
      Server:connect( lctagOptMap.dbPath )
      Server:requestEnd()
   elseif srcList[ 1 ] == "start" then
      Server:new( lctagOptMap.dbPath,
		  DBCtrl:open( lctagOptMap.dbPath, false, Util:getcwd() ) )
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
      local status = StatusServer:requestGetStatus()
      if not status then
	 break
      end
      local statusList = status.list
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
      print( string.format( "run: %d, write: %d, read: %d",
			    runCount, status.writeModeCount, status.readModeCount ) )
      TermCtrl:gotoAt( 1, #statusList + 2 )
      TermCtrl:clrLine()
      Helper.msleep( 500 )
   end
   finish( 0 )
end


if lctagOptMap.mode == "copyConf" then
   if not config:hasUserConf() then
      local confFile = io.open( lctagOptMap.confPath, "w" )
      local baseFile = io.open( Option:getSameDirFile( arg[0], "_lctags.conf" ) )
      confFile:write( baseFile:read( '*a' ) )
      confFile:close()
      baseFile:close()
   end
   print( "please edit config -- 'lctags.conf'." )
   finish( 0 )
end

if lctagOptMap.mode == "init" then
   DBCtrl:init(
      lctagOptMap.dbPath, Util:getcwd(), projDir,
      lctagOptMap.individualTypeFlag, lctagOptMap.individualStructFlag,
      lctagOptMap.individualMacroFlag )

   local analyzer = Analyzer:new(
      lctagOptMap.dbPath, Option:isValidRecordDigestSrc(), not lctagOptMap.quiet )

   local code = [[
#include <stdio.h>
main(){
  return 0;
}
]]

   local path = lctagOptMap.conf:getClangIncPath()
   if path then
      clangIncOp = string.format( "-I%s", path )
   end
   local unit, compileOp, stdMode =
      analyzer:createUnitDirect( analyzer, "test.c", { clangIncOp }, code )
   local diagList = analyzer:getDiagList( unit )

   for index, diag in ipairs( diagList ) do
      if diag.message:find( 'stddef.h' ) then
	 if not config:hasUserConf() then
	    local confFile = io.open( lctagOptMap.confPath, "w" )
	    local baseFile = io.open( Option:getSameDirFile( arg[0], "_lctags.conf" ) )
	    confFile:write( baseFile:read( '*a' ) )
	    confFile:close()
	    baseFile:close()
	 end

	 print( string.format(
		   "please set clang inc-path at getClangIncPath() in %s, and retry init.",
		   lctagOptMap.confPath ) )
	 --os.remove( lctagOptMap.dbPath )
	 finish( 1 )
      end
   end

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
      lctagOptMap.dbPath, Util:getcwd(), projDir,
      lctagOptMap.mode == "set-projDir", chgDirMap )
   finish( 0 )
end

if lctagOptMap.mode == "query" then
   local db = lctagOptMap.dbPath and DBCtrl:open( lctagOptMap.dbPath,
						  true, Util:getcwd() )

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
	 Query:execWithDb( db, lctagOptMap.query, pattern, lctagOptMap.cursorKind,
			   lctagOptMap.candidateLimit, Option:getOutputForm() )
      end
   else
      Query:execWithDb( db, lctagOptMap.query, nil, lctagOptMap.cursorKind,
			lctagOptMap.candidateLimit, Option:getOutputForm() )
   end

   db:close()
   finish( 0 )
end

if lctagOptMap.mode == "list" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, Util:getcwd() )
   
   if lctagOptMap.query == "inc" or lctagOptMap.query == "incSrc" then
      Query:outputIncRelation(
	 db, srcList[ 1 ], lctagOptMap.query == "inc",
	 lctagOptMap.depth, OutputCtrl.txt, io.stdout )
   elseif lctagOptMap.query == "incSrcHeader" then
      Query:outputIncSrcHeader( db, srcList[ 1 ], io.stdout )
   end

   db:close()
   finish( 0 )
end

if lctagOptMap.mode == "graph" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, Util:getcwd() )
   
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
   DBCtrl:setKill( lctagOptMap.dbPath, Util:getcwd(), true )
   finish( 0 )
end

if lctagOptMap.mode == "cancel-kill" then
   DBCtrl:setKill( lctagOptMap.dbPath, Util:getcwd(), false )
   finish( 0 )
end

if lctagOptMap.mode == "check-kill" then
   if DBCtrl:checkKilling( lctagOptMap.dbPath ) then
      print( "is killing" )
      finish( 1 )
   end
   print( "is not killing" )
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

if lctagOptMap.mode == "lazyUpdate" then
   Make:lazyUpdateFor(
      lctagOptMap.dbPath, lctagOptMap.target, lctagOptMap.jobs, optList )
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

if lctagOptMap.mode == "addIncRef" then
   local db = DBCtrl:open( lctagOptMap.dbPath, false, Util:getcwd() )
   db:addIncludeDirect( srcList[ 1 ], srcList[ 2 ] )
   db:close()
   finish( 0 )
end

if lctagOptMap.mode == "inq" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, Util:getcwd() )
   local nsInfo
   local target = ""

   local queryParam = QueryParam:getQuery( lctagOptMap.query )
   if queryParam then
      local ret = queryParam:getQueryParam( srcList )
      nsInfo = ret[ 1 ]
      target = ret[ 2 ]
   elseif type( srcList[ 1 ] ) == "number" or string.find( srcList[ 1 ], "^%d" ) then
      nsInfo = db:getNamespace( srcList[ 1 ] )
   else
      nsInfo = db:getNamespace( nil, srcList[ 1 ] )
      if not nsInfo then
	 nsInfo = db:getSimpleName( nil, srcList[ 1 ] )
      end
   end
   Query:queryFor( db, nsInfo, lctagOptMap.query, target, absFlag,
		   lctagOptMap.candidateLimit, Option:getOutputForm() )
   db:close()
   finish( 0 )
end

if lctagOptMap.mode == "prepare" then
   local db = DBCtrl:open( lctagOptMap.dbPath, true, Util:getcwd() )
   db:prepare()
   db:close()
   finish( 0 )
end

local analyzer = Analyzer:new(
   lctagOptMap.dbPath, Option:isValidRecordDigestSrc(), not lctagOptMap.quiet )

local db = analyzer:openDBForReadOnly( Util:getcwd() )

--- これ以降は、 srcList[1] にはソースが指定されていることを前提にする

local targetFileInfo
local targetFullPath = srcList[ 1 ]
if srcList[ 1 ] then
   targetFileInfo = db:getFileInfo( nil, db:convFullpath( srcList[ 1 ] ) )
   if targetFileInfo then
      analyzer = analyzer:newAs(
	 Option:isValidRecordDigestSrc(), not lctagOptMap.quiet,
	 db:convFullpath( db:getSystemPath( targetFileInfo.currentDir ) ) )
      targetFullPath = db:convFullpath( db:getSystemPath( targetFileInfo.path ) )
   end
end


local modOptList = {}
for index, opt in ipairs( optList ) do
   if string.find( opt, "-I", 1, true ) then
      local path = opt:sub( 3 )
      if not string.find( path, "|", 1, true ) then
	 path = db:convRelativePath( path, Util:getcwd() )
      end
      opt = "-I" .. path
   end
   table.insert( modOptList, opt )
end
optList = modOptList

db:close()

if lctagOptMap.mode == "depIncs" then
   analyzer:dumpIncludeList( targetFullPath, optList, nil )
   finish( 0 )
end

if lctagOptMap.mode == "build" then
   local src = targetFullPath
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
   
   
   for index, info in ipairs( lctagOptMap.conf:getIgnorePattern() ) do
      local fullpath = DBCtrl:convFullpath( src, Util:getcwd() )
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

   if lctagOptMap.onlyReg then
      analyzer:onlyRegister( src, optList, lctagOptMap.target )
   else
      analyzer:analyzeSource( src, optList, lctagOptMap.target, nil, true )
   end
   finish( 0 )
end

if lctagOptMap.mode == "addInc" or lctagOptMap.mode == "addStdInc" then
   if lctagOptMap.mode == "addStdInc" then
      workSrcList = {
	 "/usr/include/assert.h",
	 "/usr/include/complex.h",
	 "/usr/include/ctype.h",
	 "/usr/include/errno.h",
	 "/usr/include/fenv.h",
	 "/usr/include/float.h",
	 "/usr/include/inttypes.h",
	 "/usr/include/iso646.h",
	 "/usr/include/limits.h",
	 "/usr/include/locale.h",
	 "/usr/include/math.h",
	 "/usr/include/setjmp.h",
	 "/usr/include/signal.h",
	 "/usr/include/stdalign.h",
	 "/usr/include/stdarg.h",
	 "/usr/include/stdatomic.h",
	 "/usr/include/stdbool.h",
	 "/usr/include/stddef.h",
	 "/usr/include/stdint.h",
	 "/usr/include/stdio.h",
	 "/usr/include/stdlib.h",
	 "/usr/include/stdnoreturn.h",
	 "/usr/include/string.h",
	 "/usr/include/tgmath.h",
	 "/usr/include/threads.h",
	 "/usr/include/time.h",
	 "/usr/include/uchar.h",
	 "/usr/include/wchar.h",
	 "/usr/include/wctype.h",
      }
      srcList = {}
      for index, src in ipairs( workSrcList ) do
	 local fileObj = io.open( src )
	 if fileObj then
	    table.insert( srcList, src )
	    fileObj:close()
	 end
      end
   end
   
   for index, src in ipairs( srcList ) do
      local newAnalyzer = analyzer:newAs( false, true, Util:getcwd() )
      newAnalyzer:analyzeSource( src, optList, lctagOptMap.target, nil, false )
   end
   finish( 0 )
end

if lctagOptMap.mode == "updateForMake" then
   local src = targetFullPath
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
   lctagOptMap.mode == "call-at" or lctagOptMap.mode == "ns-at" or
   lctagOptMap.mode == "callee-at"
then
   local filePath = targetFullPath
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   analyzer:queryAt( lctagOptMap.mode, filePath, tonumber( srcList[ 2 ] ),
		     tonumber( srcList[ 3 ] ), lctagOptMap.abs,
		     lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "call-for" then
   local db = analyzer:openDBForReadOnly()

   local nsInfo = db:getNamespace( srcList[1] )
   Query:queryFor( db, nsInfo, lctagOptMap.mode, lctagOptMap.abs,
		   lctagOptMap.candidateLimit, "json" )
   finish( 0 )
end


if lctagOptMap.mode == "ref-at-all" then
   local filePath = targetFullPath
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   analyzer:refAt( filePath, tonumber( srcList[ 2 ] ),
		   tonumber( srcList[ 3 ] ), lctagOptMap.abs,
		   lctagOptMap.target, fileContents )
   finish( 0 )
end


if lctagOptMap.mode == "graph-at" then
   analyzer:graphAt(
      lctagOptMap.graph, targetFullPath, tonumber( srcList[ 2 ] ),
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

   Split:at( analyzer, targetFullPath,
	     tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
	     lctagOptMap.splitParamInfoList, lctagOptMap.subRetTypeInfo,
	     lctagOptMap.directRet, lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "comp-at" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Completion:at( analyzer, targetFullPath,
		tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "inq-at" or lctagOptMap.mode == "expand" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Completion:inqAt( analyzer, targetFullPath,
		     tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		     lctagOptMap.target, fileContents, lctagOptMap.mode )
   finish( 0 )
end

if lctagOptMap.mode == "scan" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Scan:outputIncSrcHeader( lctagOptMap.scan, analyzer,
			    targetFullPath, lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "call-func" then
   local db = analyzer:openDBForReadOnly()

   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end
   
   Completion:callFunc(
      analyzer, db, targetFullPath, srcList[2], lctagOptMap.target, fileContents )

   db:close()
   finish( 0 )
end


if lctagOptMap.mode == "diag" then
   local fileContents
   if lctagOptMap.inputFromStdin then
      fileContents = io.stdin:read( "*a" )
   end

   Completion:analyzeDiagnostic(
      analyzer, targetFullPath, lctagOptMap.target, fileContents )
   finish( 0 )
end

if lctagOptMap.mode == "cursors" then
   analyzer:dumpCurosr( targetFullPath, lctagOptMap.target, optList )
   finish( 0 )
end

if lctagOptMap.mode == "grep-cursor" then
   analyzer:grepCurosr(
      targetFullPath, lctagOptMap.target, optList, srcList[ 2 ], srcList[ 3 ] )
   finish( 0 )
end

if lctagOptMap.mode == "expand-macro" then
   analyzer:expandMacro( targetFullPath, lctagOptMap.target, lctagOptMap.conf )
   finish( 0 )
end

if lctagOptMap.mode == "cursor-at" then
   analyzer:cursorAt( targetFullPath,
		      tonumber( srcList[ 2 ] ), tonumber( srcList[ 3 ] ),
		      lctagOptMap.target )
   finish( 0 )
end

if lctagOptMap.mode == "stack" then
   StackCalc:analyze( targetFullPath, lctagOptMap.target, analyzer )
   finish( 0 )
end

if lctagOptMap.mode == "testOpe" then
   require( 'lctags.testOperator' ):at( analyzer, targetFullPath, lctagOptMap.target )
   finish( 0 )
end

if lctagOptMap.mode == "testInc" then
   require( 'lctags.testInc' ):run( analyzer, targetFullPath, lctagOptMap.target )
   finish( 0 )
end



print( "not found mode", lctagOptMap.mode )
finish( 1 )
