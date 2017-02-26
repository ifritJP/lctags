-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )
local Query = require( 'lctags.Query' )
local gcc = require( 'lctags.gcc' )
local DBCtrl = require( 'lctags.DBCtrl' )
local DBAccess = require( 'lctags.DBAccess' )

local function printUsage( message )
   if message then
      print( message )
   end
   local command = "lctags"
   print( string.format( [[
usage:
 - build DB
   %s init projDir [--lctags-db path] [--lctags-log lv] 
   %s build compiler [--lctags-log lv] [--lctags-db path] [--lctags-conf conf] [--lctags-target target] [--lctags-recSql file] comp-op [...] src
   %s shrink [--lctags-db path]
   %s chg-proj projDir [--lctags-db path]
   %s update [--lctags-db path] pattrn
 - query DB
   %s dump
   %s ref-at[a] [--lctags-db path] [--lctags-target target] file line column 
   %s def-at[a] [--lctags-db path] [--lctags-target target] file line column 
   %s call-at[a] [--lctags-db path] [--lctags-target target] file line column 
   %s -x[t|s|r][a] [--lctags-db path] [--lctags-log lv] [--use-global] symbol
   %s -xP[a] [--lctags-db path] [--lctags-log lv] [--use-global] file
   %s -c [--lctags-db path] [--lctags-log lv] [--use-global] symbol
 - graph
   %s graph <incSrc|inc|caller|callee|symbol> [-d depth] [-b|-o file] 

  option:
     init: initialize DB file. "projDir" is a root directory of your project.
     build: build DB for "src".
            "compiler" is "gcc" or "cc" or ....
            "comp-op" is compiler option. This include source file path.
     shrink: shrink DB.
     chg-proj: change project directory.
     dump: dump DB.
     --lctags-db: set DB file path.
     --lctags-log: set log level. default is 1. when lv > 1, it is datail mode.
     --lctags-conf: confing file.
     --lctags-target: set build target.
     -x: query DB.
        -xt: symbol declaration
        -xs: symbol declaration
        -xr: symbol reference
        -xP: file list
     -c: list symbol.
     def-at: symbol declaration at position
     ref-at: symbol reference at position
     call:at: function call at position
     --use-global: use GNU global when db is not found.
     graph: draw graph.
         inc: include relation.
         caller: caller graph.
         callee: callee graph.
         -d: depth.
         -b: browse graph.
         -o: output image file.

   common option:
     --lctags-quiet: discard clang diagnostic.
   ]],
	     command, command, command, command, command, command,
	     command, command, command, command, command, command,
	     command ) )
   os.exit( 1 )
end

local function loadConfig( path, exitOnErr )
   local fileHandle = io.open( path, "r" )
   if fileHandle then
      fileHandle:close()
      local chunk, err = loadfile( path )
      if chunk then
	 return chunk()
      end
   end
   if exitOnErr then
      print( "loadfile error", err )
      os.exit( 1 )
   end
   return nil
end


local function analyzeOption( argList )
   local srcList = {}
   local optList = {}
   local skipArgNum = 0
   local lctagOptMap = {}

   for index, arg in ipairs( argList ) do
      if skipArgNum > 0 then
	 skipArgNum = skipArgNum - 1
      elseif string.find( arg, "--lctags-conf", 1, true ) then
	 skipArgNum = 1
	 lctagOptMap.conf = loadConfig( argList[ index + 1 ], true )
      elseif string.find( arg, "--lctags-db", 1, true ) then
	 skipArgNum = 1
	 lctagOptMap.dbPath = argList[ index + 1 ]
	 confPath = string.gsub( lctagOptMap.dbPath, "(.*/)", "%1ctags.conf" )
	 lctagOptMap.conf = loadConfig( confPath, false )
      end
   end

   if not lctagOptMap.dbPath then
      local dir = os.getenv( "PWD" )
      repeat
	 local dbPath = dir .. "/" .. "lctags.sqlite3"
	 local dbFile = io.open( dbPath, "r" )
	 if dbFile then
	    dbFile:close()
	    lctagOptMap.dbPath = dbPath
	    break
	 end
	 dir = string.gsub( dir, "/[^/]*$", "" )
      until dir == ""
      if not lctagOptMap.dbPath then
	 if lctagOptMap.mode == "init" then
	 end
      end
   end
   
   skipArgNum = 0
   for index, arg in ipairs( argList ) do
      if index == 1 then
	 if arg == "build" then
	    lctagOptMap.mode = "build"
	    lctagOptMap.cc = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "init" then
	    lctagOptMap.mode = "init"
	    lctagOptMap.projDir = argList[ index + 1 ]
	    if not lctagOptMap.dbPath then
	       lctagOptMap.dbPath = os.getenv( "PWD" ) .. "/" .. "lctags.sqlite3"
	    end
	    
	    skipArgNum = 1
	 elseif arg == "shrink" then
	    lctagOptMap.mode = "shrink"
	 elseif arg == "shrinkFull" then
	    lctagOptMap.mode = "shrinkFull"
	 elseif arg == "forceUpdate" then
	    lctagOptMap.mode = "forceUpdate"
	 elseif arg == "chg-proj" then
	    lctagOptMap.mode = "chg-proj"
	    lctagOptMap.projDir = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "update" then
	    lctagOptMap.mode = "update"
	 elseif string.find( arg, "ref-at", 1, true ) then
	    lctagOptMap.mode = "ref-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "def-at", 1, true ) then
	    lctagOptMap.mode = "def-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "call-at", 1, true ) then
	    lctagOptMap.mode = "call-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "graph", 1, true ) then
	    lctagOptMap.mode = "graph"
	    lctagOptMap.graph = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "dump" then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = "dump"
	 elseif string.find( arg, "-x", 1, true ) then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	 elseif string.find( arg, "-c", 1, true ) then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	 end
      else
	 if skipArgNum > 0 then
	    skipArgNum = skipArgNum - 1
	 else
	    local processMode = "skip"
	    if string.find( arg, "^-" ) then
	       if arg == "--lctags-log" then
		  skipArgNum = 1
		  log( 0, tonumber( argList[ index + 1 ] ) )
	       elseif arg == "--lctags-db" then
		  skipArgNum = 1
	       elseif arg == "--lctags-conf" then
		  skipArgNum = 1
	       elseif arg == "--lctags-target" then
		  skipArgNum = 1
		  lctagOptMap.target = argList[ index + 1 ]
	       elseif arg == "--lctags-digestRec" then
		  lctagOptMap.recordDigestSrcFlag = true
	       elseif arg == "--lctags-recSql" then
		  skipArgNum = 1
		  DBAccess:recordSql( io.open( argList[ index + 1 ], "w" ) )
	       elseif arg == "--use-global" then
		  lctagOptMap.useGlobalFlag = true
	       elseif arg == "--lctags-quiet" then
		  lctagOptMap.quiet = true
	       else
		  if lctagOptMap.mode == "build" then
		     processMode = "conv"
		  elseif arg == "-b" then
		     lctagOptMap.browse = true
		  elseif arg == "-d" then
		     lctagOptMap.depth = tonumber( argList[ index + 1 ] )
		     skipArgNum = 1
		  elseif arg == "-o" then
		     lctagOptMap.outputFile = argList[ index + 1 ]
		     skipArgNum = 1
		  else
		     processMode = nil
		  end
	       end
	    else
	       if lctagOptMap.mode == "build" then
		  processMode = "conv"
		  
	       else
		  processMode = nil
	       end
	    end

	    if processMode == "conv" then
	       local cc = gcc
	       if lctagOptMap.cc ~=  "gcc" then
		  cc = lctagOptMap.conf
	       end
	       local argType, argTxt =
		  cc:convertCompileOption( lctagOptMap.cc, arg )
	       if argType == "src" then
		  table.insert( srcList, argTxt )
	       elseif argType == "opt" then
		  table.insert( optList, argTxt )
	       end
	    elseif processMode == nil then
	       table.insert( srcList, arg )
	    end
	 end
      end
   end
   return srcList, optList, lctagOptMap
end

if not arg[1] then
   printUsage( "" )
end

local srcList, optList, lctagOptMap = analyzeOption( arg )

for key, val in pairs( lctagOptMap ) do
   log( 3, key, val )
end


local projDir = os.getenv( "PWD" )
if lctagOptMap.projDir then
   projDir = lctagOptMap.projDir
end

if not lctagOptMap.mode then
   printUsage( "mode is none" )
end

if not lctagOptMap.dbPath then
   local dir = os.getenv( "PWD" )
   repeat
      local dbPath = dir .. "/" .. "lctags.sqlite3"
      local dbFile = io.open( dbPath, "r" )
      if dbFile then
	 dbFile:close()
	 lctagOptMap.dbPath = dbPath
	 break
      end
      dir = string.gsub( dir, "/[^/]*$", "" )
   until dir == ""
   if not lctagOptMap.dbPath then
      if lctagOptMap.mode == "init" then
         lctagOptMap.dbPath = os.getenv( "PWD" ) .. "/" .. "lctags.sqlite3"
      end
   end
end


if lctagOptMap.mode == "init" then
   DBCtrl:init( lctagOptMap.dbPath, os.getenv( "PWD" ), projDir )
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
      lctagOptMap.dbPath, os.getenv( "PWD" ), projDir,
      Helper.getCurrentTime() )
   os.exit( 0 )
end

if lctagOptMap.mode == "query" then
   if not Query:exec(
      lctagOptMap.dbPath, lctagOptMap.query, srcList[ 1 ],
      lctagOptMap.useGlobalFlag )
   then
      printUsage( "query is error" )
   end
   os.exit( 0 )
end

if lctagOptMap.mode == "graph" then
   if lctagOptMap.graph == "inc" or lctagOptMap.graph == "incSrc" then
      Query:outputIncRelation(
	 lctagOptMap.dbPath, srcList[ 1 ], lctagOptMap.graph == "inc",
	 lctagOptMap.depth, lctagOptMap.browse, lctagOptMap.outputFile )
   elseif lctagOptMap.graph == "caller" or lctagOptMap.graph == "callee" then
      Query:outputCallRelation(
	 lctagOptMap.dbPath, srcList[ 1 ], lctagOptMap.graph == "caller",
	 lctagOptMap.depth, lctagOptMap.browse, lctagOptMap.outputFile )
   elseif lctagOptMap.graph == "symbol" then
      Query:outputSymbolRefRelation(
	 lctagOptMap.dbPath, srcList[ 1 ], lctagOptMap.depth,
	 lctagOptMap.browse, lctagOptMap.outputFile )
   else
      printUsage( "unknown graph" )
   end
   os.exit( 0 )
end


if not lctagOptMap.dbPath then
   printUsage( "db is not found." )
end

local analyzer = Analyzer:new(
   lctagOptMap.dbPath, lctagOptMap.recordDigestSrcFlag, not lctagOptMap.quiet )

if lctagOptMap.mode == "build" then
   local src = srcList[1]

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

   
   local clangVer = require( 'libclanglua.if' ).getClangVersion()
   clangVer3 = string.gsub(
      clangVer, "^clang version (%d+)%.(%d+)%.(%d+)[^%d].*", "%1.%2.%3" )
   clangVer2 = string.gsub( clangVer3, "^(%d+)%.(%d+)[^%d].*", "%1.%2" )

   defInc = string.format(
      "/usr/lib/llvm-%s/lib/clang/%s/include/", clangVer2, clangVer3 )
   table.insert( optList, "-I" .. defInc )


   analyzer:analyzeSource( src, optList, lctagOptMap.target )
   os.exit( 0 )
end

if lctagOptMap.mode == "update" then
   local src = srcList[1]

   log( 3, "src:", src, "target:", lctagOptMap.target )
   
   analyzer:update( src, lctagOptMap.target )
   os.exit( 0 )
end

if lctagOptMap.mode == "ref-at" or
   lctagOptMap.mode == "def-at" or lctagOptMap.mode == "call-at"
then
   analyzer:queryAt(
      lctagOptMap.mode, srcList[ 1 ], tonumber( srcList[ 2 ] ),
      tonumber( srcList[ 3 ] ), lctagOptMap.abs, lctagOptMap.target )
   os.exit( 0 )
end
