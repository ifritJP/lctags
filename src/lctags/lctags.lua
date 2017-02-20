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
   %s -x[t|s|r][a] [--lctags-db path] [--lctags-log lv] [--use-global] symbol
   %s -xP[a] [--lctags-db path] [--lctags-log lv] [--use-global] file
   %s -c [--lctags-db path] [--lctags-log lv] [--use-global] symbol


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
     --use-global: use GNU global when db is not found.
   ]],
	     command, command, command, command, command, command,
	     command, command, command, command, command ) )
   os.exit( 1 )
end


local function analyzeOption( argList )
   local srcList = {}
   local optList = {}
   local skipArgNum = 0
   local lctagOptMap = {}
   for index, arg in ipairs( argList ) do
      if index == 1 then
	 if string.find( arg, "build", 1, true ) then
	    lctagOptMap.mode = "build"
	    lctagOptMap.cc = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif string.find( arg, "init", 1, true ) then
	    lctagOptMap.mode = "init"
	    lctagOptMap.projDir = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif string.find( arg, "shrink", 1, true ) then
	    lctagOptMap.mode = "shrink"
	 elseif string.find( arg, "forceUpdate", 1, true ) then
	    lctagOptMap.mode = "forceUpdate"
	 elseif string.find( arg, "chg-proj", 1, true ) then
	    lctagOptMap.mode = "chg-proj"
	    lctagOptMap.projDir = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif string.find( arg, "update", 1, true ) then
	    lctagOptMap.mode = "update"
	 elseif string.find( arg, "ref-at", 1, true ) then
	    lctagOptMap.mode = "ref-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "def-at", 1, true ) then
	    lctagOptMap.mode = "def-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "dump", 1, true ) then
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
	       if string.find( arg, "--lctags-log", 1, true ) then
		  skipArgNum = 1
		  log( 0, tonumber( argList[ index + 1 ] ) )
	       elseif string.find( arg, "--lctags-db", 1, true ) then
		  skipArgNum = 1
		  lctagOptMap.dbPath = argList[ index + 1 ]
	       elseif string.find( arg, "--lctags-target", 1, true ) then
		  skipArgNum = 1
		  lctagOptMap.target = argList[ index + 1 ]
	       elseif string.find( arg, "--lctags-digestRec", 1, true ) then
		  lctagOptMap.recordDigestSrcFlag = true
	       elseif string.find( arg, "--lctags-recSql", 1, true ) then
		  skipArgNum = 1
		  DBAccess:recordSql( io.open( argList[ index + 1 ], "w" ) )
	       elseif string.find( arg, "--use-global", 1, true ) then
		  lctagOptMap.useGlobalFlag = true
	       elseif string.find( arg, "--lctags-conf", 1, true ) then
		  skipArgNum = 1
		  local chunk, err = loadfile( argList[ index + 1 ] )
		  if not chunk then
		     print( "loadfile error", err )
		     os.exit( 1 )
		  end
		  lctagOptMap.conf = chunk()
	       else
		  if lctagOptMap.mode == "build" then
		     processMode = "conv"
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
   DBCtrl:shrinkDB( lctagOptMap.dbPath )
   os.exit( 0 )
end

if lctagOptMap.mode == "forceUpdate" then
   DBCtrl:forceUpdate( lctagOptMap.dbPath )
   os.exit( 0 )
end

if lctagOptMap.mode == "chg-proj" then
   DBCtrl:changeProjDir( lctagOptMap.dbPath, os.getenv( "PWD" ), projDir )
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


if not lctagOptMap.dbPath then
   printUsage( "db is not found." )
end

local analyzer = Analyzer:new(
   lctagOptMap.dbPath, lctagOptMap.recordDigestSrcFlag )

if lctagOptMap.mode == "build" then
   local src = srcList[1]

   local option = ""
   for index, opt in ipairs( optList ) do
      option = option .. opt .. " "
   end
   log( 2, "src:", src, "target:", lctagOptMap.target, "opt:", option )
   
   
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

   log( 2, "src:", src, "target:", lctagOptMap.target )
   
   analyzer:update( src, lctagOptMap.target )
   os.exit( 0 )
end

if lctagOptMap.mode == "ref-at" then
   analyzer:queryAt(
      true, srcList[ 1 ], tonumber( srcList[ 2 ] ),
      tonumber( srcList[ 3 ] ), lctagOptMap.abs )
   os.exit( 0 )
end

if lctagOptMap.mode == "def-at" then
   analyzer:queryAt(
      false, srcList[ 1 ], tonumber( srcList[ 2 ] ),
      tonumber( srcList[ 3 ] ), lctagOptMap.abs )
   os.exit( 0 )
end
