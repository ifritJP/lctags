-- -*- coding:utf-8 -*-
-- Copyright (C) 2017 ifritJP

local log = require( 'lctags.LogCtrl' )
local Analyzer = require( 'lctags.Analyzer' )
local Query = require( 'lctags.Query' )
local gcc = require( 'lctags.gcc' )

local function printUsage()
   local command = "lctags"
   print( string.format( [[
usage:
 - build DB
   %s init [--lctags-projDir dir] [--lctags-db path] [--lctags-log lv] 
   %s build compiler [--lctags-log lv] [--lctags-db path] [--lctags-conf conf] comp-op [...] src
   %s shrink [--lctags-db path]
 - query DB
   %s dump
   %s ref-at[a] file line column
   %s def-at[a] file line column
   %s -x[t|s|r][a] [--lctags-db path] [--lctags-log lv] symbol
   %s -xP[a] [--lctags-db path] [--lctags-log lv] file


     init: initialize DB file.
     build: build DB for "src".
            "compiler" is "gcc" or "cc" or ....
            "comp-op" is compiler option. This include source file path.
     shrink: shrink DB.
     dump: dump DB.
     --lctags-projDir: set "dir" as project directory.
     --lctags-db: set DB file path.
     --lctags-log: set log level. default is 1. when lv > 1, it is datail mode.
     -x: query DB.
        -xt: symbol declaration
        -xs: symbol declaration
        -xr: symbol reference
        -xP: file list
   ]],
	     command, command, command, command, command, command, command, command ) )
   os.exit( 1 )
end


local function analyzeOption( argList )
   local srcList = {}
   local optList = {}
   local nextArgType = nil
   local lctagOptMap = {}
   for index, arg in ipairs( argList ) do
      if index == 1 then
	 if string.find( arg, "build", 1, true ) then
	    lctagOptMap.mode = "build"
	    lctagOptMap.cc = argList[ index + 1 ]
	    nextArgType = "skip"
	 elseif string.find( arg, "init", 1, true ) then
	    lctagOptMap.mode = "init"
	 elseif string.find( arg, "shrink", 1, true ) then
	    lctagOptMap.mode = "shrink"
	 elseif string.find( arg, "ref-at", 1, true ) then
	    lctagOptMap.mode = "ref-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	    lctagOptMap.file = argList[ index + 1 ]
	    lctagOptMap.line = tonumber( argList[ index + 2 ] )
	    lctagOptMap.column = tonumber( argList[ index + 3 ] )
	    break
	 elseif string.find( arg, "def-at", 1, true ) then
	    lctagOptMap.mode = "def-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	    lctagOptMap.file = argList[ index + 1 ]
	    lctagOptMap.line = tonumber( argList[ index + 2 ] )
	    lctagOptMap.column = tonumber( argList[ index + 3 ] )
	    break
	 elseif string.find( arg, "dump", 1, true ) then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = "dump"
	 elseif string.find( arg, "-x", 1, true ) then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	 end
      else
	 if nextArgType == "skip" then
	    nextArgType = nil
	 else
	    local processMode = "skip"
	    if string.find( arg, "^-" ) then
	       if string.find( arg, "--lctags-log", 1, true ) then
		  nextArgType = "skip"
		  log( 0, tonumber( argList[ index + 1 ] ) )
	       elseif string.find( arg, "--lctags-projDir", 1, true ) then
		  nextArgType = "skip"
		  lctagOptMap.projDir = argList[ index + 1 ]
	       elseif string.find( arg, "--lctags-db", 1, true ) then
		  nextArgType = "skip"
		  lctagOptMap.dbPath = argList[ index + 1 ]
	       elseif string.find( arg, "--lctags-conf", 1, true ) then
		  nextArgType = "skip"
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
   printUsage()
end

local srcList, optList, lctagOptMap = analyzeOption( arg )

local projDir = os.getenv( "PWD" )
if lctagOptMap.projDir then
   projDir = lctagOptMap.projDir
end

if not lctagOptMap.mode then
   printUsage()
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
      else
	 printUsage()
      end
   end
end
local analyzer = Analyzer:new( lctagOptMap.dbPath, projDir )


if lctagOptMap.mode == "init" then
   analyzer:initDB()
   os.exit( 0 )
end

if lctagOptMap.mode == "shrink" then
   analyzer:shrinkDB()
   os.exit( 0 )
end

if lctagOptMap.mode == "build" then
   local src = srcList[1]
   print( "src:",  src )

   local option = ""
   for index, opt in ipairs( optList ) do
      option = option .. opt .. " "
   end
   log( 2, "src:", src, "opt:", option )
   
   
   if lctagOptMap.conf then
      for index, info in ipairs( lctagOptMap.conf:getIgnorePattern() ) do
	 if info[ 1 ] == "simple" then
	    if string.find( src, info[ 2 ], 1, true ) then
	       log( 1, "ignore:", src )
	       os.exit( 0 )
	    end
	 elseif info[ 1 ] == "lua" then
	    if string.find( src, info[ 2 ] ) then
	       log( 1, "ignore:", src )
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


   analyzer:analyzeSource( src, optList )
   os.exit( 0 )
end

if lctagOptMap.mode == "query" then
   if not Query:exec(
      lctagOptMap.dbPath, lctagOptMap.query, srcList[ 1 ], lctagOptMap.abs )
   then
      printUsage()
   end
end

if lctagOptMap.mode == "ref-at" then
   analyzer:queryAt( true, lctagOptMap.file, lctagOptMap.line,
		     lctagOptMap.column, lctagOptMap.abs )
   os.exit( 0 )
end

if lctagOptMap.mode == "def-at" then
   analyzer:queryAt( false, lctagOptMap.file, lctagOptMap.line,
		     lctagOptMap.column, lctagOptMap.abs )
   os.exit( 0 )
end
