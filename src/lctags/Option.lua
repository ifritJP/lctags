local log = require( 'lctags.LogCtrl' )
local gcc = require( 'lctags.gcc' )


local Option = {}

local function loadConfig( path, exitOnErr )
   local fileHandle = io.open( path, "r" )
   if fileHandle then
      fileHandle:close()
      local chunk, err = loadfile( path )
      if chunk then
	 return chunk()
      end
      print( err )
   end
   if exitOnErr then
      print( "loadfile error", err )
      os.exit( 1 )
   end
   return nil
end


function Option:printUsage( message )
   if message then
      print( message )
   end
   local usageTxt = string.gsub( [[
usage:
 - build DB
   %s init projDir [-it] [-is] [-im]
   %s build compiler [--lctags-out-info] [--lctags-conf conf] [--lctags-target target] [--lctags-recSql file] [--lctags-prof] [--lctags-srv] comp-op [...] src
   %s update [-j jobs] pattrn
   %s register [--lctags-conf conf] [--lctags-target target] <-i|file>
   %s depIncs comp-op src
   %s server [--lctags-target target] <start|stop>
   %s statusServer <start|stop>
   %s status
 - query DB
   %s dump <all|target|file|ref|def|call|inc> [path]
   %s ref-at[a] [--lctags-target target] [-i] file line column 
   %s def-at[a] [--lctags-target target] [-i] file line column 
   %s call-at[a] [--lctags-target target] [-i] file line column
   %s ns-at [--lctags-target target] [-i] file line column
   %s comp-at [--lctags-target target] [-i] file line column
   %s inq-at [--lctags-target target] [-i] file line column
   %s list <incSrc|inc> [-d depth] name
   %s -x[t|s|r][a]  [--use-global] symbol
   %s -xP[a]  [--use-global] file
   %s -c  [--use-global] symbol
   %s dcall
 - graph
   %s graph <incSrc|inc|caller|callee|symbol> [-d depth] [-b|-o file] [-f type] [name]
   %s graph-at <caller|callee|symbol> [-d depth] [-b|-o file] [-f type] [--lctags-target target] file line column
 - modify db
   %s rm <file|tgt> name
   %s shrink [--lctags-db path]
   %s chkFiles [--lctags-db path]
   %s chg-proj projDir [--lctags-db path]

  option:
     init: initialize DB file. "projDir" is a root directory of your project.
       -it: enable individual type mode.
       -is: enable individual struct mode.
       -is: enable individual macro mode.
     build: build DB for "src".
            "compiler" is "gcc" or "cc" or ....
            "comp-op" is compiler option. This include source file path.
     register: register source file from json.
     shrink: shrink DB.
     chg-proj: change project directory.
     dump: dump DB.
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
     -i: input from stdin for source file contents.
     --use-global: use GNU global when db is not found.
     graph: draw graph.
     graph-at: draw graph at position.
         inc: include relation.
         caller: caller graph.
         callee: callee graph.
         -d: depth.
         -b: browse graph.
         -o: output image file.
         -f: image type. (svg, png)

   common option:
     --lctags-quiet: discard clang diagnostic.
     --lctags-db: set DB file path.
     --lctags-log: set log level. default is 1. when lv > 1, it is datail mode.
   ]], '%%s', "lctags" )
   print( usageTxt )
   
   os.exit( 1 )
end

function Option:analyzeOption( argList )
   local srcList = {}
   local optList = {}
   local skipArgNum = 0
   local lctagOptMap = {}
   local converter = nil

   for index, arg in ipairs( argList ) do
      if skipArgNum > 0 then
	 skipArgNum = skipArgNum - 1
      elseif string.find( arg, "--lctags-conf", 1, true ) then
	 skipArgNum = 1
	 lctagOptMap.conf = loadConfig( argList[ index + 1 ], true )
      elseif string.find( arg, "--lctags-db", 1, true ) then
	 skipArgNum = 1
	 lctagOptMap.dbPath = argList[ index + 1 ]
	 local confPath = string.gsub(
	    lctagOptMap.dbPath, "(.*/).*", "%1lctags.conf" )
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
	    if not lctagOptMap.conf then
	       local confPath = string.gsub(
		  lctagOptMap.dbPath, "(.*/).*", "%1lctags.conf" )
	       lctagOptMap.conf = loadConfig( confPath, false )
	    end
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
	 elseif arg == "updateForMake" then
	    lctagOptMap.mode = "updateForMake"
	 elseif string.find( arg, "ref-at", 1, true ) then
	    lctagOptMap.mode = "ref-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "def-at", 1, true ) then
	    lctagOptMap.mode = "def-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "call-at", 1, true ) then
	    lctagOptMap.mode = "call-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif arg == "ns-at" then
	    lctagOptMap.mode = "ns-at"
	 elseif arg == "graph" then
	    lctagOptMap.mode = "graph"
	    lctagOptMap.graph = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "graph-at" then
	    lctagOptMap.mode = "graph-at"
	    lctagOptMap.graph = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "dump" then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = "dump"
	    if argList[ index + 1 ] == "target" then
	       lctagOptMap.query = "dumpTarget"
	    elseif argList[ index + 1 ] == "file" then
	       lctagOptMap.query = "dumpFile"
	    elseif argList[ index + 1 ] == "all" then
	       lctagOptMap.query = "dumpAll"
	    elseif argList[ index + 1 ] == "ref" then
	       lctagOptMap.query = "dumpRef"
	    elseif argList[ index + 1 ] == "def" then
	       lctagOptMap.query = "dumpDef"
	    elseif argList[ index + 1 ] == "call" then
	       lctagOptMap.query = "dumpCall"
	    elseif argList[ index + 1 ] == "inc" then
	       lctagOptMap.query = "dumpInc"
	    else
	       self:printUsage( "unknown dump option" )
	    end
	    skipArgNum = 1
	 elseif string.find( arg, "-x", 1, true ) then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	 elseif string.find( arg, "-c", 1, true ) then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	 elseif string.find( arg, "list", 1, true ) then
	    lctagOptMap.mode = "list"
	    lctagOptMap.query = argList[ index + 1 ]
	    if lctagOptMap.query == "inc" then
	       lctagOptMap.depth = 100
	    end
	    skipArgNum = 1
	 elseif arg == "comp-at" then
	    lctagOptMap.mode = arg
	 elseif arg == "inq-at" then
	    lctagOptMap.mode = arg
	 elseif arg == "chkFiles" then
	    lctagOptMap.mode = arg
	 elseif arg == "rm" then
	    lctagOptMap.mode = arg
	    lctagOptMap.rm = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "register" then
	    lctagOptMap.mode = arg
	 elseif arg == "depIncs" then
	    lctagOptMap.mode = arg
	    lctagOptMap.cc = "gcc"
	 elseif arg == "server" then
	    lctagOptMap.mode = arg
	 elseif arg == "statusServer" then
	    lctagOptMap.mode = arg
	 elseif arg == "status" then
	    lctagOptMap.mode = arg
	 elseif arg == "dcall" then
	    lctagOptMap.mode = arg
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
	       elseif arg == "--lctags-only-reg" then
		  lctagOptMap.onlyReg = true
	       elseif arg == "--lctags-target" then
		  skipArgNum = 1
		  lctagOptMap.target = argList[ index + 1 ]
	       elseif arg == "--lctags-digestRec" then
		  lctagOptMap.recordDigestSrcFlag = true
	       elseif arg == "--lctags-recSql" then
		  skipArgNum = 1
		  self.recordSqlObj = io.open( argList[ index + 1 ], "w" )
	       elseif arg == "--use-global" then
		  lctagOptMap.useGlobalFlag = true
	       elseif arg == "--lctags-quiet" then
		  lctagOptMap.quiet = true
	       elseif arg == "--lctags-prof" then
		  self.profile = true
	       elseif arg == "--lctags-lockLog" then
		  self.lockLog = true
	       elseif arg == "--lctags-srv" then
		  self.serviceFlag = true
	       else
		  if lctagOptMap.mode == "build" or lctagOptMap.mode == "depIncs"
		  then
		     processMode = "conv"
		  elseif lctagOptMap.mode == "graph" or
		     lctagOptMap.mode == "graph-at" or
		     lctagOptMap.mode == "list"
		  then
		     if arg == "-b" then
			lctagOptMap.browse = true
		     elseif arg == "-d" then
			lctagOptMap.depth = tonumber( argList[ index + 1 ] )
			skipArgNum = 1
		     elseif arg == "-o" then
			lctagOptMap.outputFile = argList[ index + 1 ]
			skipArgNum = 1
		     elseif arg == "-f" then
			lctagOptMap.imageFormat = argList[ index + 1 ]
			skipArgNum = 1
		     end
		  elseif lctagOptMap.mode == "init" then
		     if arg == "-it" then
			lctagOptMap.individualTypeFlag = true
		     elseif arg == "-is" then
			lctagOptMap.individualStructFlag = true
		     elseif arg == "-im" then
			lctagOptMap.individualMacroFlag = true
		     end
		  elseif lctagOptMap.mode == "register" then
		     if arg == "-i" then
			lctagOptMap.registerFromInfo = true
		     end
		  else
		     if arg == "-i" then
			lctagOptMap.inputFromStdin = true
		     elseif arg == "-j" then
			lctagOptMap.jobs = tonumber( argList[ index + 1 ] )
			skipArgNum = 1
		     end
		     processMode = "skip"
		  end
	       end
	    else
	       if lctagOptMap.mode == "build" or lctagOptMap.mode == "depIncs" then
		  processMode = "conv"
		  
	       else
		  processMode = nil
	       end
	    end

	    if processMode == "conv" then
	       if not converter then
		  if lctagOptMap.cc ~=  "gcc" then
		     if lctagOptMap.conf and
			lctagOptMap.conf.createCompileOptionConverter
		     then
			converter = lctagOptMap.conf:createCompileOptionConverter(
			   lctagOptMap.cc )
		     end
		  end
		  if not converter then
		     converter = gcc:createCompileOptionConverter( "gcc" )
		  end
	       end
	       local argType, argTxt = converter:convert( arg )
	       if argType == "src" then
		  table.insert( srcList, argTxt )	       elseif argType == "opt" then
		  table.insert( optList, argTxt )
	       end
	    elseif processMode == nil then
	       table.insert( srcList, arg )
	    end
	 end
      end
   end

   if lctagOptMap.mode == "build" then
      local clangVer = require( 'libclanglua.if' ).getClangVersion()
      clangVer3 = string.gsub(
      	 clangVer, "^clang version (%d+)%.(%d+)%.(%d+)[^%d].*", "%1.%2.%3" )
      clangVer2 = string.gsub( clangVer3, "^(%d+)%.(%d+)[^%d].*", "%1.%2" )

      defInc = string.format(
      	 "/usr/lib/llvm-%s/lib/clang/%s/include/", clangVer2, clangVer3 )
      table.insert( optList, "-I" .. defInc )
      -- table.insert( optList, "-I" ..  "/usr/lib/gcc/x86_64-linux-gnu/5/include" )
   end

   for key, val in pairs( lctagOptMap ) do
      log( 3, "lctagOptMap", key, val )
   end
   
   return srcList, optList, lctagOptMap
end

function Option:isValidProfile()
   return self.profile
end

function Option:isValidLockLog()
   return self.lockLog
end

function Option:getRecordSqlObj()
   return self.recordSqlObj
end

function Option:isValidService()
   return self.serviceFlag
end

return Option
