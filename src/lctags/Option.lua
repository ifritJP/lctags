local log = require( 'lctags.LogCtrl' )
local gcc = require( 'lctags.gcc' )
local config = require( 'lctags.config' )
local clang = require( 'libclanglua.if' )

local Option = {}

Option.orgDir = os.getenv( "PWD" )



function Option:printUsage( message )
   if message then
      print( message )
   end
   local usageTxt = string.gsub( [[
usage:
 - build DB
   %s init projDir [-it] [-is] [-im]
   %s build compiler [--lctags-out-info] [--lctags-conf conf] [--lctags-target target] [--lctags-recSql file] [--lctags-prof] [--lctags-srv] [--lctags-indiv] [comp-op] [...] src
   %s update [-j jobs] pattrn
   %s lazyUpdate [-j jobs]
   %s register [--lctags-conf conf] [--lctags-target target] <-i|file>
   %s depIncs [comp-op] src [--lctags-target target]
   %s addInc [comp-op] header [--lctags-target target]
   %s addStdInc [comp-op] [--lctags-target target]
   %s server [--lctags-target target] <start|stop>
   %s statusServer <start|stop|wait>
   %s status
 - query DB
   %s dump <ver|all|target|targetList|file|ref|def|call|inc|digest|prepro> [path]
   %s ref-at[a] [--lctags-target target] [-i] file line column 
   %s ref-at-all [--lctags-target target] [-i] file line column 
   %s def-at[a] [--lctags-target target] [-i] file line column 
   %s call-at[a] [--lctags-target target] [-i] file line column
   %s callee-at[a] [--lctags-target target] [-i] file line column
   %s ns-at [--lctags-target target] [-i] file line column
   %s comp-at [--lctags-target target] [-i] file line column
   %s inq-at [--lctags-target target] [-i] file line column
   %s list <incSrc|inc|incSrcHeader> [-d depth] name
   %s -x[T|t|s|r][a]  [--use-global] symbol
   %s -xP[a]  [--use-global] file
   %s -c  [--use-global] symbol
   %s dcall
   %s grep-cursors [--lctags-target target] [-i] file kind symbol

 - graph
   %s graph <incSrc|inc|caller|callee|symbol> [-d depth] [-b|-o file] [-f type] [name]
   %s graph-at <caller|callee|symbol> [-d depth] [-b|-o file] [-f type] [--lctags-target target] file line column
 - modify db
   %s rm <file|tgt> name
   %s shrink [--lctags-db path]
   %s chkFiles [--lctags-db path]
   %s chg-proj projDir [--lctags-db path] [src@dst src@dst src@dst src@dst]
   %s set-projDir projDir [--lctags-db path]
 - misc
   %s cursors [--lctags-target target] file
   %s split-at [--lctags-target target] [-i] file line column [-split-param-list sym1,sym2,...]
   %s clang-ver
   %s kill
   %s cancel-kill
   %s copyConf

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
     --lctags-conf: config file.
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

function Option:getSameDirFile( src, basename )
   local dir =
      string.find( src, "/" ) and
      string.gsub( src, "(.*/).*", "%1" ) or
      (os.getenv( "PWD" ) .. "/")
   return dir .. basename
end

function Option:getConfPathFromDbPath( dbPath )
   return Option:getSameDirFile( dbPath, "lctags.conf" )
end

function Option:analyzeOption( argList )
   local srcList = {}
   local optList = {}
   local skipArgNum = 0
   local lctagOptMap = {}
   local converter = nil

   lctagOptMap.cc = "gcc"

   local conf

   for index, arg in ipairs( argList ) do
      if skipArgNum > 0 then
	 skipArgNum = skipArgNum - 1
      elseif string.find( arg, "--lctags-conf", 1, true ) then
	 skipArgNum = 1
	 conf = config:loadConfig( argList[ index + 1 ], true )
	 if conf then
	    lctagOptMap.confPath = confPath
	 end
      elseif string.find( arg, "--lctags-db", 1, true ) then
	 skipArgNum = 1
	 lctagOptMap.dbPath = argList[ index + 1 ]
	 if not conf then
	    local confPath = Option:getConfPathFromDbPath( lctagOptMap.dbPath )
	    conf = config:loadConfig( confPath, false )
	    if conf then
	       lctagOptMap.confPath = confPath
	    end
	 end
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
	    if not conf then
	       local confPath = string.gsub(
		  lctagOptMap.dbPath, "(.*/).*", "%1lctags.conf" )
	       conf = config:loadConfig( confPath, false )
	       if conf then
		  lctagOptMap.confPath = confPath
	       end
	    end
	    break
	 end
	 dir = string.gsub( dir, "/[^/]*$", "" )
      until dir == ""
   end

   lctagOptMap.conf = config

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
	       if not conf then
		  local confPath = string.gsub(
		     lctagOptMap.dbPath, "(.*/).*", "%1lctags.conf" )
		  conf = config:loadConfig( confPath, false )
		  if conf then
		     lctagOptMap.confPath = confPath
		  end
	       end
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
	 elseif arg == "copyConf" then
	    lctagOptMap.mode = "copyConf"
	 elseif arg == "update" then
	    lctagOptMap.mode = "update"
	 elseif arg == "lazyUpdate" then
	    lctagOptMap.mode = "lazyUpdate"
	 elseif arg == "updateForMake" then
	    lctagOptMap.mode = "updateForMake"
	 elseif arg == "ref-at-all" then
	    lctagOptMap.mode = arg
	 elseif string.find( arg, "ref-at", 1, true ) == 1 then
	    lctagOptMap.mode = "ref-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "def-at", 1, true ) == 1 then
	    lctagOptMap.mode = "def-at"
	    lctagOptMap.abs = string.find( arg, "a$" )
	 elseif string.find( arg, "call-at", 1, true ) == 1 then
	    lctagOptMap.mode = "call-at"
	 elseif string.find( arg, "callee-at", 1, true ) == 1 then
	    lctagOptMap.mode = "callee-at"
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
	    elseif argList[ index + 1 ] == "targetList" then
	       lctagOptMap.query = "dumpTargetList"
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
	    elseif argList[ index + 1 ] == "incSrc" then
	       lctagOptMap.query = "dumpIncSrc"
	    elseif argList[ index + 1 ] == "digest" then
	       lctagOptMap.query = "dumpDigest"
	    elseif argList[ index + 1 ] == "prepro" then
	       lctagOptMap.query = "dumpPrepro"
	    elseif argList[ index + 1 ] == "belong" then
	       lctagOptMap.query = "dumpBelong"
	    elseif argList[ index + 1 ] == "ver" then
	       lctagOptMap.query = "dumpVersion"
	    elseif argList[ index + 1 ] == "projDir" then
	       lctagOptMap.query = "dumpProjDir"
	    else
	       self:printUsage( "unknown dump option" )
	    end
	    skipArgNum = 1
	 elseif string.find( arg, "-x", 1, true ) == 1 then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	    lctagOptMap.compatibleGlobal = true
	 elseif string.find( arg, "-c", 1, true ) == 1 then
	    lctagOptMap.mode = "query"
	    lctagOptMap.query = arg
	    lctagOptMap.compatibleGlobal = true
	 elseif arg == "list" then
	    lctagOptMap.mode = "list"
	    lctagOptMap.query = argList[ index + 1 ]
	    if lctagOptMap.query == "inc" then
	       lctagOptMap.depth = 100
	    end
	    skipArgNum = 1
	 elseif arg == "set-projDir" then
	    lctagOptMap.mode = arg
	    lctagOptMap.projDir = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "cursors" then
	    lctagOptMap.mode = arg
	    self.cursors = true
	 elseif arg == "grep-cursor" then
	    lctagOptMap.mode = arg
	 elseif arg == "expand-macro" then
	    lctagOptMap.mode = arg
	 elseif arg == "cursor-at" then
	    lctagOptMap.mode = arg
	 elseif arg == "scan" then
	    lctagOptMap.mode = arg
	    lctagOptMap.scan = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "addInc" then
	    lctagOptMap.mode = arg
	 elseif arg == "addIncRef" then -- これは emacs 用
	    lctagOptMap.mode = arg
	 elseif arg == "addStdInc" then
	    lctagOptMap.mode = arg
	 elseif arg == "split-at" then
	    lctagOptMap.mode = arg
	 elseif arg == "comp-at" then
	    lctagOptMap.mode = arg
	 elseif arg == "inq-at" then
	    lctagOptMap.mode = arg
	 elseif arg == "inq" then
	    lctagOptMap.mode = arg
	    lctagOptMap.query = argList[ index + 1 ]
	    skipArgNum = 1
	 elseif arg == "expand" then
	    lctagOptMap.mode = arg
	 elseif arg == "diag" then
	    lctagOptMap.mode = arg
	 elseif arg == "chkFiles" then
	    lctagOptMap.mode = arg
	 elseif arg == "rm" then
	    lctagOptMap.mode = arg
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
	 elseif arg == "stack" then
	    lctagOptMap.mode = arg
	 elseif arg == "clang-ver" then
	    lctagOptMap.mode = arg
	 elseif arg == "testOpe" then
	    lctagOptMap.mode = arg
	 elseif arg == "testInc" then
	    lctagOptMap.mode = arg
	 elseif arg == "kill" then
	    lctagOptMap.mode = arg
	 elseif arg == "cancel-kill" then
	    lctagOptMap.mode = arg
	 elseif arg == "call-func" then
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
		  self.recordSql = true
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
	       elseif arg == "--lctags-form" then
		  Option.outputForm = argList[ index + 1 ]
		  skipArgNum = 1
	       elseif arg == "--lctags-indiv" then
		  self.indivisualWriteFlag = true
	       elseif arg == "--lctags-uptime" then
		  skipArgNum = 1
		  self.updateTime = tonumber( argList[ index + 1 ] )
	       elseif arg == "--lctags-directRet" then
		  lctagOptMap.directRet = true
	       elseif arg == "--lctags-cursorKind" then
		  local kindId = argList[ index + 1 ]
		  local cursorKind = clang.CXCursorKind[ kindId ]
		  if not cursorKind then
		     error( "kindId is unknown: " .. kindId )
		  end
		  lctagOptMap.cursorKind = cursorKind.val
	       elseif arg == "--lctags-candidateLimit" then
		  skipArgNum = 1
		  lctagOptMap.candidateLimit = tonumber( argList[ index + 1 ] )
	       elseif arg == "--lctags-subRet" then
		  skipArgNum = 1
		  local subRetType = {}
		  for val in string.gmatch( argList[ index + 1 ], "[^/]+" ) do
		     table.insert( subRetType, val )
		  end
		  lctagOptMap.subRetTypeInfo = {}
		  lctagOptMap.subRetTypeInfo.type = subRetType[ 1 ]
		  lctagOptMap.subRetTypeInfo.non = subRetType[ 2 ]
		  lctagOptMap.subRetTypeInfo.ret = subRetType[ 3 ]
		  lctagOptMap.subRetTypeInfo.brk = subRetType[ 4 ]
		  lctagOptMap.subRetTypeInfo.cnt = subRetType[ 5 ]
	       else
		  if lctagOptMap.mode == "build" or lctagOptMap.mode == "depIncs" or
		     lctagOptMap.mode == "addStdInc" or lctagOptMap.mode == "cursors"
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
		  elseif lctagOptMap.mode == "query" then
		     if string.find( arg, "--encode-path=", 1, true ) == 1 then
			skipArgNum = 1
		     end
		  elseif lctagOptMap.mode == "split-at" then
		     if arg == "-split-param-list" then
			lctagOptMap.splitParamInfoList = {}
			for val in string.gmatch( argList[ index + 1 ], "[^,]+" ) do
			   local tokenList = {}
			   for info in string.gmatch( val, "[^:]+" ) do
			      table.insert( tokenList, info )
			   end
			   table.insert(
			      lctagOptMap.splitParamInfoList,
			      {
				 directPassFlag = tokenList[ 1 ] ~= "o",
				 directRetFlag = tokenList[ 1 ] == "r",
				 argSymbol = tokenList[ 2 ],
				 symbol = tokenList[ 3 ],
			   } )
			end
			skipArgNum = 2
		     elseif arg == "-i" then
			lctagOptMap.inputFromStdin = true
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
	       if lctagOptMap.mode == "build" or lctagOptMap.mode == "depIncs" or
		  lctagOptMap.mode == "addStdInc" or lctagOptMap.mode == "cursors"
	       then
		  processMode = "conv"
	       else
		  processMode = nil
	       end
	    end

	    if processMode == "conv" then
	       if not converter then
		  if lctagOptMap.cc ~=  "gcc" then
		     converter = lctagOptMap.conf:createCompileOptionConverter(
			lctagOptMap.cc )
		  end
		  if not converter then
		     converter = gcc:createCompileOptionConverter( "gcc" )
		  end
	       end
	       local argType, argTxt = converter:convert( arg )
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

   if lctagOptMap.mode == "build" or lctagOptMap.mode == "addInc" or
      lctagOptMap.mode == "addStdInc" or lctagOptMap.mode == "cursors"
   then
      local clangIncPath = lctagOptMap.conf:getClangIncPath()

      if clangIncPath and clangIncPath ~= "" then
	 table.insert( optList, "-I" .. clangIncPath )
      end
   end

   for key, val in pairs( lctagOptMap ) do
      log( 3, "lctagOptMap", key, val )
   end
   for key, val in pairs( srcList ) do
      log( 3, "srcList", key, val )
   end
   
   return srcList, optList, lctagOptMap
end

function Option:isValidProfile()
   return self.profile
end

function Option:isValidLockLog()
   return self.lockLog
end

function Option:isValidRecordSql()
   return self.recordSql
end

function Option:isValidService()
   return self.serviceFlag
end

function Option:isIndivisualWrite()
   return self.indivisualWriteFlag
end

function Option:getUpdateTime()
   return self.updateTime
end

function Option:isValidCursors()
   return self.cursors
end

function Option:getOrgDir()
   return self.orgDir
end

function Option:getOutputForm()
   return self.outputForm
end

return Option
