local Helper = require( 'lctags.Helper' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Option = require( 'lctags.Option' )
local Analyzer = require( 'lctags.Analyzer' )
local Util = require( 'lctags.Util' )

local Make = {}


--- list の中から更新が必要なファイルリストを返す
--
-- ヘッダファイルの更新が必要な場合、
-- そのファイルをインクルードしているソースファイルをリストに含める
local function searchNeedUpdateFiles( db, list, target )

   -- 更新対象のリストのファイル更新時間から更新が必要かどうかチェック
   if not target then
      target = ""
   end

   if not db:hasTarget( nil, target ) then
      log( 1, string.format( "The files does not have target (%s)", target ) )
      return {}
   end

   if #list == 1 then
      -- ファイル 1 つの場合は、面倒な確認はせずにそのまま返す
      local fileInfo = list[ 1 ]
      local targetInfo = db:getTargetInfo( fileInfo.id, target )
      if targetInfo and fileInfo.incFlag == 0 then
	 return list
      end
   end


   -- 更新が必要な古いファイル
   local needUpdateSrcMap = {}
   -- fileId -> ファイルの更新時間マップ
   local fileId2ModTime = {}
   -- 更新が必要なインクルードファイルのセット
   local needUpdateIncFileInfoMap = {}
   -- 更新が必要なインクルードファイルのリスト
   local needUpdateIncIdList = {}
   -- 情報が新しいファイル
   local uptodateFileMap = {}
   
   local dependFileInfoMap = {}


   -- fileInfo の更新時間を調べて、古い場合は次の処理を行なう。
   --  - ヘッダの場合 needUpdateIncFileInfoMap に追加
   --  - ソースの場合 needUpdateSrcMap に追加
   -- 新しい場合、そのファイルがインクルードしているヘッダを dependFileInfoMap に追加
   local checkModTime = function( fileInfo, validCheckIncCache )
      -- log( 3, "check path", fileInfo.path )
      if not uptodateFileMap[ fileInfo.id ] then
	 local targetInfo = db:getTargetInfo( fileInfo.id, target )
	 local addToDependFlag = false
	 if targetInfo then
	    local modTime = Helper.getFileModTime( db:getSystemPath( fileInfo.path ) )
	    fileId2ModTime[ fileInfo.id ] = modTime
	    if modTime then
	       if not targetInfo or modTime > targetInfo.updateTime then
		  -- 更新時間が古い場合はマップに登録
		  log( 3, "modified", fileInfo.path, modTime,
		       targetInfo and targetInfo.updateTime or "none" )
		  if fileInfo.incFlag ~= 0 then
		     needUpdateIncFileInfoMap[ fileInfo.id ] = fileInfo
		     table.insert( needUpdateIncIdList, fileInfo.id )
		  else
		     needUpdateSrcMap[ fileInfo.id ] = fileInfo
		  end
	       else
		  uptodateFileMap[ fileInfo.id ] = fileInfo
		  if validCheckIncCache then
		     -- 更新情報が新しい場合、
		     -- そのファイルがインクルードしているファイルをチェック対象に入れる。
		     -- mapIncludeCache だと遅いので、mapIncRefListFrom に切り替え
		     -- db:mapIncludeCache(
		     -- fileInfo
		     db:mapIncRefListFrom(
		  	fileInfo.id,
		  	function( item )
		  	   if not fileId2ModTime[ fileInfo.id ] and
			      not dependFileInfoMap[ item.id ]
			   then
		  	      local incFileInfo = db:getFileInfo( item.id )
		  	      -- log( 3, "add needUpdateIncFileInfoMap", incFileInfo.path )
		  	      dependFileInfoMap[ item.id ] = incFileInfo
			      addToDependFlag = true
		  	   end
		  	   return true
		  	end
		     )
		  end
	       end
	    end
	 else
	    log( 2, string.format( "this file is not target %s", fileInfo.path ) )
	 end
      end
      return addToDependFlag
   end

   
   log( 1, "check modified files", #list )
   for index, fileInfo in ipairs( list ) do
      if ( index % 100 ) == 0 then
	 log( 1, string.format( "checking pass 1 -- %d/%d", index, #list ), os.date() )
      end
      checkModTime( fileInfo, true )
   end

   -- include ファイルからさらに include しているファイルについてもチェック
   local checkedFileIdSet = {}
   local count = 2
   while true do
      local addToDependFlag = false
      local checkList = {}
      for fileId, fileInfo in pairs( dependFileInfoMap ) do
	 if not checkedFileIdSet[ fileId ] then
	    table.insert( checkList, fileInfo )
	    checkedFileIdSet[ fileId ] = true
	 end
      end
      for index, fileInfo in ipairs( checkList ) do
	 if ( index % 100 ) == 0 then
	    log( 1, string.format( "checking pass %d -- %d/%d",
				   count, index, #checkList ), os.date() )
	 end
	 if checkModTime( fileInfo, false ) then
	    addToDependFlag = true
	 end
      end
      if not addToDependFlag then
	 break
      end
      count = count + 1
   end

   local needUpdateIncNum = 0
   for incId in pairs( needUpdateIncFileInfoMap ) do
      needUpdateIncNum = needUpdateIncNum + 1
   end
   log( 2, "needUpdateIncNum: start", needUpdateIncNum )

   -- 更新時間が新しいファイルがインクルードしているファイルの更新情報をチェックする。
   -- ソースファイルが更新されていなくても、インクルードファイルが更新されている場合は、
   -- ソースファイルを更新する必要がある

   local isNeedUpdateFunc = function( fileInfo, incId, targetInfo )
      if incId == DBCtrl.systemFileId then
	 return false
      end
      if needUpdateIncFileInfoMap[ incId ] then
	 return true
      end
      local incFileInfo = db:getFileInfo( incId )
      modTime = fileId2ModTime[ incId ]
      if not modTime then
	 modTime = Helper.getFileModTime( db:getSystemPath( incFileInfo.path ) )
	 fileId2ModTime[ incId ] = modTime
      end
      if not modTime or modTime > targetInfo.updateTime then
	 -- インクルードファイルの更新時間が古い場合はマップに登録
	 return true
      end
      return false
   end

   local uptodateFileNum = 0
   for fileId in pairs( uptodateFileMap ) do
      uptodateFileNum = uptodateFileNum + 1
   end
   local fileId2TargetInfoMap = {}

   db:mapTargetInfo(
      string.format( "target = '%s'", target ),
      function( item )
	 fileId2TargetInfoMap[ item.fileId ] = item
	 return true
      end
   )

   -- 編集されていないファイルから、更新されているファイルをインクルードしている場合、
   -- 編集されていないファイルを更新対象にする。
   -- これは list に指定されているものを対象にする。
   db:mapIncludeCache(
      nil,
      function( item )
	 if needUpdateIncFileInfoMap[ item.id ] then
	    if uptodateFileMap[ item.baseFileId ] then
	       log( 3, "check needUpdateIncFileInfoMap",
		    needUpdateIncFileInfoMap[ item.id ].path,
		    uptodateFileMap[ item.baseFileId ].path )
	       -- 編集されていないファイルからインクルードしている
	       if not needUpdateSrcMap[ item.baseFileId ] then
		  -- インクルードしているファイルが更新が必要になっていない
		  fileInfo = db:getFileInfo( item.baseFileId )
		  -- インクルードの更新日時が古い場合ソースファイルを更新対象にする
		  if isNeedUpdateFunc( fileInfo, item.id,
				       fileId2TargetInfoMap[ item.id ] ) then
		     log( 1, "include file is modified", item.id, fileInfo.path )
		     needUpdateSrcMap[ fileInfo.id ] = fileInfo
		  end
	       end
	       -- needUpdateIncFileInfoMap[ item.id ] = nil
	       -- needUpdateIncNum = needUpdateIncNum - 1
	       -- if needUpdateIncNum <= 0 then
	       -- 	  return false
	       -- end
	    end
	 end
	 return true
      end,
      true
   )

   log( 2, "needUpdateIncNum: fix", needUpdateIncNum )


   
   local needFileList = {}
   for fileId, fileInfo in pairs ( needUpdateSrcMap ) do
      table.insert( needFileList, fileInfo )
   end

   -- 更新が必要なインクルードファイルを更新するために、
   -- 任意のソースファイルを更新対象にする。
   for fileId, fileInfo in pairs ( needUpdateIncFileInfoMap ) do
      local srcFileInfo = db:getSrcForIncOne( fileInfo, target )
      if srcFileInfo then
	 table.insert( needFileList, srcFileInfo )
	 log( 1, "needUpdateIncFileInfoMap:", srcFileInfo.path )
      end
   end
   return needFileList, needUpdateIncIdList
end


-- 共通インクルードファイルの更新を効率良く行なうために、
-- なるべくインクルードファイルが被らないように解析できるように順番を決定する
--
-- @param list 更新が必要なソースファイルリスト
-- @param needUpdateIncIdList 更新が必要なインクルードファイルリスト
function Make:decideOrderForMake( db, list, needUpdateIncIdList )
   if needUpdateIncIdList == 0 then
      -- 更新対象にインクルードファイルがない場合は list をそのまま返す
      log( 2, "decideOrderForMake: has no inc" )
      return list
   end

   local needUpdateIncIdSet = {}
   for index, incId in ipairs( needUpdateIncIdList ) do
      needUpdateIncIdSet[ incId ] = 1
   end

   local needUpdateSrcMap = {}
   for fileId, fileInfo in pairs( list ) do
      needUpdateSrcMap[ fileId ] = fileInfo
   end
   -- list がインクルードしているファイルを needUpdateIncIdSet に追加する
   log( 2, "decideOrderForMake: setup needUpdateIncIdSet" )
   db:mapIncludeCache(
      nil,
      function( item )
	 if needUpdateSrcMap[ item.baseFileId ] then
	    needUpdateIncIdSet[ item.id ] = 1
	 end
	 return true
      end
   )
   

   local needUpdateSrcId2FileInfoMap = {}
   for index, fileInfo in ipairs( list ) do
      needUpdateSrcId2FileInfoMap[ fileInfo.id ] = fileInfo
   end
      
   -- ファイルID -> ソースファイルの情報
   local fileId2SrcInfoMap = {}
   -- インクルードファイル ID -> インクルードファイルの情報
   local incId2IncInfoMap = {}
   log( 2, "decideOrderForMake: setup information" )
   db:mapIncludeCache(
      nil,
      function( item )
	 if needUpdateIncIdSet[ item.id ] and
	    needUpdateSrcId2FileInfoMap[ item.baseFileId ]
	 then
	    local srcInfo = fileId2SrcInfoMap[ item.baseFileId ]
	    if not srcInfo then
	       srcInfo = { incIdSet = {} }
	       fileId2SrcInfoMap[ item.baseFileId ] = srcInfo
	    end
	    srcInfo.incIdSet[ item.id ] = 1
	    
	    local incInfo = incId2IncInfoMap[ item.id ]
	    if not incInfo then
	       incInfo = { count = 0, srcIdSet = {} }
	       incId2IncInfoMap[ item.id ] = incInfo
	    end
	    incInfo.count = incInfo.count + 1
	    incInfo.srcIdSet[ item.baseFileId ] = 1
	 end
	 return true
      end
   )

   -- 参照数の多いインクルードをより多く参照しているソースを探すために
   -- インクルード数をカウントする
   log( 2, "decideOrderForMake: search maxRef" )
   local procIndex = 0
   for fileId, srcInfo in pairs( fileId2SrcInfoMap ) do
      srcInfo.count = 0
      for incId in pairs( srcInfo.incIdSet ) do
	 srcInfo.count = srcInfo.count + incId2IncInfoMap[ incId ].count
      end
   end

   local buildList = {}

   while true do
      local maxNum
      local maxSrcId
      for fileId, srcInfo in pairs( fileId2SrcInfoMap ) do
	 if not maxNum or maxNum < srcInfo.count then
	    maxSrcId = fileId
	    maxNum = srcInfo.count
	 end
      end
      
      if not maxSrcId or maxNum <= 2 then
	 break
      end

      table.insert( buildList, ( db:getFileInfo( maxSrcId ) ) )
      needUpdateSrcId2FileInfoMap[ maxSrcId ] = nil

      -- maxSrcId から参照しているインクルードファイルを除外する。
      log( 2, "decideOrderForMake", maxNum, buildList[ #buildList ].path )
      
      for incId in pairs( fileId2SrcInfoMap[ maxSrcId ].incIdSet ) do
	 local incInfo = incId2IncInfoMap[ incId ]
	 for srcId in pairs( incInfo.srcIdSet ) do
	    if srcId ~= maxSrcId then
	       srcInfo = fileId2SrcInfoMap[ srcId ]
	       srcInfo.count = srcInfo.count - incInfo.count
	       srcInfo.incIdSet[ incId ] = nil

	       if not maxNum or maxNum < srcInfo.count then
		  nextSrcId = srcId
		  maxNum = srcInfo.count
	       end
	    end
	 end
      end
      fileId2SrcInfoMap[ maxSrcId ] = nil
   end

   -- 残りのソースを追加する。
   -- ディレクトリがバラけた方がインクルードや解析時間の傾向が分散して
   -- 並列実行する際に効率が良くなるので、ランダムに追加する。
   restFileInfoList = {}
   for srcId, fileInfo in pairs( needUpdateSrcId2FileInfoMap ) do
      table.insert( restFileInfoList, fileInfo )
   end
   while #restFileInfoList > 0 do
      local index = math.random( #restFileInfoList )
      local fileInfo = restFileInfoList[ index ]
      table.insert( buildList, fileInfo )
      table.remove( restFileInfoList, index )
   end
   
   return buildList
end


function Make:updateFor( dbPath, target, jobs, src )
   local db = DBCtrl:open( dbPath, true, Util:getcwd() )
   local dbFullPath = db:convFullpath( dbPath )
   local targetFileInfo = db:getFileInfo( nil, src )

   

   -- src 以降のファイルをピックアップ
   local list = {}
   if not targetFileInfo then
      local condition = string.format(
	 "path like '%s%%' escape '$'", db:convPath( src ) )
      db:mapFile(
	 condition,
	 function( item )
	    table.insert( list, item )
	    return true
	 end
      )
   else
      table.insert( list, targetFileInfo )
   end
   if #list == 0 then
      log( 1, 'not match' )
      os.exit( 1 )
   end

   -- ピックアップしたファイルから、更新が必要なものを検索
   list, needUpdateIncIdList = searchNeedUpdateFiles( db, list, target )
   if #list == 0 then
      log( 1, 'all file is analyzed already' )
      return
   end

   if #list == 1 then
      -- ファイルが 1 つの場合は、make せずに直接更新する
      local analyzer = Analyzer:new(
	 dbPath, false, false,
	 db:convFullpath( db:getSystemPath( list[1].currentDir ) ))

      local fullPath = db:convFullpath( db:getSystemPath( list[1].path ) )
      db:close()
      analyzer:update( fullPath, target )
      return
   end


   list = self:decideOrderForMake( db, list, needUpdateIncIdList )


   -- list の情報を更新する makefile を生成
   local tmpName = Helper.getTempFilename( "lcmake" )
   local fileHandle = io.open( tmpName, "w" )
   if not fileHandle then
      log( 1, 'failed to open', tmpName )
      os.exit( 1 )
   end

   -- 最初のファイルは共通インクルードが多いので並列に処理せずにビルドし、
   -- 他のファイルを並列処理するように make を生成する。
   -- 共通インクルードが多い序盤に並列度が高いと無駄になるので
   -- 並列度を下げる。
   for index, fileInfo in ipairs( list ) do
      local group
      if index == 1 then
	 group = "FIRST"
      elseif index == 2 then
	 group = "SECOND"
      elseif ( index < (#list / 20) ) and ( index < 100 ) then
	 group = "THIRD"
      else
	 group = "SRCS"
      end
      fileHandle:write(
	 string.format( "%s += %d/%d%s\n", group, index, #list,
			db:getSystemPath( fileInfo.path ) ) )
   end
   db:close()

   local opt = "--lctags-uptime " .. tostring( Helper.getCurrentTime() )
   if Option:isValidProfile() then
      opt = opt .. " --lctags-prof"
   end
   if Option:isValidLockLog() then
      opt = opt .. " --lctags-lockLog"
   end
   if Option:isValidRecordSql() then
      opt = opt .. " --lctags-recSql"
   end
   if Option:isValidRecordDigestSrc() then
      opt = opt .. " --lctags-digestRec"
   end
   if Option:isValidLockLog() then
      opt = opt .. " --lctags-recSql"
   end
   if Option:isIndivisualWrite() then
      opt = opt .. " --lctags-indiv"
   end

   if not jobs then
      jobs = 1
   end
   

   fileHandle:write(
      string.format( 
	 [[
.PHONY: all build setup first second third other

FIRST := $(addsuffix .lc, $(FIRST))
SECOND := $(addsuffix .lc, $(SECOND))
THIRD := $(addsuffix .lc, $(THIRD))
SRCS := $(addsuffix .lc, $(SRCS))

#SRV=--lctags-srv

all:

build:
	$(MAKE) -f %s first
	$(MAKE) -f %s second
	$(MAKE) -j%d -f %s third
	$(MAKE) -j%d -f %s other

all: setup
	-$(MAKE) -f %s build
	@echo server stop
	%s %s cancel-kill --lctags-db %s
	%s %s statusServer stop --lctags-db %s
ifdef SRV
	%s %s server stop --lctags-db %s
endif


setup:
	%s %s statusServer start --lctags-db %s 2>&1 | tee -a %s &
	%s %s statusServer wait --lctags-db %s
ifdef SRV
	%s %s server start --lctags-db %s &
endif

first: $(FIRST)

second: $(SECOND)

third: $(THIRD)

other: $(SRCS)

%%.lc:
	@echo $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@[\1]  /@'))
	-@%s %s updateForMake %s $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@/@')) --lctags-log %d --lctags-db %s %s $(SRV) || echo "=== MAKE NG === $@"
	@%s %s check-kill > /dev/null || exit 1
]],
	 -- build
	 tmpName, tmpName, math.floor(((jobs>=3) and jobs or 3) / 3), tmpName,
	 jobs, tmpName,
	 -- all
	 tmpName,
	 arg[-1], arg[0], dbPath, arg[-1], arg[0], dbPath,
	 arg[-1], arg[0], dbPath,
	 -- setup
	 arg[-1], arg[0], dbPath, Helper.getTempFilename( "lctagsStatus" ),
	 arg[-1], arg[0], dbPath, arg[-1], arg[0], dbPath,
	 -- %%.lc
	 arg[-1], arg[0], 
	 target and ("--lctags-target " .. target ) or "",
	 log( 0, -1 ), dbPath, opt, 
	 arg[-1], arg[0] ) )

   fileHandle:close()

   -- make の実行
   log( 1, "-j:", jobs )
   if not os.execute( "make -f " .. tmpName ) then
      os.exit( 1 )
   end
   --os.remove( tmpName )
  
end


function Make:lazyUpdateFor( dbPath, target, jobs, optList )
   local db = DBCtrl:open( dbPath, false, Util:getcwd() )
   local root = db:convFullpath( "." )

   suffixTable = {
      c = "c",
      [ "c++" ] = "cpp",
      [ "cc" ] = "cpp",
      [ "cxx" ] = "cpp",
      [ "cpp" ] = "cpp"
   }

   local compileOp = ""

   for index, opt in ipairs( optList ) do
      compileOp = string.format( "%s %s", compileOp, opt )
   end

   -- directory の登録。 -Idir
   local dirList = Util:getFileList( root, "dir" )
   for index, path in ipairs( dirList ) do
      if string.find( path, "%s" ) then
	 print( "skip -- ", path )
      else
	 compileOp = string.format( "%s -I%s", compileOp, path )
      end
   end

   local time = Helper.getCurrentTime()
   
   local fileList = Util:getFileList( root, "file" )
   for index, fullPath in ipairs( fileList ) do
      local suffix = string.gsub( fullPath, ".*%.([a-zA-Z%+]+)$", "%1" )
      if suffix then
	 local lang = suffixTable[ suffix:lower() ]
	 if lang then
	    local fileInfo = db:getFileInfo( nil, fullPath )
	    if not fileInfo then
	       db:addFile( fullPath, time, 0, compileOp, root, true, target, 0 )
	    end
	 end
      end
   end
   db:commit()

   Make:updateFor( dbPath, target, jobs, root )
end

return Make
