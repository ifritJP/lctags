local Helper = require( 'lctags.Helper' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local Option = require( 'lctags.Option' )

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
   
   log( 1, string.format( "check modified files", #list ) )
   for index, fileInfo in ipairs( list ) do
      if db:hasTarget( fileInfo.id, target ) or fileInfo.incFlag ~= 0 then
	 local modTime = Helper.getFileModTime( db:getSystemPath( fileInfo.path ) )
	 fileId2ModTime[ fileInfo.id ] = modTime
	 if modTime then
	    if modTime > fileInfo.updateTime then
	       -- 更新時間が古い場合はマップに登録
	       if fileInfo.incFlag ~= 0 then
		  needUpdateIncFileInfoMap[ fileInfo.id ] = fileInfo
		  table.insert( needUpdateIncIdList, fileInfo.id )
		  log( 3, "modified", fileInfo.path, modTime )
	       else
		  needUpdateSrcMap[ fileInfo.id ] = fileInfo
	       end
	    else
	       uptodateFileMap[ fileInfo.id ] = fileInfo
	    end
	 end
      else
	 log( 1, string.format( "this file is not target %s", fileInfo.path ) )
      end
   end

   local needUpdateIncNum = 0
   for incId in pairs( needUpdateIncFileInfoMap ) do
      needUpdateIncNum = needUpdateIncNum + 1
   end

   -- 更新時間が新しいファイルがインクルードしているファイルの更新情報をチェックする。
   -- ソースファイルが更新されていなくても、インクルードファイルが更新されている場合は、
   -- ソースファイルを更新する必要がある

   local isNeedUpdateFunc = function( fileInfo, incId )
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
      if not modTime or modTime > incFileInfo.updateTime then
	 -- インクルードファイルの更新時間が古い場合はマップに登録
	 return true
      end
      return false
   end

   local uptodateFileNum = 0
   for fileId in pairs( uptodateFileMap ) do
      uptodateFileNum = uptodateFileNum + 1
   end
   db:mapIncludeCache(
      nil,
      function( item )
	 if needUpdateIncFileInfoMap[ item.id ] then
	    local fileInfo = uptodateFileMap[ item.baseFileId ]
	    if fileInfo then
	       -- ソースの更新日時が新しくても、
	       -- インクルードの更新日時が古い場合ソースファイルを更新対象にする
	       if isNeedUpdateFunc( fileInfo, item.id ) then
		  log( 1, "include file is modified", item.id )
		  needUpdateSrcMap[ fileInfo.id ] = fileInfo

		  log( 3, "excludeIncFile", removeId, fileInfo.path )
		  uptodateFileMap[ item.baseFileId ] = nil
		  uptodateFileNum = uptodateFileNum - 1
		  needUpdateIncFileInfoMap[ item.id ] = nil
		  needUpdateIncNum = needUpdateIncNum - 1
		  if uptodateFileNum <= 0 or needUpdateIncNum <= 0 then
		     return false
		  end
	       end
	    else
	       -- 更新対象のソースに、更新対象のインクルードが含まれている場合
	       -- インクルードを除外する
	       needUpdateIncFileInfoMap[ item.id ] = nil
	       needUpdateIncNum = needUpdateIncNum - 1
	       if needUpdateIncNum <= 0 then
		  return false
	       end
	    end
	 end
	 return true
      end
   )

   
   local needFileList = {}
   for fileId, fileInfo in pairs ( needUpdateSrcMap ) do
      table.insert( needFileList, fileInfo )
   end
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

   -- 残りのソースを追加する
   for srcId, fileInfo in pairs( needUpdateSrcId2FileInfoMap ) do
      table.insert( buildList, fileInfo )
   end

   return buildList
end


function Make:updateFor( dbPath, target, jobs, src )
   local db = DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   local dbFullPath = db:convFullpath( dbPath )
   local targetFileInfo = db:getFileInfo( nil, src )

   

   -- src 以降のファイルをピックアップ
   local list = {}
   if not targetFileInfo then
      local condition = string.format(
	 "path like '%s%%'", db:convPath( src ) )
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
      os.exit( 0 )
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
   -- 他のファイルを並列処理するように make を生成する
   for index, fileInfo in ipairs( list ) do
      local group
      if index == 1 then
	 group = "FIRST"
      else
	 group = "SRCS"
      end
      fileHandle:write(
	 string.format( "%s += %d/%d%s\n", group, index, #list,
			db:getSystemPath( fileInfo.path ) ) )
   end
   db:close()

   local opt = ""
   if Option:isValidProfile() then
      opt = "--lctags-prof"
   end

   if Option:isValidLockLog() then
      opt = "--lctags-lockLog"
   end
   
   

   fileHandle:write(
      string.format( 
	 [[
FIRST := $(addsuffix .lc, $(FIRST))
SRCS := $(addsuffix .lc, $(SRCS))

all: setup
	$(MAKE) -f %s first
	$(MAKE) -f %s second
	%s %s statusServer stop --lctags-db %s

setup:
	%s %s statusServer start --lctags-db %s &

first: $(FIRST)

second: $(SRCS)

%%.lc:
	@echo $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@[\1]  /@'))
	@%s %s updateForMake %s $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@/@')) --lctags-log %d --lctags-db %s %s
]],
	 tmpName, tmpName, arg[-1], arg[0], dbPath, arg[-1], arg[0], dbPath,
	 arg[-1], arg[0], 
	 target and ("--lctags-target " .. target ) or "",
	 log( 0, -1 ), dbPath, opt ) )

   fileHandle:close()

   -- make の実行
   log( 1, "-j:", jobs )
   if not os.execute( "make -f " .. tmpName .. " " ..
		      (jobs and ("-j" .. tostring( jobs )) or "" )) then
      os.exit( 1 )
   end
   --os.remove( tmpName )
  
end

return Make
