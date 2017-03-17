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
   local needUpdateFileMap = {}
   -- fileId -> ファイルの更新時間マップ
   local fileId2ModTime = {}
   -- 更新が必要なインクルードファイルのセット
   local needUpdateIncFileInfoSet = {}
   -- 更新が必要なインクルードファイルのリスト
   local needUpdateIncIdList = {}
   -- 情報が新しいファイル
   local uptodateFileList = {}
   -- 情報の更新が必要になったファイルリスト
   local appendUpdateFileList = {}
   
   for index, fileInfo in ipairs( list ) do
      log( 1, string.format( "check modified (%d/%d) %s",
			     index, #list, fileInfo.path ) )
      if db:hasTarget( fileInfo.id, target ) or fileInfo.incFlag ~= 0 then
	 local modTime = Helper.getFileModTime( db:getSystemPath( fileInfo.path ) )
	 fileId2ModTime[ fileInfo.id ] = modTime
	 if modTime then
	    if modTime > fileInfo.updateTime then
	       -- 更新時間が古い場合はマップに登録
	       if fileInfo.incFlag ~= 0 then
		  needUpdateIncFileInfoSet[ fileInfo.id ] = fileInfo
		  table.insert( needUpdateIncIdList, fileInfo.id )
		  log( 1, "modified", fileInfo.path, modTime )
	       else
		  needUpdateFileMap[ fileInfo.id ] = fileInfo
	       end
	    else
	       table.insert( uptodateFileList, fileInfo )
	    end
	 end
      else
	 log( 1, string.format( "this file is not target %s", fileInfo.path ) )
      end
   end

   -- 更新時間が新しいファイルがインクルードしているファイルの更新情報をチェックする
   for index, fileInfo in ipairs( uptodateFileList ) do
      log( 1, string.format( "check depending include files (%d/%d) %s",
			     index, #uptodateFileList, fileInfo.path ) )
      local prev = os.clock()
      local incFileIdSet = db:getIncludeCache( fileInfo )
      for fileId in pairs( incFileIdSet ) do
	 local incFileInfo = db:getFileInfo( fileId )
	 if incFileInfo.id ~= DBCtrl.systemFileId then
	    modTime = fileId2ModTime[ fileId ]
	    if not modTime then
	       modTime = Helper.getFileModTime( db:getSystemPath( incFileInfo.path ) )
	       fileId2ModTime[ fileId ] = modTime
	    end
	    if not modTime or modTime > incFileInfo.updateTime then
	       -- インクルードファイルの更新時間が古い場合はマップに登録
	       log( 1, "include file is modified", incFileInfo.path )
	       table.insert( appendUpdateFileList, fileInfo )
	       
	       local removeList = {}
	       for needUpdateIncFileId in pairs( needUpdateIncFileInfoSet ) do
		  if incFileIdSet[ needUpdateIncFileId ] then
		     table.insert( removeList, needUpdateIncFileId )
		  end
	       end
	       for index, removeId in ipairs( removeList ) do
		  log( 3, "excludeIncFile", removeId, fileInfo.path )
		  needUpdateIncFileInfoSet[ removeId ] = nil
		  needUpdateFileMap[ removeId ] = nil
	       end
	       break
	    end
	 end
      end
   end

   -- 更新するファイルが、更新が必要なインクルードファイルを
   -- インクルードしているかどうかを確認する
   for fileId, fileInfo in pairs( needUpdateFileMap ) do
      log( 1, string.format( "check include files %d %s",
			     #uptodateFileList, fileInfo.path ) )
      local incFileIdSet = db:getIncludeCache( fileInfo )
      local removeList = {}
      for needUpdateIncFileId in pairs( needUpdateIncFileInfoSet ) do
	 if fileId ~= needUpdateIncFileId and incFileIdSet[ needUpdateIncFileId ]
	 then
	    table.insert( removeList, needUpdateIncFileId )
	 end
      end
      -- インクルードしているファイルを更新から除外
      for index, removeId in ipairs( removeList ) do
	 log( 3, "excludeIncFile", removeId, fileInfo.path )
	 needUpdateIncFileInfoSet[ removeId ] = nil
	 needUpdateFileMap[ removeId ] = nil
      end
   end

   local needFileList = {}

   for fileId, fileInfo in pairs ( needUpdateFileMap ) do
      table.insert( needFileList, fileInfo )
      log( 1, "needUpdateFileMap:", fileInfo.path )
   end
   for fileId, fileInfo in pairs ( appendUpdateFileList ) do
      table.insert( needFileList, fileInfo )
      log( 1, "appendUpdateFileList:", fileInfo.path )
   end
   for fileId, fileInfo in pairs ( needUpdateIncFileInfoSet ) do
      local srcFileInfo = db:getSrcForIncOne( fileInfo, target )
      if srcFileInfo then
	 table.insert( needFileList, srcFileInfo )
	 log( 1, "needUpdateIncFileInfoSet:", srcFileInfo.path )
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
   -- list がインクルードしているファイルを needUpdateIncIdSet に追加する
   for index, fileInfo in ipairs( list ) do
      for incId in pairs( db:getIncludeCache( fileInfo ) ) do
	 needUpdateIncIdSet[ incId ] = 1
      end
   end
   

   local needUpdateSrcId2FileInfoMap = {}
   for index, fileInfo in ipairs( list ) do
      needUpdateSrcId2FileInfoMap[ fileInfo.id ] = fileInfo
   end
   

   
   -- ファイルID -> ソースファイルの情報
   local fileId2SrcInfoMap = {}
   -- インクルードファイル ID -> インクルードファイルの情報
   local incId2IncInfoMap = {}
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
   local maxNum
   local maxSrcId
   for fileId, srcInfo in pairs( fileId2SrcInfoMap ) do
      srcInfo.count = 0
      for incId in pairs( srcInfo.incIdSet ) do
	 srcInfo.count = srcInfo.count + incId2IncInfoMap[ incId ].count
      end
      if not maxNum or maxNum < srcInfo.count then
	 maxSrcId = fileId
	 maxNum = srcInfo.count
      end
   end
   if not maxSrcId then
      log( 2, "decideOrderForMake not found max" )
      return list
   end

   local buildList = {}
   table.insert( buildList, ( db:getFileInfo( maxSrcId ) ) )
   needUpdateSrcId2FileInfoMap[ maxSrcId ] = nil

   repeat
      -- maxSrcId から参照しているインクルードファイルを除外し、
      -- 次にインクルード数が多いものを見つける
      log( 2, "decideOrderForMake", maxNum, buildList[ #buildList ].path )
      
      maxNum = 0
      local nextSrcId
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
      if nextSrcId then
	 table.insert( buildList, ( db:getFileInfo( nextSrcId ) ) )
	 needUpdateSrcId2FileInfoMap[ maxSrcId ] = nil
	 maxSrcId = nextSrcId
      end
   until nextSrcId == nil

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

   fileHandle:write(
      string.format( 
	 [[
FIRST := $(addsuffix .lc, $(FIRST))
SRCS := $(addsuffix .lc, $(SRCS))

all: first
	$(MAKE) -f %s second

first: $(FIRST)

second: $(SRCS)

%%.lc:
	@echo $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@[\1]  /@'))
	@%s %s updateForMake %s $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@/@')) --lctags-log %d --lctags-db %s %s
]],
	 tmpName, arg[-1], arg[0], 
	 target and ("--lctags-target " .. target ) or "", log( 0, -1 ), dbPath,
	 Option:isValidProfile() and "--lctags-prof" or "" ) )

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
