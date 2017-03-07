local Helper = require( 'lctags.Helper' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )

local Make = {}


local function searchNeedUpdateFiles( db, list )
   --- リストの中から更新が必要なファイルを検出

   -- 更新が必要な古いファイル
   local needUpdateFileMap = {}
   -- fileId -> ファイルの更新時間マップ
   local fileId2ModTime = {}
   -- 更新が必要なインクルードファイルのセット
   local needUpdateIncFileInfoSet = {}
   -- 情報が新しいファイル
   local uptodateFileList = {}
   -- 情報の更新が必要になったファイルリスト
   local appendUpdateFileList = {}

   -- 更新対象のリストのファイル更新時間から更新が必要かどうかチェック
   for index, fileInfo in ipairs( list ) do
      local modTime = Helper.getFileModTime( db:getSystemPath( fileInfo.path ) )
      fileId2ModTime[ fileInfo.id ] = modTime
      if modTime then
	 if modTime > fileInfo.updateTime then
	    -- 更新時間が古い場合はマップに登録
	    needUpdateFileMap[ fileInfo.id ] = fileInfo
	    if fileInfo.incFlag ~= 0 then
	       needUpdateIncFileInfoSet[ fileInfo.id ] = fileInfo
	    end
	 else
	    table.insert( uptodateFileList, fileInfo )
	 end
      end
   end

   -- 更新時間が新しいファイルがインクルードしているファイルの更新情報をチェックする
   for index, fileInfo in ipairs( uptodateFileList ) do
      local fileId2FlieInfoMap = {}
      db:getIncludeFileSet( fileId2FlieInfoMap, fileInfo )
      for fileId, incFileInfo in pairs( fileId2FlieInfoMap ) do
	 modTime = fileId2ModTime[ fileId ]
	 if not modTime then
	    modTime = Helper.getFileModTime( db:getSystemPath( incFileInfo.path ) )
	    fileId2ModTime[ fileId ] = modTime
	 end
	 if not modTime or modTime > incFileInfo.updateTime then
	    -- インクルードファイルの更新時間が古い場合はマップに登録
	    table.insert( appendUpdateFileList, fileInfo )
	    
	    local removeList = {}
	    for needUpdateIncFileId in pairs( needUpdateIncFileInfoSet ) do
	       if fileId2FlieInfoMap[ needUpdateIncFileId ] then
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

   -- 更新するファイルが、更新が必要なインクルードファイルを
   -- インクルードしているかどうかを確認する
   for fileId, fileInfo in pairs( needUpdateFileMap ) do
      local fileId2FlieInfoMap = {}
      db:getIncludeFileSet( fileId2FlieInfoMap, fileInfo )
      local removeList = {}
      for needUpdateIncFileId in pairs( needUpdateIncFileInfoSet ) do
	 if fileId2FlieInfoMap[ needUpdateIncFileId ] then
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
      log( 2, "needUpdateFileMap:", fileInfo.path )
   end
   for fileId, fileInfo in pairs ( appendUpdateFileList ) do
      table.insert( needFileList, fileInfo )
      log( 2, "appendUpdateFileList:", fileInfo.path )
   end
   for fileId, fileInfo in pairs ( needUpdateIncFileInfoSet ) do
      table.insert( needFileList, DBCtrl:getSrcForIncOne( fileInfo ) )
      log( 2, "needUpdateIncFileInfoSet:", fileInfo.path )
   end
   return needFileList
end


function Make:updateFor( dbPath, target, jobs, src )
   local db = DBCtrl:open( dbPath, true, os.getenv( "PWD" ) )
   local targetFileInfo = db:getFileInfo( nil, src )

   -- src 以降のファイルをピックアップ
   local list = {}
   if not targetFileInfo then
      local condition = string.format(
	 "path like '%s%%' AND incFlag = 0", db:convPath( src ) )
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

   list = searchNeedUpdateFiles( db, list )


   -- list の情報を更新する makefile を生成
   local tmpName = Helper.getTempFilename( "lcmake" )
   local fileHandle = io.open( tmpName, "w" )
   if not fileHandle then
      log( 1, 'failed to open', tmpName )
      os.exit( 1 )
   end

   for index, fileInfo in ipairs( list ) do
      fileHandle:write(
	 string.format( "SRCS += %d/%d%s\n", index, #list,
			db:getSystemPath( fileInfo.path ) ) )
   end
   db:close()

   fileHandle:write(
      string.format( 
	 [[
SRCS := $(addsuffix .lc, $(SRCS))

all: $(SRCS)

%%.lc:
	@echo $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@[\1]  /@'))
	@%s %s updateForMake %s $(patsubst %%.lc,%%,$(shell echo $@ | sed 's@^\([^/]*/[^/]*\)/@/@')) --lctags-log 2
]],
	 arg[-1], arg[0],
	 target and ("--lctags-target " .. target ) or "" ) )

   fileHandle:close()

   -- make の実行
   log( 1, "-j:", jobs )
   os.execute( "make -f " .. tmpName .. " " ..
		  (jobs and ("-j" .. tostring( jobs )) or "" ))
   os.remove( tmpName )
  
end

return Make
