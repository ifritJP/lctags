local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )

local obj = {}


function obj.dot(
      db, targetId, allIdList, id2BaseIdSetMap, relIf,
      browseFlag, outputFile, imageFormat )
   if not imageFormat then
      imageFormat = "svg"
   end
   
   local fileHandle
   if not outputFile then
      if browseFlag then
	 outputFile = Helper.getTempFilename( "lctags" ) .. "." .. imageFormat
      else
	 outputFile = "lctags_graph." .. imageFormat
      end
   end
   local dotFile = outputFile .. ".dot"
   fileHandle = io.open( dotFile, "w" )

   if not fileHandle then
      log( 1, "failed to open image file" )
      os.exit( 1 )
   end
      
   fileHandle:write( "digraph relation {\n" )
   fileHandle:write(
      'rankdir = "' .. (relIf.reverseFlag and "RL" or "LR") .. '";\n' )

   local dir2FileIdListMap = {}
   local fileId2idMap = {}
   local isFileFlag = true
   local dirNum = 0
   local sameDirNum = 0
   for index, id in ipairs( allIdList ) do
      local fileId = relIf:getFileId( id )
      local dir
      if not fileId then
	 fileId = 0
	 dir = ""
      else
	 local fileInfo = db:getFileInfo( fileId )
	 dir = string.gsub( fileInfo.path, "^(.*)/[^/]+$", "%1" )
      end
      local list = dir2FileIdListMap[ dir ]
      if not list then
	 list = {}
	 dir2FileIdListMap[ dir ] = list
	 dirNum = dirNum + 1
      else
	 sameDirNum = sameDirNum + 1
      end
      table.insert( list, fileId )

      if fileId ~= id then
	 isFileFlag = false
      end

      if not fileId2idMap[ fileId ] then
	 fileId2idMap[ fileId ] = {}
      end
      
      table.insert( fileId2idMap[ fileId ], id )
   end

   local outputNode = function( id )
      local color = ''
      if targetId == id then
	 color = 'color = "red"'
      else
	 color = 'color = "green"'
	 relIf:mapBaseFor(
	    id,
	    function( item )
	       color = ''
	       return false
	 end )
      end
	    
      fileHandle:write( string.format(
			   '"%d:%s" [tooltip="%s", %s];\n',
			   id, relIf:getName( id ), relIf:getTooltip( id ), color ) )
   end
   
   if isFileFlag then
      if dirNum >= 2 and sameDirNum >= 2 then
	 local index = 0
	 for dirPath, fileIdList in pairs( dir2FileIdListMap ) do
	    if dirPath == "" then
	       outputNode( id )
	    else
	       index = index + 1
	       fileHandle:write( string.format( "subgraph cluster_0%d {", index ) )
	       fileHandle:write( string.format( 'label = "%s"; fontcolor = "red";',
						dirPath ) )
	       for index, fileId in pairs( fileIdList ) do
		  outputNode( fileId )
	       end
	       fileHandle:write( "}" )
	    end
	 end
      else
	 for index, id in ipairs( allIdList ) do
	    outputNode( id )
	 end
      end
   else
      local fileClusterFunc = function( fileId )
	 local idList = fileId2idMap[ fileId ]
	 if fileId ~= 0 then
	    fileHandle:write( string.format( "subgraph cluster_%d {", fileId ) )
	    fileHandle:write(
	       string.format( 'label = "%s"; fontcolor = "black";\n',
			      db:getFileInfo( fileId ).path ) )
	 end

	 for index, id in ipairs( idList ) do
	    outputNode( id )
	 end

	 if fileId ~= 0 then
	    fileHandle:write( "}" )
	 end
      end
      -- id が file を示さない場合、file でグルーピングする
      if dirNum >= 2 and sameDirNum >= 2 then
	 for dirPath, fileIdList in pairs( dir2FileIdListMap ) do
	    if dirPath == "" then
	       for index, fileId in pairs( fileIdList ) do
		  fileClusterFunc( fileId )
	       end
	    else
	       fileHandle:write(
		  string.format( "subgraph cluster_0%d {", fileIdList[ 1 ] ) )
	       fileHandle:write(
		  string.format( 'label = "%s"; fontcolor = "green";\n', dirPath ) )
	    
	       for index, fileId in pairs( fileIdList ) do
		  fileClusterFunc( fileId )
	       end

	       fileHandle:write( "}" )
	    end
	 end
      else
	 for fileId, idList in pairs( fileId2idMap ) do
	    fileClusterFunc( fileId )
	 end
      end
   end
   
   for id, baseIdSet in pairs( id2BaseIdSetMap ) do
      for baseId in pairs( baseIdSet ) do
	 local baseTxt = tonumber( baseId ) .. ':' .. relIf:getName( baseId )
	 local dstTxt = tonumber( id ) .. ':' .. relIf:getName( id )
	 fileHandle:write(
	    string.format(
	       '"%s" -> "%s";\n',
	       relIf.reverseFlag and baseTxt or dstTxt,
	       relIf.reverseFlag and dstTxt or baseTxt ) )
      end
   end
   fileHandle:write( "}\n" )
   fileHandle:close()

   os.execute( string.format(
   		  "dot -T%s -o %s %s", imageFormat, outputFile, dotFile ) )
   os.remove( dotFile )

   if browseFlag then
      os.execute( "firefox " .. outputFile )
      os.remove( outputFile )
   end
end


function obj.txt(
      db, targetId, allIdList, id2BaseIdSetMap, relIf, fileHandle )

   if not fileHandle then
      log( 1, "failed to open image file" )
      os.exit( 1 )
   end


   pathList = {}
   for index, baseId in pairs( allIdList ) do
      table.insert( pathList, relIf:getTooltip( baseId ) )
   end
   
   table.sort( pathList )
   for index, path in pairs( pathList ) do
      if path ~= "" then
	 Util:printLocateDirect( fileHandle, "path", path, 1, false )
      end
   end
end

return obj
