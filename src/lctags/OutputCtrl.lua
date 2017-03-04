local Util = require( 'lctags.Util' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )

local obj = {}


function obj.dot(
      targetId, allIdList, id2BaseIdSetMap, relIf,
      browseFlag, outputFile, imageFormat )
   if not imageFormat then
      imageFormat = "svg"
   end
   
   local fileHandle
   if not outputFile then
      if browseFlag then
	 outputFile = Helper.getTempFilename( "lctags" )
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

   for index, id in ipairs( allIdList ) do
      fileHandle:write( string.format(
			   '"%d:%s" [tooltip="%s", %s];\n',
			   id, relIf:getName( id ), relIf:getTooltip( id ),
			   targetId == id and "color = red" or "" ) )
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
      targetId, allIdList, id2BaseIdSetMap, relIf, fileHandle )

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
