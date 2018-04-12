local Helper = require( 'lctags.Helper' )
local Option = require( 'lctags.Option' )
local log = require( 'lctags.LogCtrl' )
local clang = require( 'libclanglua.if' )

local Util = {}

function Util:getToken( filePath, line, column, fileContents )
   local txt = self:getFileLineText( filePath, line, fileContents )
   local front = string.sub( txt, 1, column )
   local tail = string.sub( txt, column + 1 )
   local num
   front, num = string.gsub( front, "^.*[^%w_]+([%w_]+)$", "%1" )
   if num == 0 then
      front = ""
   end
   tail, num = string.gsub( tail, "^([%w_]+)[^%w_]+.*$", "%1" )
   if num == 0 then
      tail = ""
   end
   return front .. tail
end

function Util:getFileLineText( filePath, line, fileContents )
   if line < 0 then
      return ""
   end
   local handle = io.open( filePath, "r" )
   if not handle then
      return "<not found>"
   end
   local lineNo = 1
   if fileContents then
      for text in string.gmatch( fileContents, "[^\n]*\n" ) do
	 if lineNo == line then
	    return string.sub( text, 1, #text - 1 )
	 end
	 lineNo = lineNo + 1
      end
   else
      repeat
	 local text = handle:read( '*l' )
	 if lineNo == line then
	    return text
	 end
	 lineNo = lineNo + 1
      until not text
   end
   return ""
end


function Util:printLocate(
      db, symbol, fileId, line, absFlag, printLine, fileContents )
   local fileInfo = db:getFileInfo( fileId )
   if fileInfo then
      if fileInfo.path == "" then
	 log( 2, "skip system file" )
	 return
      end
   else
	 log( 2, "unknown fileId", fileId )
	 return
   end
   
   local baseDir = absFlag and "" or os.getenv( "PWD" )
   local path = db:getSystemPath( fileInfo.path, baseDir )
   self:printLocateDirect( io.stdout, symbol, path, line, printLine, fileContents )
end

function Util:printLocateDirect(
      outputHandle, symbol, path, line, printLine, fileContents )
   if symbol == "" then
      symbol = "none"
   end
   -- GNU globalフォーマット
   outputHandle:write(
      string.format(
	 "%-16s %4d %-16s %s\n", symbol, line, path,
	 printLine and Util:getFileLineText( path, line, fileContents ) or "" ) )
end

function Util:calcFileDigest( path )
   if path then
   end
   local fileObj = io.open( path, "r" )
   if not fileObj then
      log( 1, "file open error", path )
      return nil
   end
   local digest = Helper.openDigest( "md5" )
   while true do
      local txt = fileObj:read( 1000 * 10 )
      if not txt then
	 return digest:fix()
      end
      digest:write( txt )
   end
end

function Util:calcTextDigest( text )
   local digest = Helper.openDigest( "md5" )
   digest:write( text )
   return digest:fix()
end

function Util:profile( func, path )
   if not Option:isValidProfile() then
      return func()
   end

   local profiler = require( 'ProFi' )
   profiler:start()
   
   local result = func()
   
   profiler:stop()
   profiler:writeReport( path )

   return result
end

function Util:mkdirWithParent( path )
   local dir = ""
   for name in string.gmatch( path, "[^/]+" ) do
      dir = dir .. "/" .. name 
      local result = Helper.mkdir( dir )
      if result ~= 0 and result ~= "EEXIST" then
	 log( 1, "mkdirWithParent", dir, result )
	 return
      end
   end
end

function Util:dumpCursorInfo( cursor, depth, prefix, cursorOffset )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()
   local level = 5

   if Option:isValidCursors() then
      level = 1
   end

   
   log( level,
	function()
	   local range = cursor:getCursorExtent()
	   local startFile, startLine, startColmn, startOffset =
	      clang.getLocation( range:getRangeStart() )
	   local endFile, endLine, endColmn, endOffset =
	      clang.getLocation( range:getRangeEnd() )
	   return string.format(
	      "%s |%s%s %s(%d) %d %s %s %s %d:%d-%d:%d",
	      string.rep( "  ", depth ),
	      prefix and (prefix .. " ") or "", txt, 
	      clang.getCursorKindSpelling( cursorKind ), cursorKind,
	      cursor:hashCursor(), "",
	      cursorOffset or "",
	      clang.isExprKind( cursorKind ),
	      startLine, startColmn, endLine, endColmn )
	end
   )
end


function Util:convertXmlTxt( txt )
   if txt == nil or txt == "" then
      return ""
   end
   txt = string.gsub( txt, '&', "&amp;" )
   txt = string.gsub( txt, '>', "&gt;" )
   txt = string.gsub( txt, '<', "&lt;" )
   txt = string.gsub( txt, '"', "&quot;" )
   txt = string.gsub( txt, "'", "&apos;" )
   return txt
end

function Util:outputResult( diagLevel, func, diagList )
   local stream = io.stdout

   stream:write( '<lctags_result>' )

   if not diagList then
      diagList = {}
   end

   if func then
      func( diagList, stream )
   end

   stream:write( '<diagnostics>' )
   for index, diag in ipairs( diagList ) do
      if diag.level >= diagLevel then
	 stream:write( '<message>' .. self:convertXmlTxt( diag.message ) .. '</message>' )
      end
   end
   stream:write( '</diagnostics>' )
   stream:write( '</lctags_result>\n' )
end


-- cursor で定義している型の基本型の cxtype を返す
function Util:getRootType( cursor )
   local kind = cursor:getCursorKind()

   if kind == clang.core.CXCursor_TypedefDecl then
      local cxtype = cursor:getTypedefDeclUnderlyingType()
      while clang.isPointerType( cxtype ) do
	 cxtype = cxtype:getPointeeType()
      end
      local resultType = cxtype:getResultType()
      local work = clang.getDeclCursorFromType( cxtype )
      if work:getCursorKind() == clang.core.CXCursor_NoDeclFound then
	 return cxtype
      end
      return Util:getRootType( work )
   end
   local cxtype = cursor:getCursorType()
   while clang.isPointerType( cxtype ) do
      cxtype = cxtype:getPointeeType()
   end
   return cxtype
end

-- cursor で定義している型の基本型の cursor を返す
function Util:getRootTypeCursor( cursor )
   local kind = cursor:getCursorKind()

   if kind == clang.core.CXCursor_TypedefDecl then
      cxtype = cursor:getTypedefDeclUnderlyingType()
      local work = clang.getDeclCursorFromType( cxtype )
      if work:getCursorKind() == clang.core.CXCursor_NoDeclFound then
	 return cursor
      end
      return self:getRootTypeCursor( work )
   end
   return cursor
end


function Util:getTokenKindSpelling( kind )
   if kind == clang.core.CXToken_Punctuation then
      return "CXToken_Punctuation"
   elseif kind == clang.core.CXToken_Keyword then
      return "CXToken_Keyword"
   elseif kind == clang.core.CXToken_Identifier then
      return "CXToken_Identifier"
   elseif kind == clang.core.CXToken_Literal then
      return "CXToken_Literal"
   elseif kind == clang.core.CXToken_Comment then
      return "CXToken_Comment"
   end
   return "None"
end

function Util:getFileList( path, mode, checkFunc )
   local extra = ""
   if mode == "file" then
      extra = "-type f"
   elseif mode == "dir" then
      extra = "-type d"
   end
      

   local fileList = {}
   local pipe = io.popen( string.format( "find %s %s", path, extra ) )
   while true do
      local fullPath = pipe:read( '*l' )
      if not fullPath then
	 break
      end
      if (not checkFunc) or (checkFunc and checkFunc( fullPath )) then
	 table.insert( fileList, fullPath )
      end
   end
   return fileList
end

function Util:getSameDirFile( src, basename )
   return Option:getSameDirFile( src, basename )
end

return Util

