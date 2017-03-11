local clang = require( 'libclanglua.if' )
local log = require( 'lctags.LogCtrl' )

local Complete = {}

function getTokenKindSpelling( kind )
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
end

function convertXmlTxt( txt )
   if txt == nil or txt == "" then
      return ""
   end
   txt = string.gsub( txt, '>', "&gt;" )
   return string.gsub( txt, '<', "&lt;" )
end

function createCandidate(
      canonical, simple, kind, typeName,
      filePath, startLine, startColmun, endLine, endColmun )
   if kind == clang.core.CXCursor_FunctionDecl or
      kind == clang.core.CXCursor_CXXMethod
   then
      kind = 'f'
   elseif kind == clang.core.CXCursor_FieldDecl then
      kind = 'm'
   elseif kind == clang.core.CXCursor_ClassDecl then
      kind = 'c'
   elseif kind == clang.core.CXCursor_TypedefDecl then
      kind = 't'
   elseif kind == clang.core.CXCursor_EnumDecl then
      kind = 'E'
   elseif kind == clang.core.CXCursor_EnumConstantDecl then
      kind = 'E'
   elseif kind == clang.core.CXCursor_StructDecl then
      kind = 's'
   elseif kind == clang.core.CXCursor_UnionDecl then
      kind = 'u'
   elseif kind == clang.core.CXCursor_VarDecl then
      kind = 'v'
   elseif kind == clang.core.CXCursor_MacroDefinition then
      kind = 'M'
   elseif kind == clang.core.CXCursor_Namespace then
      kind = 'n'
   end
      
   return string.format( [[
<candidate>
<canonical>%s</canonical>
<simple>%s</simple>
<kind>%s</kind>
<type>%s</type>
<file>%s</file>
<startPos>
<line>%d</line>
<column>%d</column>
</startPos>
<endPos>
<line>%d</line>
<column>%d</column>
</endPos>
</candidate>]],
      convertXmlTxt( canonical ), convertXmlTxt( simple ), kind,
      convertXmlTxt( typeName ), filePath or "",
      startLine or 0, startColmun or 0, endLine or 0, endColmun or 0 )
end


-- startIndex 以降で () の終端を検索する
function Complete:searchParenEnd( tokenInfoList, startIndex, lastIndex )
   local num = 1
   for index = startIndex, lastIndex do
      local token = tokenInfoList[ index ].token
      if token == '(' then
	 num = num + 1
      elseif token == ')' then
	 num = num - 1
	 if num == 0 then
	    return index
	 end
      end
   end
   return nil
end

-- startIndex 以前で () の開始を検索する
function Complete:searchParenBegin( tokenInfoList, startIndex, lastIndex )
   log( 2, "searchParenBegin", startIndex, lastIndex )
   local num = 1
   for index = lastIndex, startIndex, -1 do
      local token = tokenInfoList[ index ].token
      if token == '(' then
	 num = num - 1
	 if num == 0 then
	    return index
	 end
      elseif token == ')' then
	 num = num + 1
      end
   end
   return nil
end


function Complete:checkStatement( tokenInfoList, startIndex, checkIndex )
   -- 解析対象の式を決定する


   local blockStartIndex

   if not startIndex then
      -- ブロックの開始か終端を見つける
      for index = checkIndex, 1, -1 do
	 tokenInfo = tokenInfoList[ index ]
	 if tokenInfo.token == '{' or tokenInfo.token == '}' then
	    blockStartIndex = index
	    break
	 end
      end

      if not blockStartIndex or blockStartIndex > checkIndex then
	 -- ブロックがない。 つまり checkIndex は関数内にない。
	 log( 2, "out of function" )
	 return nil
      end
   else
      blockStartIndex = startIndex
   end
   
   -- blockStartIndex 〜 checkIndex までにはブロックはない
   -- blockStartIndex 〜 checkIndex の式を分離

   local statementStartIndex = blockStartIndex
   log( 2, "statementStartIndex", statementStartIndex, checkIndex )
   
   local index = blockStartIndex
   while index < checkIndex do
      local tokenInfo = tokenInfoList[ index ]
      local token = tokenInfo.token
      if tokenInfo.kind == clang.core.CXToken_Keyword and (token == "if" or token == "while" or token == "for")
      then
	 if tokenInfoList[ index + 1 ].token ~= '(' then
	    log( 1, "illegal ()", tokenInfo.line, tokenInfo.column )
	    os.exit( 1 )
	 else
	    local endIndex = self:searchParenEnd( tokenInfoList, index + 1, checkIndex )
	    if not endIndex then
	       -- 条件式の中が補完対象
	       if token == "if" or token == "while" then
		  local parenIndex = self:checkStatement(
		     tokenInfoList, index + 1, checkIndex )
		  if parenIndex then
		     if parenIndex < checkIndex then
			parenIndex = parenIndex + 1
		     end
		     return statementStartIndex, parenIndex, statementStartIndex
		  end
	       end
	       
	       log( 2, "this is in condition. not support" )
	       return nil, index, statementStartIndex
	    end
	    index = endIndex + 1
	 end
      elseif token == "(" then
	 local endIndex = self:searchParenEnd( tokenInfoList, index + 1, checkIndex )
	 if not endIndex then
	    -- カッコ中が補完対象
	    local parenIndex = self:checkStatement(
	       tokenInfoList, index + 1, checkIndex )
	    if parenIndex then
	       if parenIndex < checkIndex then
		  parenIndex = parenIndex + 1
	       end
	       return statementStartIndex, parenIndex
	    end
	    
	    log( 2, "this is in paren" )
	    return nil, index, statementStartIndex
	 end
	 index = endIndex + 1
      elseif token == ";" or token == "," then
	 statementStartIndex = index
	 log( 2, "statement end",  statementStartIndex )
	 index = index + 1
      else
	 index = index + 1
      end
   end

   -- statementStartIndex 〜 checkIndex の間に不完全な () はない。
   

   -- statementStartIndex 〜 checkIndex の間で式があれば、
   -- checkIndex を含む式の開始位置を検索する
   log( 2, "search start", checkIndex, statementStartIndex )
   index = checkIndex
   while index >= statementStartIndex do
      local tokenInfo = tokenInfoList[ index ]
      local token = tokenInfo.token
      if token == ")" then
	 local beginIndex = self:searchParenBegin(
	    tokenInfoList, statementStartIndex, index - 1 )
	 if not beginIndex then
	    log( 2, "illegal ()", tokenInfo.line, tokenInfo.column )
	    os.exit( 1 )
	 end
	 index = beginIndex - 1
	 log( 2, "search parenBegin", index )
      elseif token == "." or token == "->" then
	 index = index - 1
      elseif string.find( token, "[%+%-%/%%%^%*%&%|%>%<%=%(,;]" ) then
	 -- + - * / % ^ & | > < = ( , ; の場合
	 log( 2, "search end", statementStartIndex, index + 1 )
	 return statementStartIndex, index + 1, statementStartIndex
      else
	 index = index - 1
      end
   end

   -- statementStartIndex 〜 checkIndex の間に式はない
   log( 2, "checkStatement end", statementStartIndex, checkIndex )
   
   return checkIndex, nil, statementStartIndex + 1
end


function Complete:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      startIndex, endIndex, newLineNo, newColmun )
   local prevLine = nil
   local targetLine
   local targetColmun
   local prevKind = nil
   for index = startIndex, endIndex do
      local tokenInfo = tokenInfoList[ index ]
      local aColumn = tokenInfo.column
      local tokenEnd = 0
      local tokenKind = tokenInfo.kind
      local aLine = tokenInfo.line
      local token = tokenInfo.token
      if aLine ~= prevLine then
	 prevLine = aLine
	 fileHandle:write( '\n' .. string.rep( ' ', aColumn - 1 ) )
	 newLineNo = newLineNo + 1
	 newColmun = aColumn - 1
      end
      if tokenKind == clang.core.CXToken_Keyword or
	 tokenKind == clang.core.CXToken_Literal
      then
	 fileHandle:write( ' ' )
	 fileHandle:write( token )
	 fileHandle:write( ' ' )
	 newColmun = newColmun + 2
	 tokenEnd = -1
      elseif prevKind == clang.core.CXToken_Identifier and
	 tokenKind == clang.core.CXToken_Identifier
      then
	 fileHandle:write( ' ' )
	 fileHandle:write( token )
	 newColmun = newColmun + 1
      elseif tokenKind == clang.core.CXToken_Comment then
	 -- skip
      else
	 fileHandle:write( token )
      end
      newColmun = newColmun + #token
      prevKind = tokenKind

      if index == targetIndex then
	 targetLine = newLineNo
	 targetColmun = newColmun + tokenEnd
      end

      if token == "(" or token == "{" then
	 table.insert( depthList, 1, token )
      elseif token == ")" or token == "}" then
	 table.remove( depthList, 1 )
      end
      
   end
   return newLineNo, newColmun, targetLine, targetColmun
end

-- tokenInfoList に格納されているトークンから解析用ソースコードを生成する
function Complete:createSourceForAnalyzing( tokenInfoList )
   
   -- clang の解析対象にする token を決定する
   local checkIndex = #tokenInfoList
   local targetIndex = #tokenInfoList
   local prefix = nil
   local compMode = "member"
   if tokenInfoList[ checkIndex ].token == '.' or
      tokenInfoList[ checkIndex ].token == '->' or
      tokenInfoList[ checkIndex ].token == '::'
   then
      compMode = "member"
      prefix = ""
      targetIndex = checkIndex - 1
      checkIndex = checkIndex - 1
   elseif string.find( tokenInfoList[ checkIndex ].token, "^[%a_]+" ) then
      -- シンボル
      compMode = "symbol"
      prefix = tokenInfoList[ checkIndex ].token
      targetIndex = checkIndex - 1

      checkIndex = checkIndex - 1
      if tokenInfoList[ checkIndex ].token == '.' or
	 tokenInfoList[ checkIndex ].token == '->' or
	 tokenInfoList[ checkIndex ].token == '::'
      then
	 compMode = "member"
	 targetIndex = checkIndex - 1
	 checkIndex = checkIndex - 1
      end
   end
   log( 2, targetIndex, prefix, checkIndex )

   local statementStartIndex, incompletionIndex, syntaxStartIndex =
      self:checkStatement( tokenInfoList, nil, checkIndex )

   log( 2, "decide statementStartIndex",
	statementStartIndex, incompletionIndex, targetIndex )

   local fileHandle = {
      __txt = "",
      write = function( self, txt )
	 self.__txt = self.__txt .. txt
      end
   }

   local newLineNo = 1
   local newColmun = 0
   local prevLine = -1

   local depthList = {}

   local targetLine
   local targetColmun

   local frontSyntax = ""
   newLineNo, newColmun, targetLine, targetColmun = self:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      1, statementStartIndex, newLineNo, newColmun )

   if incompletionIndex then
      newLineNo, newColmun, targetLine, targetColmun = self:concatToken(
	 fileHandle, depthList, tokenInfoList, targetIndex,
	 incompletionIndex, checkIndex, newLineNo, newColmun )
   end

   if compMode == "member" and syntaxStartIndex then
      local workHandle = {
	 __txt = "",
	 write = function( self, txt )
	    self.__txt = self.__txt .. txt
	 end
      }
      self:concatToken(
	 workHandle, depthList, tokenInfoList, targetIndex,
	 syntaxStartIndex, checkIndex, newLineNo, newColmun )
      frontSyntax = string.gsub( workHandle.__txt, "%s", "" )
   end

   fileHandle:write( ';' )

   for index, depth in ipairs( depthList ) do
      if depth == '(' then
	 fileHandle:write( ')' )
      else
	 fileHandle:write( '}' )
      end
   end

   
   log( 3, fileHandle.__txt )
   log( 3, targetLine, targetColmun )

   return fileHandle.__txt, targetLine, targetColmun, prefix, compMode, frontSyntax
end


function Complete:completeSymbol( db, path, cursor, compMode, prefix, frontSyntax )
   if compMode == "symbol" then
      frontSyntax = ""
   end
   log( 2, "frontSyntax", frontSyntax )

   local pattern = ""
   if string.find( frontSyntax, "^::" ) then
      local workNsInfo = db:getNamespace( nil, frontSyntax )
      pattern = string.format(
	 "(name GLOB '%s%s*' AND parentId = %d) OR name GLOB '%s::@enum::*::%s*'",
	 frontSyntax, prefix, workNsInfo.id,
	 (frontSyntax == "::") and "" or frontSyntax, prefix )
   else
      -- 指定位置の名前空間からアクセス可能なシンボルの検索式を生成する
      local workNsInfo
      if frontSyntax == "" then
	 pattern = string.format( 
	    "name GLOB '::%s*' AND parentId = %d OR " ..
	       "name GLOB '::@enum::*::%s*'",
	    prefix, db.rootNsId, prefix )
      else
	 workNsInfo = db:getNamespace( nil, "::" .. frontSyntax )
	 if workNsInfo then
	    pattern = string.format( 
	       "name GLOB '*%s::%s*' AND parentId = %d OR " ..
		  "name GLOB '::%s::@enum::*::%s*'",
	       frontSyntax, prefix, workNsInfo.id, frontSyntax, prefix )
	 end
      end
      local nsTokenList = clang.getNamespaceList( cursor )
      local fullnameBase = ""
      for index, token in ipairs( nsTokenList ) do
	 fullnameBase = fullnameBase .. "::" .. token
	 local workNsInfo
	 if compMode == "symbol" then
	    workNsInfo = db:getNamespace( nil, fullnameBase )
	    if pattern ~= "" then
	       pattern = pattern .. " OR "
	    end
	    pattern = pattern .. string.format(
	       "name GLOB '*::%s*' AND parentId = %d OR " ..
		  "name GLOB '%s::@enum::*::%s*'",
	       prefix, workNsInfo.id, fullnameBase, prefix )
	 else
	    workNsInfo = db:getNamespace(
	       nil, fullnameBase .. "::" .. frontSyntax )
	    if workNsInfo then
	       if pattern ~= "" then
		  pattern = pattern .. " OR "
	       end
	       pattern = pattern .. string.format(
		  "name GLOB '*::%s*' AND parentId = %d OR " ..
		     "name GLOB '%s::%s::@enum::*::%s*'",
		  prefix, workNsInfo.id,
		  fullnameBase, frontSyntax, prefix )
	    end
	 end
      end
   end

   log( 2, "pattern", pattern )
   -- パターンから名前空間を探す
   local nsList = {}
   db:mapNamespace(
      pattern,
      function( item )
	 table.insert( nsList, item )
	 log( 3, "candidate", item.name )
	 return true
      end
   )

   -- static は末尾に数値が付くので、その名前空間を探す
   local newList = {}
   for index, nsInfo in ipairs( nsList ) do
      table.insert( newList, nsInfo )
      db:mapNamespace(
	 "parentId = " .. tostring( nsInfo.id ),
	 function( item )
	    if string.find( item.name, "^" .. nsInfo.name .. "::[%d]+$" ) then
	       table.insert( newList, item )
	       log( 3, "candidate static", item.name )
	    end
	    return true
	 end
      )
   end
   nsList = newList

   -- 対象ファイルがインクルードしているファイルを取得する
   local fileInfo = db:getFileInfo( nil, path )
   local fileId2IncFileInfoListMap = {}
   local fileId2FileInfoMap = {}
   db:getIncludeFileSet(
      fileId2FileInfoMap, fileInfo, fileId2IncFileInfoListMap )
   fileId2FileInfoMap[ fileInfo.id ] = fileInfo

   print( string.format(
	     [=[
<complete>
<prefix>%s</prefix>
]=], prefix ) )

   -- 名前空間候補から、このファイルからアクセスできる名前を探す
   for index, nsInfo in ipairs( nsList ) do
      db:mapDecl(
	 nsInfo.id,
	 function( item )
	    log( 3, "decl", nsInfo.id, item.fileId, nsInfo.name )
	    if fileId2FileInfoMap[ item.fileId ] then
	       if string.find( nsInfo.name, "::@%w+^" ) then
		  -- skip
	       else
		  local fileInfo = db:getFileInfo( item.fileId )
		  local simpleName = string.gsub( nsInfo.name, "::%d+$", "" )
		  simpleName = string.gsub( simpleName, ".*::([%w%-%_])", "%1" )
		  print( createCandidate(
			    nsInfo.name, simpleName,
			    item.type, "", db:getSystemPath( fileInfo.path ),
			    item.line, item.column, item.endLine, item.endColmun ) )
	       end
	       return false
	    end
	    return true
	 end
      )
   end
   print( '</complete>' )
end


-- member アクセス
function Complete:completeMember(
      db, path, declCursor, cursor, compMode, prefix, frontSyntax )
   local kind = cursor:getCursorKind()
   local declKind = declCursor:getCursorKind()

   local typeCursor
   
   if kind >= clang.core.CXCursor_FirstExpr and
      kind <= clang.core.CXCursor_LastExpr
   then
      typeCursor = clang.getDeclCursorFromType( cursor:getCursorType() )
   elseif declKind == clang.core.CXCursor_FunctionDecl or
      declKind == clang.core.CXCursor_ParenExpr 
   then
      typeCursor = clang.getDeclCursorFromType( declCursor:getCursorResultType() )
   else
      typeCursor = clang.getDeclCursorFromType( declCursor:getCursorType() )
   end
   log( 2, "typeCursor", typeCursor, typeCursor:getCursorSpelling(),
	clang.getCursorKindSpelling( typeCursor:getCursorKind() ),
	declCursor:getCursorSpelling() )

   while typeCursor:getCursorKind() == clang.core.CXCursor_TypedefDecl do
      local cxtype = typeCursor:getTypedefDeclUnderlyingType()
      typeCursor = cxtype:getTypeDeclaration()
   end
   
   if not prefix then
      prefix = ""
   end

   print( string.format(
	     [=[
<complete>
<prefix>%s</prefix>
]=], prefix ) )

   local function visitFunc( aCursor, parent, anonymousDeclList, appendInfo )
      local name = aCursor:getCursorSpelling()
      local memberKind = aCursor:getCursorKind()
      log( 2, aCursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( memberKind ) )
      if memberKind == clang.core.CXCursor_FieldDecl or
	 memberKind == clang.core.CXCursor_CXXMethod or
	 memberKind == clang.core.CXCursor_EnumConstantDecl
      then
	 local cxtype = aCursor:getCursorType()
	 local typeDecl = cxtype:getTypeDeclaration()
	 
	 for index, anonymousDecl in ipairs( anonymousDeclList ) do
	    if anonymousDecl:hashCursor() == typeDecl:hashCursor() then
	       table.remove( anonymousDeclList, index )
	       break
	    end
	 end
	 
	 local typeName = cxtype:getTypeSpelling()
	 local startInfo, endInfo = db:getRangeFromCursor( aCursor )
	 local fileInfo = startInfo and db:getFileInfo(
	    nil, startInfo[ 1 ]:getFileName() )
	 
	 if prefix == "" or string.find( name, prefix, 1, true ) == 1 then
	    print( createCandidate(
		      name, name, memberKind, typeName,
		      db:getSystemPath( fileInfo.path ),
		      startInfo and startInfo[ 2 ],
		      startInfo and startInfo[ 3 ],
		      endInfo and endInfo[ 2 ],
		      endInfo and endInfo[ 3 ] ) )
	 end
      elseif memberKind == clang.core.CXCursor_StructDecl or
	 memberKind == clang.core.CXCursor_UnionDecl
      then
	 if name == "" then
	    table.insert( anonymousDeclList, aCursor )
	 end
      end
   end

   local anonymousDeclList = {}
   clang.visitChildrenFast(
      typeCursor, visitFunc, anonymousDeclList, nil, 1 )
   repeat
      -- anonymous 構造体のメンバーは、
      -- 親から直接アクセス可能のでリストアップする
      local newList = {}
      for index, anonymousDecl in ipairs( anonymousDeclList ) do
	 clang.visitChildrenFast( anonymousDecl, visitFunc, newList, nil, 1 )
      end
      anonymousDeclList = newList
   until #newList == 0
   
   print( '</complete>' )
end   

function Complete:at( analyzer, path, line, column, target, fileContents )

   if not target then
      target = ""
   end

   local analyzerForTokenize = analyzer:newAs( false, false )
   local unit, compileOp, newAnalyzer =
      analyzerForTokenize:createUnit( path, target, false, fileContents )


   local tokenP = clang.mkCXTokenPArray( nil, 1 )

   local cxfile = unit:getFile( path )
   local currentLoc = unit:getLocation( cxfile, line, column + 1 )
   
   local beginPos = unit:getLocationForOffset( cxfile, 0 )
   local endPos = unit:getLocationForOffset( cxfile, 130 )
   local range = beginPos:getRange( currentLoc )
   local num = unit:tokenize( range, tokenP:getPtr() )

   local cxtokenArray = clang.mkCXTokenArray( tokenP:getItem( 0 ), num )
   local tokenInfoList = {}
   for index = 0, num - 1 do
      local cxtoken = clang.CXToken:new( cxtokenArray:getItem( index ) )
      local loc = unit:getTokenLocation( cxtoken )
      local aCxfile, aLine, aColumn, anOffset = clang.getFileLocation(
	 loc.__ptr, clang.core.clang_getFileLocation )
      local tokenKind = cxtoken:getTokenKind()
      local token = unit:getTokenSpelling( cxtoken )

      table.insert( tokenInfoList,
		    { token = token, kind = tokenKind,
		      line = aLine, column = aColumn, offset = anOffset } )
      log( 3, index + 1, getTokenKindSpelling( tokenKind ), token )
   end
   unit:disposeTokens( cxtokenArray:getPtr(), cxtokenArray:getLength() )


   local fileTxt, targetLine, targetColmun, prefix, compMode, frontSyntax =
      self:createSourceForAnalyzing( tokenInfoList )


   --newAnalyzer:update( path, target )

   newAnalyzer:queryAtFunc(
      path, targetLine, targetColmun, target, fileTxt,
      function( db, targetFileId, nsInfo, declCursor, cursor )
	 local kind = cursor:getCursorKind()
	 
	 if compMode == "symbol" or kind == clang.core.CXCursor_CompoundStmt then
	    self:completeSymbol( db, path, cursor, compMode, prefix, frontSyntax )
	 else
	    self:completeMember(
	       db, path, declCursor, cursor, compMode, prefix, frontSyntax )
	 end
      end
   )
end


return Complete
