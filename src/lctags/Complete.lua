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
      filePath, startLine, startColmun, endLine, endColmun, value )
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
<val>%s</val>
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
      convertXmlTxt( typeName ),
      value and string.format( "%d", value ) or "", filePath or "",
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

   log( 2, "concatToken", targetIndex, startIndex, endIndex )
   
   local targetLine
   local targetColmun
   for index = startIndex, endIndex do
      local tokenInfo = tokenInfoList[ index ]
      local aColumn = tokenInfo.column
      local tokenKind = tokenInfo.kind
      local aLine = tokenInfo.line
      local token = tokenInfo.token

      if tokenKind == clang.core.CXToken_Comment then
	 -- skip
      else
	 if aLine ~= newLineNo then
	    
	    fileHandle:write( string.rep( '\n', aLine - newLineNo ) ..
				 string.rep( ' ', aColumn - 1 ) )
	    prevLine = aLine
	    newLineNo = aLine
	    newColmun = aColumn
	 else
	    fileHandle:write( string.rep( ' ', aColumn - newColmun ) )
	 end
	 fileHandle:write( token )
	 newColmun = aColumn + #token

	 if index == targetIndex then
	    targetLine = newLineNo
	    targetColmun = newColmun - 1
	    log( 2, "decide target pos", token, targetLine, targetColmun )
	 end

	 if token == "(" or token == "{" then
	    table.insert( depthList, 1, token )
	 elseif token == ")" or token == "}" then
	    table.remove( depthList, 1 )
	 end
      end
   end

   log( 2, newLineNo, newColmun, targetLine, targetColmun, prevLine )
   
   return newLineNo, newColmun, targetLine, targetColmun, prevLine
end

-- tokenInfoList に格納されているトークンから解析用ソースコードを生成する
function Complete:createSourceForAnalyzing( fileContents, tokenInfoList, mode )
   
   -- clang の解析対象にする token を決定する
   local checkIndex = #tokenInfoList
   local targetIndex = #tokenInfoList
   local prefix = nil
   local compMode = "member"

   if mode == "comp" then
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
   else
      if string.find( tokenInfoList[ checkIndex ].token, "^[%a_]+" ) then
	 -- シンボル
	 compMode = "symbol"
	 prefix = ""
	 if tokenInfoList[ checkIndex ].token == '.' or
	    tokenInfoList[ checkIndex ].token == '->' or
	    tokenInfoList[ checkIndex ].token == '::'
	 then
	    compMode = "member"
	 end
      end
   end
   log( 2, targetIndex, prefix, checkIndex )

   local statementStartIndex, incompletionIndex, syntaxStartIndex =
      self:checkStatement( tokenInfoList, nil, checkIndex )

   log( 2, "decide statementStartIndex",
	statementStartIndex, incompletionIndex, targetIndex, checkIndex )


   local fileHandle = {
      __txt = "",
      write = function( self, txt )
	 self.__txt = self.__txt .. txt
      end
   }

   -- tokenInfoList にはマクロ定義の \ の情報が入らないので、
   -- tokenInfoList からは マクロ定義を正常に再現できない。
   -- そこで、解析に直接関係のない statementStartIndex より前の箇所は
   -- 元のソースをそのまま展開する

   -- 元ソースを展開する行数を取得
   local rawEndLine = tokenInfoList[ statementStartIndex ].line - 1
   local rawEndTokenIndex = statementStartIndex - 1
   for index = statementStartIndex - 1, 1, -1 do
      if tokenInfoList[ index ].line <= rawEndLine then
	 rawEndTokenIndex = index
	 break
      end
   end

   if rawEndTokenIndex > 0 then
      -- 元ソースを展開する
      local lineNo = 1
      local crIndex = 0
      for lineNo = 1, rawEndLine do
	 crIndex = string.find( fileContents, "\n", crIndex + 1, true )
      end
      fileHandle:write( fileContents:sub( 1, crIndex ) )
   end
   log( 2, "rawEnd", rawEndLine, rawEndTokenIndex )
   

   -- 解析に不要な情報を削除して、 token を展開する
   local newLineNo = rawEndTokenIndex + 1
   local newColmun = 0

   local depthList = {}

   local targetLine
   local targetColmun
   
   local frontSyntax = ""
   newLineNo, newColmun, targetLine, targetColmun = self:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      rawEndTokenIndex + 1, statementStartIndex, newLineNo, newColmun )

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
   if not targetLine then
      targetLine = newLineNo
      targetColmun = newColmun + 1
   end

   for index, depth in ipairs( depthList ) do
      if depth == '(' then
	 fileHandle:write( ')' )
      else
	 fileHandle:write( '}' )
      end
   end

   
   log( 3, fileHandle.__txt )
   log( 3, "tgt:", targetLine, targetColmun )

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
	 "(name GLOB '%s%s*' AND parentId = %d) OR " ..
	    "name GLOB '%s%s*::[0-9]*' OR name GLOB '%s::@enum::*::%s*'",
	 frontSyntax, prefix, workNsInfo.id, frontSyntax, prefix,	 
	 (frontSyntax == "::") and "" or frontSyntax, prefix )
   else
      -- 指定位置の名前空間からアクセス可能なシンボルの検索式を生成する
      local workNsInfo
      if frontSyntax == "" then
	 pattern = string.format( 
	    "name GLOB '::%s*' AND parentId = %d OR " ..
	       "name GLOB '::%s*::[0-9]*' OR name GLOB '::@enum::*::%s*'",
	    prefix, db.rootNsId, prefix, prefix )
      else
	 workNsInfo = db:getNamespace( nil, "::" .. frontSyntax )
	 if workNsInfo then
	    pattern = string.format( 
	       "name GLOB '*%s::%s*' AND parentId = %d OR " ..
		  "name GLOB '*%s::%s*::[0-9]*' OR name GLOB '::%s::@enum::*::%s*'",
	       frontSyntax, prefix, workNsInfo.id,
	       frontSyntax, prefix, frontSyntax, prefix )
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
		  "name GLOB '%s::*%s*::[0-9]*' OR name GLOB '%s::@enum::*::%s*'",
	       prefix, workNsInfo.id,
	       fullnameBase, prefix, fullnameBase, prefix )
	 else
	    workNsInfo = db:getNamespace(
	       nil, fullnameBase .. "::" .. frontSyntax )
	    if workNsInfo then
	       if pattern ~= "" then
		  pattern = pattern .. " OR "
	       end
	       pattern = pattern .. string.format(
		  "name GLOB '*::%s*' AND parentId = %d OR " ..
		     "name GLOB '%s::%s::*%s*::[0-9f]*' OR " ..
		     "name GLOB '%s::%s::@enum::*::%s*'",
		  prefix, workNsInfo.id,
		  fullnameBase, frontSyntax, prefix,
		  fullnameBase, frontSyntax, prefix )
	    end
	 end
      end
   end

   log( 2, "pattern", pattern )
   -- パターンから名前空間を探す
   local nsList = {}
   local id2InfoSet = {}
   db:mapNamespace(
      pattern,
      function( item )
	 table.insert( nsList, item )
	 id2InfoSet[ item.id ] = item
	 log( 3, "candidate", item.name )
	 return true
      end
   )

   -- 
   local globalNsList = {}
   local enumNsId2InfoMap = {}
   local enumCount = 0
   local localNsList = {}
   for index, nsInfo in ipairs( nsList ) do
      if string.find( nsInfo.name, "::%d+$" ) then
	 if id2InfoSet[ nsInfo.parentId ] then
	    table.insert( localNsList, nsInfo )
	 end
      elseif string.find( nsInfo.name, "@enum::[%w_]+::[%w_]+$" ) then
	 enumNsId2InfoMap[ nsInfo.id ] = nsInfo
	 enumCount = enumCount + 1
      else
	 table.insert( globalNsList, nsInfo )
      end
   end
   

   log( 2, "search include" )
   -- 対象ファイルがインクルードしているファイルを取得する
   local fileInfo = db:getFileInfo( nil, path )
   local incFileIdSet = db:getIncludeCache( fileInfo )
   incFileIdSet[ fileInfo.id ] = 1
   
   print( string.format(
	     [=[
<complete>
<prefix>%s</prefix>
]=], prefix ) )

   if enumCount > 100 then
      -- enum 値の場合、DB の問い合わせを削減するため親の enum 型の定義情報を使う
      local readyCount = 0
      local enumNsId2DeclInfoMap = {}
      local readyEnumNsInfoSet = {}
      for nsId, nsInfo in pairs( enumNsId2InfoMap ) do
	 if not readyEnumNsInfoSet[ nsInfo.id ] then
	    db:mapDeclForParent(
	       nsInfo.parentId,
	       function( item )
		  if not readyEnumNsInfoSet[ item.nsId ] then
		     local workNsInfo = enumNsId2InfoMap[ item.nsId ]
		     readyCount = readyCount + 1
		     readyEnumNsInfoSet[ item.nsId ] = 1
		     if incFileIdSet[ item.fileId ] and workNsInfo then
			local fileInfo = db:getFileInfo( item.fileId )
			local simpleName = string.gsub( workNsInfo.name, "::%d+$", "" )
			simpleName = string.gsub( simpleName, ".*::([%w%-%_])", "%1" )
			print( createCandidate(
				  workNsInfo.name, simpleName,
				  item.type, "",
				  db:getSystemPath( fileInfo.path ),
				  item.line, item.column, item.endLine,
				  item.endColmun ) )
		     end
		  end
		  return true
	       end
	    )
	    if not readyEnumNsInfoSet[ nsInfo.id ] then
	       log( 1, "not exist enum", nsInfo.name )
	       table.insert( globalNsList, nsInfo )
	    end
	 end
      end
   else
      for nsId, nsInfo in pairs( enumNsId2InfoMap ) do
	 table.insert( globalNsList, nsInfo )
      end
   end


   log( 2, "search access ns", pattern, #globalNsList )
   -- 名前空間候補の中で、このファイルからアクセスできる名前を探す。
   -- ここの処理が遅い。。。
   for index, nsInfo in ipairs( globalNsList ) do
      db:mapDecl(
	 nsInfo.id,
	 function( item )
	    local fileInfo = db:getFileInfo( item.fileId )
	    log( 3, "decl", nsInfo.id, item.fileId, nsInfo.name, fileInfo.path )

	    if incFileIdSet[ item.fileId ] then
	       local fileInfo = db:getFileInfo( item.fileId )
	       local simpleName = string.gsub( nsInfo.name, "::%d+$", "" )
	       simpleName = string.gsub( simpleName, ".*::([%w%-%_])", "%1" )
	       print( createCandidate(
			 nsInfo.name, simpleName,
			 item.type, "", db:getSystemPath( fileInfo.path ),
			 item.line, item.column, item.endLine, item.endColmun ) )
	       return false
	    end
	    return true
	 end
      )
   end

   log( 2, "search static", #localNsList )
   -- static は末尾に file ID が付くので、その file がアクセス可能か探す
   for index, nsInfo in ipairs( localNsList ) do
      local fileTxt = string.gsub( nsInfo.name, ".*::(%d+)$", "%1" )
      local fileId = tonumber( fileTxt )
      log( 2, nsInfo.name, fileId )
      if incFileIdSet[ fileId ] then
	 db:mapDecl(
	    nsInfo.id,
	    function( item )
	       log( 3, "decl", nsInfo.id, item.fileId, nsInfo.name )
	       local fileInfo = db:getFileInfo( item.fileId )
	       local simpleName = string.gsub( nsInfo.name, "::%d+$", "" )
	       simpleName = string.gsub( simpleName, ".*::([%w%-%_])", "%1" )
	       print( createCandidate(
			 nsInfo.name, simpleName,
			 item.type, "", db:getSystemPath( fileInfo.path ),
			 item.line, item.column, item.endLine, item.endColmun ) )
	       return false
	    end
	 )
      end
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
      log( 2, "cxtype", typeCursor, typeCursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( typeCursor:getCursorKind() ) )
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
      log( 2, "visitFunc", aCursor:getCursorSpelling(),
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
      -- 親から直接アクセス可能なのでリストアップする
      local newList = {}
      for index, anonymousDecl in ipairs( anonymousDeclList ) do
	 clang.visitChildrenFast( anonymousDecl, visitFunc, newList, nil, 1 )
      end
      anonymousDeclList = newList
   until #newList == 0
   
   print( '</complete>' )
end   

function Complete:at( analyzer, path, line, column, target, fileContents )
   self:analyzeAt( "comp", analyzer, path, line, column, target, fileContents )
end

function Complete:inqAt( analyzer, path, line, column, target, fileContents )
   self:analyzeAt( "inq", analyzer, path, line, column, target, fileContents )
end


function Complete:analyzeAt( mode, analyzer, path, line, column, target, fileContents )

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

   if not fileContents then
      fileContents = io.open( path ):read( '*a' )
   end

   local fileTxt, targetLine, targetColmun, prefix, compMode, frontSyntax =
      self:createSourceForAnalyzing( fileContents, tokenInfoList, mode )

   if targetLine then
      --newAnalyzer:update( path, target )
      newAnalyzer:queryAtFunc(
	 path, targetLine, targetColmun, target, fileTxt,
	 function( db, targetFileId, nsInfo, declCursor, cursor )
	    local kind = cursor:getCursorKind()

	    if mode == "comp" then
	       if compMode == "symbol" or kind == clang.core.CXCursor_CompoundStmt then
		  self:completeSymbol( db, path, cursor, compMode, prefix, frontSyntax )
	       else
		  self:completeMember(
		     db, path, declCursor, cursor, compMode, prefix, frontSyntax )
	       end
	    else
	       self:inqCursor( db, path, declCursor )
	    end
	 end
      )
   else
      -- targetLine がない時は、解析する対象がない単なるシンボル補完
      local db = newAnalyzer:openDBForReadOnly()
      self:completeSymbol( db, path, nil, "symbol", prefix, frontSyntax )
   end
end

function Complete:inqCursor( db, path, cursor )
   local kind = cursor:getCursorKind()

   
   if kind == clang.core.CXCursor_EnumConstantDecl then
      local parent = cursor:getCursorSemanticParent()

      local fullnameBase = ""
      db:SymbolDefInfoListForCursor(
	 parent,
	 function( item )
	    parentNsInfo = db:getNamespace( item.nsId )
	    fullnameBase = parentNsInfo.name
	    return false
	 end
      )
      
      print( string.format(
		[=[
<complete>
<prefix>%s</prefix>
]=], "" ) )
      clang.visitChildrenFast(
      	 parent,
      	 function( aCursor, parent, anonymousDeclList, appendInfo )
	    startInfo, endInfo = db:getRangeFromCursor( aCursor )
	    local fileInfo = startInfo and db:getFileInfo(
	       nil, startInfo[ 1 ]:getFileName() )
	    local name = aCursor:getCursorSpelling()
	    local canonical = name
	    local memberKind = aCursor:getCursorKind()

      	    print( createCandidate(
      		      canonical, name, memberKind, fullnameBase,
      		      db:getSystemPath( fileInfo.path ),
      		      startInfo and startInfo[ 2 ], startInfo and startInfo[ 3 ],
      		      endInfo and endInfo[ 2 ], endInfo and endInfo[ 3 ],
		      aCursor:getEnumConstantDeclUnsignedValue() ) )
      	 end, nil, nil, 1 )
      print( '</complete>' )
   end
end

return Complete
