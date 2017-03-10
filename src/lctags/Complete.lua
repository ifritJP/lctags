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
		     return statementStartIndex, parenIndex
		  end
	       end
	       
	       log( 2, "this is in condition. not support" )
	       return nil, index
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
	    return nil, index
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
      elseif string.find( token, "[%+%-%/%%%^%*%&%|%>%<%=%(,;]" ) then
	 -- + - * / % ^ & | > < = ( , ; の場合
	 log( 2, "search end", statementStartIndex, index + 1 )
	 return statementStartIndex, index + 1
      else
	 index = index - 1
      end
   end

   -- statementStartIndex 〜 checkIndex の間に式はない
   log( 2, "checkStatement end", statementStartIndex, checkIndex )
   
   return checkIndex
end


function Complete:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      startIndex, endIndex, newLineNo, newColmun )
   local prevLine = nil
   local targetLine
   local targetColmun
   for index = startIndex, endIndex do
      local tokenInfo = tokenInfoList[ index ]
      local aColumn = tokenInfo.column
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
      else
	 fileHandle:write( token )
      end
      newColmun = newColmun + #token

      if index == targetIndex then
	 targetLine = newLineNo
	 targetColmun = newColmun
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

   local statementStartIndex, incompletionIndex =
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
   newLineNo, newColmun, targetLine, targetColmun = self:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      1, statementStartIndex, newLineNo, newColmun )

   if incompletionIndex then
      newLineNo, newColmun, targetLine, targetColmun = self:concatToken(
	 fileHandle, depthList, tokenInfoList, targetIndex,
	 incompletionIndex, checkIndex, newLineNo, newColmun )
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

   return fileHandle.__txt, targetLine, targetColmun, prefix, compMode
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


   local fileTxt, targetLine, targetColmun, prefix, compMode =
      self:createSourceForAnalyzing( tokenInfoList )


   --newAnalyzer:update( path, target )

   if compMode == "symbol" then
      print( string.format(
		[=[
<complete>
<prefix>%s</prefix>
]=], prefix ) )

      local db = analyzer:openDBForReadOnly()

      -- 対象ファイルがインクルードしているファイルを取得する
      local fileInfo = db:getFileInfo( nil, path )
      local fileId2IncFileInfoListMap = {}
      local fileId2FileInfoMap = {}
      db:getIncludeFileSet(
	 fileId2FileInfoMap, fileInfo, fileId2IncFileInfoListMap )
      fileId2FileInfoMap[ fileInfo.id ] = fileInfo

      -- ルートの名前空間を探す
      local nsList = {}
      db:mapNamespace(
	 string.format( "name GLOB '::%s*' AND parentId = %d",
			prefix, db.rootNsId ),
	 function( item )
	    table.insert( nsList, item )
	    return true
	 end
      )

      -- ルートの名前空間で、このファイルからアクセスできる名前を探す
      for index, nsInfo in ipairs( nsList ) do
	 db:mapDecl(
	    nsInfo.id,
	    function( item )
	       if fileId2FileInfoMap[ item.fileId ] then
		  print( string.format( '<candidate>%s</candidate>', nsInfo.name ) )
		  return false
	       end
	       return true
	    end
	 )
      end
      print( '</complete>' )
      return
   end
   
   newAnalyzer:queryAtFunc(
      path, targetLine, targetColmun, target, fileTxt,
      function( db, targetFileId, nsInfo, declCursor, cursor )

	 local declKind = declCursor:getCursorKind()
	 local kind = cursor:getCursorKind()

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
	      declCursor:getCursorSpelling() )
	 nsInfo = db:getNamespaceFromCursor( typeCursor )

	 print( string.format(
		   [=[
<complete>
<prefix>%s</prefix>
]=], prefix or "" ) )
	 
	 db:mapNamespace(
	    string.format( 
	       "parentId = %d AND name like '%%::%s%%'", nsInfo.id, prefix or "" ),
	    function( item )
	       print( string.format( '<candidate>%s</candidate>', item.name ) )
	       return true
	    end
	 )
	 print( '</complete>' )
      end
   )
end


return Complete
