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


function Complete:createForAnalyzingSrc(
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

function Complete:at( analyzer, path, line, column, target )

   local analyzerForTokenize = analyzer:newAs( false, false )
   local unit, compileOp, newAnalyzer = analyzerForTokenize:createUnit( path, target )


   -- local compResults = unit:codeCompleteAt( path, line, column, nil, 0, 0 )

   -- print( compResults.NumResults, compResults.Results )

   -- local compArray = clang.mkCXCompletionResultArray(
   --    compResults.Results, compResults.NumResults )

   -- for index = 0, compArray:getLength() - 1 do
   --    local comp = compArray:getItem( index )
   --    print( comp, clang.getCursorKindSpelling( comp.CursorKind ),
   -- 	     comp.CompletionString )

   --    local compStr = clang.CXCompletionString:new( comp.CompletionString )
   --    for chunkIndex = 0, compStr:getNumCompletionChunks() do
   -- 	 print( compStr:getCompletionChunkText( chunkIndex ),
   -- 		compStr:getCompletionAnnotation( chunkIndex ) )
   --    end
   -- end


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

   -- clang の解析対象にする token を決定する
   local checkIndex = #tokenInfoList
   local targetIndex = #tokenInfoList
   local memberAccessFlag = nil
   if tokenInfoList[ checkIndex ].token == '.' or
      tokenInfoList[ checkIndex ].token == '->' or
      tokenInfoList[ checkIndex ].token == '::'
   then
      memberAccessFlag = ""
      targetIndex = checkIndex - 1
      checkIndex = checkIndex - 1
   elseif string.find( tokenInfoList[ checkIndex ].token, "^[%a_]+" ) then
      -- シンボル
      memberAccessFlag = tokenInfoList[ checkIndex ].token
      targetIndex = checkIndex - 1

      checkIndex = checkIndex - 1
      if tokenInfoList[ checkIndex ].token == '.' or
	 tokenInfoList[ checkIndex ].token == '->' or
	 tokenInfoList[ checkIndex ].token == '::'
      then
	 targetIndex = checkIndex - 1
	 checkIndex = checkIndex - 1
      end
   end
   log( 2, targetIndex, memberAccessFlag, checkIndex )

   local statementStartIndex, incompletionIndex =
      self:checkStatement( tokenInfoList, nil, checkIndex )

   log( 2, "decide statementStartIndex",
	statementStartIndex, incompletionIndex, targetIndex )

   --local fileHandle = io.stdout
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
   newLineNo, newColmun, targetLine, targetColmun = self:createForAnalyzingSrc(
      fileHandle, depthList, tokenInfoList, targetIndex,
      1, statementStartIndex, newLineNo, newColmun )

   if incompletionIndex then
      newLineNo, newColmun, targetLine, targetColmun = self:createForAnalyzingSrc(
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

   newAnalyzer:update( path, target )
   
   newAnalyzer:queryAtFunc(
      path, targetLine, targetColmun, target, fileHandle.__txt,
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
	 
	 db:mapNamespace(
	    "parentId = " .. tonumber( nsInfo.id ),
	    function( item )
	       print( item.name )
	       return true
	    end
	 )
      end
   )
end


return Complete
