local Util = require( 'lctags.Util' )
local clang = require( 'libclanglua.if' )
local log = require( 'lctags.LogCtrl' )
local Helper = require( 'lctags.Helper' )

local Completion = {}

local function getTokenKindSpelling( kind )
   return Util:getTokenKindSpelling( kind )
end

local function convertXmlTxt( txt )
   return Util:convertXmlTxt( txt )
end

local function createCandidate(
      canonical, simple, expand, kind, typeName, result, hash,
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
<expand>%s</expand>
<kind>%s</kind>
<type>%s</type>
<result>%s</result>
<hash>%s</hash>
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
      convertXmlTxt( canonical ), convertXmlTxt( simple ),
      convertXmlTxt( expand ), kind, convertXmlTxt( typeName ), result, hash,
      value and string.format( "%d", value ) or "", filePath or "",
      startLine or 0, startColmun or 0, endLine or 0, endColmun or 0 )
end

-- startIndex 以降から lastIndex までで endToken を検索する。
-- 途中に beginToken, endToken ペアがある場合は無視する
function Completion:searchPairEnd(
      tokenInfoList, startIndex, lastIndex, pairToken )
   local beginToken, endToken = pairToken[ 1 ], pairToken[ 2 ]
   local num = 1
   for index = startIndex, lastIndex do
      local token = tokenInfoList[ index ].token
      if token == beginToken then
	 num = num + 1
      elseif token == endToken then
	 num = num - 1
	 if num == 0 then
	    return index
	 end
      end
   end
   return nil
end


function Completion:searchParenEnd( tokenInfoList, startIndex, lastIndex )
   return self:searchPairEnd( tokenInfoList, startIndex, lastIndex, { '(', ')' } )
end

-- lastIndex 以前から startIndex の間で、閉じられていない (, { を検索する。
-- 見つけた場合はその index を返す。無い場合は nil。
function Completion:searchPairBegin(
      tokenInfoList, startIndex, lastIndex, pairToken )
   local beginToken, endToken = pairToken[ 1 ], pairToken[ 2 ]
   log( 2, "searchPairBegin", startIndex, lastIndex )
   local num = 1
   for index = lastIndex, startIndex, -1 do
      local token = tokenInfoList[ index ].token
      if token == beginToken then
	 num = num - 1
	 if num == 0 then
	    return index
	 end
      elseif token == endToken then
	 num = num + 1
      end
   end
   return nil
end

-- lastIndex 以前から startIndex の間で、閉じられていない ( を検索する。 
function Completion:searchParenBegin( tokenInfoList, startIndex, lastIndex )
   return self:searchPairBegin( tokenInfoList, startIndex, lastIndex, { '(', ')' } )
end


-- lastIndex 以前から startIndex の間で token を検索する。 
function Completion:searchToken( tokenInfoList, startIndex, lastIndex, targetToken )
   log( 2, "searchChar", startIndex, lastIndex )
   for index = lastIndex, startIndex, -1 do
      local token = tokenInfoList[ index ].token
      if token == targetToken then
	 return index
      end
   end
   return nil
end



-- startIndex 〜 checkIndex の中で解析対象の式を決定する。
--
-- startIndex 〜 checkIndex の中に文はない。
-- checkIndex から前に戻る。
-- 戻り値: startIndex, endIndex
--   startIndex: 解析対象の式の開始インデックス
function Completion:checkStatementReverse( tokenInfoList, startIndex, checkIndex )
   log( 2, "checkStatementReverse", startIndex, checkIndex )
   local index = checkIndex
   while index > startIndex do
      local tokenInfo = tokenInfoList[ index ]
      local token = tokenInfo.token
      if token == ")" or token == ']' then
	 local pairToken = { '(', ')' }
	 if token == ']' then
	    pairToken = { '[', ']' }
	 end
	 local beginIndex = self:searchPairBegin(
	    tokenInfoList, startIndex, index - 1, pairToken )
	 if not beginIndex then
	    error( string.format( "illegal paran, %d", index ) )
	 end
	 index = beginIndex - 1
      elseif token == "," then
	 return index + 1
      else
	 index = index - 1
      end
   end
   return startIndex
end

-- startIndex 〜 checkIndex の中で解析対象の式を決定する。
--
-- 戻り値: statementStartIndex, incompletionIndex, syntaxStartIndex
--  statementStartIndex: 先頭からこのインデックスまでを解析対象とする
--  incompletionIndex: このインデックスから checkIndex までを解析対象の式とする
--                    補完する prefix は含まない。
--  syntaxStartIndex: このインデックスから checkIndex までを解析対象の式とする。
--                    補完する prefix も含む。
function Completion:checkStatement( tokenInfoList, startIndex, checkIndex )


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
   
   -- blockStartIndex 〜 checkIndex までにはブロックはない。
   -- 以降で blockStartIndex 〜 checkIndex の式を分離する

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
	       -- 条件式の中が補完対象。
	       -- この場合は "(" の token 処理にまかせる。
	       index = index + 1
	    else
	       -- 補完対象は条件式内に無いので、条件式は処理対象から外す。
	       index = endIndex + 1
	    end
	 end
      elseif token == "(" or token == '[' then
	 local pairToken = { '(', ')' }
	 if token == '[' then
	    pairToken = { '[', ']' }
	 end
	 local endIndex = self:searchPairEnd(
	    tokenInfoList, index + 1, checkIndex, pairToken )
	 if not endIndex then
	    -- カッコ中が補完対象
	    local exprStartIndex = self:checkStatementReverse(
	       tokenInfoList, index + 1, checkIndex )
	    return statementStartIndex, exprStartIndex, exprStartIndex
	 else
	    index = endIndex + 1
	 end
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
	 log( 2, "search end", statementStartIndex, index + 1, token )
	 return statementStartIndex, index + 1, statementStartIndex
      else
	 index = index - 1
      end
   end

   -- statementStartIndex 〜 checkIndex の間に式はない
   log( 2, "checkStatement end", statementStartIndex, checkIndex )
   
   return checkIndex, nil, statementStartIndex + 1
end


function Completion:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      startIndex, endIndex, newLineNo, newColmun, compMode )

   log( 2, "concatToken", targetIndex, startIndex, endIndex,
	newLineNo, newColmun, compMode )

   local targetToken
   
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

	 if compMode == "expand" and index == targetIndex then
	    -- 型名だけだと clang の解析に失敗するので sizeof を付ける
	    local sizeofToken = "sizeof(" .. token .. ")"
	    fileHandle:write( sizeofToken )
	    newColmun = aColumn + #sizeofToken
	 else
	    fileHandle:write( token )
	    newColmun = aColumn + #token
	 end

	 if index == targetIndex then
	    targetLine = newLineNo
	    if compMode == "expand" then
	       targetColmun = newColmun - 2
	    else
	       targetColmun = newColmun - 1
	    end
	    targetToken = token
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
   
   return newLineNo, newColmun, targetLine, targetColmun, targetToken
end

-- tokenInfoList に格納されているトークンから解析用ソースコードを生成する
function Completion:createSourceForAnalyzing(
      fileContents, tokenInfoList, mode, targetIndex )

   log( 2, "createSourceForAnalyzing: targetIndex", targetIndex, mode )
   
   -- clang の解析対象にする token を決定する
   local orgTargetIndex = targetIndex
   local checkIndex = targetIndex
   local targetIndex = targetIndex
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
      elseif tokenInfoList[ checkIndex ].token == "case" then
	 compMode = "case"
      elseif string.find( tokenInfoList[ checkIndex ].token,
			  "[%+%-%/%%%^%*%&%|%>%<%=%(,;]" )
      then
	 -- 2項演算
	 compMode = "binOP"
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
   elseif mode == "expand" then
      compMode = "expand"
      prefix = ""
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

   local statementStartIndex, incompletionIndex, syntaxStartIndex

   if compMode == "case" then
      -- case の場合、switch に与えている値の情報から候補を挙げる。
      -- その為に switch に与えている値を解析対象とする
      local blockIndex = self:searchPairBegin(
	 tokenInfoList, 1, checkIndex - 1, { '{', '}' } )
      local switchParenIndex = self:searchPairBegin(
	 tokenInfoList, 1, blockIndex - 2, { '(', ')' } )
      local switchIndex = switchParenIndex - 1
      if tokenInfoList[ switchIndex ].token ~= "switch" then
	 error( "not found switch keyword" )
      end
      statementStartIndex = switchIndex - 1
      incompletionIndex = switchParenIndex + 1
      syntaxStartIndex = switchParenIndex + 1
      targetIndex = blockIndex - 2
      checkIndex = blockIndex - 2
   elseif compMode == "expand" and tokenInfoList[ checkIndex - 1 ].token == "case" then
      statementStartIndex = checkIndex - 2
      incompletionIndex = checkIndex
      syntaxStartIndex = checkIndex
   else
      statementStartIndex, incompletionIndex, syntaxStartIndex =
	 self:checkStatement( tokenInfoList, nil, checkIndex )
   end

   log( 2, "decide statementStartIndex",
	compMode, statementStartIndex, incompletionIndex,
	targetIndex, checkIndex, syntaxStartIndex )

   local termChar = ';'
   if tokenInfoList[ statementStartIndex ].token == ',' or
      tokenInfoList[ statementStartIndex - 1 ].token == ',' or
      ( tokenInfoList[  statementStartIndex ].token == '{' and
	   tokenInfoList[  statementStartIndex - 1 ].token ~= ')' ) or
      ( tokenInfoList[  statementStartIndex - 1 ].token == '{' and
	   tokenInfoList[  statementStartIndex - 2 ].token ~= ')' ) 
   then
      termChar = ''
   end

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

   local depthList = {}
   if rawEndTokenIndex <= 0 then
      os.exit( 0 )
   end

   -- 元ソースを展開する
   local lineNo = 1
   local crIndex = 0
   for lineNo = 1, rawEndLine do
      crIndex = string.find( fileContents, "\n", crIndex + 1, true )
   end
   fileHandle:write( fileContents:sub( 1, crIndex ) )

   for index = 1, rawEndTokenIndex do
      token = tokenInfoList[ index ].token
      if token == "(" or token == "{" then
	 table.insert( depthList, 1, token )
      elseif token == ")" or token == "}" then
	 table.remove( depthList, 1 )
      end
   end


   local tailTxt = ""
   if #depthList > 0 then
      -- C++ のクラスは、後方で宣言したメンバーにアクセスできるので、
      -- 解析位置より後のソースも解析に含める必要がある。

      local rdepthList = {}
      local illegalTailFlag = false

      for index = #tokenInfoList, orgTargetIndex, -1 do
	 token = tokenInfoList[ index ].token
	 if token == "(" or token == "{" then
	    if #rdepthList > 0 then
	       local token = tokenInfoList[ #rdepthList ].token
	       if token == "(" and prevToken == ")" or
		  token == "{" and prevToken == "}"
	       then
		  table.remove( rdepthList )
	       else
		  illegalTailFlag = true
		  break
	       end
	    else
	       illegalTailFlag = true
	       break
	    end
	 elseif token == ")" or token == "}" then
	    table.insert( rdepthList, index )
	 end
      end

      log( 2, "depthList, illegalTailFlag",
	   #depthList, #rdepthList, illegalTailFlag,
	   orgTargetIndex, #tokenInfoList )
      if #rdepthList == #depthList and not illegalTailFlag then

	 for index, token in ipairs( depthList ) do
	    local rToken = tokenInfoList[ rdepthList[ index ] ].token
	    if token == "(" and rToken == ")" or
	       token == "{" and rToken == "}"
	    then
	       --;
	    else
	       log( 2, "illegalTailFlag", rToken, token )
	    end
	 end
	 if not illegalTailFlag then
	    local targetLine = tokenInfoList[ targetIndex ].line
	    for lineNo = rawEndLine + 1, targetLine do
	       crIndex = string.find( fileContents, "\n", crIndex + 1, true )
	    end
	    if crIndex then
	       tailTxt = fileContents:sub( crIndex )
	    else
	       tailTxt = ""
	    end
	    log( 2, "tailTxt", tailTxt )
	 end
      else
	 for index, rIndex in ipairs( rdepthList ) do
	    log( 3, rIndex, tokenInfoList[ rIndex ].token )
	 end
      end
   end
   log( 2, "rawEnd", rawEndLine, rawEndTokenIndex )
   

   -- 解析に不要な情報を削除して、 token を展開する
   --local newLineNo = rawEndTokenIndex + 1
   local newLineNo = rawEndLine + 1
   local newColmun = 1

   local targetLine
   local targetColmun
   local targetToken
   
   local frontSyntax = ""
   newLineNo, newColmun, targetLine, targetColmun, targetToken = self:concatToken(
      fileHandle, depthList, tokenInfoList, targetIndex,
      rawEndTokenIndex + 1, statementStartIndex, newLineNo, newColmun, compMode )

   if incompletionIndex then
      newLineNo, newColmun, targetLine, targetColmun, targetToken = self:concatToken(
	 fileHandle, depthList, tokenInfoList, targetIndex,
	 incompletionIndex, checkIndex, newLineNo, newColmun, compMode )
   end

   if ( compMode == "member" or compMode == "expand" ) and syntaxStartIndex then
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
      log( 2, "frontSyntax", frontSyntax )
   end

   fileHandle:write( termChar )
   if not targetLine then
      targetLine = newLineNo
      targetColmun = newColmun + 1
   end

   if tailTxt == "" then
      for index, depth in ipairs( depthList ) do
	 if depth == '(' then
	    fileHandle:write( ')' )
	 else
	    fileHandle:write( '}' )
	 end
      end
   else
      fileHandle.__txt = fileHandle.__txt .. tailTxt
   end
   
   log( 4, fileHandle.__txt )
   log( 2, "tgt:", targetLine, targetColmun, prefix, targetToken )

   return fileHandle.__txt, targetLine, targetColmun, prefix, compMode, frontSyntax, targetToken
end


function Completion:completeSymbol( db, path, cursor, compMode, prefix, frontSyntax )
   print( string.format(
	     [=[
<completion>
<prefix>%s</prefix>
]=], prefix ) )

   self:completeSymbolFunc(
      db, path, cursor, compMode, prefix, frontSyntax,
      function( item, nsInfo )
	 local fileInfo = db:getFileInfo( item.fileId )
	 local simpleName = string.gsub( nsInfo.name, "::%d+$", "" )
	 simpleName = string.gsub( simpleName, ".*::([%w%-%_])", "%1" )
	 print( createCandidate(
		   nsInfo.name, simpleName, "", item.type,
		   "", "", "", db:getSystemPath( fileInfo.path ),
		   item.line, item.column, item.endLine, item.endColmun ) )
      end
   )
			    
   print( '</completion>' )
end

function Completion:getIncludeFileIdList( path, analyzer, db, target, fileContents )
   --currentFile からインクルードしているヘッダセット取得
   local unit, compileOp, newAnalyzer =
      analyzer:createUnit( path, target, false, fileContents )
   local incList = clang.getInclusionList( unit )
   local incFileIdSet = {}
   for index, incFile in ipairs( incList ) do
      local incPath = db:convFullpath( incFile:getIncludedFile():getFileName() )
      local incFileInfo = db:getFileInfo( nil, incPath )
      if incFileInfo then
	 incFileIdSet[ incFileInfo.id ] = true
      else
	 log( 1, "not found", incPath )
      end
   end
   return incFileIdSet
end


function Completion:completeSymbolFunc(
      db, path, cursor, compMode, prefix, frontSyntax, func )
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
   if pattern ~= "" then
      db:mapNamespace(
	 pattern,
	 function( item )
	    table.insert( nsList, item )
	    id2InfoSet[ item.id ] = item
	    log( 3, "candidate", item.name )
	    return true
	 end
      )
   end

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
			func( item, workNsInfo )
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
	       func( item, nsInfo )
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
	       func( item, nsInfo )
	       return false
	    end
	 )
      end
   end
end





local function getCursorAt( transUnit, filepath, line, column )
   local cxfile = transUnit:getFile( filepath )
   local location = transUnit:getLocation( cxfile, line, column )

   return transUnit:getCursor( location )
end

-- cursor で定義している型の基本型の cxtype を返す
local function getRootType( cursor )
   return Util:getRootType( cursor )
end

-- cursor で定義している型の基本型の cursor を返す
local function getRootTypeCursor( cursor )
   return Util:getRootTypeCursor( cursor )
end


local function calcDigest( txt )
   digest = Helper.openDigest( "md5" )
   digest:write( txt )
   return digest:fix()
end

local function outputCandidate( db, prefix, aCursor, hash2typeMap, anonymousDeclList )
   local name = aCursor:getCursorSpelling()
   local childKind = aCursor:getCursorKind()
   log( 2, "outputCandidate", aCursor:getCursorSpelling(),
	clang.getCursorKindSpelling( childKind ) )
   if childKind == clang.core.CXCursor_CXXBaseSpecifier then
      local superDecl = aCursor:getCursorReferenced()
      log( 2, "super", superDecl:getCursorSpelling() )
      local function visitFunc( aCursor, parent, anonymousDeclList, appendInfo )
	 outputCandidate( db, prefix, aCursor, hash2typeMap, anonymousDeclList )
      end
      clang.visitChildrenFast(
	 superDecl, visitFunc, anonymousDeclList, nil, 1 )
      return
   end
   if childKind == clang.core.CXCursor_FieldDecl or
      childKind == clang.core.CXCursor_CXXMethod or
      childKind == clang.core.CXCursor_FunctionDecl or
      childKind == clang.core.CXCursor_EnumConstantDecl
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
      local fileInfo = startInfo and startInfo[ 1 ] and db:getFileInfo(
	 nil, startInfo[ 1 ]:getFileName() )

      if prefix == "" or string.find( name, prefix, 1, true ) == 1 then

	 local typeCursor
	 local digest = ""
	 local expand = ""
	 local rootType
	 if childKind == clang.core.CXCursor_CXXMethod or
	    childKind == clang.core.CXCursor_FunctionDecl
	 then
	    typeCursor = aCursor
	    rootType = cxtype
	 else
	    typeCursor = clang.getDeclCursorFromType( cxtype )
	    rootType = getRootType( typeCursor )
	    local rootTypeSpell = rootType:getTypeSpelling()
	    if rootTypeSpell ~= "" then
	       digest = calcDigest( rootTypeSpell )
	       hash2typeMap[ digest ] = rootType
	       if clang.isPointerType( cxtype ) then
		  expand = "->"
	       else
		  expand = "."
	       end
	    end
	 end

	 local resultType = rootType:getResultType()
	 log( 2, "outputCandidate: rootType",
	      rootType:getTypeSpelling(), resultType:getTypeSpelling() )
	 if resultType.kind ~= clang.core.CXType_Invalid then
	    local str
	 
	    if childKind == clang.core.CXCursor_CXXMethod or
	       childKind == clang.core.CXCursor_FunctionDecl
	    then
	       if childKind == clang.core.CXCursor_CXXMethod then
		  str = typeName
	       else
		  str = clang.getCurosrPlainText( typeCursor.__ptr )
		  str = string.gsub( str, '{.*$', '' )
		  if (not str) or str == "" then
		     str = rootType:getTypeSpelling()
		  end
	       end
	       log( 3, "str:", str, aCursor:getCursorSpelling() )
	       str = string.gsub(
		  str, '.*' .. aCursor:getCursorSpelling() .. '%s*%(', "(" )
	       
	       -- 決め打ちで __THROW __attribute_pure__ __nonnull を除去する。。。
	       str = string.gsub( str, '%)[^()]*__THROW.*;', ")" )
	       str = string.gsub( str, '%)[^()]*__attribute_pure__.*;', ")" )
	       str = string.gsub( str, '%)[^()]*__nonnull.*;', ")" )

	       str = string.gsub( str, '.*%(', "(" )

	       
	       str = string.gsub( str, "const$", "" )
	       log( 3, "str:", str )
	    else
	       childKind = clang.core.CXCursor_FunctionDecl
	       
	       local rootCursor = getRootTypeCursor( typeCursor )
	       str = clang.getCurosrPlainText( rootCursor.__ptr )
	       if string.gmatch( str, ".*%).*%(" ) then
		  str = string.gsub( str, ";.*", "" )
		  str = string.gsub( str, '.*%).*%(', "(" )
	       else
		  str = "("
		  local argNum = 0
		  while true do
		     local argType = rootType:getArgType( argNum )
		     if argType:getTypeSpelling() == "" then
			break
		     end
		     if #str == 1 then
			str = str .. argType:getTypeSpelling()
		     else
			str = str .. "," .. argType:getTypeSpelling()
		     end
		     
		     argNum = argNum + 1
		  end
		  str = str .. ")"
	       end
	    end
	    str = string.gsub( str, ' ,', "," )
	    local args = string.gsub( str, ";.*", "" )
	    args = string.gsub( args, "[\n\t]", ' ' )
	    args = string.gsub( args, " +", " " )
	    args = string.gsub( args, " +$", "" )
	    args = string.gsub( args, "%( *void *%)$", "()" )
	    name = name .. args
	 end

	 
	 print( createCandidate(
		   name, name, expand, childKind, typeName,
		   resultType:getTypeSpelling(), digest,
		   fileInfo and db:getSystemPath( fileInfo.path ) or "",
		   startInfo and startInfo[ 2 ],
		   startInfo and startInfo[ 3 ],
		   endInfo and endInfo[ 2 ],
		   endInfo and endInfo[ 3 ] ) )
      end
   elseif childKind == clang.core.CXCursor_StructDecl or
      childKind == clang.core.CXCursor_UnionDecl
   then
      if name == "" then
	 table.insert( anonymousDeclList, aCursor )
      end
   end
end

local function outputMemberCandidate( db, prefix, typeCursor, hash2typeMap )
   log( 2, "outputMemberCandidate" )
   local function visitFunc( aCursor, parent, anonymousDeclList, appendInfo )
      outputCandidate( db, prefix, aCursor, hash2typeMap, anonymousDeclList )
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
end   

-- member アクセス
function Completion:completeMember(
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

   local frontExprTxt = string.gsub( clang.getCurosrPlainText( cursor.__ptr ), " ", "" )
   if compMode == "expand" then
      -- clang.getCurosrPlainText() が sizeof() の ) を拾ってしまうので、ここで除去
      frontExprTxt = string.gsub( frontExprTxt, "%)$", "" )
   end
   log( 2, "frontExprTxt", frontExprTxt )
   frontExprTxt = string.gsub( frontExprTxt, "[};][\n%w]*$", "" )
   if clang.isPointerType( cursor:getCursorType() ) then
      frontExprTxt = frontExprTxt .. "->"
   else
      frontExprTxt = frontExprTxt .. "."
   end

   if typeCursor:getCursorKind() == clang.core.CXCursor_ClassDecl then
      -- テンプレートクラスの場合、
      -- typeCursor ではメンバーを列挙できないので AST を解析しなおす
      clang.visitChildrenFast(
   	 typeCursor:getCursorSemanticParent(),
   	 function( aCursor, parent, anonymousDeclList, appendInfo )
   	    local cursorKind = aCursor:getCursorKind()
   	    if ( cursorKind == clang.core.CXCursor_ClassDecl or
   		    cursorKind == clang.core.CXCursor_ClassTemplate ) and
   	       aCursor:getCursorSpelling() == typeCursor:getCursorSpelling()
   	    then
   	       log( 2, "completeMember: found class" )
   	       typeCursor = aCursor
   	    end
   	    return 1
   	 end, anonymousDeclList, nil, 1 )
   end
   
   while typeCursor:getCursorKind() == clang.core.CXCursor_TypedefDecl do
      local cxtype = typeCursor:getTypedefDeclUnderlyingType()
      typeCursor = cxtype:getTypeDeclaration()
      log( 2, "cxtype", typeCursor, typeCursor:getCursorSpelling(),
	   clang.getCursorKindSpelling( typeCursor:getCursorKind() ) )
   end

   
   if not prefix then
      prefix = ""
   end


   local hash2typeMap = {}
   
   print( string.format(
	     [=[
<completion>
<prefix>%s</prefix>
<frontExpr>%s</frontExpr>
]=], convertXmlTxt( prefix ), convertXmlTxt( frontExprTxt ) ) )


   outputMemberCandidate( db, prefix, typeCursor, hash2typeMap )
   

   local knownTypeHash = {}
   while true do
      local newHash2TypeMap = {}
      local hasType = false
      for hash, cxtype in pairs( hash2typeMap ) do
	 knownTypeHash[ hash ] = cxtype
	 hasType = true
	 print( string.format( [[
<typeInfo>
<hash>%s</hash>
]], hash ) )
	 outputMemberCandidate(
	    db, "", clang.getDeclCursorFromType( cxtype ), newHash2TypeMap )
	 print( "</typeInfo>" )
      end
      if not hasType then
	 break
      end
      -- 既に出力済みの type は除外する
      hash2typeMap = {}
      for hash, cxtype in pairs( newHash2TypeMap ) do
	 if not knownTypeHash[ hash ] then
	    hash2typeMap[ hash ] = cxtype
	 end
      end
   end
   
   print( '</completion>' )
end   

function Completion:at( analyzer, path, line, column, target, fileContents )
   self:analyzeAt( "comp", analyzer, path, line, column, target, fileContents )
end

function Completion:inqAt( analyzer, path, line, column, target, fileContents, mode )
   self:analyzeAt(
      ( mode == "inq-at" ) and "inq" or mode,
      analyzer, path, line, column, target, fileContents )
end

function Completion:outputResult( level, func )
   Util:outputResult( level, func )
end

function Completion:analyzeAt(
      mode, analyzer, path, line, column, target, fileContents )

   if not target then
      target = ""
   end

   path = analyzer:convFullpath( path )
   
   local analyzerForTokenize = analyzer:newAs(
      log( -4 ) >= 2 and true or false, false )
   local unit, compileOp, newAnalyzer =
      analyzerForTokenize:createUnit( path, target, false, fileContents )
   log( 2, "fileContents", fileContents and #fileContents )

   local tokenP = clang.mkCXTokenPArray( nil, 1 )

   local cxfile = unit:getFile( path )
   local currentLoc = unit:getLocation( cxfile, line, column + 1 )

   if not fileContents then
      fileContents = io.open( path ):read( '*a' )
   end
   
   local beginPos = unit:getLocationForOffset( cxfile, 0 )
   local fileSize = #fileContents
   currentLoc = unit:getLocationForOffset( cxfile, fileSize )
   local range = beginPos:getRange( currentLoc )
   local num = unit:tokenize( range, tokenP:getPtr() )

   local cxtokenArray = clang.mkCXTokenArray( tokenP:getItem( 0 ), num )
   local tokenInfoList = {}
   local targetIndex 
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
      if aLine < line or aLine == line and aColumn <= column + 1
      then
	 targetIndex = #tokenInfoList
      end
      
      log( 4, index + 1, getTokenKindSpelling( tokenKind ), token, aLine, aColumn )
   end
   unit:disposeTokens( cxtokenArray:getPtr(), cxtokenArray:getLength() )

   local fileTxt, targetLine, targetColmun, prefix, compMode, frontSyntax, targetToken =
      self:createSourceForAnalyzing( fileContents, tokenInfoList, mode, targetIndex )

   self:outputResult(
      clang.core.CXDiagnostic_Error,
      function( diagList )
	 if targetLine then
	    newAnalyzer:queryAtFunc(
	       path, targetLine, targetColmun, target, false, fileTxt, diagList,
	       function( db, targetFileId, nsInfo, declCursor, cursor )
		  log( 2, "analyzeAt: mode", mode, compMode, declCursor, cursor )
		  local kind = cursor:getCursorKind()

		  if mode == "comp" then
		     if compMode == "symbol" or
			kind == clang.core.CXCursor_CompoundStmt
		     then
			self:completeSymbol(
			   db, path, cursor, compMode, prefix, frontSyntax )
		     elseif compMode == "binOP" then
			self:expandCursor( db, path, cursor, frontSyntax )
		     else
			self:completeMember(
			   db, path, declCursor, cursor, compMode, prefix, frontSyntax )
		     end
		  else
		     self:expandCursor( db, path, cursor, frontSyntax )
		  end
	       end
	    )
	 else
	    -- targetLine がない時は、解析する対象がない単なるシンボル補完
	    self:completeSymbol( newAnalyzer:openDBForReadOnly(),
				 path, nil, "symbol", prefix, frontSyntax )
	 end
      end
   )
end


function Completion:expandCursor( db, path, cursor, frontSyntax )
   local kind = cursor:getCursorKind()
   local orgCursor = cursor

   while true do
      log( 2, "inq", kind, clang.getCursorKindSpelling( kind ),
	   cursor:getCursorSpelling() )
      if kind == clang.core.CXCursor_DeclRefExpr or
	 kind == clang.core.CXCursor_TypeRef or
	 kind == clang.core.CXCursor_MemberRefExpr
      then
	 -- 変数、変数宣言は、その変数の型に変換
	 --cursor = cursor:getCursorDefinition()
	 cursor = cursor:getCursorReferenced()
	 kind = cursor:getCursorKind()
      elseif kind == clang.core.CXCursor_VarDecl or
	 kind == clang.core.CXCursor_FieldDecl or
	 kind == clang.core.CXCursor_ParmDecl
      then
	 -- 変数、変数宣言は、その変数の型に変換
	 local declType = getRootType( cursor )
	 log( 2, "inq3", declType:getTypeSpelling() )
	 cursor = declType:getTypeDeclaration()
	 kind = cursor:getCursorKind()
      elseif kind == clang.core.CXCursor_TypedefDecl then
	 cursor = getRootTypeCursor( cursor )
	 kind = cursor:getCursorKind()
      else
	 break
      end
   end

   log( 2, "inq2", kind, clang.getCursorKindSpelling( kind ) )

   if kind == clang.core.CXCursor_StructDecl or
      kind == clang.core.CXCursor_ClassDecl
   then
      self:completeMember(
	 db, path, cursor, orgCursor, "expand", "", frontSyntax )
   end
   
   if kind == clang.core.CXCursor_EnumConstantDecl or
      kind == clang.core.CXCursor_EnumDecl
   then
      log( 2, "enum" )
      local enumCursor = cursor
      if kind == clang.core.CXCursor_EnumConstantDecl then
	 enumCursor = cursor:getCursorSemanticParent()
      end

      local fullnameBase = ""
      db:SymbolDefInfoListForCursor(
	 enumCursor,
	 function( item )
	    parentNsInfo = db:getNamespace( item.nsId )
	    fullnameBase = parentNsInfo.name
	    return false
	 end
      )
      
      print( string.format(
		[=[
<completion>
<prefix>%s</prefix>
]=], "" ) )
      clang.visitChildrenFast(
      	 enumCursor,
      	 function( aCursor, enumCursor, anonymousDeclList, appendInfo )
	    startInfo, endInfo = db:getRangeFromCursor( aCursor )
	    local fileInfo = startInfo and db:getFileInfo(
	       nil, startInfo[ 1 ]:getFileName() )
	    local name = aCursor:getCursorSpelling()
	    local canonical = name
	    local memberKind = aCursor:getCursorKind()

      	    print( createCandidate(
      		      canonical, name, "", memberKind, fullnameBase, "", "",
      		      db:getSystemPath( fileInfo.path ),
      		      startInfo and startInfo[ 2 ], startInfo and startInfo[ 3 ],
      		      endInfo and endInfo[ 2 ], endInfo and endInfo[ 3 ],
		      aCursor:getEnumConstantDeclUnsignedValue() ) )
      	 end, nil, nil, 1 )
      print( '</completion>' )
   else
      outputCandidate( db, "", cursor, {}, {} )
   end
end

function Completion:analyzeDiagnostic( analyzer, path, target, fileContents )
   local newAnalyzer = analyzer:newAs(
      log( -4 ) >= 2 and true or false, false )
   
   path = analyzer:convFullpath( path )

   self:outputResult(
      clang.core.CXDiagnostic_Warning,
      function( diagList )
	 newAnalyzer:queryAtFunc(
	    path, 0, 0, target, false, fileContents, diagList, nil )
      end
   )
end

function Completion:callFunc( analyzer, db, currentFile, pattern, target, fileContents )
   pattern = pattern or ""
   local cond = string.format(
      "symbolDecl.type = %d OR symbolDecl.type = %d",
      clang.core.CXCursor_FunctionDecl, clang.core.CXCursor_MacroDefinition )
   cond = string.format( "(%s) AND (namespace.name LIKE '%%%s%%')", cond, pattern )

   local currentFileInfo = db:getFileInfo( nil, db:convFullpath( currentFile ) )
   if not currentFileInfo then
      error( string.format( "not register this file -- %s", currentFile ) )
   end

   local currentIncSet = {}
   -- db:mapIncludeCache(
   --    currentFileInfo,
   --    function( item )
   -- 	 currentIncSet[ item.id ] = true
   -- 	 return true
   --    end
   -- )


   --currentFile からインクルードしているヘッダセット取得
   currentIncSet = self:getIncludeFileIdList(
      currentFile, analyzer, db, target, fileContents )
   
   self:outputResult(
      clang.core.CXDiagnostic_Error,
      function( diagList, stream )
	 stream:write( '<functionList>' )
	 
	 db:mapJoin(
	    "namespace", "symbolDecl", "namespace.id = symbolDecl.nsId",
	    cond, 10000,
	    "namespace.name, symbolDecl.nsId, "
	    .. "symbolDecl.fileId, symbolDecl.line, symbolDecl.column",
	    function( item )

	       if not string.find( item.name, pattern, 1, true ) or
		  currentFileInfo and
		  string.find( item.name, ":[%d]+$" ) and
		  item.fileId ~= currentFileInfo.id
	       then
		  -- 終端に数値がつくのはファイル内の関数なので除外
	       else
		  local fileInfo = db:getFileInfo( item.fileId )
		  if fileInfo.incFlag == 0 and
		     fileInfo.id ~= currentFileInfo.id and
		     not currentIncSet[ fileInfo.id ]
		  then
		     -- ヘッダに定義されていない別ファイルの関数は除外
		  else
		     stream:write( "<function>" )
		     local name = db:getNamespace( item.nsId ).name
		     name = string.gsub( name, "::[%d]+$", "" )
		     if not string.find( name, "::", 2, true ) then
			name = string.gsub( name, "::", "" )
		     end
		     stream:write( string.format(
				      "<name>%s</name>\n", name ) )
		     if fileInfo.incFlag ~= 0 and
			not currentIncSet[ fileInfo.id ]
		     then
			stream:write( string.format(
					 "<include>%s</include>\n",
					 db:getSystemPath( fileInfo.path ) ) )
		     end
		     stream:write( string.format(
				      "<declaration>%s</declaration>\n",
				      db:getSystemPath( fileInfo.path ) ) )
		     stream:write( "</function>\n" )
		  end
	       end
	       return true
	    end
	 )
	 stream:write( '</functionList>' )
      end
   )
end

return Completion

