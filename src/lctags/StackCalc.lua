local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local DynamicCall = require( 'lctags.DynamicCall' )

local StackCalc = { funcNsId2StackInfo = {} }


local CompInfo = {}
function CompInfo:new()
   local obj = {
      -- このブロックの NET スタックサイズ
      size = 0,
      -- このブロックから呼び出している関数の nsId -> nsInfo マップ
      calleeId2NsInfoMap = {},
      -- このブロック内にある CompInfo リスト
      child = {},
      -- このブロックのスタックサイズが確定した場合、その最大サイズが入る。
      -- 確定前は nil
      maxSize = nil,
      -- このブロックのスタックサイズが確定した場合の、呼び出し関数の nsInfo。
      -- 確定前は nil。
      -- 関数コールしていない場合 nil。
      calleeForMax = nil,
      -- 子ブロックを含めたスタックサイズが確定した場合、その最大サイズが入る。
      -- 確定前は nil
      grossMaxSize = nil,
      -- 子ブロックを含めたスタックサイズが確定した場合、呼び出し関数の nsInfo。
      -- 確定前は nil
      -- 関数コールしていない場合 nil。
      grossCalleeForMax = nil,
      -- 確定している子ブロックのスタックサイズ。
      childMaxSize = 0,
      -- 確定している子ブロックの callee。
      -- 関数コールしていない場合 nil。
      childCalleeForMax = nil,
      -- loop している関数が 1 度ループした場合の、
      -- 子ブロックを含めたスタックサイズが確定した場合、その最大サイズが入る。
      -- loop している関数がない場合は、 grossMaxSize と同じものが入る。
      -- 確定前は nil
      grossMaxSizeLoop1 = nil,
      -- loop している関数が 1 度ループした場合の、
      -- 子ブロックを含めたスタックサイズが確定した場合、呼び出し関数の nsInfo。
      -- loop している関数がない場合は、 grossCalleeForMax と同じものが入る。
      -- 確定前は nil
      -- 関数コールしていない場合 nil。
      grossCalleeForMaxLoop1 = nil,

   }
   setmetatable( obj, { __index = CompInfo } )
   return obj
end

-- ループしている関数呼び出しを、1度呼び出した場合のスタック使用量を計算する
function CompInfo:tryLoopFix( parentSize, checkedNsIdSet, funcNsId2StackInfoMap )
   if self.grossMaxSizeLoop1 then
      return self.grossMaxSizeLoop1
   end
   local loopStackSize = self.maxSize
   local calleeForLoopMax
   if not self.maxSize then
      -- 呼び出し関数の最大スタックを解析
      for calleeId, nsInfo in pairs( self.calleeId2NsInfoMap ) do
	 if checkedNsIdSet[ calleeId ] then
	    -- loop で解析中
	    log( 2, "tryLoopFix skip analyzing", nsInfo.name )
	    loopStackSize = parentSize + self.size
	 else
	    log( 2, "tryLoopFix analyzing", nsInfo.name )
	    checkedNsIdSet[ calleeId ] = -1
	    local stackInfo = funcNsId2StackInfoMap[ calleeId ]
	    if stackInfo then
	       compInfo = stackInfo.comp[1]
	       local loopFixSize
	       if compInfo.grossMaxSize then
		  -- 確定している場合
		  loopFixSize = compInfo.grossMaxSize
	       else
		  -- 確定していない場合その関数を解決する
		  log( 2, "tryLoopFix", parentSize + self.size, nsInfo.name )
		  loopFixSize = compInfo:tryLoopFix(
		     parentSize + self.size + 4, checkedNsIdSet,
		     funcNsId2StackInfoMap )
	       end
	       if not loopStackSize or loopFixSize > loopStackSize then
		  loopStackSize = loopFixSize
		  calleeForLoopMax = nsInfo
	       end
	    end
	    checkedNsIdSet[ calleeId ] = nil
	 end
      end
      -- 全ての呼び出し関数のサイズが確定したらサイズを更新する
      -- self.maxSize = loopStackSize
      -- self.calleeForMax = calleeForLoopMax
      log( 2, "tryLoopFix maxSize", self.maxSize, calleeForMax and calleeForMax.name )
   end

   local childMaxSizeLoop1 = 0
   local childCalleeForMaxLoop1 = nil

   if #self.child > 0 then
      -- 子ブロックのサイズを確定させる
      local newChild = {}
      for index, childCompInfo in ipairs( self.child ) do
	 local loopFixSize
	 if childCompInfo.grossMaxSize then
	    loopFixSize = childCompInfo.grossMaxSize
	 else
	    -- 確定していない場合そのブロックを解決する
	    log( 2, "tryLoopFix child", parentSize + self.size )
	    loopFixSize = childCompInfo:tryLoopFix(
	       parentSize + self.size, checkedNsIdSet,
	       funcNsId2StackInfoMap )
	    log( 2, "tryLoopFix child end", parentSize + self.size, loopFixSize )
	 end
	 if childMaxSizeLoop1 < loopFixSize then
	    childMaxSizeLoop1 = loopFixSize
	    childCalleeForMaxLoop1 = childCompInfo.grossCalleeForMaxLoop1
	    log( 3, "child max", loopFixSize,
		 childCalleeForMaxLoop1 and childCalleeForMaxLoop1.name )
	 end
      end
   end

   if loopStackSize > childMaxSizeLoop1 then
      self.grossMaxSizeLoop1 = loopStackSize
      self.grossCalleeForMaxLoop1 = calleeForLoopMax
   else	 
      self.grossMaxSizeLoop1 = childMaxSizeLoop1
      self.grossCalleeForMaxLoop1 = childCalleeForMaxLoop1
   end
   return self.grossMaxSizeLoop1
end

function CompInfo:tryFix( parentSize, funcNsId2StackInfoMap )
   if self.grossMaxSize then
      return true
   end

   if not self.maxSize then
      -- 呼び出し関数の最大スタックを解析
      local calleeMaxSize = 0
      local calleeForMax
      local fixedCalleeList = {}
      local hasNoFix = false
      for calleeId, nsInfo in pairs( self.calleeId2NsInfoMap ) do
	 local stackInfo = funcNsId2StackInfoMap[ calleeId ]
	 if stackInfo then
	    compInfo = stackInfo.comp[1]
	    if compInfo.grossMaxSize then
	       if calleeMaxSize < compInfo.grossMaxSize + 4 then
		  calleeMaxSize = compInfo.grossMaxSize + 4
		  calleeForMax = nsInfo
	       end
	       table.insert( fixedCalleeList, calleeId )
	    else
	       hasNoFix = true
	    end
	 else
	    hasNoFix = true
	 end
      end
      -- for index, calleeId in ipairs( fixedCalleeList ) do
      -- 	 self.calleeId2NsInfoMap[ calleeId ] = nil
      -- end
      if not hasNoFix then
	 -- 全ての呼び出し関数のサイズが確定したらサイズを更新する
	 self.maxSize = parentSize + self.size + calleeMaxSize
	 self.calleeForMax = calleeForMax
	 log( 2, "tryFix maxSize", calleeForMax and calleeForMax.name )
      end
   end
   
   if #self.child > 0 then
      -- 子ブロックのサイズを確定させる
      local newChild = {}
      for index, childCompInfo in ipairs( self.child ) do
	 childCompInfo:tryFix( self.size, funcNsId2StackInfoMap )
	 if childCompInfo.grossMaxSize then
	    if self.childMaxSize < childCompInfo.grossMaxSize then
	       self.childMaxSize = childCompInfo.grossMaxSize
	       self.childCalleeForMax = childCompInfo.grossCalleeForMax
	       log( 3, "child max",
		    self.childCalleeForMax and self.childCalleeForMax.name )
	    end
	 else
	    table.insert( newChild, childCompInfo )
	 end
      end
      self.child = newChild
   end

   if self.maxSize and #self.child == 0 then
      if self.maxSize > parentSize + self.childMaxSize then
	 self.grossMaxSize = self.maxSize
	 self.grossCalleeForMax = self.calleeForMax
      else	 
	 self.grossMaxSize = parentSize + self.childMaxSize
	 self.grossCalleeForMax = self.childCalleeForMax
      end
      return true
   end

   log( 3, "maxsize, #child", self.maxSize, #self.child )
   return false
end

local StackInfo = {}
function StackInfo:new( nsInfo )
   local obj = { maxSize = 0, nsInfo = nsInfo, comp = { CompInfo:new() } }
   setmetatable( obj, { __index = StackInfo } )
   return obj
end

function StackInfo:fix( funcNsId2StackInfoMap )
   return self.comp[1]:tryFix( 0, funcNsId2StackInfoMap )
end

function StackInfo:fixLoop( funcNsId2StackInfoMap )
   self.comp[1]:tryLoopFix( 0, {}, funcNsId2StackInfoMap )
   local nsInfo = self.comp[1].grossCalleeForMaxLoop1
   log( 2, "fixLoop", self.comp[1].grossMaxSizeLoop1, nsInfo and nsInfo.name )
end


local targetKindList = {
   clang.core.CXCursor_UnexposedDecl,
   clang.core.CXCursor_Namespace,
   clang.core.CXCursor_ClassDecl,
   clang.core.CXCursor_ClassTemplate,
   clang.core.CXCursor_StructDecl,
   clang.core.CXCursor_CXXMethod,
   clang.core.CXCursor_Destructor,
   clang.core.CXCursor_FunctionDecl,
   clang.core.CXCursor_Constructor,
}

local function isFuncDecl( cursorKind )
   return cursorKind == clang.core.CXCursor_CXXMethod or
      cursorKind == clang.core.CXCursor_Destructor or
      cursorKind == clang.core.CXCursor_Constructor or
      cursorKind == clang.core.CXCursor_FunctionDecl
end

local function dumpCursorInfo( cursor, depth, prefix, cursorOffset )
   local cursorKind = cursor:getCursorKind()
   local txt = cursor:getCursorSpelling()

   log( 5,
	function()
	   return string.format(
	      "%s %s%s %s(%d) %d %s %s",
	      string.rep( "  ", depth ),
	      prefix and (prefix .. " ") or "", txt, 
	      clang.getCursorKindSpelling( cursorKind ), cursorKind,
	      cursor:hashCursor(), "",
	      cursorOffset or ""  )
	end
   )
end



function visitCompFunc( cursor, parent, stackCalc, exInfo )
   local cursorKind = cursor:getCursorKind()

   dumpCursorInfo( cursor, stackCalc.depth, "visitCompFunc:", cursorOffset )

   if cursorKind == clang.core.CXCursor_CallExpr then
      stackCalc:addCall( cursor )
   end
   
   if cursorKind == clang.core.CXCursor_VarDecl then
      stackCalc:addVarSize( cursor )
   elseif cursorKind == clang.core.CXCursor_CompoundStmt then
      stackCalc:analyzeComp( cursor )
   elseif clang.isExpression( cursorKind ) or clang.isStatement( cursorKind ) then
      stackCalc.depth = stackCalc.depth + 1
      clang.visitChildrenFast( cursor, visitCompFunc, stackCalc, nil, 1 )
      stackCalc.depth = stackCalc.depth - 1
   end
end

function dumpStack( compInfo, parentSize )
   if not parentSize then
      parentSize = 0
   end
   -- if #compInfo.child > 0 then
   --    for index, childInfo in ipairs( compInfo.child ) do
   -- 	 dumpStack( childInfo, parentSize + compInfo.size )
   --    end
   -- end
   log( 1, "stack size = ", compInfo.size + parentSize, 
	compInfo.grossMaxSizeLoop1,
	compInfo.grossCalleeForMaxLoop1 and compInfo.grossCalleeForMaxLoop1.name )
end

function visitFunc( cursor, parent, stackCalc, exInfo )
   local cursorKind = cursor:getCursorKind()
   local cursorOffset = tostring( exInfo[ 2 ] )

   dumpCursorInfo( cursor, stackCalc.depth, "visitFunc:", cursorOffset )

   if isFuncDecl( cursorKind ) then
      local nsInfo = stackCalc.db:getNamespaceFromCursor( cursor )
      if not nsInfo then
	 return
      end
      stackCalc.depth = stackCalc.depth + 1
      local stackInfo = StackInfo:new( nsInfo )
      table.insert( stackCalc.stackInfoList, stackInfo )
      clang.visitChildrenFast(
	 cursor, visitFunc, stackCalc,
	 {
	    clang.core.CXCursor_ParmDecl,
	    clang.core.CXCursor_CompoundStmt,
	 }, 1 )
      stackCalc:addStackInfo( nsInfo, stackInfo )
      table.remove( stackCalc.stackInfoList )
      stackCalc.depth = stackCalc.depth - 1
   elseif cursorKind == clang.core.CXCursor_ParmDecl then
      stackCalc:addVarSize( cursor )
   elseif cursorKind == clang.core.CXCursor_CompoundStmt then
      stackCalc:analyzeComp( cursor )
   else
      stackCalc.depth = stackCalc.depth + 1
      clang.visitChildrenFast( cursor, visitFunc, stackCalc, targetKindList, 1 )
      stackCalc.depth = stackCalc.depth - 1
   end
end

function StackCalc:addVarSize( cursor )
   local cxtype = cursor:getCursorType()
   local size = cxtype:getSizeOf()
   if size >= 0 then
      local storageClass = cursor:getStorageClass()
      if storageClass ~= clang.core.CX_SC_Auto and
	 storageClass ~= clang.core.CX_SC_Register and
	 storageClass ~= clang.core.CX_SC_None
      then
	    log( 1, "skip var", cursor:getCursorSpelling(), storageClass )
	    return
      end
      
      log( 1, "size = ", size, cursor:getCursorSpelling() )
      local stackInfo = self.stackInfoList[ #self.stackInfoList ]
      if stackInfo then
	 local compInfo = stackInfo.comp[ #stackInfo.comp ]
	 compInfo.size = compInfo.size + size
      end	 
   end
end

function StackCalc:addCall( cursor )
   local refExpr = DynamicCall:searchExpr(
      cursor,
      { clang.core.CXCursor_MemberRefExpr, clang.core.CXCursor_DeclRefExpr } )

   if not refExpr then
      return
   end

   local declCursor = refExpr:getCursorReferenced()
   if not declCursor then
      return
   end

   local dynamicCallFlag
   if not isFuncDecl( declCursor:getCursorKind() ) then
      -- 関数ポインタ呼び出しの場合、関数ポインタの型定義を取得
      dynamicCallFlag = true
      declCursor = clang.getDeclCursorFromType( declCursor:getCursorType() )
      if not declCursor then
	 return
      end
   end
   
   local nsInfo = self.db:getNamespaceFromCursor( declCursor )
   if not nsInfo then
      return
   end


   local hasBodyFlag
   -- 関数本体定義の有無の確認
   if not dynamicCallFlag then
      self.db:mapDecl(
	 nsInfo.id,
	 function( item )
	    if item.hasBodyFlag ~= 0 then
	       hasBodyFlag = true
	       return false
	    end
	    return true
	 end
      )
   else
      -- 関数ポインタコールはとりあえず除外
   end
   if not hasBodyFlag then
      -- 関数定義が解析対象外のものはサイズ計算から除外する
      log( 2, "not declaration", nsInfo.name )
      return
   end
   

   local stackInfo = self.stackInfoList[ #self.stackInfoList ]
   if stackInfo then
      log( 2, "add call", nsInfo.name )
      local compInfo = stackInfo.comp[ #stackInfo.comp ]
      compInfo.calleeId2NsInfoMap[ nsInfo.id ] = nsInfo
   end
end

function StackCalc:addStackInfo( nsInfo, stackInfo )
   if #stackInfo.comp[1].child == 0 then
      -- 先頭の child がないのはプロトタイプ宣言なので無視
      return
   end

   self.funcNsId2StackInfo[ nsInfo.id ] = stackInfo
end



function StackCalc:analyzeComp( cursor )
   local stackInfo = self.stackInfoList[ #self.stackInfoList ]
   if stackInfo then
      table.insert( stackInfo.comp, CompInfo:new() )
      log( 1, "analyzeComp: start", #stackInfo.comp )
   end
   self.depth = self.depth + 1
   clang.visitChildrenFast( cursor, visitCompFunc, self, nil, 1 )
   self.depth = self.depth - 1
   if stackInfo then
      local compInfo = stackInfo.comp[ #stackInfo.comp ]
      table.remove( stackInfo.comp )
      log( 1, "analyzeComp: end", #stackInfo.comp )
      local parentInfo = stackInfo.comp[ #stackInfo.comp ]
      table.insert( parentInfo.child, compInfo )
   end
end


function StackCalc:analyze( path, target, analyzer )
   if not target then
      target = ""
   end
   local unit, compileOp, newAnalyzer, stdMode =
      analyzer:createUnit( path, target, false )

   self.depth = 0
   self.stackInfoList = {}
   self.db = newAnalyzer:openDBForReadOnly()
   clang.visitChildrenFast(
      unit:getTranslationUnitCursor(), visitFunc, self, targetKindList, 1 )


   for funcNsId, stackInfo in pairs( self.funcNsId2StackInfo ) do
      log( 1, "before func:", stackInfo.nsInfo.name )
      dumpStack( stackInfo.comp[1] )
   end

   
   local needFixStackInfoSet = {}
   for funcNsId, stackInfo in pairs( self.funcNsId2StackInfo ) do
      needFixStackInfoSet[ funcNsId ] = stackInfo
   end

   while true do
      local fixNsIdList = {}
      local hasNeedFix = false
      local hasFix = false
      for funcNsId, stackInfo in pairs( needFixStackInfoSet ) do
	 local nsInfo = self.db:getNamespace( funcNsId )
	 log( "fix: nsInfo", nsInfo.name )
	 if stackInfo:fix( self.funcNsId2StackInfo ) then
	    hasFix = true
	    table.insert( fixNsIdList, funcNsId )
	 else
	    hasNeedFix = true
	 end
      end
      for index, nsId in ipairs( fixNsIdList ) do
      -- fix した nsInfo を除外する
	 needFixStackInfoSet[ nsId ] = nil
      end
      if not hasNeedFix then
	 break
      end
      if not hasFix then
	 log( 1, "has loop or recursive" )
	 break
      end
   end


   log( 1, "---- fix loop ----" )
   local hasNeedFix = false
   local hasFix = false
   for funcNsId, stackInfo in pairs( needFixStackInfoSet ) do
      local nsInfo = self.db:getNamespace( funcNsId )
      log( "fixLoop: nsInfo", nsInfo.name )
      stackInfo:fixLoop( self.funcNsId2StackInfo )
   end
   
   log( 1, "---- fixed ----" )
   
   for funcNsId, stackInfo in pairs( self.funcNsId2StackInfo ) do
      log( 1, "func:", stackInfo.nsInfo.name )
      dumpStack( stackInfo.comp[1] )
   end
   
   self.db:close()
end


return StackCalc
