local clang = require( 'libclanglua.if' )
local DBCtrl = require( 'lctags.DBCtrl' )
local log = require( 'lctags.LogCtrl' )
local DynamicCall = require( 'lctags.DynamicCall' )

local StackCalc = {}

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
   if #compInfo.child > 0 then
      for index, childInfo in ipairs( compInfo.child ) do
	 dumpStack( childInfo, parentSize + compInfo.size )
      end
   end
   log( 1, "stack size = ", compInfo.size + parentSize )
   for index, callee in ipairs( compInfo.calleeList ) do
      log( 1, "callee = ", callee.name )
   end
end

function visitFunc( cursor, parent, stackCalc, exInfo )
   local cursorKind = cursor:getCursorKind()
   local cursorOffset = tostring( exInfo[ 2 ] )

   dumpCursorInfo( cursor, stackCalc.depth, "visitFunc:", cursorOffset )

   if isFuncDecl( cursorKind ) then
      stackCalc.depth = stackCalc.depth + 1
      local stackInfo = { funcCursor = cursor,
			  comp = { { size = 0, calleeList = {}, child = {} } } }
      table.insert( stackCalc.stackInfoList, stackInfo )
      clang.visitChildrenFast(
	 cursor, visitFunc, stackCalc,
	 {
	    clang.core.CXCursor_ParmDecl,
	    clang.core.CXCursor_CompoundStmt,
	 }, 1 )
      dumpStack( stackInfo.comp[1] )
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
   if size ~= clang.core.CXTypeLayoutError_Dependent then
      log( 1, "size = ", size )
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

   if not isFuncDecl( declCursor:getCursorKind() ) then
      declCursor = clang.getDeclCursorFromType( declCursor:getCursorType() )
      if not declCursor then
	 return
      end
   end
   
   local nsInfo = self.db:getNamespaceFromCursor( declCursor )
   if not nsInfo then
      return
   end

   local stackInfo = self.stackInfoList[ #self.stackInfoList ]
   if stackInfo then
      local compInfo = stackInfo.comp[ #stackInfo.comp ]
      table.insert( compInfo.calleeList, nsInfo )
   end
end

function StackCalc:analyzeComp( cursor )
   local stackInfo = self.stackInfoList[ #self.stackInfoList ]
   if stackInfo then
      table.insert( stackInfo.comp, { size = 0, calleeList = {}, child = {} } )
   end
   self.depth = self.depth + 1
   clang.visitChildrenFast( cursor, visitCompFunc, self, nil, 1 )
   self.depth = self.depth - 1
   if stackInfo then
      local compInfo = stackInfo.comp[ #stackInfo.comp ]
      table.remove( stackInfo.comp )
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
   self.db:close()
end


return StackCalc
