%module libclanglua_coreBase
%{
#include <clang-c/Index.h>
#include <clang-c/CXCompilationDatabase.h>

#if LUA_VERSION_NUM >= 502
#define LUA_LEN( LUA, INDEX ) luaL_len( LUA, INDEX )
#else
#define LUA_LEN( LUA, INDEX ) lua_objlen( LUA, INDEX )
#endif    
%}

%include <carrays.i>
%array_functions(char *, charArray);
//@array@


%include <typemaps.i>
%apply unsigned* OUTPUT {unsigned *};



%{
static enum CXChildVisitResult CXCursorVisitor_wrap(
    CXCursor cursor, CXCursor parent, CXClientData client_data )
{
    lua_State * pLua = (lua_State *)client_data;

    // param = __libclang_visit[#__libclang_visit]
    lua_checkstack( pLua, 4 );
    lua_getglobal( pLua, "__libclang_visit" );
    lua_pushinteger( pLua, LUA_LEN( pLua, -1 ) );
    lua_gettable( pLua, -2 );
    lua_remove( pLua, -2 );
    // func = param[1]
    lua_pushinteger( pLua, 1 );
    lua_gettable( pLua, -2 );

    if ( lua_istable( pLua, -1 ) ) {
      static CXFile prevFile = NULL;
      if ( LUA_LEN( pLua, -1 ) == 0 ) {
	prevFile = NULL;
      }
      
      // exInfo = param[2]
      lua_pushinteger( pLua, 2 );
      lua_gettable( pLua, -3 );

      int * pKindArray = NULL;
      SWIG_ConvertPtr( pLua, -1, (void**)&pKindArray,SWIGTYPE_p_int,0);
      if ( pKindArray == NULL ) {
	printf( "illegal pointer\n" );
	exit( 1 );
      }
      int result = pKindArray[ 0 ];
      pKindArray = pKindArray + 1;

      lua_pop( pLua, 1 );

      int index;
      int findFlag = 0;
      if ( pKindArray[ 0 ] == CXCursor_InvalidFile ) {
	findFlag = 1;
      }
      else {
	for ( index = 0; pKindArray[ index ] != CXCursor_InvalidFile; index++ ) {
	  if ( cursor.kind == pKindArray[ index ] ) {
	    findFlag = 1;
	    break;
	  }
	}
      }
      
      if ( findFlag ) {
	CXSourceLocation loc = clang_getCursorLocation( cursor );
	CXFile cxfile;
	unsigned int line;
	unsigned int column;
	unsigned int offset;

	clang_getFileLocation( loc,  &cxfile, &line, &column, &offset );

	int equalsPrevFileFlag = clang_File_isEqual( prevFile, cxfile );
	prevFile = cxfile;
      
      
	lua_pushinteger( pLua, LUA_LEN( pLua, -1 ) + 1 );
	lua_createtable( pLua, 2, 0 );

	{
	  CXCursor * resultptr;
	
	  lua_pushinteger( pLua, 1 );
	
	  resultptr = (CXCursor *) malloc(sizeof(CXCursor));
	  memmove(resultptr, &cursor, sizeof(CXCursor));
	  SWIG_NewPointerObj( pLua,(void *) resultptr,SWIGTYPE_p_CXCursor,1);

	  lua_settable( pLua, -3 );
	}

	{
	  CXCursor * resultptr;
	
	  lua_pushinteger( pLua, 2 );
	
	  resultptr = (CXCursor *) malloc(sizeof(CXCursor));
	  memmove(resultptr, &parent, sizeof(CXCursor));
	  SWIG_NewPointerObj( pLua,(void *) resultptr,SWIGTYPE_p_CXCursor,1);

	  lua_settable( pLua, -3 );
	}
	
	lua_pushinteger( pLua, 3 );
	lua_pushboolean( pLua, !equalsPrevFileFlag );
	lua_settable( pLua, -3 );

	lua_settable( pLua, -3 );
      }

      lua_pop( pLua, 1 );
      return result;
    }
    {
        CXCursor * resultptr;
        resultptr = (CXCursor *) malloc(sizeof(CXCursor));
        memmove(resultptr, &cursor, sizeof(CXCursor));
        SWIG_NewPointerObj( pLua,(void *) resultptr,SWIGTYPE_p_CXCursor,1);
    }

    {
        CXCursor * resultptr;
        resultptr = (CXCursor *) malloc(sizeof(CXCursor));
        memmove(resultptr, &parent, sizeof(CXCursor));
        SWIG_NewPointerObj( pLua,(void *) resultptr,SWIGTYPE_p_CXCursor,1);
    }
    
    // exInfo = param[2]
    lua_pushinteger( pLua, 2 );
    lua_gettable( pLua, -5 );
    lua_remove( pLua, -5 );

    int hasErr = lua_pcall( pLua, 3, 1, 0 );

    static int orrurError = 0;
    if ( hasErr != 0 ) {
        orrurError = 1;
        const char * pMessage = lua_tostring( pLua, -1 );
        if ( pMessage == NULL ) {
            pMessage = "";
        }
        printf( "visit error: %s\n", pMessage );
        lua_pop( pLua, 1 );
        return CXChildVisit_Break;
    }
    if ( !lua_isnumber( pLua, -1 ) ) {
        orrurError = 1;
        lua_pop( pLua, 1 );
        printf( "visit error: return code is not number.\n" );
        return CXChildVisit_Break;
    }
    enum CXChildVisitResult result = lua_tointeger( pLua, -1 );
    lua_pop( pLua, 1 );

    if ( orrurError ) {
        return CXChildVisit_Break;
    }

    return result;
}
%}

%typemap(in) (CXCursorVisitor visitor, CXClientData client_data) {
  $1 = CXCursorVisitor_wrap;
  $2 = L;
}


%{
static void CXInclusionVisitor_wrap(
  CXFile included_file, CXSourceLocation* inclusion_stack,
  unsigned include_len, CXClientData client_data )
{
    lua_State * pLua = (lua_State *)client_data;

    printf( "%s\n", __func__ );

    // param = __libclang_visit[#__libclang_visit]
    lua_checkstack( pLua, 4 );
    lua_getglobal( pLua, "__libclang_visit" );
    lua_pushinteger( pLua, LUA_LEN( pLua, -1 ) );
    lua_gettable( pLua, -2 );
    lua_remove( pLua, -2 );
    // func = param[1]
    lua_pushinteger( pLua, 1 );
    lua_gettable( pLua, -2 );

    {
      CXFile * resultptr = (CXFile *)malloc(sizeof(CXFile));
      *resultptr = included_file;
      SWIG_NewPointerObj( pLua,(void *) resultptr,SWIGTYPE_p_void,1);
    }

    {
      CXSourceLocation * resultptr;
      resultptr = (CXSourceLocation *) malloc(sizeof(CXSourceLocation));
      memmove( resultptr, inclusion_stack, sizeof(CXSourceLocation));
      SWIG_NewPointerObj( pLua,(void *) resultptr,SWIGTYPE_p_CXSourceLocation,1);
    }

    lua_pushinteger( pLua, include_len );
    
    // exInfo = param[2]
    lua_pushinteger( pLua, 2 );
    lua_gettable( pLua, -6 );
    lua_remove( pLua, -6 );

    int hasErr = lua_pcall( pLua, 4, 0, 0 );
    if ( hasErr != 0 ) {
        const char * pMessage = lua_tostring( pLua, -1 );
        if ( pMessage == NULL ) {
            pMessage = "";
        }
        printf( "visit error: %s\n", pMessage );
    }
}
%}

%typemap(in) (CXInclusionVisitor visitor, CXClientData client_data) {
  $1 = CXInclusionVisitor_wrap;
  $2 = L;
  printf( "%s\n", __func__ );
}


%typemap(out) time_t {
    lua_pushinteger( L, (unsigned long)$result );
    lua_pushinteger( L, ((unsigned long)$result) >> 32 );
    SWIG_arg += 2;
 }

%include "clang-c/Platform.h"
%include "clang-c/CXString.h"
%include "clang-c/Index.h"
 //%include "clang-c/CXCompilationDatabase.h"

