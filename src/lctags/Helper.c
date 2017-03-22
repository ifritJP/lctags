/** Copyright (C) 2017 ifritJP */

#include <string.h>
#include <openssl/evp.h>
#include <openssl/engine.h>
#include <lauxlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <pthread.h>
#include <semaphore.h>
#include <fcntl.h>           /* For O_* constants */
#include <sys/stat.h>
#include <limits.h>
#include <stdlib.h>
#include <sys/time.h>

#ifndef NAME_MAX
#define NAME_MAX 256
#endif

#define DIGEST_ID "lctags.digest"
#define LOCK_ID "lctags.lock"


#define LOCK_DEFAULT_NAME "default"
#define LOCK_NAME_PREFIX "/lctags"

#define toString(L, ID)                         \
    void * pUserData = lua_touserdata( L, 1 );  \
    char buf[ 100 ];                            \
    sprintf( buf, "<%s:%p>", ID, pUserData );     \
    lua_pushstring( pLua, buf );                \
    return 1;



#define toDigest(L)                                        \
    ((helper_digest_t *)luaL_checkudata(L, 1, DIGEST_ID))

#define toLock(L)                                        \
    ((helper_lock_t *)luaL_checkudata(L, 1, LOCK_ID))

typedef struct {
    EVP_MD_CTX mctx;
    int mdsize;
} helper_digestInfo_t;

typedef struct {
    helper_digestInfo_t * pInfo;
} helper_digest_t;

typedef struct {
    sem_t * pSem;
    int depth;
    char name[ NAME_MAX ];
} helper_lockInfo_t;

typedef struct {
    helper_lockInfo_t * pInfo;
} helper_lock_t;

typedef const EVP_MD * (evp_md_func_t)(void);

typedef struct {
    const char * pName;
    evp_md_func_t * pMd;
} helper_typeMap_t;


static int helper_openDigest( lua_State * pLua );
static int helper_createLock( lua_State * pLua );
static int helper_deleteLock( lua_State * pLua );
static int helper_getTime( lua_State * pLua );
static int helper_msleep( lua_State * pLua );
static int helper_chdir( lua_State * pLua );
static int helper_mkdir( lua_State * pLua );
static int helper_getFileModTime( lua_State * pLua );
static int helper_getCurrentTime( lua_State * pLua );
static int helper_getTempFilename( lua_State * pLua );
static int helper_tostring( lua_State * pLua );


static int helper_digest_write( lua_State * pLua );
static int helper_digest_fix( lua_State * pLua );
static int helper_digest_gc( lua_State * pLua );
static int helper_digest_tostring( lua_State * pLua );


static int helper_lock_begin( lua_State * pLua );
static int helper_lock_fin( lua_State * pLua );
static int helper_lock_isLocking( lua_State * pLua );
static int helper_lock_gc( lua_State * pLua );
static int helper_lock_tostring( lua_State * pLua );


static helper_digestInfo_t * helper_setup( const EVP_MD * pEvpMd );
static int helper_write( helper_digestInfo_t * pInfo, const void * pData, int size );
static int helper_fix( helper_digestInfo_t * pInfo, void * pMDBuf, int size );
static void * helper_dispose( helper_digestInfo_t * pInfo );


static const luaL_Reg s_if_lib[] = {
    { "openDigest", helper_openDigest },
    { "createLock", helper_createLock },
    { "deleteLock", helper_deleteLock },
    { "msleep", helper_msleep },
    { "chdir", helper_chdir },
    { "mkdir", helper_mkdir },
    { "getTime", helper_getTime },
    { "getFileModTime", helper_getFileModTime },
    { "getCurrentTime", helper_getCurrentTime },
    { "getTempFilename", helper_getTempFilename },
    {"__tostring", helper_tostring },
    {NULL, NULL}
};

static const luaL_Reg s_digestObj_lib[] = {
    { "write", helper_digest_write },
    { "fix", helper_digest_fix },
    { "__gc", helper_digest_gc },
    {"__tostring", helper_digest_tostring },
    {NULL, NULL}
};

static const luaL_Reg s_lockObj_lib[] = {
    { "begin", helper_lock_begin },
    { "fin", helper_lock_fin },
    { "isLocking", helper_lock_isLocking },
    { "__gc", helper_lock_gc },
    {"__tostring", helper_lock_tostring },
    {NULL, NULL}
};


static const helper_typeMap_t s_typeMap[] = {
    { "md5", EVP_md5 },
    { "sha", EVP_sha },
    { "sha224", EVP_sha224 },
    { "sha256", EVP_sha256 },
    { "sha384", EVP_sha384 },
    { "sha512", EVP_sha512 },

    
    { NULL, NULL }
};


static void helper_setupObjMethod(
    lua_State * pLua, const char * pName, const luaL_Reg * pReg )
{
    luaL_newmetatable(pLua, pName );
    lua_pushvalue(pLua, -1);
    lua_setfield(pLua, -2, "__index");

#if LUA_VERSION_NUM >= 502
    luaL_setfuncs(pLua, pReg, 0);

    lua_pop(pLua, 1);
#else
    luaL_register(pLua, NULL, pReg );

    lua_pop(pLua, 1);
#endif
}

static void * helper_newUserData(
    lua_State * pLua, const char * pName, size_t size )
{
    void * pBuf = lua_newuserdata( pLua, size );
    if ( pBuf == NULL ) {
        printf( "%s: lua_newuserdata() retrn NULL\n", __func__ );
        return NULL;
    }
    
#if LUA_VERSION_NUM >= 502
    luaL_setmetatable( pLua, pName );
#else
    luaL_getmetatable( pLua, pName );
    lua_setmetatable( pLua, -2 );
#endif

    return pBuf;
}

int luaopen_lctags_Helper( lua_State * pLua )
{
    helper_setupObjMethod( pLua, DIGEST_ID, s_digestObj_lib );
    helper_setupObjMethod( pLua, LOCK_ID, s_lockObj_lib );

#if LUA_VERSION_NUM >= 502
    luaL_newlib( pLua, s_if_lib );
#else
    luaL_register( pLua, "lctags.Helper", s_if_lib );
#endif
    return 1;
}


static int helper_getTime( lua_State * pLua )
{
    struct timeval tm;
    gettimeofday( &tm, NULL );
    lua_pushinteger( pLua, tm.tv_sec );
    lua_pushinteger( pLua, tm.tv_usec );
    return 2;
}

static int helper_msleep( lua_State * pLua )
{
    int index;
    int msec = luaL_checkinteger( pLua, 1 );

    usleep( ( msec % 1000 ) * 1000 );
    sleep( msec / 1000 );
    
    return 0;
}

static int helper_chdir( lua_State * pLua )
{
    lua_pushinteger( pLua, chdir( lua_tostring( pLua, 1 ) ) );
    return 1;
}

static int helper_mkdir( lua_State * pLua )
{
    if ( mkdir( lua_tostring( pLua, 1 ), 0777 ) == 0 ) {
	lua_pushinteger( pLua, 0 );
    }
    else {
	switch ( errno ) {
	case EACCES:
	    lua_pushstring( pLua, "EACCES" );
	    break;
	case EEXIST:
	    lua_pushstring( pLua, "EEXIST" );
	    break;
	case ENOENT:
	    lua_pushstring( pLua, "ENOENT" );
	    break;
	case ENOSPC:
	    lua_pushstring( pLua, "ENOSPC" );
	    break;
	case ENOTDIR:
	    lua_pushstring( pLua, "ENOTDIR" );
	    break;
	case EROFS:
	    lua_pushstring( pLua, "EROFS" );
	    break;
	default:
	    lua_pushinteger( pLua, errno );
	    break;
	}
    }
    return 1;
}

static int helper_getFileModTime( lua_State * pLua )
{
  const char * pPath = lua_tostring( pLua, 1 );
  struct stat aStat;

  if ( stat( pPath, &aStat ) != 0 ) {
    return 0;
  }
  lua_pushinteger( pLua, aStat.st_mtime );
  return 1;
}

static int helper_getCurrentTime( lua_State * pLua )
{
  lua_pushinteger( pLua, time( NULL ) );
  return 1;
}



static int helper_openDigest( lua_State * pLua )
{
    int index;
    const helper_typeMap_t * pTypeMap = s_typeMap;
    const char * pName = lua_tostring( pLua, 1 );
    const EVP_MD * pMd = NULL;
    if ( pName == NULL ) {
        return 0;
    }

    for ( ; pTypeMap->pName != NULL; pTypeMap++ ) {
        if ( strcmp( pTypeMap->pName, pName ) == 0 ) {
            pMd = pTypeMap->pMd();
            break;
        }
    }
    if ( pMd == NULL ) {
        printf( "not found -- %s\n", pName );
        return 0;
    }
    
    helper_digestInfo_t * pInfo = helper_setup( pMd );
    if ( pInfo == NULL ) {
        return 0;
    }
    helper_digest_t * pHelper =
        helper_newUserData( pLua, DIGEST_ID, sizeof( *pHelper ) );
    if ( pHelper == NULL ) {
        helper_dispose( pInfo );
        return 0;
    }
    pHelper->pInfo = pInfo;

    return 1;
}

static int helper_deleteLock( lua_State * pLua )
{
    const char * pName = lua_tostring( pLua, 1 );
    if ( pName == NULL ) {
        pName = LOCK_NAME_PREFIX LOCK_DEFAULT_NAME;
    }
    sem_unlink( pName );
    return 0;
}

static int helper_createLock( lua_State * pLua )
{
    const char * pName = lua_tostring( pLua, 1 );
    if ( pName == NULL ) {
        pName = LOCK_DEFAULT_NAME;
    }
    char name[ NAME_MAX - 3 ];
    snprintf( name, sizeof( name ), LOCK_NAME_PREFIX "%s", pName );
    name[ sizeof( name ) - 1 ] = '\0';
    
    helper_lock_t * pLock =
        helper_newUserData( pLua, LOCK_ID, sizeof( helper_lock_t ) );
    if ( pLock == NULL ) {
        return 0;
    }
    pLock->pInfo = NULL;

    // /dev/shm ˆÈ‰º‚Éƒtƒ@ƒCƒ‹‚ª¶¬‚³‚ê‚é
    sem_t * pSem = sem_open(
        name, O_CREAT, S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH, 1 );
    if ( pSem == SEM_FAILED ) {
        printf( "%s: sem_open() retrn NULL\n", __func__ );
        return 0;
    }

    {
	int val = 0;
	sem_getvalue( pSem, &val );
	if ( val > 1 ) {
	    printf( "sem_getvalue - %d\n", val );
	    exit( 1 );
	}
    }
		      
    helper_lockInfo_t * pInfo = malloc( sizeof( helper_lockInfo_t ) );
    if ( pInfo == NULL ) {
        printf( "%s: malloc() return NULL\n", __func__ );
        sem_close( pSem );
        return 0;
    }
    pLock->pInfo = pInfo;
    pInfo->pSem = pSem;
    pInfo->depth = 0;
    strcpy( pInfo->name, name );

    return 1;
}

static int helper_getTempFilename( lua_State * pLua )
{
  char * pBuf;
  const char * pPrefix = lua_tostring( pLua, 1 );
  static const char * pForm = "12345678FFFF";
  static int number = 0;
  const char * pTmpDir = getenv( "TMP" );
  if ( pTmpDir == NULL ) {
    pTmpDir = "/tmp";
  }

  if ( pPrefix == NULL ) {
    return 0;
  }
  int length = strlen( pTmpDir ) + strlen( pPrefix ) + strlen( pForm ) + 1;
  pBuf = malloc( length );
  if ( pBuf == NULL ) {
    return 0;
  }
  snprintf( pBuf, length, "%s/%s%08x%04x", pTmpDir, pPrefix, getpid(), number );
  number++;

  lua_pushstring( pLua, pBuf );

  free( pBuf );
  return 1;
}


static int helper_tostring( lua_State * pLua )
{
    lua_pushstring( pLua, "<helperID>" );
    return 1;
}

static int helper_digest_write( lua_State * pLua )
{
    helper_digest_t * pHelper = toDigest( pLua );
    size_t size;
    const char * pValue = lua_tolstring( pLua, 2, &size );

    if ( helper_write( pHelper->pInfo, pValue, (int)size ) ) {
        lua_pushboolean( pLua, 1 );
    }
    else {
        lua_pushboolean( pLua, 0 );
    }
    
    return 1;
}

static int helper_digest_fix( lua_State * pLua )
{
    helper_digest_t * pHelper = toDigest( pLua );
    helper_digestInfo_t * pInfo = pHelper->pInfo;
    unsigned char * pBuf = malloc( pInfo->mdsize * 2 );
    if ( pBuf == NULL ) {
        return 0;
    }
    if ( helper_fix( pHelper->pInfo, pBuf, pInfo->mdsize ) ) {
        int index;
        for ( index = pInfo->mdsize - 1; index >= 0 ; index-- ) {
            char buf[ 3 ];
            snprintf( buf, 3, "%02x", pBuf[ index ] );
            memcpy( pBuf + index * 2, buf, 2 );
        }
        lua_pushlstring( pLua, pBuf, pInfo->mdsize * 2 );
    }
    else {
        lua_pushnil( pLua );
        
    }
    free( pBuf );
    return 1;
}

static int helper_digest_gc( lua_State * pLua )
{
    helper_digest_t * pHelper = toDigest( pLua );

    helper_dispose( pHelper->pInfo );

    return 0;
}

static int helper_digest_tostring( lua_State * pLua )
{
    toString( pLua, DIGEST_ID );
}


static int helper_lock_begin( lua_State * pLua )
{
    helper_lock_t * pLock = toLock( pLua );

    if ( pLock->pInfo->depth > 0 ) {
        pLock->pInfo->depth++;
        return 0;
    }
    
    

    sem_wait( pLock->pInfo->pSem );

    pLock->pInfo->depth++;

    return 0;
}

static int helper_lock_fin( lua_State * pLua )
{
    helper_lock_t * pLock = toLock( pLua );

    if ( pLock->pInfo->depth > 1 ) {
        pLock->pInfo->depth--;
        return 0;
    }
    if ( pLock->pInfo->depth < 0 ) {
        printf( "no lock\n" );
        return 0;
    }

    if ( sem_post( pLock->pInfo->pSem ) != 0 ) {
	printf( "%s: post error %d\n", __func__, errno );
    }

    pLock->pInfo->depth = 0;

    return 0;
}

static int helper_lock_isLocking( lua_State * pLua )
{
    helper_lock_t * pLock = toLock( pLua );

    if ( pLock->pInfo->depth >= 1 ) {
        lua_pushinteger( pLua, 1 );
        return 1;
    }

    int val = 0;
    sem_getvalue( pLock->pInfo->pSem, &val );

    if ( val == 0 ) {
        lua_pushinteger( pLua, 2 );
    }
    else {
        lua_pushboolean( pLua, 0 );
    }
    return 1;
}


static int helper_lock_gc( lua_State * pLua )
{
    helper_lock_t * pLock = toLock( pLua );

    if ( pLock->pInfo != NULL ) {
        if ( pLock->pInfo->depth > 0 ) {
            sem_post( pLock->pInfo->pSem );
        }
	sem_close( pLock->pInfo->pSem );
        sem_unlink( pLock->pInfo->name );
        free( pLock->pInfo );
    }
    return 1;
}

static int helper_lock_tostring( lua_State * pLua )
{
    toString( pLua, LOCK_ID );
}



static helper_digestInfo_t * helper_setup( const EVP_MD * pEvpMd )
{
    if ( pEvpMd == NULL ) {
        return NULL;
    }

    helper_digestInfo_t * pInfo = malloc( sizeof( helper_digestInfo_t ) );
    if ( pInfo == NULL ) {
        return NULL;
    }

    pInfo->mdsize = EVP_MD_size( pEvpMd );
    EVP_MD_CTX_init( &pInfo->mctx );

    if ( !EVP_DigestInit_ex( &pInfo->mctx, pEvpMd, NULL) ) {
        EVP_MD_CTX_cleanup( &pInfo->mctx );
        free( pInfo );
        return NULL;
    }
    
    return pInfo;
}

static int helper_write( helper_digestInfo_t * pInfo, const void * pData, int size )
{
    return EVP_DigestUpdate( &pInfo->mctx, pData, size );
}

static int helper_fix( helper_digestInfo_t * pInfo, void * pMDBuf, int size )
{
    if ( size < pInfo->mdsize ) {
        return 0;
    }
    
    if ( !EVP_DigestFinal_ex(&pInfo->mctx, pMDBuf, NULL ) ) {
        return 0;
    }
    return 1;
}

static void * helper_dispose( helper_digestInfo_t * pInfo )
{
    EVP_MD_CTX_cleanup( &pInfo->mctx );

    free( pInfo );
}
    
#ifdef HELPER_STAND_ALONE
int main(){
    helper_digestInfo_t * pInfo = helper_setup( EVP_md5() );

    if ( pInfo == NULL ) {
        return 1;
    }

    helper_write( pInfo, "123", 3 );

    unsigned char * pData = malloc( pInfo->mdsize );

    helper_fix( pInfo, pData, pInfo->mdsize );

    int index;
    for ( index = 0; index < pInfo->mdsize; index++ ) {
        printf( "%02x", (unsigned char)pData[ index ] );
    }
    printf( "\n" );

    helper_dispose( pInfo );
}
#endif
