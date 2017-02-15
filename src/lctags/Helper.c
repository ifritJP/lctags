/** Copyright (C) 2017 ifritJP */

#include <string.h>
#include <openssl/evp.h>
#include <openssl/engine.h>
#include <lauxlib.h>
#include <unistd.h>


#define HELPER_ID ""

#define toHelper(L)                                        \
    ((helper_userData_t *)luaL_checkudata(L, 1, HELPER_ID))

typedef struct {
    EVP_MD_CTX mctx;
    int mdsize;
} helper_digestInfo_t;

typedef struct {
    helper_digestInfo_t * pInfo;
} helper_userData_t;

typedef const EVP_MD * (evp_md_func_t)(void);

typedef struct {
    const char * pName;
    evp_md_func_t * pMd;
} helper_typeMap_t;


static int helper_openDigest( lua_State * pLua );
static int helper_msleep( lua_State * pLua );
static int helper_chdir( lua_State * pLua );
static int helper_tostring( lua_State * pLua );

static int helper_digest_write( lua_State * pLua );
static int helper_digest_get( lua_State * pLua );
static int helper_digest_fix( lua_State * pLua );
static int helper_digest_gc( lua_State * pLua );
static int helper_digest_tostring( lua_State * pLua );


static helper_digestInfo_t * helper_setup( const EVP_MD * pEvpMd );
static int helper_write( helper_digestInfo_t * pInfo, const void * pData, int size );
static int helper_fix( helper_digestInfo_t * pInfo, void * pMDBuf, int size );
static void * helper_dispose( helper_digestInfo_t * pInfo );


static const luaL_Reg s_if_lib[] = {
    { "openDigest", helper_openDigest },
    { "msleep", helper_msleep },
    { "chdir", helper_chdir },
    {"__tostring", helper_tostring },
    {NULL, NULL}
};

static const luaL_Reg s_obj_lib[] = {
    { "write", helper_digest_write },
    { "fix", helper_digest_fix },
    { "__gc", helper_digest_gc },
    {"__tostring", helper_digest_tostring },
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


int luaopen_lctags_Helper( lua_State * pLua )
{
    luaL_newmetatable(pLua, HELPER_ID );
    lua_pushvalue(pLua, -1);
    lua_setfield(pLua, -2, "__index");
    luaL_setfuncs(pLua, s_obj_lib, 0);
    lua_pop(pLua, 1);

    
    luaL_newlib( pLua, s_if_lib );
    return 1;
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
        printf( "not found -- %s", pName );
        return 0;
    }
    
    helper_digestInfo_t * pInfo = helper_setup( pMd );
    helper_userData_t * pHelper =
        lua_newuserdata( pLua, sizeof( *pHelper ) );
    if ( pHelper == NULL ) {
        helper_dispose( pInfo );
        return 0;
    }
    pHelper->pInfo = pInfo;
    
    luaL_setmetatable( pLua, HELPER_ID );

    return 1;
}



static int helper_tostring( lua_State * pLua )
{
    lua_pushstring( pLua, "<helperID>" );
    return 1;
}

static int helper_digest_write( lua_State * pLua )
{
    helper_userData_t * pHelper = toHelper( pLua );
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
    helper_userData_t * pHelper = toHelper( pLua );
    helper_digestInfo_t * pInfo = pHelper->pInfo;
    char * pBuf = malloc( pInfo->mdsize * 2 );
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
        return 1;
    }
    return 0;
}

static int helper_digest_gc( lua_State * pLua )
{
    helper_userData_t * pHelper = toHelper( pLua );

    helper_dispose( pHelper->pInfo );

    return 0;
}

static int helper_digest_tostring( lua_State * pLua )
{
    helper_userData_t * pHelper = toHelper( pLua );
    char buf[ 100 ];
    sprintf( buf, "<helper:%p>", pHelper );
    lua_pushstring( pLua, buf );
    return 1;
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
