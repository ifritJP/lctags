# coding:utf-8

apiInfoMap = {
    "dumpDir": { "param": ["--lctags-form", "json"] },
    "matchFile": { "param": ["?pattern", "?option", "--lctags-form", "json"] },
    "searchFile": { "param": [["?path", (lambda val: val.replace( "_", "$_" ))],
                              "--lctags-form", "json",
                              "--lctags-candidateLimit", "?limit"] },
    "searchDecl": { "param": [["?name", (lambda val: val.replace( "_", "$_" ))],
                              "--lctags-form", "json",
                              "--lctags-candidateLimit", "?limit"] },
    "defAtFileId": { "param": ["?fileId", "--lctags-form", "json"] },
    "callee": { "param": ["?nsId", "--lctags-form", "json"] },
    "caller": { "param": ["?nsId", "--lctags-form", "json"] },
    "refSym": { "param": ["?nsId", "--lctags-form", "json"] },
    "refDir": { "param": ["?path", "--lctags-form", "json"] },
    "refFile": { "param": ["?fileId", "?path", "--lctags-form", "json"] },
    "reqDir": { "param": ["?path", "--lctags-form", "json"] },
    "reqFile": { "param": ["?fileId", "?path", "--lctags-form", "json"] },
    "decl": { "param": ["?nsId", "--lctags-form", "json"] },
}
