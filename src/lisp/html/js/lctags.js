var lctags_window_map = new Map();

var lctags_getPath = function( path, confId ) {
    return "/lctags/" + path + "?confId=" + confId;
}

function lctags_getCookies( cookiesEle ) {
    $.ajax({
        url: '/lctags/get?command=cookies',
        type: 'GET',
        timeout: 5000
    }).done(function(data) {
        var cookieListObj = data.list;
        var cookieList = [];
        for (val in cookieListObj) {
            cookieList.push( cookieListObj[val] );
        }
        cookieList = cookieList.sort(
            function( obj1, obj2 ) {
                return obj1.cookie < obj2.cookie;
            });

        var validSymbol = false;
        for (val in cookieList) {
            var obj = document.createElement( "a" );
            var cookieInfo = cookieList[ val ];
            obj.innerHTML = cookieInfo.db;
            obj.href = "/lctags/start?cookie=" + cookieInfo.cookie;

            cookiesEle.appendChild( obj );
            cookiesEle.appendChild( document.createElement( "br" ) );

            if ( cookieInfo.targetSymbol ) {
                validSymbol = true;
                var obj = document.createElement( "a" );
                var cookieInfo = cookieList[ val ];
                obj.innerHTML = cookieInfo.db;
                if ( cookieInfo.targetSymbol ) {
                    obj.innerHTML = obj.innerHTML + "  (" + cookieInfo.targetSymbol + ")";
                }

                obj.href = "/lctags/start?cookie=" + cookieInfo.cookie + "&jumpCallGraph=t";

                cookiesEle.appendChild( obj );
                cookiesEle.appendChild( document.createElement( "br" ) );
            }
        }
        if ( validSymbol ) {
            var obj = document.createElement( "a" );
            obj.innerHTML = "jump to lastSymbol";
            obj.href = "/lctags/start?&jumpLastSymbol=t";

            cookiesEle.appendChild( obj );
        }
        
    }).fail(function() {
    });
};

function lctags_dumpDir_decideFile( confId ) {
    var filePathObj = $('#filepath');

    var path = filePathObj.val();
    var map = filePathObj.data( "map" );
    var fileId = map.get( path );

    lctags_openFileTab( confId, fileId, path );
}

function lctags_dumpDir_decideSymbol( confId ) {
    var symbolObj = $('#symbol');

    var path = symbolObj.val();
    var map = symbolObj.data( "map" );
    var nsId = map.get( path );

    lctags_openCallGraph( confId, nsId, symbolObj.val() );
}


function lctags_autocomplete( targetObj, submitObj, urlFunc, func ) {
    var listMax = 100;
    var candidate2IdMap = new Map();
    var inqId = 0;
    var prevInput = "";
    var maxFlag = false;
    targetObj.val( "" );
    targetObj.autocomplete( { source: [] } );
    
    targetObj.keyup( function( event ) {
        var val = targetObj.val();
        if ( val.length == 0 ) {
            targetObj.autocomplete( "destroy" );
            targetObj.autocomplete( { source: [] } );
            return;
        }
        if ( prevInput == val || candidate2IdMap.has( val ) ) {
            return;            
        }
        if ( val.indexOf( " " ) == -1 ) {
            if ( prevInput != "" && !maxFlag && val.startsWith( prevInput ) ) {
                return;
            }
        }
        setTimeout(
            function( prev ) {
                return function() {
                    var input = targetObj.val();
                    if ( prev != input ) {
                        return;
                    }
                    var nowInq = inqId;
                    submitObj.prop( "disabled", true );
                    $.ajax({
                        url: urlFunc( input ),
                        type: 'GET',
                        timeout: 10 * 1000
                    }).done(function(data) {
                        if ( nowInq != inqId ) {
                            return;
                        }
                        inqId++;

                        candidate2IdMap.clear();
                        var candidateList = [];
                        maxFlag = func( data.lctags_result,
                                        candidateList, candidate2IdMap );
                        candidateList = candidateList.sort();

                        targetObj.autocomplete( "destroy" );
                        targetObj.autocomplete({
                            minLength: 0,
                            source: candidateList
                        });
                        targetObj.autocomplete( "search", "" );
                        targetObj.data( "map", candidate2IdMap );
                        prevInput = targetObj.val();
                        submitObj.prop( "disabled", false );
                    }).fail(function() {
                    });
                };
            }( targetObj.val() ), 300 );
    });
    
}


function lctags_dumpDir( confId, projDir ) {

    var limit = 100;

    lctags_autocomplete(
        $('#filepath'), $("#filepathGo"),
        function( input ) {
            return lctags_getPath( 'inq', confId ) +
                "&command=searchFile&path=" +
                input.replace( / /g, "%%" ) + "&limit=" + limit;
        },
        function( data, candidateList, candidate2IdMap ) {
            data.searchFile.forEach( function( info ) {
                var fileInfo = info.info;
                var path = fileInfo.path;
                if ( path.startsWith( projDir + "/" ) ) {
                    path = path.replace( projDir + "/", "" );
                }
                candidateList.push( path );
                candidate2IdMap.set( path, fileInfo.fileId );
            } );
            return candidateList.length == 100;
        }
    );


    lctags_autocomplete(
        $('#symbol'), $("#symbolGo"),
        function( input ) {
            return lctags_getPath( 'inq', confId ) +
                "&command=searchDecl&name=" +
                input.replace( / /g, "%" ) + "&limit=" + limit;
        },
        function( data, candidateList, candidate2IdMap ) {
            data.searchDecl.forEach( function( info ) {
                var nameInfo = info.info;
                var name = nameInfo.name;
                if ( candidate2IdMap.has( name ) ) {
                    return;
                }
                candidateList.push( name );
                candidate2IdMap.set( name, nameInfo.nsId );
            } );
            return data.searchDecl.length == 100;
        }
    );
    

    
    $.ajax({
        url: lctags_getPath( "inq", confId ) + "&command=dumpDir",
        type: 'GET',
        timeout: 5000
    }).done(function(data) {
        var dirListObj = data.lctags_result.dumpDir;
        var dirList = [];
        for (val in dirListObj) {
            dirList.push( dirListObj[ val ].path );
        }
        dirList = dirList.sort();

        var parentObj = $('#file-list' ).get( 0 );
        for (val in dirList) {
            var obj = document.createElement( "button" );
            obj.type = "button";
            var path = dirList[ val ];
            obj.innerHTML = path;
            jQuery.data( obj, "opened", false );
            var fileListObj = document.createElement( "div" );
            obj.onclick = function( path, obj ) {
                return function() {
                    var opened = jQuery.data( obj, "opened" );
                    if ( !opened ) {
                        var refDirButton = document.createElement( "button" );
                        refDirButton.type = "button";
                        refDirButton.innerHTML = "refDir";
                        refDirButton.classList.add( "dir_file" );
                        refDirButton.onclick = function( path ) {
                            return function() {
                                lctags_openRefDirTab( confId, path );
                            };
                        }( path );
                        
                        obj.appendChild( refDirButton );
                        obj.appendChild( document.createElement( "br" ) );
                        
                        lctags_matchFile( confId, path, obj );
                    }
                    else {
                        while (obj.firstChild) {
                            obj.removeChild(obj.firstChild);
                        }
                    }
                    jQuery.data( obj, "opened", !opened );
                };
            }( path, fileListObj );
            
            parentObj.appendChild( obj );
            parentObj.appendChild( fileListObj );
            //parentObj.appendChild( document.createElement( "br" ) );
        }
    }).fail(function() {
    });
}

function lctags_openRefDirTab( confId, path ) {
    var key = "lctags:refDir:" + path;
    var win = lctags_window_map.get( key );
    if ( win ) {
        win.close();
    }
    var newWindow = window.open(
        lctags_getPath( "gen/func-module-graph.html", confId ) +
            "&path=" + path, key );
    lctags_window_map.set( key, newWindow );
}

function lctags_openRefFileTab( confId, fileId, path ) {
    var key = "lctags:refFile:" + path;
    var win = lctags_window_map.get( key );
    if ( win ) {
        win.close();
    }
    var newWindow = window.open(
        lctags_getPath( "gen/func-module-graph.html", confId ) +
            "&fileId=" + fileId + "&path=" + path, key );
    lctags_window_map.set( key, newWindow );
}


function lctags_openFileTab( confId, fileId, path ) {
    var key = "lctags:file:" + fileId;
    var win = lctags_window_map.get( key );
    if ( win ) {
        win.close();
    }
    var newWindow = window.open(
        lctags_getPath( "gen/file.html", confId ) +
            "&fileId=" + fileId + "&path=" + path, key );
    lctags_window_map.set( key, newWindow );
}

function lctags_matchFile( confId, dirPath, parentObj ) {
    $.ajax({
        url: lctags_getPath( 'inq', confId ) + '&command=matchFile&pattern=' + dirPath,
        type: 'GET',
        timeout: 5000
    }).done(function(data) {
        var fileListObj = data.lctags_result.matchFile;
        var fileList = [];
        for (val in fileListObj) {
            fileList.push( fileListObj[ val ].info );
        }
        fileList = fileList.sort(
            function( obj1, obj2 ) {
                return obj1.path.localeCompare( obj2.path );
            });
        
        for (val in fileList) {
            var obj = document.createElement( "button" );
            obj.type = "button";
            obj.classList.add( "dir_file" );
            var info = fileList[ val ];
            var path = info.path.replace( dirPath + "/", "" );
            obj.innerHTML = path + " (" + info.fileId + ")";
            obj.onclick = function( info ) {
                return function() {
                    lctags_openFileTab( confId, info.fileId, info.path );
                };
            }(info);
            parentObj.appendChild( obj );
            parentObj.appendChild( document.createElement( "br" ) );
        }
    }).fail(function() {
    });
}

function lctags_openCallGraph( confId, nsId, name ) {
    var key = "lctags:func:" + nsId;
    var win = lctags_window_map.get( key );
    if ( win ) {
        win.close();
    }
    var newWindow = window.open(
        lctags_getPath( "gen/func-call-graph.html", confId ) +
            "&nsId=" + nsId + "&name=" + name, key );
    lctags_window_map.set( key, newWindow );
}

function lctags_getFileInfo( confId, fileId, filePath ) {
    $.ajax({
        url: lctags_getPath( 'inq', confId ) + "&command=defAtFileId&fileId=" + fileId,
        type: 'GET',
        timeout: 5000
    }).done(function(data) {
        var defListObj = data.lctags_result.defAtFileId;
        var defList = [];
        var idMap = new Map();
        for (val in defListObj) {
            var info = defListObj[ val ].info;
            var key = "" + info.nsId + info.type;
            if ( idMap.has( key ) ) {
                continue;
            }
            defList.push( info );
            idMap.set( key );
        }

        defList = defList.sort(
            function( obj1, obj2 ) {
                return obj1.name.localeCompare( obj2.name );
            });

    
       
        var parentObj = $('#file-cont' ).get( 0 );

        var refFileButton = document.createElement( "button" );
        refFileButton.type = "button";
        refFileButton.innerHTML = "refDir";
        refFileButton.classList.add( "dir_file" );
        refFileButton.onclick = function( fileId, path ) {
            return function() {
                lctags_openRefFileTab( confId, fileId, path );
            };
        }( fileId, filePath );
        
        parentObj.appendChild( refFileButton );
        parentObj.appendChild( document.createElement( "br" ) );

        
        var listing = function( labelName, typeList, titleTypeList ) {
            var typeSet = new Set( typeList );
            var titleSet = new Set( titleTypeList );

            var label = document.createElement( "h1" );
            label.innerHTML = labelName;
            parentObj.appendChild( label );
            
            return function( info ) {
                if ( !typeSet.has( info.type ) ) {
                    return;
                }

                var obj = document.createElement( "button" );
                obj.type = "button";
                
                obj.innerHTML = info.name + " (" + info.nsId + ")";
                obj.onclick = function( info ) {
                    return function() {
                        lctags_openCallGraph( confId, info.nsId, info.name );
                    };
                }(info);

                if ( titleSet.has( info.type ) ) {
                    var label = document.createElement( "h2" );
                    label.innerHTML = info.name;
                    parentObj.appendChild( label );
                }
                
                parentObj.appendChild( obj );
                parentObj.appendChild( document.createElement( "br" ) );
            };
        };

        // 現状マクロは参照元の namespace が全て 0 になってしまい、
        // コールグラフ表示の意味がないがないので出さない
        // defList.forEach( listing( "macro", [ "MacroDefinition" ]) );
        
        defList.forEach( listing( "typedef", [ "TypedefDecl" ] ) );
        
        defList.forEach( listing(
            "enums", [ "EnumDecl", "EnumConstantDecl" ], [ "EnumDecl" ] ) );

        defList.forEach( listing(
            "structs/unions/classes",
            [ "ClassDecl", "UnionDecl", "StructDecl", "FieldDecl" ],
            [ "ClassDecl", "UnionDecl", "StructDecl" ]) );

        defList.forEach( listing( "variables", [ "VarDecl" ]) );
        
        defList.forEach( listing( "functions", [ "FunctionDecl", "CXXMethod",
                                                 "Constructor", "Destructor"]) );
        
        
        
    }).fail(function() {
    });
}

function lctags_funcCallGraph_force( confId, nsId, name ) {

    var obj;
    var paramInfo = {
        svgClick: function() {
            ;
        },
        nodeClick: function( node ) {
            d3.event.stopPropagation();
            if ( node.opened ) {
                obj.addNodeLink( [], [] );
            }
            else {
                node.opened = true;
                //obj.lctags_deleteNode( node );

                var nsId = node.nsId;
                $.ajax({
                    url: lctags_getPath( 'inq', confId ) + '&command=callee&nsId=' + nsId,
                    type: 'GET',
                    timeout: 5000
                }).done(function(data) {
                    var funcListObj = data.lctags_result.callee;

                    var nodeInfoArray = [];
                    var linkInfoArray = [];
                    for (val in funcListObj) {
                        var info = funcListObj[ val ].info;

                        if ( !obj.nodeMap.has( info.nsId ) ) {
                            nodeInfoArray.push(
                                { nsId: info.nsId,
                                  name: info.name, pos: [ node.x, node.y ] } );
                        }

                        linkInfoArray.push(
                            { src: nsId, dst: info.nsId } );
                    }

                    obj.addNodeLink( nodeInfoArray, linkInfoArray );
                }).fail(function() {
                });
            }
        }
    };
    
    obj = lctags_graph_force( paramInfo );

    obj.addNodeLink(
        [ { nsId: nsId, name: name, pos: [ 0, 0 ] } ], null );
}


function lctags_funcCallGraph_tree( projDir, confId, nsId, name ) {

    var paramInfo = {
        svgClick: function() {
            ;
        },
        nodeClick: function( obj, node ) {
            var nsId = node.nsId;
            var nodeId = node.id;
            var command = "callee";
            if ( obj.expandMode == "caller" ) {
                command = "caller";
            }
            else if ( obj.expandMode == "refSym" ) {
                command = "refSym";
            }
            $.ajax({
                url: lctags_getPath( 'inq', confId ) +
                    "&command=" + command + '&nsId=' + nsId,
                type: 'GET',
                timeout: 10 * 1000
            }).done(function(data) {
                var funcListObj = data.lctags_result[ command ];

                var nodeInfoArray = [];
                var nsIdSet = new Set();
                for (val in funcListObj) {
                    var info = funcListObj[ val ].info;

                    if ( !nsIdSet.has( info.nsId ) ) {
                        nsIdSet.add( info.nsId );
                        nodeInfoArray.push(
                            { nsId: info.nsId, name: info.name,
                              type: info.type, fileId: info.fileId } );
                    }
                }

                
                var fileListObj = data.lctags_result[ "fileList" ];
                var fileInfoArray = [];
                for (val in fileListObj) {
                    var info = fileListObj[ val ].info;

                    fileInfoArray.push(
                        { fileId: info.fileId, path: info.path } );
                }
                
                nodeInfoArray = nodeInfoArray.sort( function( obj1, obj2 ) {
                    return obj1.name.localeCompare( obj2.name );
                });

                obj.addChild( nodeId, nodeInfoArray, fileInfoArray );
            }).fail(function() {
            });
        },
        pathClick: function( obj, path ) {
            d3.event.stopPropagation();
            var srcNode = path.source.data;
            var dstNode = path.target.data;

            if ( obj.expandMode == "caller" ) {
                srcNode = path.target.data;
                dstNode = path.source.data;
            }
            
            var command = "callPair";
            if ( obj.expandMode == "refSym" ) {
                command = "refPair";
                srcNode = path.target.data;
                dstNode = path.source.data;
            }
            $.ajax({
                url: lctags_getPath( 'inq', confId ) +
                    '&command=' + command + '&nsId=' + dstNode.nsId +
                    "&belongNsId=" + srcNode.nsId,
                type: 'GET',
                timeout: 5 * 1000
            }).done(function(data) {
                var funcListObj = data.lctags_result.callPair;
            }).fail(function() {
            });
        },
        nodeContext: function( obj, node ) {
            d3.event.stopPropagation();
            var nsId = node.nsId;
            $.ajax({
                url: lctags_getPath( 'inq', confId ) + '&command=openDecl&nsId=' + nsId,
                type: 'GET',
                timeout: 5000
            }).done(function(data) {
                var funcListObj = data.lctags_result.callee;

            }).fail(function() {
            });
        },
        openNewWindow: function( nsId, name ) {
            lctags_openCallGraph( confId, nsId, name );
        }
    };
    
    var obj = lctags_graph_tree( projDir, paramInfo );

    obj.addChild( null, [ { nsId: nsId, name: name, pos: [ 0, 0 ] } ], [] );

    $.ajax({
        url: lctags_getPath( 'inq', confId ) + "&command=decl&nsId=" + nsId,
        type: 'GET',
        timeout: 10 * 1000
    }).done(function(data) {
        var typeName = data.lctags_result.decl[0].info.type;
        if ( !( typeName == "FunctionDecl" || typeName == "CXXMethod" ||
                typeName == "Constructor" || typeName == "Destructor" ) ) {
                    obj.setExpandMode( "refSym" );
                }
    }).fail(function() {
    });
}

function lctags_moduleGraph_tree( projDir, confId, fileId, path ) {

    var fileId2FileInfoMap = new Map();
    var nsId2NodeMap = new Map();
    var dirPath2InfoMap = new Map();
    var nsIdSeed = 1;
    var fileIdSet = new Set();
    
    var paramInfo = {
        svgClick: function() {
        },
        nodeClick: function( obj, node ) {
            var localNode = nsId2NodeMap.get( node.nsId );
            obj.addChild( node.id, localNode.children, [] );
        },
        pathClick: function( obj, path ) {
        },
        nodeContext: function( obj, node ) {
        },
        openNewWindow: function( nsId, name ) {
        },
    };

   
    if ( !projDir.endsWith( "/" ) ) {
        projDir = projDir + "/";
    }
    // var targetDir = dir;
    // if ( targetDir.startsWith( projDir ) ) {
    //     targetDir = targetDir.substring( projDir.length );
    // }


    $.ajax({
        url: lctags_getPath( 'inq', confId ) + '&command=refFile' +
            '&fileId=' + fileId + '&path=' + path,
        type: 'GET',
        timeout: 60 * 1000
    }).done(function(data) {
        
        var refDirObj = data.lctags_result.refFile;

        var nodeInfoArray = [];
        var linkInfoArray = [];

        var fileList = [];
        data.lctags_result.fileList.forEach( function( item ) {
            var info = item.info;

            var path = info.path;
            if ( path.startsWith( projDir ) ) {
                path = "./" + path.substring( projDir.length );
            }

            var fileInfo = fileId2FileInfoMap.get( info.fileId );
            if ( fileInfo == null ) {
                fileInfo = {};
                fileInfo.path = path;
                fileInfo.refFileId2refListMap = new Map();
                fileId2FileInfoMap.set( info.fileId, fileInfo );
                fileIdSet.add( info.fileId );
                fileList.push( { fileId: info.fileId, path: path } );
            }
        });

        refDirObj.forEach( function( val ) {
            var info = val.info;

            var declFileInfo = fileId2FileInfoMap.get( info.declFileId );
            var refList = declFileInfo.refFileId2refListMap.get( info.refFileId );
            if ( refList == null ) {
                refList = [];
                declFileInfo.refFileId2refListMap.set( info.refFileId, refList );
            }
            refList.push( info );
        });

        if ( path.startsWith( projDir ) ) {
            path = "./" + path.substring( projDir.length );
        }
        
        var rootObj = { nsId: fileId, name: path };
        rootObj.children = [];

        nsId2NodeMap.set( rootObj.nsId, rootObj );

        var fileInfo = fileId2FileInfoMap.get( rootObj.nsId );
        var path2InfoMap = new Map();
        fileInfo.refFileId2refListMap.forEach( function( refList, refFileId ) {
            var childPath = fileId2FileInfoMap.get( refFileId ).path;
            var findIndex = childPath.indexOf( "/" );
            var prefix = "";
            var parentObj = rootObj;
            while ( findIndex > 0 ) {
                var dirName = childPath.substring( 0, findIndex + 1 );
                childPath = childPath.substring( findIndex + 1 );
                findIndex = childPath.indexOf( "/" );
                prefix = prefix + dirName;

                var info = path2InfoMap.get( prefix );
                if ( info == null ) {
                    info = {};
                    path2InfoMap.set( prefix, info );
                    
                    while ( fileIdSet.has( nsIdSeed ) ) {
                        nsIdSeed++;
                    }
                    info.nsId = nsIdSeed;
                    info.fileId = nsIdSeed;
                    info.name = dirName;
                    fileList.push( { fileId: nsIdSeed, path: prefix } );
                    info.children = [];
                    fileIdSet.add( nsIdSeed );
                    nsId2NodeMap.set( info.nsId, info );
                    nsIdSeed++;
                    parentObj.children.push( info );
                }
                parentObj = info;
            }
            
            var child = {};
            child.nsId = refFileId;
            child.fileId = refFileId;
            child.name = fileId2FileInfoMap.get( refFileId ).path;
            child.children = [];
            parentObj.children.push( child );
            nsId2NodeMap.set( child.nsId, child );

            refList = refList.sort( function( obj1, obj2 ) {
                return obj1.refLine > obj2.refLine;
            });

            refList.forEach( function( refInfo ) {
                while ( fileIdSet.has( nsIdSeed ) ) {
                    nsIdSeed++;
                }
                var refChild = {};
                refChild.nsId = nsIdSeed;
                refChild.fileId = refInfo.refFileId;
                refChild.name = "line: " + refInfo.refLine;
                nsIdSeed++;
                nsId2NodeMap.set( refChild.nsId, refChild );
                child.children.push( refChild );
            });
        });


        var obj = lctags_graph_tree( projDir, paramInfo );
        obj.addChild( null, [ { nsId: fileId, name: path } ], fileList );

        
    }).fail(function() {
    });

}

