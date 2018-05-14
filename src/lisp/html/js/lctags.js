var lctags_graph_window_map = new Map();
var lctags_file_window_map = new Map();

var lctags_getPath = function( path, confId ) {
    return "/lctags/" + path + "?confId=" + confId;
}

function lctags_getCookies( cookiesEle ) {
    $.ajax({
        url: '/lctags/inq?command=cookies',
        type: 'GET',
        timeout: 5000,
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


function lctags_dumpDir( confId ) {
    $.ajax({
        url: lctags_getPath( "inq", confId ) + "&command=dumpDir",
        type: 'GET',
        timeout: 5000,
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

function lctags_matchFile( confId, dirPath, parentObj ) {
    $.ajax({
        url: lctags_getPath( 'inq', confId ) + '&command=matchFile&pattern=' + dirPath,
        type: 'GET',
        timeout: 5000,
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
            obj.innerHTML = info.fileId + ":" + path;
            obj.onclick = function( info ) {
                return function() {
                    var key = "lctags:file:" + info.fileId;
                    var win = lctags_file_window_map.get( key );
                    if ( win ) {
                        win.close();
                    }
                    var newWindow = window.open(
                        lctags_getPath( "gen/file.html", confId ) +
                            "&fileId=" + info.fileId, key );
                    lctags_file_window_map.set( key, newWindow );
                };
            }(info);
            parentObj.appendChild( obj );
            parentObj.appendChild( document.createElement( "br" ) );
        }
    }).fail(function() {
    });
}

function lctags_getFileInfo( confId, fileId ) {
    $.ajax({
        url: lctags_getPath( 'inq', confId ) + "&command=defAtFileId&fileId=" + fileId,
        type: 'GET',
        timeout: 5000,
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
        for (val in defList) {
            var info = defList[ val ];

            if ( !( info.type == "FunctionDecl" || info.type == "CXXMethod" ||
                    info.type == "Constructor" || info.type == "Destructor" ) ) {
                        continue;
                    }

            var obj = document.createElement( "button" );
            obj.type = "button";
            
            
            obj.innerHTML = info.name;
            obj.onclick = function( info ) {
                return function() {
                    var key = "lctags:func:" + info.nsId;
                    var win = lctags_graph_window_map.get( key );
                    if ( win ) {
                        win.close();
                    }
                    var newWindow = window.open(
                        lctags_getPath( "gen/func-call-graph.html", confId ) +
                            "&nsId=" + info.nsId + "&name=" + info.name, key );
                    lctags_graph_window_map.set( key, newWindow );
                };
            }(info);
            
            parentObj.appendChild( obj );
            parentObj.appendChild( document.createElement( "br" ) );
        }
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


function lctags_funcCallGraph_tree( confId, nsId, name ) {

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
                            { nsId: info.nsId, name: info.name, type: info.type } );
                    }
                }

                nodeInfoArray = nodeInfoArray.sort( function( obj1, obj2 ) {
                    return obj1.name.localeCompare( obj2.name );
                });

                obj.addChild( nodeId, nodeInfoArray );
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
                url: lctags_getPath( 'inq', confId ) + '&command=defBody&nsId=' + nsId,
                type: 'GET',
                timeout: 5000
            }).done(function(data) {
                var funcListObj = data.lctags_result.callee;

            }).fail(function() {
            });
        }
    };
    
    var obj = lctags_graph_tree( paramInfo );

    obj.addChild( null, [ { nsId: nsId, name: name, pos: [ 0, 0 ] } ] );
}
