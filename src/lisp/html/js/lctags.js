function lctags_dumpDir() {
    $.ajax({
        url: '/lctags/inq?command=dumpDir',
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
                        lctags_matchFile( path, obj );
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

function lctags_matchFile( dirPath, parentObj ) {
    $.ajax({
        url: '/lctags/inq?command=matchFile&pattern=' + dirPath,
        type: 'GET',
        timeout: 5000,
    }).done(function(data) {
        var fileListObj = data.lctags_result.matchFile;
        var fileList = [];
        for (val in fileListObj) {
            fileList.push( fileListObj[ val ].info );
        }
        for (val in fileList) {
            var obj = document.createElement( "button" );
            obj.type = "button";
            obj.classList.add( "dir_file" );
            var info = fileList[ val ];
            var path = info.path.replace( dirPath + "/", "" );
            obj.innerHTML = info.fileId + ":" + path;
            obj.onclick = function( info ) {
                return function() {
                    window.open(
                        "/lctags/gen/file.html?fileId=" + info.fileId,
                        "lctags:file:" + info.fileId );
                };
            }(info);
            parentObj.appendChild( obj );
            parentObj.appendChild( document.createElement( "br" ) );
        }
    }).fail(function() {
    });
}

function lctags_getFileInfo( fileId ) {
    $.ajax({
        url: '/lctags/inq?command=defAtFileId&fileId=' + fileId,
        type: 'GET',
        timeout: 5000,
    }).done(function(data) {
        var defListObj = data.lctags_result.defAtFileId;
        var defList = [];
        var idMap = new Map();
        for (val in defListObj) {
            var info = defListObj[ val ].info
            if ( idMap.has( info.nsId ) ) {
                continue;
            }
            defList.push( info );
            idMap.set( info.nsId );
        }
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
                    window.open(
                        "/lctags/gen/func-call-graph.html?nsId=" + info.nsId +
                            "&name=" + info.name,
                        "lctags:func:" + info.nsId );
                };
            }(info);
            
            parentObj.appendChild( obj );
            parentObj.appendChild( document.createElement( "br" ) );
        }
    }).fail(function() {
    });
}

function lctags_funcCallGraph( nsId, name ) {
// http://bl.ocks.org/tgk/6068367

    var paramInfo = {
        svgClick: function() {
            ;
        },
        nodeClick: function( node ) {
            d3.event.stopPropagation();
            if ( node.opened ) {
                obj.lctags_addNodeLink( [], [] );
            }
            else {
                node.opened = true;
                //obj.lctags_deleteNode( node );

                var nsId = node.nsId;
                $.ajax({
                    url: '/lctags/inq?command=callee&nsId=' + nsId,
                    type: 'GET',
                    timeout: 5000,
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

                    obj.lctags_addNodeLink( nodeInfoArray, linkInfoArray );
                }).fail(function() {
                });
            }
        }
    };
    
    var obj = lctags_graph( paramInfo );

    obj.lctags_addNodeLink(
        [ { nsId: nsId, name: name, pos: [ 0, 0 ] } ], null );
}
