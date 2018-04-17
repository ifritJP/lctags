window.onload = function() {
    var newName = 0;
    var cy = cytoscape({
	container: document.getElementById('cy'),
        elements: [
	    { data: { id: 'a', name: 'A' } },
            { data: { id: 'b', name: 'B' } },
            { data: { id: 'c', name: 'C' } },
            {
		data: {
		    id: 'ab',
		    source: 'a',
		    target: 'b'
		}
	    }],
	style: [
	    {
		selector: 'node',
		style: {
		    'content': "data(name)"
		}
	    }
	]
    });

    cy.on('tap', 'node', function(){
	var work = 'd' + newName;
	var curNode = this.data('id')
	cy.add( [ { group: "nodes", data: { id: work, name: work } } ] );
	cy.add( [ { group: "edges", data: { id: 'D' + newName,
					    source: curNode, target: work } } ] );
	newName = newName + 1;


	var layout = cy.elements().layout( { name: 'breadthfirst', directed: true } );
	// var layout = cy.elements().layout( { name: 'cose' } );
	// var layout = cy.elements().layout( { name: 'concentric' } );
	layout.run();

	// cy.add( [ { group: "edges",
	// 		 data: { id: 'ac', source: 'a', target: 'c' } } ] );
	// cy.add( [ { group: "nodes",
	// 	    data: { id: 'd', name: 'D' } },
	// 	  { group: "edges", data: { id: 'ad', source: 'a', target: 'd' } },
	// 	  { group: "edges", data: { id: 'cd', source: 'c', target: 'd' } }
	// 	] );
	
	 // cy.fit();
	
	// try { // your browser may block popups
	//     window.open( this.data('href') );
	// } catch(e){ // fall back on url change
	//     window.location.href = this.data('href');
	// }
    });
}

function dumpDir() {
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

	var parentObj = $('#hoge' ).get( 0 );
	for (val in dirList) {
	    var obj = document.createElement( "button" );
	    obj.type = "button";
	    var path = dirList[ val ];
	    obj.innerHTML = path;
	    var fileListObj = document.createElement( "div" );
	    obj.onclick = function( path, obj ) {
		return function() {
		    if ( !obj.dataset[ 'opened' ] ) {
			obj.dataset[ 'opened' ] = true;
			matchFile( path, obj );
		    }
		};
	    }( path, fileListObj );
	    
	    parentObj.appendChild( obj );
	    parentObj.appendChild( fileListObj );
	    //parentObj.appendChild( document.createElement( "br" ) );
	}
    }).fail(function() {
    });
}

function matchFile( dirPath, parentObj ) {
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
	    var info = fileList[ val ];
	    var path = info.path.replace( dirPath + "/", "" );
	    obj.innerHTML = info.fileId + ":" + path;
	    obj.onclick = function( info ) {
		return function() {
		    window.open(
			"/lctags/gen/file.html?id=" + info.fileId, "newrtab" );
		};
	    }(info);
	    parentObj.appendChild( obj );
	    parentObj.appendChild( document.createElement( "br" ) );
	}
    }).fail(function() {
    });
}

function getFileInfo( fileId ) {
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
			"/lctags/gen/file.html?id=" + info.fileId, "newrtab" );
		};
	    }(info);
	    
	    parentObj.appendChild( obj );
	    parentObj.appendChild( document.createElement( "br" ) );
	}
    }).fail(function() {
    });
}
