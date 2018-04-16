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

