var width = 960,
    height = 400;

var fill = d3.scale.category20();

var force = d3.layout.force()
    .size([width, height])
    .nodes( [] )
    .linkDistance(100)
    .charge(-150)
    .on("tick", tick);

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height)
    .on("mousemove", mousemove)
    .on("click", mousedownCanvas);

var defs = svg.append("defs")

defs.append("marker")
    .attr({
        "id":"arrow",
        "markerUnits":"userSpaceOnUse",
        "markerWidth":"12",
        "markerHeight":"12",
        "viewBox": "0 0 10 10",
        "refX": "17",
        "refY": "6",
        "orient": "auto"
    })
    .append("path")
    .attr("d", "M2,2 L10,6 L2,10 L6,6 L2,2")
    .style( "fill", "red" );

svg.append("rect")
    .attr("width", width)
    .attr("height", height);

var nodes = force.nodes(),
    links = force.links(),
    node = svg.selectAll(".node"),
    link = svg.selectAll(".link");

var label = svg.selectAll("label");

var cursor = svg.append("circle")
    .attr("r", 30)
    .attr("transform", "translate(-100,-100)")
    .attr("class", "cursor");

var drag = d3.behavior.drag()
    .origin(function(d) { return d; })
    .on("dragstart", dragstart)
    .on("drag", drag)
    .on("dragend", dragend);

addNode( [ 0, 0 ] );

function mousemove() {
    cursor.attr("transform", "translate(" + d3.mouse(this) + ")");
}

function mousedownCanvas() {
    addNode( d3.mouse(this) )
}


function addNode( point ) {
    var node = {x: point[0], y: point[1], name: "hoge" };

    // add links to any nearby nodes
    nodes.forEach(function(target) {
        var x = target.x - node.x,
            y = target.y - node.y;
        if (Math.sqrt(x * x + y * y) < 40) {
            links.push({source: node, target: target});
        }
    });

    nodes.push(node);

    restart();
}


function mousedownNode(d, i) {
    var node = nodes.splice(i, 1)[0];

    links = links.filter(function(l) {
        return l.source !== d && l.target !== d;
    });
    d3.event.stopPropagation();
    restart();
}

function restart() {
    node = node.data(nodes);

    var nodeEnter = node.enter().append( "circle" )
        .attr("class", "node")
        .attr("r", 10)
        .style( "opacity", 1)
        .on( "click", mousedownNode )
        .call( drag );

    node.exit().remove();

    label = label.data( nodes );

    label.enter().append("text")
        .attr("class", "label")
        .attr( "fill", "black" )
        .attr("dx", 14)
        .attr("dy", ".35em")
        .text( function(d) { return d.name } );
    
    label.exit().remove();
    
    link = link.data(links);

    link.enter().append( "line" )
        .attr("class", "link")
        .style( "stroke", "black" )
        .attr( "stroke-width", 2 )
        .attr( "marker-end", "url(#arrow)" );
    
    link.exit()
        .remove();

    force.start();
}

function tick() {
    link.attr("x1", function(d) { return d.source.x; })
        .attr("y1", function(d) { return d.source.y; })
        .attr("x2", function(d) { return d.target.x; })
        .attr("y2", function(d) { return d.target.y; });

    node.attr("cx", function(d) { return d.x; })
        .attr("cy", function(d) { return d.y; });

    label.attr("x", function(d) { return d.x; })
        .attr("y", function(d) { return d.y; });
}

function dragstart(d) {
    d3.event.sourceEvent.stopPropagation();
    d3.select(this).classed("dragging", true);
}

function drag(d) {
    d3.select(this).attr("x", d.x = d3.event.x)
        .attr("y", d.y = d3.event.y);
    force.start();
}

function dragend(d) {
    d3.select(this).classed("dragging", false);
}
