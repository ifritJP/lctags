function lctags_graph_force( paramInfo ) {

    var obj = {};

    var width = window.innerWidth;
    var height = window.innerHeight;

    // ノードの半径
    var node_r = 10;
    
    // バインドするデータ
    obj.nodes = [];
    obj.links = [];
    obj.nodeMap = new Map();
    obj.orgX = width / 2;
    obj.orgY = height / 2;
    obj.dragX = 0;
    obj.dragY = 0;
    obj.focus = null;
    obj.dragMove = d3.drag()
               .on("start", lctags_svg_move_dragstarted)
               .on("drag", lctags_svg_move_dragged)
               .on("end", lctags_svg_move_dragended);
    obj.dragSelect = d3.drag()
               .on("start", lctags_svg_select_dragstarted)
               .on("drag", lctags_svg_select_dragged)
               .on("end", lctags_svg_select_dragended); 
    obj.dragMode = obj.dragMove;

    var svg = d3.select("body").append("svg")
        .attr("width", width).attr("height", height)
        .on("click", paramInfo.svgClick )
        .on( "contextmenu",
             function( d, i ) {
                 // ブラウザの contextmenu を表示しない
                 d3.event.preventDefault();
                 
                 // svg の右クリックはモード切り替え
                 if ( obj.dragMode === obj.dragMove ) {
                     obj.dragMode = obj.dragSelect;
                     svg.style( "cursor", "crosshair" );
                 }
                 else {
                     obj.dragMode = obj.dragMove;
                     svg.style( "cursor", "default" );
                 }
                 svg.call( obj.dragMode );
             } )
        .call( obj.dragMode );

    // 矢印の定義
    var defs = svg.append("defs");
    var marker = defs.append("marker")
        .attr( "id", "arrow" )
        .attr( "markerUnits", "userSpaceOnUse" )
        .attr( "markerWidth", "12" )
        .attr( "markerHeight", "12" )
        .attr( "viewBox", "0 0 10 10" )
        .attr( "refX", "17" )
        .attr( "refY", "6" )
        .attr( "orient", "auto" );
    marker.append("path")
        .attr("d", "M2,2 L10,6 L2,10 L6,6 L2,2")
        .style( "fill", "red" );

    var g = svg.append("g").attr(
        "transform", "translate(" + obj.orgX + "," + obj.orgY + ")");

    // SVG の画像要素
    obj.sel = {};
    obj.sel.node = g.selectAll(".node");
    obj.sel.link = g.selectAll(".link");
    obj.sel.label = g.selectAll(".label");

    var selectRect = null;

    var simulation = d3.forceSimulation( obj.nodes )
        .on("tick", lctags_ticked );

    // ドラッグ制御
    var drag = d3.drag()
        .on("start", lctags_dragstarted)
        .on("drag", lctags_dragged)
        .on("end", lctags_dragended);

    obj.addNodeLink = function( nodeInfoArray, linkInfoArray ) {
        var modFlag = false;
        
        if ( nodeInfoArray ) {
            for ( index in nodeInfoArray ) {
                var info = nodeInfoArray[ index ];
                var node = { x: info.pos[0],
                             y: info.pos[1],
                             name: info.name, nsId: info.nsId,
                             opened: false, selected: false,
                             dstMap: new Map(), srcMap: new Map()
                           };
                obj.nodes.push( node );
                obj.nodeMap.set( node.nsId, node );
                modFlag = true;
            }
        }

        if ( linkInfoArray ) {
            for ( index in linkInfoArray ) {
                var info = linkInfoArray[ index ];
                if ( info.src != info.dst ) {
                    var srcNode = obj.nodeMap.get( info.src );
                    var dstNode = obj.nodeMap.get( info.dst );

                    var link = { source: srcNode, target: dstNode };
                    
                    srcNode.dstMap.set( dstNode, link );
                    dstNode.srcMap.set( srcNode, link );

                    var weight =
                        srcNode.dstMap.size +
                        srcNode.srcMap.size +
                        dstNode.dstMap.size +
                        dstNode.srcMap.size;
                    link.weight = weight;
                    
                    obj.links.push( link );
                    modFlag = true;
                }
            }
        }

        
        lctags_update( obj, modFlag );
    };


    obj.deleteNode = function( node ) {
        // node を削除する

        var deleteNode = function( delNode ) {
            obj.nodes = obj.nodes.filter(function(n) {
                return delNode.nsId != n.nsId;
            });
            obj.nodeMap.delete( delNode.nsId );

            delNode.srcMap.forEach( function( link, srcNode ) {
                srcNode.dstMap.delete( delNode );
            } );
            delNode.dstMap.forEach( function( link, dstNode ) {
                dstNode.srcMap.delete( delNode );
            } );


            // node に繋がっている link を削除
            obj.links = obj.links.filter(function(l) {
                if ( l.source.index == delNode.index ||
                     l.target.index == delNode.index ) {
                    return false;
                }

                l.weight = l.source.dstMap.size +
                    l.source.srcMap.size +
                    l.target.dstMap.size +
                    l.target.srcMap.size;
                
                return true;
            });
        };

        if ( node.selected ) {
            var list = [];
            obj.nodes.forEach(
                function( n ) {
                    if ( n.selected ) {
                        list.push( n );
                    }
                } );
            list.forEach( deleteNode );
        }
        else {
            deleteNode( node );
        }
        // graph を更新
        lctags_update( obj, true );
    };

    function lctags_update( obj, startForceFlag ) {

        // transition
        var t = d3.transition().duration(750);    

        // node の更新  ======>

        // 新しく nodes をバインド
        obj.sel.node = obj.sel.node.data(
            obj.nodes, function( d ) { return d.nsId; } );
        // バインドした情報に存在しない DOM を削除
        obj.sel.removedNode = obj.sel.node.exit();
        obj.sel.removedNode.transition( t ).attr( "r", 1e-4 ).remove();

        // 既存の node に対する操作
        obj.sel.node.transition( t )        
            .style( "fill",
                    function( node ) {
                        if ( node.opened ) {
                            return "black";
                        }
                        else {
                            return "green";
                        }
                    });

        // 新しくバインドした nodes を元に DOM を生成
        obj.sel.node = obj.sel.node.enter().append("circle")
            .style("fill", "green")
            .attr("r", node_r )
            .call( drag ) // ドラッグ対象とする
            .on( "contextmenu",
                 function( d, i ) {
                     // 親 element にイベントを通知しない
                     d3.event.stopPropagation();
                     // ブラウザの contextmenu を表示しない
                     d3.event.preventDefault();

                     // node の右クリックは削除
                     obj.deleteNode( d );
                 } )
            .on("click",
                function( node ) {
                    obj.focus = node;
                    paramInfo.nodeClick( node );
                })
        // 追加分の DOM を生成する
            .merge( obj.sel.node ) // 前の DOM とマージする
            .style( "stroke",
                    function( node ) {
                        if ( node.selected ) {
                            return "red";
                        }
                        return "none";
                    } );
        
        // label の更新 ======>
        obj.sel.label = obj.sel.label.data( obj.nodes );
        obj.sel.label.exit().remove();

        obj.sel.label.style( "z-index",
                             function( node ) {
                                 if ( node === obj.focus ) {
                                     return 100;
                                 }
                                 return 0;
                             } )
            .text( function(d) { return d.name; } )
            .attr( "fill",
                   function( node ) {
                       if ( node === obj.focus ) {
                           return "red";
                       }
                       return "gray";
                   } );
        
        obj.sel.label = obj.sel.label.enter().append("text")
            .attr("class", "label")
            .attr( "fill", "gray" )
            .attr( "font-weight", "bold" )
            .attr( "font-size", 15 )
            // .attr( "stroke", "blue" )
            // .attr( "stroke-width", 1 )
            .attr("dx", 18)
            .attr("dy", ".35em")
            .style( "pointer-events", "none" )
            .text( function(d) { return d.name; } )
            .merge( obj.sel.label );

        // link の更新 ======>
        obj.sel.link = obj.sel.link.data( obj.links );
        obj.sel.link.exit().remove();
        
        obj.sel.link = obj.sel.link.enter().append("line")
            .style( "stroke", "black" )
            .style( "pointer-events", "none" )
            .attr( "stroke-width", 2 )
            .attr( "marker-end", "url(#arrow)" )
            .merge( obj.sel.link );

        if ( startForceFlag ) {
            // forceSimulation 開始
            simulation.nodes( obj.nodes )
                .velocityDecay(0.80)
                .force("charge", d3.forceManyBody().strength(-200))
            // 中心に集るとごちゃごちゃするので、集めない。
            // .force("center", d3.forceCenter() )
                .force( "link",
                        d3.forceLink( obj.links )
                        .distance(
                            function( link ) {
                                var dis = link.weight * 10 + 70;
                                if ( dis > 250 ) {
                                    return 250;
                                }
                                return dis;
                            })
                        .strength(0.9).iterations(2) )
                .alpha(1)
                .alphaTarget(0).restart();
        }

    };
        

    function lctags_ticked() {
        obj.sel.link
            .attr("x1", function(d) { return d.source.x; })
            .attr("y1", function(d) { return d.source.y; })
            .attr("x2", function(d) { return d.target.x; })
            .attr("y2", function(d) { return d.target.y; });

        obj.sel.node
            .attr("cx", function( node ) { return node.x; })
            .attr("cy", function( node ) { return node.y; });

        // obj.sel.removedNode
        //     .attr("cx", function( node ) { node.x += 2; return node.x; })
        //     .attr("cy", function( node ) { return node.y; });
            

        obj.sel.label.attr("x", function(d) { return d.x; })
            .attr("y", function(d) { return d.y; });
    }

    function lctags_dragstarted(d) {
        obj.focus = d;
        lctags_update( obj, false );
        if (!d3.event.active) {
            simulation.alphaTarget(0.3).restart();
        }
        d.fx = d.x;
        d.fy = d.y;
    }

    function lctags_dragged(d) {
        d.fx = d3.event.x;
        d.fy = d3.event.y;
    }

    function lctags_dragended(d) {
        if (!d3.event.active) {
            simulation.alphaTarget(0);
        }
        d.fx = null;
        d.fy = null;
    }

    function lctags_svg_move_dragstarted(d) {
        obj.dragX = d3.event.x;
        obj.dragY = d3.event.y;
    }

    function lctags_svg_move_dragged(d) {
        var x = obj.orgX - ( obj.dragX - d3.event.x );
        var y = obj.orgY - ( obj.dragY - d3.event.y );
        g.attr( "transform", "translate(" + x + "," + y + ")");
    }

    function lctags_svg_move_dragended(d) {
        obj.orgX -= ( obj.dragX - d3.event.x );
        obj.orgY -= ( obj.dragY - d3.event.y );
        g.attr( "transform", "translate(" + obj.orgX + "," + obj.orgY + ")");
    }

    function lctags_svg_select_dragstarted(d) {
        obj.dragX = d3.event.x - obj.orgX;
        obj.dragY = d3.event.y - obj.orgY;
        selectRect = g.append("rect")
            .attr( "x", obj.dragX )
            .attr( "y", obj.dragY )
            .attr( "width", 5 )
            .attr( "height", 5 )
            .style( "fill", "none" )
            .style( "stroke", "black" );


        obj.nodes.forEach( function( node ) {node.selected = false; } );
        if (!d3.event.active) {
            simulation.alphaTarget(0.3).restart();
        }
    }

    function lctags_svg_select_dragged(d) {

        var x1 = obj.dragX;
        var y1 = obj.dragY;
        var posX = d3.event.x - obj.orgX;
        var posY = d3.event.y - obj.orgY;

        if ( x1 > posX ) {
            // swap
            var work = x1; x1 = posX; posX = work;
        }
        if ( y1 > posY ) {
            // swap
            var work = y1; y1 = posY; posY = work;
        }
        
        var width = posX - x1;
        var height = posY - y1;
        posX -= node_r;
        posY -= node_r;

        selectRect
            .attr( "x", x1 )
            .attr( "y", y1 )
            .attr( "width", width )
            .attr( "height", height );

        obj.nodes.forEach(
            function( node ) {
                if ( node.x >= x1 && ( node.x <= posX ) &&
                     node.y >= y1 && ( node.y <= posY ) ) {
                    node.selected = true;
                }
                else { 
                    node.selected = false;
                }
            } );
        lctags_update( obj, false );
    }

    function lctags_svg_select_dragended(d) {
        selectRect.remove();
        lctags_update( obj, false );
        if (!d3.event.active) {
            simulation.alphaTarget(0);
        }
    }

    return obj;
}
