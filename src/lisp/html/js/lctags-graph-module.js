function lctags_graph_module( paramInfo ) {

    var obj = {};

    var width = window.innerWidth;
    var height = window.innerHeight;
    var idSeed = 0;

    // ノードの半径
    var node_r = 7;


    function linkD( d ) {
        return linkD2( d.source, d.target );
    }

    function linkDPrev( d ) {
        var src = d.source.data;
        var dst = d.target.data;
        return linkD2( { x: src.x0, y: src.y0 },
                       { x: dst.x0, y: dst.y0 } );
    }
    
    function linkD2( src, dst ) {
        // return "M" + src.x + "," + src.y
        //     + "C" + (src.x + dst.y) / 1 + "," + src.y
        //     + " " + (src.x + dst.y) / 1 + "," + dst.y
        //     + " " + dst.x + "," + dst.y;
        return "M" + src.x + "," + src.y
            + "L" + dst.x + "," + dst.y;
    }
    
    
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

    var g = svg.append("g").attr(
        "transform", "translate(" + obj.orgX + "," + obj.orgY + ")");

    // SVG の画像要素
    obj.sel = {};
    obj.sel.node = g.selectAll(".node");
    obj.sel.link = g.selectAll(".link");

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
                if ( obj.nodeMap.has( info.nsId ) ) {
                    continue;
                }
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
        var nodeRoot = g.selectAll("g.node")
            .data( obj.nodes,
                   function(d) { return d.id || (d.id = idSeed++); });

        
        var nodeEnter = nodeRoot.enter().append("svg:g")
                .attr("class", "node")
                .attr( "id", function( d ) { return "node" + d.id; } )
                .attr("transform", "translate( 0, 0 )" )
                .style( "cursor", "pointer" );

        nodeEnter.append("svg:circle")
            .attr("r", node_r)
            .style( "stroke", "steelblue" )
            .style( "stroke-width", "1.5px" );

        nodeEnter.append("svg:text")
            .attr("x", function(d) {
                return 10;
            })
            .attr("dy", ".35em")
            .attr("font-size", "11px")
            .attr( "font-weight", "bold" )
            .text(function(d) { return d.name; });


        // ノード削除  (最終位置を親のノード位置とする)
        var nodeExit = nodeRoot.exit().remove();

        obj.sel.node = nodeRoot.merge( nodeEnter );


        // リンク更新
        var link = g.selectAll("path.link")
                .data( obj.links,
                       function(d) {
                           // link の id は、
                           // (src ノード id) | (tgt ノード id の 26bit 左シフト)
                           // js の数値が 64bit 浮動少数点で、
                           // 52 ビットの整数表現が可能なので、
                           // 上位 26 ビットを target ノードの id、
                           // 下位 26 ビットを source ノードの id とする。
                           // よって、 ノード数が 26bit を越えると破綻する。
                           return d.source.id | (d.target.id << 26);
                       }
                     );

        // リンク追加 ( 初期位置を親のノード位置とする )
        var linkEnter = link.enter().insert("svg:path", "g")
            .attr("class", "link")
            .attr("fill", "none" )
            .attr("stroke", "#ccc" )
            .attr("stroke-width", "5px" )
            .attr("d", function(d) {
                return linkD( d );
            })
            .style( "cursor", "pointer" );


        // リンクの削除 (最終位置を親のノード位置とする)
        link.exit().remove();

        obj.sel.link = link.merge( linkEnter );
        
        
        if ( startForceFlag ) {
            // forceSimulation 開始
            simulation.nodes( obj.nodes )
                .velocityDecay(0.80)
                .force("charge", d3.forceManyBody().strength(-500))
            // 中心に集るとごちゃごちゃするので、集めない。
                .force("center", d3.forceCenter() )
                .force( "link",
                        d3.forceLink( obj.links )
                        .distance( 
                            function( link ) {
                                return 200;
                            })
                        .strength(0.1).iterations(2) )
                .alpha(1)
                .alphaTarget(0).restart();
        }

    };
        

    function lctags_ticked() {
        obj.sel.link
            .attr("d", function(d) {
                return linkD( d );
            });

        obj.sel.node
            .attr("transform",
                  function(d) {
                          return "translate(" + d.x + "," + d.y + ")";
                      });
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
