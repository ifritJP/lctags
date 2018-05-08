function lctags_graph_tree( paramInfo ) {

    var obj = {};
    
    var width = window.innerWidth,
        height = window.innerHeight,
        idSeed = 0,
        rootNode;

    obj.nsId2CountMap = new Map();
    obj.inquiredNsIdSet = new Set();

    obj.depthOffset = [ 10 ];
    obj.orgX = 0;
    obj.orgY = 0;
    obj.dragX = 0;
    obj.dragY = 0;

    obj.dragMove = d3.drag()
               .on("start", lctags_svg_move_dragstarted)
               .on("drag", lctags_svg_move_dragged)
               .on("end", lctags_svg_move_dragended);
    obj.dragMode = obj.dragMove;


    var tree = d3.tree().size([ height - 30, width - 20 ]);

    function linkD( d ) {
        return linkD2( d.source, d.target );
    }

    function linkD2( src, dst ) {
        return "M" + src.y + "," + src.x
            + "C" + (src.y + dst.y) / 2 + "," + src.x
            + " " + (src.y + dst.y) / 2 + "," + dst.x
            + " " + dst.y + "," + dst.x;
    }

    var svg = d3.select("body").append("svg")
        .attr("width", width).attr("height", height)
        .on("click", paramInfo.svgClick )
        .on( "contextmenu",
             function( d, i ) {
                 // ブラウザの contextmenu を表示しない
                 d3.event.preventDefault();
                 var popup = d3.select("body").append( "div" )
                     .style( "width", "50%" )
                     .style( "height", "20%" )
                     .style( "position", "absolute" )
                     .style( "top", "0px" )
                     .style( "left", "0px" );

                 popup.append( "button" )
                     .text( "close" )
                     .on( "click",
                          function() {
                              popup.remove();
                          });
                 popup.append( "br" );
                 
                 var textarea = popup
                     .append( "textarea" )
                     .style( "width", "100%" )
                     .style( "height", "100%" )
                     .text( JSON.stringify( rootNode ) );

                 
                 textarea
             } )
        .call( obj.dragMode );


    var defs = svg.append("defs");
    var marker = defs.append("filter")
        .attr( "x", "0" )
        .attr( "y", "0" )
        .attr( "width", "1" )
        .attr( "height", "1" )
        .attr( "id", "background-dynamic" );
    marker.append("feFlood")
        .attr( "flood-opacity", 0.1 )
        .attr( "flood-color", "red");
    marker.append("feComposite")
        .attr( "in", "SourceGraphic" );

    
    d3.select( window ).on( "resize", function() {
        width = window.innerWidth;
        height = window.innerHeight;
        svg.attr("width", width )
            .attr("height", height );
        tree = d3.tree().size([ height - 30, width - 20 ]);
        update( rootNode );
    });

    var g = svg.append("g").attr( "transform", "translate( 0, 0 )");
    
    function update(source) {
        var duration = 500;

        var nodeHierarchy = d3.hierarchy( rootNode );
        tree( nodeHierarchy );

        
        // ノードの深さに応じて座標を決定 ( depth は D3 によって計算されている )
        var workSrc = null;
        nodeHierarchy.each(
            function(d) {
                if ( !workSrc && d.data.id == source.id ) {
                    workSrc = d;
                }
                d.y = obj.depthOffset[ d.depth ];
            });
        source = workSrc;

        // ノードにデータをバインド
        var nodesInfo = nodeHierarchy.descendants();
        var node = g.selectAll("g.node")
                .data( nodesInfo,
                       function(d) { return d.data.id || (d.data.id = idSeed++); });

        // 追加ノード生成
        var nodeEnter = node.enter().append("svg:g")
                .attr("class", "node")
                .attr("transform",
                      function(d) {
                          var val = source;
                          return "translate(" + val.data.y0 + "," + val.data.x0 + ")";
                      })
		.style( "cursor", "pointer" )
                .on( "contextmenu",
                     function( d, i ) {
                         // ブラウザの contextmenu を表示しない
                         d3.event.preventDefault();
                         paramInfo.nodeContext( obj, d.data );
                     } )
                .on("click", function(d) {
                    toggle(d.data); 
                });

        nodeEnter.transition()
            .duration(duration)
            .attr("transform",
                  function(d) {
                      return "translate(" + d.y + "," + d.x + ")";
                  });
        

        var nodeColor = function( d ) {
            if ( d.data.inquired ) {
                return d.data._children ? "green" : "#f88";
            }
            else {
                return "white";
            }
        };
        var textColor = function( d ) {
            if ( obj.nsId2CountMap.get( d.data.nsId ) >= 2 ) {
                if ( obj.inquiredNsIdSet.has( d.data.nsId ) ) {
                    return "blue";
                }
                return "green";
            }
            return "black";
        };
        var textFilter = function( d ) {
            if ( d.data.type == "TypedefDecl" ) {
                return "url(#background-dynamic)";
            }
            return "none";
        };
        
        
        nodeEnter.append("svg:circle")
            .attr("r", 1e-6)
            .style("fill", nodeColor )
            .style( "stroke", "steelblue" )
            .style( "stroke-width", "1.5px" )
            .transition()
            .duration(duration)
            .attr("r", 6);

        nodeEnter.append("svg:text")
            .attr("x", function(d) {
                return 10;
            })
            .attr("dy", ".35em")
            .attr("font-size", "11px")
            .attr("fill", textColor )
            .attr("filter", textFilter )
            .text(function(d) { return d.data.name; });


        // text の幅を取得
        nodeEnter.each(
            function( pd, pindex ) {
                d3.select( this ).selectAll( "text" ).each(
                    function( d, index ) {
                        var rect = this.getBBox();
                        d.data.textWidth = rect.width;
                    }); } );
        

        // 既存ノード移動
        var nodeUpdate = node.transition()
                .duration(duration)
                .attr("transform",
                      function(d) { return "translate(" + d.y + "," + d.x + ")"; });

        nodeUpdate.select("circle")
            .attr("r", 6)
            .style("fill", nodeColor );

        nodeUpdate.select("text")
            .attr("fill", textColor )
            .attr("filter", textFilter )
            .style("fill-opacity", 1);

        // ノード削除  (最終位置を親のノード位置とする)
        var nodeExit = node.exit().transition()
                .duration(duration)
                .attr("transform",
                      function(d) {
                          var val = source;
                          return "translate(" + val.y + "," + val.x + ")";
                      })
                .remove();

        nodeExit.select("circle")
            .attr("r", 1e-6);

        nodeExit.select("text")
            .style("fill-opacity", 1e-6);

        // リンク更新
        var link = g.selectAll("path.link")
                .data(nodeHierarchy.links(),
                      function(d) {
                          // link の id は、
                          // (src ノード id) | (tgt ノード id の 26bit 左シフト)
                          // js の数値が 64bit 浮動少数点で、
                          // 52 ビットの整数表現が可能なので、
                          // 上位 26 ビットを target ノードの id、
                          // 下位 26 ビットを source ノードの id とする。
                          // よって、 ノード数が 26bit を越えると破綻する。
                          return d.source.data.id | (d.target.data.id << 26);
                      }
                     );

        // リンク追加 ( 初期位置を親のノード位置とする )
        link.enter().insert("svg:path", "g")
            .attr("class", "link")
            .attr("fill", "none" )
            .attr("stroke", "#ccc" )
            .attr("stroke-width", "1.5px" )
            .attr("d", function(d) {
                var pos = {};
                pos.x = source.data.x0;
                pos.y = source.data.y0;
                return linkD2( pos, pos );
            })
            .transition()
            .duration(duration)
            .attr("d", linkD);

        // 既存リンクの移動
        link.transition()
            .duration(duration)
            .attr("d", linkD);

        // リンクの削除 (最終位置を親のノード位置とする)
        link.exit().transition()
            .duration(duration)
            .attr("d", function(d) {
                return linkD2( source, source );
            })
            .remove();


        var textWidthList = [];
        nodesInfo.forEach(function(d) {
            // ノードの現在位置を保存
            d.data.x0 = d.x;
            d.data.y0 = d.y;

            // depth 毎の text サイズを計算
            var width = textWidthList[ d.depth ] || 0;
            var newWidth = d.data.textWidth + 30;
            if ( width < newWidth ) {
                textWidthList[ d.depth ] = newWidth;
            }
        });

        // depth 毎の offset を計算
        var totalWidth = 0;
        textWidthList.forEach( function( width, index ) {
            totalWidth += width;
            obj.depthOffset[ index + 1 ] = totalWidth;
        } );
    }

    // Toggle children.
    function toggle(d) {
        if (d.inquired) {
            if ( d.children ) {
                d._children = d.children;
                d.children = null;
            }
            else {
                d.children = d._children;
                d._children = null;
            }
            update( d );
        } else {
            obj.inquiredNsIdSet.add( d.nsId );
            d.inquired = true;
            d._children = null;
            paramInfo.nodeClick( obj, d );
        }
    }

    obj.newNode = function( nsId, name ) {
        obj.nsId2CountMap.set( nsId, (obj.nsId2CountMap.get( nsId ) || 0) + 1 );
        
        return {
            nsId: nsId,
            name: name,
            inquired: false,
            x0: 0,
            y0: 0
        };
    };

    obj.setNodeType = function( nodeId, type ) {
        var nodeHierarchy = d3.hierarchy( rootNode );
        var workSrc = null;
        nodeHierarchy.each(
            function(d) {
                if ( !workSrc && d.data.id == nodeId ) {
                    workSrc = d.data;
                }
            });
        if ( workSrc ) {
            workSrc.type = type;
        }
    };

    obj.addChild = function( parentId, nodeList ) {

        if ( parentId == null ) {
            rootNode = obj.newNode( nodeList[0].nsId, nodeList[0].name );
            update( rootNode );
        }
        else {
            var nodeHierarchy = d3.hierarchy( rootNode );
            var workSrc = null;
            nodeHierarchy.each(
                function(d) {
                    if ( !workSrc && d.data.id == parentId ) {
                        workSrc = d.data;
                    }
                });

            if ( workSrc == null ) {
                workSrc = {};
            }

            if ( !workSrc.children ) {
                workSrc.children = [];
            }
            nodeList.forEach(
                function( src ) {
                    workSrc.children.push( obj.newNode( src.nsId, src.name ) );
                });
            update( workSrc );
        }
    };


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
    

    return obj;
};
