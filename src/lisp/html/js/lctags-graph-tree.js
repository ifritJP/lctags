var reset_node = function( obj, rootNode ) {
    obj.nsId2CountMap = new Map();
    obj.inquiredNsIdSet = new Set();

    if ( rootNode ) {
        rootNode.children = null;
        rootNode.inquired = false;
    }
    
    obj.depthOffset = [ 10 ];
    obj.fileId2PathMap = new Map();
};

function lctags_graph_tree( projDir, paramInfo ) {

    var obj = {};

    obj.projDir = projDir;
    
    var width = window.innerWidth,
        height = window.innerHeight,
        idSeed = 0,
        rootNode;

   
    reset_node( obj, rootNode );


    obj.colorList = [
        "#ddd", "#ccf", "#cfc", "#dff",
        "#fdd", "#fdf", "#dd8",
        "#bbb", "#bbd", "#bdb", "#bdd",
        "#dbb", "#dbd", "#bb8"
    ];
    obj.getColor = function( colorId ) {
        return obj.colorList[ colorId % obj.colorList.length ];
    };

    obj.updateCount = 0;
    obj.orgX = 0;
    obj.orgY = 0;
    obj.dragX = 0;
    obj.dragY = 0;

    var node_r = 6;

    obj.dragMove = d3.drag()
        .on("start", lctags_svg_move_dragstarted)
        .on("drag", lctags_svg_move_dragged)
        .on("end", lctags_svg_move_dragended);
    obj.dragSelect = d3.drag()
        .on("start", lctags_svg_select_dragstarted)
        .on("drag", lctags_svg_select_dragged)
        .on("end", lctags_svg_select_dragended); 
    obj.dragMode = obj.dragMove;
    obj.expandMode = "callee";
    obj.selectMode = "close";

    var headerHeight = 50;
    var marginWidth = 20;
    var marginHeigth = 30;

    var selectRect = null;

    d3.select("html").style( "height", "100%" );
    d3.select("body")
        .style( "height", "100%" );
    
    var tree = d3.tree().size([ height - marginHeigth - headerHeight,
                                width - marginWidth ]);

    function linkD( d ) {
        return linkD2( d.source, d.target );
    }

    function linkD2( src, dst ) {
        return "M" + src.y + "," + src.x
            + "C" + (src.y + dst.y) / 2 + "," + src.x
            + " " + (src.y + dst.y) / 2 + "," + dst.x
            + " " + dst.y + "," + dst.x;
    }

    
    make_header();
    
    var svg = d3.select("body").append("svg")
            .attr("width", width).attr("height", height - headerHeight)
            .on("click", paramInfo.svgClick )
            .on( "contextmenu",
                 function( d, i ) {
                     // 右クリックでブラウザの contextmenu を表示しない。
                     d3.event.preventDefault();
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


    var fitTo = function() {
        width = window.innerWidth;
        height = window.innerHeight;

        d3.select( "#lctags_callgraph_header" )
            .style( "width", width - marginWidth + "px" );

        d3.select( "#lctags_callgraph_header_parent" )
            .style( "position", "sticky" )
            .style( "width", width - marginWidth + "px" );
        

        svg.attr("width", width )
            .attr("height", height - headerHeight );
        tree = d3.tree().size([ height - 30 - headerHeight, width - 20 ]);
        update();
    };
    
    d3.select( window ).on( "resize",
                            function() {
                                fitTo();
                            } );

    var g = svg.append("g").attr( "transform", "translate( 0, 0 )");

    function hideNode( src, dst, updateFlag ) {
        src.children = src.children.filter(
            function( child ) {
                return child.nsId != dst.nsId;
            });
        src.hideList.push( dst );

        if ( updateFlag ) {
            update();
        }
    }
    
    function update() {
        var duration = 500;

        var nodeHierarchy = d3.hierarchy( rootNode );
        tree( nodeHierarchy );


        var id2HieMap = new Map();
        var prevUpdateCount = obj.updateCount;
        obj.updateCount++;
        // ノードの深さに応じて座標を決定 ( depth は D3 によって計算されている )
        nodeHierarchy.each(
            function(d) {
                d.y = obj.depthOffset[ d.depth ];
                if ( d.parent ) {
                    d.data.parent = d.parent.data;
                }
                else {
                    d.data.parent = rootNode;
                }
                d.data.updateCount = obj.updateCount;
                id2HieMap.set( d.data.id, d );
            });

        // ノードにデータをバインド
        var nodesInfo = nodeHierarchy.descendants();
        var node = g.selectAll("g.node")
                .data( nodesInfo,
                       function(d) { return d.data.id || (d.data.id = idSeed++); });

        var nodeColor = function( d ) {
            if ( d.data.selected ) {
                return "#f0f";
            }
            if ( d.data.hideList.length > 0 ) {
                return "#88f";
            }
            if ( d.data.inquired ) {
                return d.data._children ? "green" : "#f88";
            }
            else {
                return "white";
            }
        };
        
        // 追加ノード生成
        var nodeEnter = node.enter().append("svg:g")
                .attr("class", "node")
                .attr( "id", function( d ) { return "node" + d.data.id; } )
                .attr("transform",
                      function(d) {
                          var val = d.data.parent;
                          while ( val.prevUpdateCount != prevUpdateCount ) {
                              val = val.parent;
                              if ( val.id == rootNode.id ) {
                                  break;
                              }
                          }
                          return "translate(" + val.y0 + "," + val.x0 + ")";
                      })
                .style( "cursor", "pointer" )
                .on( "contextmenu",
                     function( d, i ) {
                         // ブラウザの contextmenu を表示しない
                         d3.event.preventDefault();
                         paramInfo.nodeContext( obj, d.data );

                         var curNode = g.selectAll("g.node").
                                 select( "circle" ).select( function( ddd ) {
                                     if ( ddd === d ) {
                                         return this;
                                     }
                                     return null;
                                 } );
                         curNode.style( "fill", "blue" )
                             .transition()
                             .duration( duration )
                             .style( "fill", nodeColor );
                     })
                .call( d3.drag() )
                .on("click", function(d) {
                    d3.event.stopPropagation();
                    d3.event.preventDefault();
                    toggle(d.data); 
                });

        nodeEnter.transition()
            .duration(duration)
            .attr("transform",
                  function(d) {
                      return "translate(" + d.y + "," + d.x + ")";
                  });
        

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
            if ( d.data.type == "TypedefDecl" || d.data.type == "FieldDecl" ) {
                return "url(#background-dynamic)";
            }
            return "none";
        };
        var linkColor = function( d ) {
            var fileInfo = obj.fileId2PathMap.get( d.target.data.fileId );
            return obj.getColor( fileInfo && fileInfo.colorId || 0 );

        };
        var linkDash = function( d ) {
            var fileInfo = obj.fileId2PathMap.get( d.target.data.fileId );
            if ( fileInfo.checked ) {
                return "none";
            }
            return "8,2";
        };
        
        
        
        nodeEnter.append("svg:circle")
            .attr("r", 1e-6)
            .style("fill", nodeColor )
            .style( "stroke", "steelblue" )
            .style( "stroke-width", "1.5px" )
            .transition()
            .duration(duration)
            .attr("r", node_r );

        nodeEnter.append("svg:text")
            .attr("x", function(d) {
                return 10;
            })
            .attr("dy", ".35em")
            .attr("font-size", "11px")
            .attr( "font-weight", "bold" )
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
            .attr("r", node_r)
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
                          var data = d.data.parent;
                          while ( data.updateCount != obj.updateCount ) {
                              data = data.parent;
                              if ( data.id == rootNode.id ) {
                                  break;
                              }
                          }
                          var val = id2HieMap.get( data.id );
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
            .attr("stroke", linkColor )
            .attr("stroke-dasharray", linkDash )
            .attr("stroke-width", "5px" )
            .attr("d", function(d) {
                var pos = {};
                var val = d.source;
                while ( val.data.prevUpdateCount != prevUpdateCount ) {
                    val = val.parent;
                    if ( val.data.id == rootNode.id ) {
                        break;
                    }
                }
                pos.x = val.data.x0;
                pos.y = val.data.y0;
                return linkD2( pos, pos );
            })
            .style( "cursor", "pointer" )
            .on( "click", function( d ) {
                d3.event.preventDefault();
                hideNode( d.source.data, d.target.data, true );
            })
            .on( "contextmenu", function( d ) {
                d3.event.preventDefault();
                paramInfo.pathClick( obj, d );

                var curLink = g.selectAll("path.link").select( function( ddd ) {
                    if ( ddd === d ) {
                        return this;
                    }
                    return null;
                } );
                curLink.attr( "stroke", "red" )
                    .transition()
                    .duration( duration )
                    .attr( "stroke", linkColor );
            })
            .transition()
            .duration(duration)
            .attr("d", linkD);

        // 既存リンクの移動
        link.transition()
            .duration(duration)
            .attr("stroke-dasharray", linkDash )
            .attr("d", linkD);

        // リンクの削除 (最終位置を親のノード位置とする)
        link.exit().transition()
            .duration(duration)
            .attr("d", function(d) {
                var val = d.source;
                while ( val.data.updateCount != obj.updateCount ) {
                    val = val.parent;
                    if ( val.data.id == rootNode.id ) {
                        break;
                    }
                }
                val = id2HieMap.get( val.data.id );
                return linkD2( val, val );
            })
            .remove();


        var textWidthList = [];
        nodesInfo.forEach(function(d) {
            // ノードの現在位置を保存
            d.data.x0 = d.x;
            d.data.y0 = d.y;
            d.data.prevUpdateCount = d.data.updateCount;

            // depth 毎の text サイズを計算
            var width = textWidthList[ d.depth ] || 0;
            var newWidth = d.data.textWidth + 40;
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

    function expandHided( node ) {
        node.hideList.forEach( function( data ) {
            node.children.push( data );
        } );
        node.hideList = [];
    }
    
    function expandNode(d) {
        if (d.inquired) {
            if ( !d.children ) {
                d.children = d._children;
                expandHided( d );
                d._children = null;
                update();
            }
        } else {
            obj.inquiredNsIdSet.add( d.nsId );
            d.inquired = true;
            d._children = null;
            paramInfo.nodeClick( obj, d );
        }
    }
    function closeNode(d) {
        d._children = d.children;
        d.children = null;
        update();
    }
    
    // Toggle children.
    function toggle(d) {
        if (d.inquired) {
            if ( d.children ) {
                closeNode( d );
            }
            else {
                expandNode( d );
            }
        } else {
            expandNode( d );
        }
    }

    obj.newNode = function( nsId, name, fileId ) {
        obj.nsId2CountMap.set( nsId, (obj.nsId2CountMap.get( nsId ) || 0) + 1 );
        
        return {
            nsId: nsId,
            name: name,
            fileId: fileId,
            inquired: false,
            hideList: [],
            updateCount: 0,
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

    obj.addChild = function( parentId, nodeList, fileList ) {

        if ( parentId == null ) {
            rootNode = obj.newNode(
                nodeList[0].nsId, nodeList[0].name, nodeList[0].fileId );
            update();
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

            fileList.forEach( function( file ) {
                if ( !obj.fileId2PathMap.get( file.fileId ) ) {
                    obj.fileId2PathMap.set(
                        file.fileId,
                        { path: file.path, fileId: file.fileId,
                          colorId: obj.fileId2PathMap.size } );
                }
            } );

            obj.fileIdList = [];
            obj.fileId2PathMap.forEach( function( info ) {
                obj.fileIdList.push( info );
            });
            obj.fileIdList.sort(
                function( obj1, obj2 ) {
                    return obj1.path.localeCompare( obj2.path );
                }
            );

            if ( obj.fileListBody ) {
                obj.fileListBody.remove();
            }
            obj.fileListBody = obj.fileListPopup.append( "div" );
            
            obj.fileIdList.forEach( function( info ) {
                var path = info.path;
                if ( path == "" ) {
                    path = "<system>";
                }
                obj.fileListBody.append( "input" )
                    .attr( "id", "fileId" + info.fileId )
                    .attr( "value", "" + info.fileId )
                    .attr( "checked", info.checked )
                    .attr( "type", "checkbox" )
                    .on( "click", function( d ) {
                        info.checked = !info.checked;
                        update();
                    });
                obj.fileListBody
                    .append( "label" )
                    .attr( "for", "fileId" + info.fileId )
                    .style( "background", function() {
                        return obj.getColor( info.colorId );
                    })
                    .style("margin-left", "10px" )
                    .style("font-size", "12px")
                    .style( "fontWeight", "bold" )
                    .style( "white-space", "nowrap" )
                    .text( path.replace( obj.projDir + "/", "" ) );
                obj.fileListBody.append( "br" );
            } );
            

            if ( workSrc == null ) {
                workSrc = {};
            }
            if ( !workSrc.children ) {
                workSrc.children = [];
            }
            nodeList.forEach(
                function( src ) {
                    var node = obj.newNode( src.nsId, src.name, src.fileId );
                    node.type = src.type;
                    workSrc.children.push( node );
                });
            update();
        }
    };

    function make_header() {

        obj.fileListPopupParent = d3.select("body").append( "div" )
            .style( "z-index", "1" )
            .style( "width", "20%" )
            .style( "height", window.innerHeight - headerHeight + "px" )
            .style( "float", "right" )
            .style( "position", "absolute" )
            .style( "display", "none" )
            .style( "top", "0" )
            .style( "right", "0" );
        
        obj.fileListPopup = obj.fileListPopupParent.append( "div" )
            .style( "background", "#eee" )
            .style( "width", "100%" )
            .style( "height", window.innerHeight - 100 + "px" )
            .style( "overflow", "scroll" )
            .style( "position", "sticky" )
            .style( "top", "0" )
            .style( "left", "0" );

        
        obj.fileListPopup.append( "button" )
            .text( "close" )
            .on( "click",
                 function() {
                     obj.fileListPopupParent.remove();
                 });

       
        var headerParent = d3.select("body").append("div")
                .attr( "id", "lctags_callgraph_header_parent" )
                .style( "position", "sticky" )
                .style( "top", "0" )
                .style( "left", "0" )
                .style( "height", headerHeight + "px" )
                .style( "width", "100%" );
        
        
        var header = headerParent.append("div")
                .attr( "id", "lctags_callgraph_header" )
                .style( "position", "sticky" )
                .style( "background", "#ccc" )
                .style( "left", "0" )
                .style( "top", "0" )
                .style( "width", window.innerWidth - marginWidth + "px" )
                .style( "height", headerHeight + "px" );

        var offset = 0;

        offset += 10;
        header.append( "button" )
            .text( "fileList" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     if ( obj.fileListPopupParent.style( "display" ) == "none" ) {
                         obj.fileListPopupParent.style( "display", "block" );
                     }
                     else {
                         obj.fileListPopupParent.style( "display", "none" );
                     }
                 });

        offset += 10;
        header.append( "button" )
            .text( "export" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     // ツリーのデータを JSON でエクスポート。
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
                                  obj.fileListPopupParent.style( "display", "none" );
                              });
                     popup.append( "br" );

                     var exportNode = function( node ) {
                         var clone = {};
                         clone.nsId = node.nsId;
                         clone.type = node.type;
                         clone.name = node.name;
                         clone.children = [];
                         if ( node.children ) {
                             node.children.forEach( function( d ) {
                                 clone.children.push( exportNode( d ) );
                             });
                         }
                         return clone;
                     };
                     var cloneRoot = exportNode( rootNode );
                     
                     var textarea = popup
                             .append( "textarea" )
                             .style( "width", "100%" )
                             .style( "height", "100%" )
                             .text( JSON.stringify( cloneRoot ) );
                 });

        
        offset += 10;
        var expandModeSelect = header.append( "select" )
                .style( "position", "relative" )
                .style( "top", headerHeight / 4 + "px" )
                .style( "left", offset + "px" )
                .on( "change", function() {
                    var svg = d3.select("body").select("svg");

                    obj.expandMode = this.options[ this.selectedIndex ].text;
                    reset_node( obj, rootNode );
                    update();
                });
        expandModeSelect.append( "option" )
            .text( "callee" );
        expandModeSelect.append( "option" )
            .text( "caller" );
        expandModeSelect.append( "option" )
            .text( "refSym" );
        

        offset += 10;
        var dragModeSelect = header.append( "select" )
                .style( "position", "relative" )
                .style( "top", headerHeight / 4 + "px" )
                .style( "left", offset + "px" )
                .on( "change", function() {
                    var svg = d3.select("body").select("svg");

                    if ( this.selectedIndex == 0 ) {
                        obj.dragMode = obj.dragMove;
                        svg.style( "cursor", "default" );
                    }
                    else if ( this.selectedIndex == 1 ) {
                        obj.dragMode = obj.dragSelect;
                        obj.selectMode = "expand";
                        svg.style( "cursor", "crosshair" );
                    }
                    else if ( this.selectedIndex == 2 ) {
                        obj.dragMode = obj.dragSelect;
                        obj.selectMode = "close";
                        svg.style( "cursor", "crosshair" );
                    }
                    else if ( this.selectedIndex == 3 ) {
                        obj.dragMode = obj.dragSelect;
                        obj.selectMode = "hide";
                        svg.style( "cursor", "crosshair" );
                    }
                    svg.call( obj.dragMode );

                });
        dragModeSelect.append( "option" )
            .text( "move" );
        dragModeSelect.append( "option" )
            .text( "expandRegion" );
        dragModeSelect.append( "option" )
            .text( "closeRegion" );
        dragModeSelect.append( "option" )
            .text( "hideRegion" );


        offset += 10;
        header.append( "button" )
            .text( "expandNodes" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     // ノードを open する。
                     // ただし、他のノードで展開済み、 名無しノードは open しない。
                     var nodeMap = new Map();
                     var excludeSet = new Set();
                     excludeSet.add( 1 );
                     d3.hierarchy( rootNode ).each( function( d ) {
                         var data = d.data;
                         var setFlag = false;
                         if ( !data.inquired && data.children == null ) {
                             if ( !nodeMap.get( data.nsId ) &&
                                  !excludeSet.has( data.nsId ) ) {
                                      nodeMap.set( data.nsId, data );
                                      setFlag = true;
                                  }
                         }
                         if ( !setFlag ) {
                             excludeSet.add( data.nsId );
                         }
                     });

                     nodeMap.forEach( function( node ) {
                         expandNode( node );
                     });
                 });


        offset += 10;
        header.append( "button" )
            .text( "showHidedNodes" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     var nodeList = [];
                     d3.hierarchy( rootNode ).each( function( node ) {
                         if ( node.data.hideList.length > 0 ) {
                             nodeList.push( node.data );
                         }
                     });
                     
                     nodeList.forEach( function( node ) {
                         expandHided( node );
                     });
                     update();
                 });

        offset += 10;
        header.append( "button" )
            .text( "hideSameNodes" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     var nodeList = [];
                     var nodeMap = new Map();
                     d3.hierarchy( rootNode ).each( function( node ) {
                         var existNode = nodeMap.get( node.data.nsId );
                         if ( existNode && existNode.inquired ) {
                             nodeList.push( node );
                         }
                         else {
                             nodeMap.set( node.data.nsId, node.data );
                         }
                     });
                     nodeList.forEach( function( node ) {
                         hideNode( node.parent.data, node.data, false );
                     });
                     update();
                 });
        
        offset += 10;
        header.append( "button" )
            .text( "resize" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     var depth = 0;
                     var prevX = -100;
                     var overlapSize = 0;
                     var maxY = svg.attr( "width" ) - marginWidth;
                     d3.hierarchy( rootNode ).each( function( d ) {
                         var tailPos = d.data.y0 + d.data.textWidth;
                         if ( maxY < tailPos ) {
                             maxY = tailPos;
                         }
                         if ( depth != d.depth ) {
                             prevX = -100;
                         }
                         else {
                             var data = d.data;
                             var size = prevX + node_r * 2 - data.x0;
                             if ( overlapSize < size ) {
                                 overlapSize = size;
                             }
                         }
                         prevX = d.data.x0;
                         depth = d.depth;
                     });
                     if ( overlapSize > 0 || maxY > svg.attr( "width" ) ) {
                         var ratio = 1 + 2 * overlapSize / (node_r * 2);

                         width = maxY;
                         height = svg.attr( "height" ) * ratio;
                         

                         headerParent
                             .style( "position", "sticky" )
                             .style( "width", width + "px" );

                         obj.fileListPopupParent.style("height", height + "px" );
                         
                         svg.attr("height", height);
                         svg.attr( "width", width );
                         tree.size([ height, width ]);
                         update();
                     }
                 });


        offset += 10;
        header.append( "button" )
            .text( "fit" )
            .style( "position", "relative" )
            .style( "left", offset + "px" )
            .style( "top", headerHeight / 4 + "px" )
            .on( "click",
                 function() {
                     fitTo();
                 } );
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
        obj.dragY = d3.event.y - obj.orgY - headerHeight;
        selectRect = g.append("rect")
            .attr( "x", obj.dragX )
            .attr( "y", obj.dragY )
            .attr( "width", 5 )
            .attr( "height", 5 )
            .style( "pointer-events", "none" )
            .style( "fill", "none" )
            .style( "stroke", "black" );


        d3.hierarchy( rootNode ).each( function( d ) {
            d.data.selected = false;
        });
    }

    function lctags_svg_select_dragged(d) {

        var x1 = obj.dragX;
        var y1 = obj.dragY;
        var posX = d3.event.x - obj.orgX;
        var posY = d3.event.y - obj.orgY - headerHeight;

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

        d3.hierarchy( rootNode ).each( function( d ) {
            var data = d.data;
            if ( data.x0 >= y1 && ( data.x0 <= posY ) &&
                 data.y0 >= x1 && ( data.y0 <= posX ) ) {
                     data.selected = true;
                 }
            else { 
                data.selected = false;
            }
        } );
        update();
    }

    function lctags_svg_select_dragended(d) {
        selectRect.remove();

        d3.hierarchy( rootNode ).each( function( d ) {
            var data = d.data;
            if ( data.selected ) {
                data.selected = false;
                if ( obj.selectMode == "close" ) {
                    closeNode( data );
                }
                else if ( obj.selectMode == "expand" ) {
                    expandNode( data );
                }
                else {
                    hideNode( d.parent.data, data, true );
                }
            }
        } );
    }

    return obj;
};
