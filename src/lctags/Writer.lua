local Writer = {}

local XML = {}
Writer.XML = XML

local function convertXmlTxt( txt )
   if txt == nil or txt == "" then
      return ""
   end
   if type( txt ) == "number" then
      return string.format( "%g", txt )
   end
   txt = string.gsub( txt, '&', "&amp;" )
   txt = string.gsub( txt, '>', "&gt;" )
   txt = string.gsub( txt, '<', "&lt;" )
   txt = string.gsub( txt, '"', "&quot;" )
   txt = string.gsub( txt, "'", "&apos;" )
   return txt
end

function XML:open( stream )
   self.stream = stream
   self.elementList = {}
   self.depth = 0
   return self
end

function XML:startParent( name, arrayFlag )
   self:startElement( name )
end

function XML:startElement( name )
   table.insert( self.elementList, name )
   self.stream:write( string.format( '<%s>', name ) )
   self.depth = self.depth + 1
end

function XML:endElement()
   local name = table.remove( self.elementList )
   self.stream:write( string.format( '</%s>', name ) )
   self.depth = self.depth - 1
   if self.depth == 0 then
      self.stream:write( '\n' )
   elseif self.depth < 0 then
      error( "illegal depth" )
   end
end

function XML:writeValue( val )
   self.stream:write( convertXmlTxt( val ) )
end

function XML:write( name, val )
   self:startElement( name )
   self:writeValue( val )
   self:endElement( name )
end

function XML:fin()
end


local JSON = {}
Writer.JSON = JSON

local function convertJsonTxt( txt )
   if txt == nil or txt == "" then
      return ""
   end
   txt = string.gsub( txt, '"', '\\"' )
   txt = string.gsub( txt, '\\', '\\\\' )
   txt = string.gsub( txt, '\n', '\\n' )
   return txt
end


function JSON:open( stream )
   self.stream = stream
   self.layerQueue = {}
   self:startLayer()


   return self
end

function JSON:getLayerInfo()
   if #self.layerQueue == 0 then
      return nil
   end
   return self.layerQueue[ #self.layerQueue ]
end

function JSON:startLayer( arrayFlag, madeByArrayFlag )
   local info = {}
   info.state = 'none' -- none, named, valued, termed
   info.arrayFlag = arrayFlag
   info.name = self.prevName
   info.madeByArrayFlag = madeByArrayFlag
   info.elementNameSet = {}
   info.parentFlag = true

   table.insert( self.layerQueue, info )

   self.stream:write( arrayFlag and "[" or "{" )
end

function JSON:endLayer()
   if #self.layerQueue == 0 then
      error( "illegal depth" )
   end

   while #self.layerQueue > 0 do
      local info = self:getLayerInfo()
      if info.arrayFlag then
	 self.stream:write( string.format( ']' ) )
      else
	 self.stream:write( string.format( '}' ) )
      end
      table.remove( self.layerQueue )
      local parentInfo = self:getLayerInfo()
      if parentInfo and parentInfo.madeByArrayFlag then
	 ;
      else
	 break
      end
   end
   if #self.layerQueue == 0 then
      self.stream:write( '\n' )
   end
end

function JSON:equalLayerState( state )
   return self.layerQueue[ #self.layerQueue ].state == state
end

function JSON:isArrayLayer( state )
   return self.layerQueue[ #self.layerQueue ].arrayFlag
end


function JSON:setLayerState( state )
   self.layerQueue[ #self.layerQueue ].state = state
end

function JSON:getLayerName()
   return self.layerQueue[ #self.layerQueue ].name
end

function JSON:addElementName( name )
   local info = self:getLayerInfo()
   local nameSet = info.elementNameSet
   if not info.arrayFlag and nameSet[ name ] then
      error( "exist same name: " .. name )
   end
   nameSet[ name ] = true
end

function JSON:startParent( name, arrayFlag )
   self:addElementName( name )

   if self:equalLayerState( 'termed' ) or self:equalLayerState( 'named' ) or
      self:equalLayerState( 'valued' )
   then
      self.stream:write( "," )
   elseif self:equalLayerState( 'none' ) then
      ;
   end

   local parentInfo = self:getLayerInfo()
   if not arrayFlag and parentInfo and parentInfo.arrayFlag then
      self:startLayer( false, true )
   end
   
   self.stream:write( string.format( '"%s": ', name ) )
   self:startLayer( arrayFlag )
   self.prevName = name
end

function JSON:startElement( name )
   self:addElementName( name )

   if self:equalLayerState( 'termed' ) then
      self.stream:write( "," )
   elseif self:equalLayerState( 'named' ) then
      error( 'illegal layer state' )
   elseif self:equalLayerState( 'none' ) then
      ;
   end
   if self:isArrayLayer( state ) then
      self:startLayer( false, true )
   end

   local info = self:getLayerInfo()

   if info.openElement then
      error( 'illegal openElement' )
   end
   info.openElement = true
   
   self.stream:write( string.format( '"%s": ', name ) )
   self:setLayerState( 'named' )
   self.prevName = name
end

function JSON:endElement()
   if self:equalLayerState( 'none' ) or self:equalLayerState( 'termed' ) then
      self:endLayer()
   elseif self:equalLayerState( 'valued' ) then
      local info = self:getLayerInfo()
      if info.openElement then
	 info.openElement = false
      end
      if self:getLayerInfo().madeByArrayFlag then
	 self:endLayer()
      end
   else
      error( 'illegal layer state ' .. self:getLayerName() )
   end
   self:setLayerState( 'termed' )
end

function JSON:writeValue( val )
   local txt
   if type( val ) == "number" then
      txt = string.format( "%g", val )
   else
      txt = string.format( '"%s"', convertJsonTxt( val ) )
   end
   
   self.stream:write( txt )
   self:setLayerState( 'valued' )
end

function JSON:write( name, val )
   self:startElement( name )
   self:writeValue( val )
   self:endElement()
end

function JSON:fin()
   if self:equalLayerState( 'none' ) or self:equalLayerState( 'termed' ) then
      self:endLayer()
   else
      error( 'illegal' )
   end
end

return Writer
