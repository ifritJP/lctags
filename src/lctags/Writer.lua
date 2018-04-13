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
   return self
end

function XML:startParent( name )
   self:startElement( name )
end

function XML:startElement( name )
   table.insert( self.elementList, name )
   self.stream:write( string.format( '<%s>', name ) )
end

function XML:endElement()
   local name = table.remove( self.elementList )
   self.stream:write( string.format( '</%s>', name ) )
end

function XML:writeValue( val )
   self.stream:write( convertXmlTxt( val ) )
end

function XML:write( name, val )
   self:startElement( name )
   self:writeValue( val )
   self:endElement( name )
end



local JSON = {}
Writer.JSON = JSON

local function convertJsonTxt( txt )
   if txt == nil or txt == "" then
      return ""
   end
   if type( txt ) == "number" then
      return string.format( "%g", txt )
   end
   txt = string.gsub( txt, '"', '\\"' )
   txt = string.gsub( txt, '\\', '\\\\' )
   txt = string.gsub( txt, '\n', '\\n' )
   return txt
end


function JSON:open( stream )
   self.stream = stream
   self.layerState = {}
   self.layerName = {}
   self:startLayer()


   return self
end

function JSON:startLayer()
   self.stream:write( "{" )   
   table.insert( self.layerState, 'none' ) -- none, named, valued, termed
   table.insert( self.layerName, self.prevName )
end

function JSON:endLayer()
   if #self.layerState > 0 then
      self.stream:write( string.format( '}' ) )
      table.remove( self.layerState )
      table.remove( self.layerName )
   end
end

function JSON:equalLayerState( state )
   return self.layerState[ #self.layerState ] == state
end

function JSON:setLayerState( state )
   self.layerState[ #self.layerState ] = state
end

function JSON:startParent( name )
   if self:equalLayerState( 'termed' ) or self:equalLayerState( 'named' ) or
      self:equalLayerState( 'valued' )
   then
      self.stream:write( "," )
   elseif self:equalLayerState( 'none' ) then
      ;
   end
   self.stream:write( string.format( '"%s": ', name ) )
   self:startLayer()
   self.prevName = name
end


function JSON:startElement( name )
   if self:equalLayerState( 'termed' ) then
      self.stream:write( "," )
   elseif self:equalLayerState( 'named' ) then
      error( 'illegal layer state' )
   elseif self:equalLayerState( 'none' ) then
      ;
   end
   self.stream:write( string.format( '"%s": ', name ) )
   self:setLayerState( 'named' )
   self.prevName = name
end

function JSON:endElement()
   if self:equalLayerState( 'none' ) or self:equalLayerState( 'termed' ) then
      self:endLayer()
   elseif self:equalLayerState( 'valued' ) then
      ;
   else
      error( 'illegal layer state ' .. self.layerName[ #self.layerName ] )
   end
   self:setLayerState( 'termed' )
end

function JSON:writeValue( val )
   self.stream:write( string.format( '"%s"', convertJsonTxt( val ) ) )
   self:setLayerState( 'valued' )
end

function JSON:write( name, val )
   self:startElement( name )
   self:writeValue( val )
   self:endElement()
end

return Writer
