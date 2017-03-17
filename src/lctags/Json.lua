local log = require( 'lctags.LogCtrl' )

local JsonStream = {}
function JsonStream:new( stream )
   local obj = {
      stream = stream,
      index = 1
   }
   setmetatable( obj, { __index = JsonStream } )
   return obj
end

function JsonStream:fillBuf()
   if not self.stream then
      return false
   end
   self.buf = self.stream:read( 1000 * 10 )
   if not self.buf then
      self:clear()
      return false
   end
   self.index = 1
   return true
end

function JsonStream:readChar( num )
   local txt = ""
   for index = 1, num do
      if not self.buf or #self.buf < self.index then
	 if not self:fillBuf() then
	    return nil
	 end
      end
      self.index = self.index + 1
      txt = txt .. self.buf:sub( self.index - 1, self.index - 1 )
   end
   return txt
end

function JsonStream:readToPattern( func, pattern, index )
   local txt = self.buf:sub( index, index )
   index = index + 1
   while true do
      local endIndex = string.find( self.buf, pattern, index )
      if endIndex then
	 self.index = endIndex + 1
	 local finishFlag
	 local token
	 finishFlag, token, index = func( index, endIndex )
	 if token and #token > 0 then
	    txt = txt .. token
	 end
	 if finishFlag then
	    return txt
	 end
      else
	 -- バッファ内では文字列確定しない
	 if #self.buf >= index then
	    txt = txt .. string.sub( self.buf, index )
	 end
	 self:fillBuf()
	 index = 1
      end
   end
end

function JsonStream:pushToken( token )
   self.pushedToken = token
end

function JsonStream:clear()
   self.stream = nil
end

function JsonStream:readToken()
   if self.pushedToken then
      local token = self.pushedToken
      self.pushedToken = nil
      return token
   end

   local token = self:readTokenSub()
   return token
end

function JsonStream:readTokenSub()
   if not self.stream then
      return nil
   end
   while true do
      if not self.buf or #self.buf < self.index then
	 if not self:fillBuf() then
	    return nil
	 end
      end
      local index = string.find( self.buf, "[^%s]", self.index )
      if index then
	 local oneChar = string.sub( self.buf, index, index )
	 if string.find( oneChar, "[%[%]%{%},:]" ) then
	    self.index = index + 1
	    return oneChar
	 elseif string.find( oneChar, "t" ) then
	    self.index = index
	    if self:readChar( 4 ) == "true" then
	       return "true"
	    end
	    log( 1, "illegal format -- true", self.buf )
	    return nil
	 elseif string.find( oneChar, "f" ) then
	    self.index = index
	    if self:readChar( 5 ) == "false" then
	       return "false"
	    end
	    log( 1, "illegal format -- false", self.buf )
	    return nil
	 elseif string.find( oneChar, "n" ) then
	    self.index = index
	    if self:readChar( 4 ) == "null" then
	       return "null"
	    end
	    log( 1, "illegal format -- null", self.buf )
	    return nil
	 elseif string.find( oneChar, "[%d%-]" ) then
	    -- 数値
	    return self:readToPattern(
	       function( startIndex, endIndex )
		  if not string.find( self.buf, "[,}%]]", endIndex ) then
		     log( 1, "illegal format -- ",
			  string.sub( self.buf, startIndex, endIndex ) )
		     return true, nil
		  end
		  self:pushToken( string.sub( self.buf, endIndex, endIndex ) )
		  if startIndex == endIndex then
		     return true, ""
		  end
		  return true, string.sub( self.buf, startIndex, endIndex - 1 )
	       end, '[^%d%-%.eE%+]', index )
	 elseif oneChar == '"' then
	    -- 文字列
	    return self:readToPattern(
	       function( startIndex, endIndex )
		  return self:readString( startIndex, endIndex )
	       end, '["\\]', index )
	 else
	    log( 1, "illegal format -- ", oneChar )
	    return nil
	 end
      else
	 self.buf = nil
      end
   end
end

function JsonStream:readString( startIndex, endIndex )
   oneChar = string.sub( self.buf, endIndex, endIndex )
   if oneChar == '"' then
      -- 文字列確定
      return true, string.sub( self.buf, startIndex, endIndex )
   end
   -- \
   local txt = ""
   if endIndex > startIndex then
      txt = string.sub( self.buf, startIndex, endIndex - 1 )
   end
   if oneChar == '\\' then
      txt = txt .. '\\'
   elseif oneChar == '"' then
      txt = txt .. '"'
   elseif oneChar == '/' then
      txt = txt .. '/'
   elseif oneChar == 'b' then
      txt = txt .. "\b"
   elseif oneChar == 'f' then
      txt = txt .. '\f'
   elseif oneChar == 'n' then
      txt = txt .. '\n'
   elseif oneChar == 'r' then
      txt = txt .. '\r'
   elseif oneChar == 't' then
      txt = txt .. '\t'
   elseif oneChar == 'u' then
      log( 1, "not supprot" )
      os.exit( 1 )
   end
   return false, txt, self.index + 1
end


local Json = {}

function Json:fromText( text )
   local obj = {
      read = function()
	 local txt = text
	 text = nil
	 return txt
      end,
   }
   return Json:fromStream( obj )
end

function Json:fromStream( stream )
   local jsonStream = JsonStream:new( stream )

   local json = {
      jsonStream = jsonStream,
      keyList = {},
   }

   setmetatable( json, { __index = Json } )

   return json
end

function Json:convertFrom( txt )
   local json = Json:fromText( txt )
   if not json then
      return nil
   end
   return json:readValue()
end


function Json:readValue( func )
   if not func then
      func = function( val )
	 return val
      end
   end
   
   local token = self.jsonStream:readToken()
   if not token then
      self.jsonStream:clear()
      return nil
   end
   if token:sub( 1, 1 ) == '"' then
      return token:sub( 2, #token - 1 )
   end
   if token == "true" then
      return true
   end
   if token == "false" then
      return false
   end
   if token == "null" then
      return nil
   end
   if token == "[" then
      local array = {}
      local length = 0
      while true do
	 local nextToken = self.jsonStream:readToken()
	 if nextToken == nil then
	    return array
	 end
	 if nextToken == "]" then
	    return array
	 elseif length > 0 then
	    if nextToken ~= "," then
	       log( 1, "nothing ',' in array" )
	       return nil
	    end
	 else
	    self.jsonStream:pushToken( nextToken )
	 end
	 table.insert( self.keyList, string.format( "[%d]", length ) )
	 local value = self:readValue( func )
	 table.remove( self.keyList )
	 if value then
	    table.insert( array, value )
	    length = length + 1
	 end
      end
   end
   if token == "{" then
      local obj = {}
      local length = 0
      while true do
	 local nextToken = self.jsonStream:readToken()
	 if nextToken == nil then
	    return nil
	 end
	 if nextToken == "}" then
	    return func( obj, self.keyList, "object" )
	 end
	 if length > 0 then
	    if nextToken ~= ',' then
	       log( 1, "nothing ',' in obj" )
	       return nil
	    end
	    nextToken = self.jsonStream:readToken()
	 end
	 if nextToken:sub( 1, 1 ) ~= '"' then
	    log( 1, "nothing key in obj" )
	    return nil
	 end

	 local key = nextToken:sub( 2, #nextToken - 1 )
	 nextToken = self.jsonStream:readToken( nextToken )
	 if nextToken ~= ":" then
	    log( 1, "nothing ':' after", key )
	    return nil
	 end
	 table.insert( self.keyList, key )
	 local value = self:readValue( func )
	 table.remove( self.keyList )
	 if value then
	    obj[ key ] = value
	    length = length + 1
	 end
      end
   end
   return tonumber( token )
end


return Json
