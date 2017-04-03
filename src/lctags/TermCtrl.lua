local TermCtrl = {}

local escp = "\x1b["

function printEscpCode( val, code )
   io.stdout:write( escp .. tostring( val ) .. code )
end

function TermCtrl:clr()
   printEscpCode( 2, "J" )
end

function TermCtrl:clrLine()
   printEscpCode( 2, "K" )
end

function TermCtrl:gotoAt( x, y )
   printEscpCode( y, ";" .. tostring( x ) .. "H" )
end

return TermCtrl
