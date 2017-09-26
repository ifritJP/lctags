local list = { { val = 1 }, { val = 4 }, { val = 2 } }

table.sort( list,
	    function( item1, item2 )
	       return item1.val > item2.val
	    end
)

for index, val in pairs( list ) do
   print( index, val.val )
end
