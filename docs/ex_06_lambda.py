

Supplier = [ [3],   [4],   [5] ]
Part     = [ [0],   [1],   [2] ]
SPJoin   = [ [3,0], [4,1], [3,1], [5,0], [3,2] ]

def select( collection, filter_function ):
    return filter( filter_function, collection )

def execute():
  global Supplier, Part, SPJoin

  q = select( Supplier, 
	      lambda s : 
	      0 == len( select( Part,  
				lambda p :
				0 == len( select( SPJoin, 
						  lambda sp : 
						  sp[0] == s[0] and sp[1] == p[0] )))))

  return q
  
