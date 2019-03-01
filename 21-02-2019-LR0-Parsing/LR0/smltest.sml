fun substringer x (y::ys) = (case Atom.compare(x,y) of
												EQUAL => ys
												|_	  => (substringer x ys)	)
	|substringer x []	  = []