val a = 10;
type RHS = Atom.atom list

fun prntRHS (x:RHS) = case x of
						(y::ys) => (print ((Atom.toString y)^" "); prntRHS ys )
						| []	=> ()

val alist:RHS = map (Atom.atom) ["ee","ff","gg"]
val blist:RHS = map (Atom.atom) ["aa","bb"]

structure StringKey =
struct
    type ord_key = RHS
    fun compare ((x::xs),(y::ys)) = (case Atom.compare (x,y) of
        								  GREATER => GREATER
        								| LESS	  => LESS
        								| EQUAL	  => compare (xs,ys))
    	|compare ((x::xs),[])	  = GREATER
    	|compare ([]	,(y::ys)) = LESS
    	|compare ([],	[])		  = EQUAL 

    fun f (x:ord_key) :RHS = x
end

structure SSet = RedBlackSetFn(StringKey)

val t = SSet.empty;
val t = SSet.add(t,alist);
val t = SSet.add(t,blist);
val b = SSet.member (t,alist);
val al = SSet.listItems(t);

val mapp = AtomMap.singleton(Atom.atom "A",t)

val al = map (StringKey.f) al;

fun prnt_rhs_list ((x:RHS)::(xs:RHS list)) = ( prntRHS x;print " | ";prnt_rhs_list xs  )
	| prnt_rhs_list []    = ()

val it = prnt_rhs_list al;





