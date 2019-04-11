# Easy-C

Easy-C is a strictly typed and type infered language which compiles to C.

  - written in SML
  - no need to define types of each variable
  - strictly typed 
  - better constant folding
  - dead code removal
  - has direct c feature which allows to write c code from within the language


# Eg:
```cpp
fun decrease(a)
beg	
	ret (a-1)
end

fun main()
beg
	a = 100
	while a>0 
	beg
		a =  decrease(a)
		$ printf("%d ",a) ; $

	end
end
```

Compiles to

```cpp
int decrease(int a){
   return (a- 1 );
}
void main(){
   int a =  100 ;
   while(a> 0 ){
      a = decrease(a);
       printf("%d ",a) ; 
   }
}
```




### Development

Maybe during my summer vacation . 
If I get free time after my internship   :p
Feel free to write your test cases in the folder test
raise issue if you find any



### Todos

 - New types (also user defined types)
 - Arrays and trees
 - tuples and pattern matching
 - higher order functions


