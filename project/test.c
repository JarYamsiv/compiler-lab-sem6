#include <stdio.h>
#include <stdint.h>
void temp(){
   int b =  1 ;
   int c =  77 ;
   int a =  23 ;
}
int swap(){
   int a =  10 ;
   int b =  15 ;
   int t = a;
   a = b;
   b = t;
   if( a > b ){
      return  3 ;
   }
   else{
      return a+b;
   }
}
int main(){
   int a =  0 ;
   int b =  3 +a;
   b = b+ 1 ;
   while( b < 20 ){
 
			printf("%d\n",b);
		
      if( b == 17 ){
         return  5 ;
      }
      b = b+ 1 ;
   }
   return  12 ;
}
