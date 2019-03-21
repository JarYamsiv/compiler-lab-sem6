#include <stdio.h>
void temp(){
   int b =  0 ;
   if( b > 0 ){
      void c =  45 ;
   }
   return c;
}
void swap(){
   int a =  10 ;
   int b =  15 ;
   int t = a;
   a = b;
   b = t;
}
void main(){
   int a =  0 ;
   int b =  3 +a;
   b = b+ 1 ;
   b();
   return  12 +b[ 0 ];
}
