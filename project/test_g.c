#include "core.h"
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
