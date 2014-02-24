struct S;

int foo(void){
  if (foo) { // who would do this? But it is legal...
     return 1;
  }
} 

int main(void) {
  return 0;
}
