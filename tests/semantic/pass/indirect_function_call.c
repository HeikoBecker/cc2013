struct S {int i;} *foo(void) {return 0;}
struct S *bar(void) {return foo();}


int main(void) {
  if (foo()  < (&bar)()) {
    return 0;
  }
  return 0;
}

