int bar(void) {return 0;}
int foo(int (f)(void)) {return f();}

int main(void) {
  return foo(bar);
}
