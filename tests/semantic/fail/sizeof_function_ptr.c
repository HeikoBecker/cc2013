void foo(void) {;}

int main(void) {
  int a;
  int b;
  int c;
  a = sizeof(foo);
  b = sizeof(&foo);
  void (*fooptr)(void);
  c = sizeof(fooptr);
  return (a == b) && (b == c);
}
