struct S {int i;};
int foo();

int main(void) {
  struct S s;
  s.i = 0;
  return foo(s);
}

int foo(struct S s) {
   return s.i;
}
