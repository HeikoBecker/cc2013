struct S {int i;};

int foo(struct S s) {return s->i;}

int main(void) {
  struct S s;
  s.i = 0;
  return foo(s);
}
