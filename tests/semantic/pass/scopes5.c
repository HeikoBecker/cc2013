
int main(void) {
  struct S {struct T {int i;} t;};
  struct T t;
  struct Y {struct T {int j;} t;};
  struct T t2;
  t.i = 42;
  t2.j = 42;
  return t.i;
}
