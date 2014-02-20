int main(void) {
  struct S {int i;} s;
  struct T {int i;} t;
  void *v;
  v = &s;
  if (&t == v) {
    return 0;
  }
  return 1;
}
