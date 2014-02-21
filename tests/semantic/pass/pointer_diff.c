int main(void) {
  struct S { int i; int j;} s;
  if (&s.i - &s.j) {
    return 0;
  }
  return 0;
}
