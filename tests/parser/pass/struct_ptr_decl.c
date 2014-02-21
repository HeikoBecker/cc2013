int main(void) {
  struct S {int i;} *s;
  if (s) {
      return 1;
    }
  return 2;
}
