struct S {int s;};
int main(void) {
  struct S *s1;
  struct S *s2;
  if (s1 == s2) {
    return 0;
  }
  return 1;
}
