struct S {int s;};
int main(void) {
  struct S *s1;
  void *v;
  v  = s1;
  if (s1 == v) {
    return 0;
  }
  return 1;
}
