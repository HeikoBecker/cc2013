struct S;

int main(void) {
  struct S *s1;
  if (1) {
    struct S {int i;} *s2;
    s1 < s2;
  }
}
