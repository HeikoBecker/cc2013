struct S {int i;};

int main(void) {
  struct S *s1;
  if (1) {
    struct S {int j;} *s2;
    s2->j;
  }
}
