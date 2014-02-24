struct S;

int main(void) {
  if (1) {
      struct S {int i;};
      struct S s;
      s.i = 0;
    } else {
      struct S {int i;};
      struct S s;
      s.i = 0;
    }
  return 0;
}

