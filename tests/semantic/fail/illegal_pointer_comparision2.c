int main(void) {
  struct S {int i;};
  struct T {int i;};
  struct S *sptr;
  struct T *tptr;
  if (sptr < tptr) {
    return 0;
  }
  return 1;
}
