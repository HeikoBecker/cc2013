int main(void) {
  int* i;
  int* j;
  struct S {int* iptr;} s;
  i = 0;
  j = i;
  if (j == 0)  {
    s.iptr = 0;
    struct S *sptr;
    sptr = &s;
    if (sptr == 0) {
      sptr = 0;
      if (0 == sptr) {
        return 0;
      }
    }
  }
  return 0;
}
