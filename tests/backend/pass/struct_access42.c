int main(void) {
  struct S {int i; char c;} s;
  s.i = 40;
  struct S *sptr;
  sptr = &s;
  sptr->c = 2;
  return sptr->i + s.c;
}
