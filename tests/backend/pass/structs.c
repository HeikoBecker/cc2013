struct S {int i;};
int main(void) {
  struct S s;
  struct S* sptr;
  int i;
  s.i = 0;
  sptr->i = 0;
  i = s.i;
  i = sptr->i;
  return i;
}
