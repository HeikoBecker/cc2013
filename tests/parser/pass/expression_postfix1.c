struct S {int i;};

int main(void) {
  struct S *sptr;
  struct S **sptrptr;
  sptrptr = &sptr;
  return (*sptrptr)->i;
}
