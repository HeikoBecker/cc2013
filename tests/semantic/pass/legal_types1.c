struct S {
  int (*f)(void); // function pointer are allowed in structs
};

int main(void) {
  struct S s;
  s.f = main;
  return 0;
}
