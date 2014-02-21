int main(void) {
  char a;
  struct S {int i;} s;
  return s.a; // <-- no member named a in struct S
}
