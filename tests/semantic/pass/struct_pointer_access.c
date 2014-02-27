int main() {
  struct S* a;
  struct S {int x;};
  (*a).x = 3;
}
