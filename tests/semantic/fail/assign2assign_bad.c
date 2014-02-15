int main(void) {
  int a;
  int b;
  (a = b) = 0; // this should fail, because the result of an assignment is not a lvalue
  return a;
}
