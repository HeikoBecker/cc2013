int main(void) {
  foo: return 0;
  goto foo;
}

int foo(void) {
  foo: return 0;
  goto foo;
}
