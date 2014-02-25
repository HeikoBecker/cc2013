struct S;

int main(void) {
foo:;
}

int bar(void) {
  goto foo;
  return 0;
}
