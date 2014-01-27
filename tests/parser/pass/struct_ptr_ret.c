struct S {int i;};

struct S* foo(void) {return 0;};

int main(void ) {
  if (foo() == 0) {
    return 0;
  }
  return 42;
}
