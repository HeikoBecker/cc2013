struct S* foo(void);

struct S {int i;};

int main(void) {
  if (foo()) {
    foo()->i;
  } else {
    foo()->i;
  }
  return 0;
}


struct S* foo(void) {return 0;}
