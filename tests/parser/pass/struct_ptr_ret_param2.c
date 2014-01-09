struct S {int i;};

struct S s;

struct S *foo(void) {
  return &s;
}

int main(void) {
  s.i  = 0;
  return foo()->i == 0;
}
