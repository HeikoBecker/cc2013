int foo(int, char);

int main(void) {
  if (&foo) {
    foo(1,1);
  }
  return foo(1,1);
}

int foo(int i, char c) {
  if (i == c) {
  return 0;
  } return 0;
}
