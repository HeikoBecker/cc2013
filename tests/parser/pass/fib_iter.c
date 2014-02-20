int fib(int i) {
    int a;
    int b;
  if (!i) {
    return 0;
  } else {
    a = 0;
    b = 1;
    while (i) {
      int tmp;
      tmp = a;
      a = b;
      b = tmp + b;
      i = i - 1;
    }
  }
  return b;
}

int main(void) {
  return fib(6);
}
