int fib_h(char n, int a, int b) {
  if(!n) {
    return b;
  }
  return fib_h(n-1,b,a+b);
}

int fib(int a) {
  return fib_h(a, 0, 1);
}

int main(void) {
  return fib(5);
}
