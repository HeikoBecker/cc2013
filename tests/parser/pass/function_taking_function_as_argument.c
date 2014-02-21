int f(int g(void));

int main(void) {
  return f(main);
}

int f(int g(void)) {
  return 0;
}
