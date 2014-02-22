int main(void) {
  int a;
  a = 1;
  if (a) {
    a = 0;
  } else {
    a = 2;
  }
  if (a) {
    int a;
    a = 2;
    return a;
  }
  return a;
}
