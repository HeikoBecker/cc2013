int main(void) {
  int *ip;
  int a;
  int b;
  ip = &a;
  ip = ip  +1;
  ip = 1 + ip;
  *(ip+1) = 4;
  return 0;
}
