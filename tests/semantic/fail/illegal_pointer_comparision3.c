int main(void) {
  int *i;
  char *j;
  if (&i < &j) {
    return 0;
  }
  return 1;
}
