int (*(*fun_one)(char *,int));
void (*foo)(int);
int main(void) {
  int i;
  char c;
  struct S {
    int i;
    char c;
    struct S* s;
    struct T {
      int i;
      char c;
    } t;
  };
  return 0;
}
