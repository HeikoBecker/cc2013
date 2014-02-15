int foo(int, int);
int foo(int i, int j);
int foo(int i, int k);
int main(void) {return foo(1,-1);}

int foo(int i, int j) {
  return i+j;
}
