int foo(char c) {return c;}

int main(void) {
  return sizeof( foo(1,2) );
}
