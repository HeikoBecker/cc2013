char
foo(char a, char b, char c)
{
  if (c - a - b)
    return 'a';
  return 'b';
}

int
main(void)
{
  return sizeof(foo(1,2,3));
}
