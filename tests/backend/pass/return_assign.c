int f(void) 
{
  {
    struct S {int x;} s;
    return (s.x  = 1);
  }
}

