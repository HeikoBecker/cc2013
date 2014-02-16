void print(int);

int main() {
  // look at diffent scopes
  int a;
  a = 4;
  print(a);
  {
    char a;
    a = 5;
    print(a);

    {
      {
      int a;
      }
      int a;
    }

  }

  {
    int a;
    {
      char a;
    }

  }

  print(a);
}
