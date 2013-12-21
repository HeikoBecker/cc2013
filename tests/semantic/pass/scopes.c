int main() {
  // look at diffent scopes
  int a;
  a = 4;
  // print(a)
  {
    int a;
    a = 5;
    // print(a)
  }
  // print(a)
}
