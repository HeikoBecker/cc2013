struct S {char c;};

int bar(struct S {int i;} s);  // I
int bar2(struct S {int i;});

int foo(struct S { char x; } s){
  int bar(struct S {int i;} s); // this is illegal, as the S in I is not the same as the S here
  int bar2(struct S {int i;});
  return 0;
}

int main(void) {return 0;}
