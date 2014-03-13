struct S {char c;};

/*int bar(struct S {int i;} s);*/ // this would be illegal
/*int bar2(struct S {int i;});*/

int foobar(struct S {int i;} s);
int foobar2(struct S {int i;});


int foo(struct S { char x; } s){
  int bar(struct S {int i;} s);
  int bar2(struct S {int i;});
  return 0;
}

int main(void) {return 0;}
