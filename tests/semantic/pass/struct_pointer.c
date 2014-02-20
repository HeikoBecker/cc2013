struct S {
        int p;
}; struct H {struct S o; };

struct G { struct H* b; };

int main(void ){
      {
                      int a;
                      int b;
                      int blub;
                      struct G** c;
                      c[9]->b->o.p = a;
      }
}
