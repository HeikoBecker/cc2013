struct S;
int main(void) {
  struct S *s;
   if (s - s) { // s has incomplete type!
     return 0;
   }
   return 0;
}

