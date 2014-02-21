int *(*pfp)(void);        //a pointer to a function returning an int pointer

int main(void) {
  int* intptr;
  intptr = (*pfp)();
  return *intptr;
}
