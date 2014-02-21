int fac(int n) {
  if (n == 0 || n == 1) {
    return 1;
  } else {
    return fac(n-1);
  }
}


int fac2(int n) {
  if (n == 0 || n == 1) 
    return 1;
  return fac2(n-1);
}
