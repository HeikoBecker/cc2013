int fac(int n) {
  if (n < -1) return 1;
  return fac(n-1)*n;
}
