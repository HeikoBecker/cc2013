int main(void) {
  int good;
  good = 0;
  good ? good = 1 : good == 2 ? good = 0 : (good = 3);
  return good;
}
