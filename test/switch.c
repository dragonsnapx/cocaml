int main() {
  int x = 2;
  int y = 0;
  switch (x) {
    case 1:
      y = 42;
      break;
    case 2:
      y = 99;
      break;
    default:
      y = -1;
  }
  return y;
}
