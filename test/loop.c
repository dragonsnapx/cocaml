int square(int x) {
  return x * x;
}

int main() {
  int x = 0;
  for (int i = 0; i < 10; i = i + 1) {
    x = square(x);
  }
  return x;
}
