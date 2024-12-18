// Global variables
int global_count = 0;
static int static_global = 1;

struct Point {
  int x;
  int y;
};

// Function to add two numbers
int add(int a, int b) {
  return a + b;
}

// Function demonstrating bit operations
int bit_operations(int num1, int num2) {
  int result = 0;
  result |= (num1 & num2);   // AND + OR
  result ^= (num1 | num2);   // XOR + OR
  result = ~result;          // NOT
  return result;
}

int main() {
  // Data types
  int a = 10;
  float b = 3.14;
  char c = 'C';
  long d = 100000L;

  // Strings
  /*
  char str1[20] = "Hello";
  char str2[20] = " World";
  */

  // Static local variable
  static int static_local = 0;

  // Expressions and assignments
  a += 5;
  b = b * 2.0;

  // Struct usage
  /*
  struct Point p1;
  p1.x = 1;
  p1.y = 2;
  */

  // For-loop
  int j = 0;
  for (int i = 0; i < 5; i++) {
    if (i % 2 == 0) {
        j += i;
    } else {
        j -= i;
    }
  }

  // While loop 
  int count = 0;
  while (count < 3) {
    count++;
  }

  // Do-while loop
  int do_count = 0;
  do {
    do_count++;
  } while (do_count < 3);

  // Function call
  int sum = add(a, j); // replace j with p1.x

  // Bit operations
  int bit_result = bit_operations(5, 3); 

  // Static variable usage 
  static_local++;
  static_global++;

  // Return global variable
  global_count = sum;
  return global_count;
}