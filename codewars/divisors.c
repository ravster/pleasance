#include <stdio.h>

int divisors(int n) {
  if (n == 1) { return 1; }
  int count = 2; // 1, and n itself

  for(int i = 2; i <= n/2; i++) {
    if (n % i == 0) {
      count++;
    }
  }

  return count;
}

int main() {
  printf("%d\n", divisors(4));
  printf("%d\n", divisors(5));
  printf("%d\n", divisors(12));
  printf("%d\n", divisors(30));
}
