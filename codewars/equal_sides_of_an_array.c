#include <stdio.h>

int find_even_index(const int *values, int length) {
  for (int i = 0; i < length; i++) {
    int left = 0, right = 0;
    for (int j = 0; j < i; j++) {
      left += values[j];
    }
    for (int j = i+1; j < length; j++) {
      right += values[j];
    }
    if (left == right) { return i; }
  }

  return -1;
}

int main() {
  int a1[] = {1,2,3,4,3,2,1};
  printf("%d\n", find_even_index(a1, 7));
}
