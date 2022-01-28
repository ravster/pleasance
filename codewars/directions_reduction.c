#include <stdlib.h>
#include <stdio.h>

int is_opposite(char a, char b) {
  if((a == 'N' && b == 'S') ||
     (a == 'S' && b == 'N') ||
     (a == 'E' && b == 'W') ||
     (a == 'W' && b == 'E')) {
    return 1;
  }

  return 0;
}

char** dirReduc(char** arr, int s, int* s2) {
  // This is a stack problem.  That's it.  Ugh.  The word-problem makes it more confusing than it has to be.

  char** ret = malloc(s * sizeof(char*)); // This is the stack to output.
  *s2 = 0;

  for(int i = 0; i < s; i++) {
    if (*s2 < 1) {
      // Push the newest thing onto the stack and move to the next iteration.
      ret[*s2] = arr[i];
      (*s2)++;
      continue;
    }

    if(is_opposite(ret[(*s2)-1][0], arr[i][0])) {
      // Top of stack is opposite to this, so neutralize it.
      (*s2)--;
      continue;
    }

    // Not opposite, so push this onto stack
    ret[(*s2)++] = arr[i];
  }

  return ret;
}

int main(){
  char* d1[4] = {"NORTH", "SOUTH", "WEST", "EAST"};
  int* lg = malloc(sizeof(int));
  char** a1 = dirReduc(d1, 4, lg);
  printf("lg %d\n", *lg);
  for (int i = 0; i < *lg; i++) {
    printf("%s, ", a1[i]);
  }
}
