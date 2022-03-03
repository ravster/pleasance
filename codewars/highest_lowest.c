#include <stdio.h>
#include <string.h>
#include <stdlib.h>

void high_and_low (const char *strnum, char *result)
{
  int highest =  -2147483648;
  int lowest =  2147483647;
  char* dup = strdup(strnum);
  char* a1;
  a1 = strtok(dup, " ");
  do {
    int a2 = atoi(a1);
    if (a2 > highest) {
      highest = a2;
    }
    if (a2 < lowest) {
      lowest = a2;
    }
    a1 = strtok(NULL, " ");
  } while (a1 != NULL);

  sprintf(result, "%d %d", highest, lowest);
}

int main(){
  char* result = malloc(3000);
  high_and_low("8 3 -5 42 -1 0 0 -9 4 7 4 -4", result);
  printf("--> %s\n", result);
}
