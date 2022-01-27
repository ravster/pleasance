#include <stdlib.h>
#include <stdio.h>

int* parse(char* program)
{
  int value = 0;
  int* out = (int*) malloc(20 * sizeof(int));
  int count = 0;
  while(*program) {
    switch(*program) {
        case 'i':
        value++;
        break;
        case 'd':
        value--;
        break;
        case 's':
        value = value * value;
	break;
        case 'o':
        out[count] = value;
        count++;
    }
    program++;
  }

  return out;
}

int main(){
  int* a1 = parse("isoisoiso");
  for (int i = 0; i < 20; i++) {
    printf("%d, ", a1[i]);
  }
}
