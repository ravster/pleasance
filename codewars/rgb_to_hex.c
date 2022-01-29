#include <stdlib.h>
#include <string.h>
#include <stdio.h>

int my_round(int x) {
  if (x < 0) return 0;
  if (x > 255) return 255;
  return x;
}

char* decToHex(int x) {
  char* hex = malloc(3); // We know that this can be only 2 chars.
  hex = strdup("00"); // Set to valid initial state.
  int i = 1, temp;
  int quotient = my_round(x);
  while(quotient!=0) {
    temp = quotient % 16;
    //To convert integer into character
    if( temp < 10) {
      temp += 48; // Using external knowledge of the ASCII character set in this conditional.
    } else {
      temp += 55;
    }
    hex[i--]= temp; // Write from right to left
    quotient = quotient / 16;
  }

  return hex;
}

int rgb(int r, int g, int b, char *output)
{
  output[0] = 0; // reset the string.
  strcat(output, decToHex(r));
  strcat(output, decToHex(g));
  strcat(output, decToHex(b));

  return strlen(output); // Some meaningful value.
}

int main(){
  char out[10] = {0};
  rgb(0, 0, 0, &out[0]);
  printf("--> %s\n", out);
  rgb(1, 2, 3, &out[0]);
  printf("--> %s\n", out);
}
