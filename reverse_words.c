#include <stdlib.h>
#include <string.h>
#include <stdio.h>

char* r2(char* in) {
  int len = strlen(in);
  char* out = calloc(len + 10, sizeof(char));

  for (int i = 0; i < len; i++) {
    out[i] = in[len - 1 - i];
  }

  return out;
}

char* reverseWords(const char* text) {
  int len = strlen(text);
  char* out = calloc(len+10, sizeof(char));

  char* temp1 = calloc(len, sizeof(char));
  for (int i = 0; i < len; i++) {
    if (text[i] == ' ') {
      char* ug = r2(temp1);
      strcat(out, ug);
      free(ug);
      temp1[0] = 0;
      out[i] = text[i];
    } else {
      char* a1 = calloc(20, sizeof(char));
      sprintf(a1, "%c", text[i]);
      strcat(temp1, a1);
      free(a1);
    }
  }
  char* ug = r2(temp1);
  strcat(out, ug); // Do the last one
  free(ug);
  free(temp1);

  return out;
}

int main() {
  char* a1 = reverseWords("apple");
  printf("%s\n", a1);
  char* a2 = reverseWords("apple hahahaha");
  printf("%s\n", a2);
  char* a3 = reverseWords("apple   gobblede gook");
  printf("%s\n", a3);
  free(a1);
  free(a2);
  free(a3);
}
