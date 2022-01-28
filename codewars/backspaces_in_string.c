#include <stdlib.h>
#include <stdio.h>
#include <string.h>

char *strclr(char *s)
{
  int len = strlen(s);
  char* out = calloc(len + 1, sizeof(char));
  int i = 0;
  char* it = s;

  while(*it) {
    if((*it) == '#') {
      out[i-1] = 0;
      if(i > 0){
	i--;
      }
    } else {
      out[i++] = *it;
    }

    it++;
  }

  return out;
}

int main(){
  printf("--> %s\n", strclr("abc#d##c"));
  printf("--> %s\n", strclr("abc##d######"));
  printf("--> %s\n", strclr("######"));
  printf("--> %s\n", strclr(""));
  printf("--> %s\n", strclr("abjd####jfk#"));
  printf("--> %s\n", strclr("VI##SHQ46zV#"));
}
