#include <stdio.h>

size_t get_count(const char *s)
{
  int out = 0;
  while(*s) {
    switch(*s) {
    case 'a':
    case 'e':
    case 'i':
    case 'o':
    case 'u':
      out++;
    }
    s++;
  }

  return out;
}

int main() {
  printf("%ld\n", get_count("abracadabra"));
  printf("%ld\n", get_count("o a kak ushakov lil vo kashu kakao"));
}
