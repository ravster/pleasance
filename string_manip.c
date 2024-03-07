// gcc -g -Wall -o string_manip string_manip.c
//
// Usage: ./string_manip "base with a lot of spaces" lot other
//
// string1 should be the string that we want to put at the beginning, end, and middle
// of a "base string".
// marker is a substring in base-string that will be replaced by the contents of other-
// string. marker MUST appear only once in the base_string.

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

int main(int argc, char* argv[]) {
  if (argc != 4) {
    printf("Usage: ./string_manip \"base with a lot of spaces\" lot other\n");
    exit(1);
  }

  char* base_string = argv[1];
  char* marker = argv[2];
  char* other_string = argv[3];

  printf("Prepend\n");
  size_t len_both = strlen(base_string) + strlen(other_string);
  char* both = malloc(len_both + 10);
  both[0] = 0;
  strcpy(both, other_string);
  strcat(both, base_string);
  printf("%s\n\n\n", both);

  printf("Replace marker\n");
  both[0] = 0;
  char* marker_pos = strstr(base_string, marker);
  size_t len_first_section = marker_pos - base_string;
  strncpy(both, base_string, len_first_section);
  both[len_first_section] = 0;
  strcat(both, other_string);
  strcat(both, marker_pos + strlen(marker));
  printf("%s\n\n", both);

  printf("Append\n");
  both[0] = 0;
  strcpy(both, base_string);
  strcat(both, other_string);
  printf("%s\n\n", both);

  return(0);
}
