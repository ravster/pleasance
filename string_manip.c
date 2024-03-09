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

char*
prepend(char* base_string, char* other_string) {
  size_t len_both = strlen(base_string) + strlen(other_string);
  char* both = malloc(len_both + 10);
  both[0] = 0;

  strcpy(both, other_string);
  strcat(both, base_string);
  return both;
}

char*
replace(char* base, char* marker, char* other) {
  size_t len_both = strlen(base) + strlen(other);
  char* both = malloc(len_both + 10);
  both[0] = 0;

  char* marker_pos = strstr(base, marker);
  size_t len_first_section = marker_pos - base;
  strncpy(both, base, len_first_section);
  both[len_first_section] = 0;
  strcat(both, other);
  char* second_section = marker_pos + strlen(marker);
  strcat(both, second_section);
  return both;
}

char*
append(char* base, char* other) {
  size_t len_both = strlen(base) + strlen(other);
  char* both = malloc(len_both + 10);
  both[0] = 0;

  strcpy(both, base);
  strcat(both, other);
  return both;
}

int main(int argc, char* argv[]) {
  if (argc != 4) {
    printf("Usage: ./string_manip \"base with a lot of spaces\" lot other\n");
    exit(1);
  }

  char* base_string = argv[1];
  char* marker = argv[2];
  char* other_string = argv[3];

  printf("Prepend\n");
  char* out;
  out = prepend(base_string, other_string);
  printf("%s\n\n\n", out);

  printf("Replace marker\n");
  out = replace(base_string, marker, other_string);
  printf("%s\n\n", out);

  printf("Append\n");
  out = append(base_string, other_string);
  printf("%s\n\n", out);

  return(0);
}
