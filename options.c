// gcc -g -Wall -o options options.c
//
// Usage: options 4.91 swn

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

float current_price = 0;

// Splits the string and returns elements 2..5
void atof_2_to_5(float* out, char* in) {
  char* line = in;
  line = strtok(line, "\t");
  line = strtok(NULL, "\t");

  line = strtok(NULL, "\t");
  out[0] = atof(line);
  line = strtok(NULL, "\t");
  out[1] = atof(line);
  line = strtok(NULL, "\t");
  out[2] = atof(line);
  line = strtok(NULL, "\t");
  out[3] = atof(line);
}

void process_line(char* line) {
  float vals[4] = {0, 0, 0, 0};
  atof_2_to_5(vals, line);
  float strike = vals[0];
  float last = vals[1];
  float bid = vals[2];
  float ask = vals[3];

  if ((last < bid) || (last > ask)) {
    return;
  }

  float expire = last/current_price;
  float exercise = (last+strike-current_price)/current_price;

  if ((expire < 0.01) || (exercise < 0.01)) {
    return;
  }

  printf("%.2f\t %.2f\t %.2f\t %.2f\t %.2f\t %.2f\n",
    strike, last, bid, ask,
    expire,
    exercise);
}

int main(int argc, char* argv[]) {
  if (argc != 3) {
    printf("Usage: options 4.91 swn.txt\n");
    exit(1);
  }

  current_price = atof(argv[1]);
  char* filename = argv[2];

  printf("strike\t last\t bid\t ask\t expire\t exercise\n");

  FILE* f = fopen(filename, "r");
  char line[256];
  while(fgets(line, 256, f)) {
    char* tmp = strdup(line);
    process_line(tmp);
    free(tmp);
  }
  fclose(f);

  return(0);
}
