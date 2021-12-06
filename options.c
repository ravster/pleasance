// gcc -g -Wall -o options options.c
//
// Usage: options 4.91 swn

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

float current_price = 0;

void process_line(char* line) {
  line = strtok(line, "\t");
  line = strtok(NULL, "\t");

  line = strtok(NULL, "\t");
  float strike = atof(line);
  line = strtok(NULL, "\t");
  float last = atof(line);
  line = strtok(NULL, "\t");
  float bid = atof(line);
  line = strtok(NULL, "\t");
  float ask = atof(line);

  float expire = last/current_price;
  float exercise = (last+strike-current_price)/current_price;

  if ((expire >= 0.01) && (exercise >= 0.01)) {
    printf("%f, %f, %f, %f, %f, %f\n",
	   strike, last, bid, ask,
	   expire,
	   exercise);
  }
}

int main(int argc, char* argv[]) {
  if (argc != 3) {
    printf("Usage: options 4.91 swn.txt\n");
    exit(1);
  }

  current_price = atof(argv[1]);
  char* filename = argv[2];

  printf("strike, last, bid, ask, expire, exercise\n");

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
