// gcc -g -Wall -o ten_ninety ten_ninety.c
//
// Usage: ten_ninety swn.csv

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

float get_close(char* in) {
  char* line = in;
  line = strtok(line, ",");
  line = strtok(NULL, ",");
  line = strtok(NULL, ",");
  line = strtok(NULL, ",");
  line = strtok(NULL, ",");
  return atof(line);
}

int is_first_line = 1;
float closes[500];
int count = 0;
void process_line(char* line) {
  if (is_first_line ==1) {
    is_first_line = 0;
    return;
  }
  float close = 0;
  close = get_close(line);
  closes[count] = close;
  count++;
  return;
}

float changes[500];
void calc_changes() {
  for (int i = 0; i < count-1; i++) {
    changes[i] = (closes[i +1] - closes[i]) / closes[i];
  }
}

int float_compare(const void * elem1, const void * elem2) {
  float e1 = *(float*) elem1; // cast to float*, and then deref that.
  float e2 = *(float*) elem2;

  if (e1 > e2) return 1;
  return -1;
}

float sorted_changes[500];
void sort_changes() {
  for (int i = 0; i < count-1; i++) {
    sorted_changes[i] = changes[i];
  }

  qsort(sorted_changes, count-1, sizeof(float),
	float_compare);
}

int main(int argc, char* argv[]) {
  if (argc != 2) {
    printf("Usage: ten_ninety T.csv\n");
    exit(1);
  }
  char* filename = argv[1];

  FILE* f = fopen(filename, "r");
  char line[256];
  while(fgets(line, 256, f)) {
    char* tmp = strdup(line);
    process_line(tmp);
    free(tmp);
  }
  fclose(f);

  printf("count=%d\n", count);

  calc_changes();
  sort_changes();

  int ten_dec = (count / 10) -2;
  int ninety_dec = count * 9/ 10;
  printf("10th percentile: %f, %d\n", sorted_changes[ten_dec], ten_dec);
  printf("90th percentile: %f, %d\n", sorted_changes[ninety_dec], ninety_dec);

  return(0);
}
