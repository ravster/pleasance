// Copyright 2022 Ravi Desai <ravi@ravidesai.com>
// Distributed under the terms of the GNU Affero GPL version 3 or any later version.
//
// gcc -g -Wall -o nn nn.c -lm

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

float *opens, *highs, *lows, *closes;
float *trs, *atr10s, *ma, *close_plus_15;
char** dates;

void load_arrs(char* line, int i) {
  char* l2 = line;
  l2 = strtok(l2, ",");
  dates[i] = strdup(l2);

  l2 = strtok(NULL, ",");
  float f = atof(l2);
  opens[i] = f;

  l2 = strtok(NULL, ",");
  f = atof(l2);
  highs[i] = f;

  l2 = strtok(NULL, ",");
  f = atof(l2);
  lows[i] = f;

  l2 = strtok(NULL, ",");
  f = atof(l2);
  closes[i] = f;
}

int load_ohlc(char* filepath) {
  FILE* stream = fopen(filepath, "r");
  char line[100];
  int i = 0;
  while (fgets(line, 100, stream))
    {
        char* tmp = strdup(line);
	load_arrs(tmp, i);
        free(tmp); // NOTE strtok clobbers tmp
	i++;
    }
  fclose(stream);

  return i;
}

/* n is num_rows */
void calc_true_range(int n) {
  trs = (float*)calloc(n, sizeof(float));
  // We start from index 1 instead of index 0 because we can't calc TR for the first datapoint.
  for(int i = 1; i < n; i++) {
    float pr_close = closes[i-1];
    float h = highs[i];
    float l = lows[i];
    float a = fabs(h - l);
    float b = fabs(h - pr_close);
    float c = fabs(pr_close - l);

    trs[i] = fmax(a, fmax(b, c));
  }
}

void calc_atr_10(int n) {
  atr10s = (float*)malloc(n * sizeof(float));
  // We start from index 10 instead of index 0 because we can't calc TR for the first 10 datapoints.
  for(int i = 10; i < n; i++) {
    float sum = 0;
    for(int j = -10; j <= 0; j++) {
      sum += trs[i+j];
    }
    atr10s[i] = sum / 10;
  }
}

void calc_moving_average(int n, int period) {
  ma = (float*)malloc(n * sizeof(float));
  for(int i = period; i < n; i++) {
    float sum = 0;
    for(int j = -period; j <= 0; j++) {
      sum += closes[i+j];
    }
    ma[i] = sum / period;
  }
}

void calc_close_plus_15(int n, int period) {
  close_plus_15 = (float*)malloc(n * sizeof(float));
  int last = n-period;
  for(int i = 0; i < last; i++) {
    close_plus_15[i] = closes[i + period];
  }
}

/* Normalize value in between 0 and 1. */
float normalize_0_1(float val, float min, float maxmin_diff) {
  return (val - min) / maxmin_diff;
}

int main (int argc, char** argv) {
  /* validate input */
  if (argc != 2) {
    printf("\nUsage: nn qyld.csv\n");
  }

  /* Load data */
  int num_rows = 3000; // Max rows we want to consider.
  opens = (float*) malloc(num_rows * sizeof(float));
  highs = (float*) malloc(num_rows * sizeof(float));
  lows = (float*) malloc(num_rows * sizeof(float));
  closes = (float*) malloc(num_rows * sizeof(float));
  dates = malloc(num_rows * sizeof(char*));
  num_rows = load_ohlc(argv[1]);

  calc_true_range(num_rows);
  calc_atr_10(num_rows);
  calc_moving_average(num_rows, 20);
  calc_close_plus_15(num_rows, 15);

  printf("\ndone");
}
