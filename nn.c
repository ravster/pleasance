// Copyright 2022 Ravi Desai <ravi@ravidesai.com>
// Distributed under the terms of the GNU Affero GPL version 3 or any later version.
//
// gcc -g -Wall -o nn nn.c -lm

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>

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

void calc_min_max(float* min, float* max, float* data, int num_rows, int start, int end) {
  *min = data[start];
  *max = data[start];
  for (int i = start; i < end; i++) {
    if(data[i] < *min) {
      *min = data[i];
    }
    if(data[i] > *max) {
      *max = data[i];
    }
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
    exit(1);
  }

  /* Load data */
  int num_rows = 3000; // Max rows we want to consider.
  opens = (float*) malloc(num_rows * sizeof(float));
  highs = (float*) malloc(num_rows * sizeof(float));
  lows = (float*) malloc(num_rows * sizeof(float));
  closes = (float*) malloc(num_rows * sizeof(float));
  dates = malloc(num_rows * sizeof(char*));
  num_rows = load_ohlc(argv[1]);

  float min = 0;
  float max = 0;
  calc_min_max(&min, &max, closes, num_rows, 0, num_rows);
  float* normalizedCloses = (float*) malloc(num_rows * sizeof(float));
  for(int i = 20; i < num_rows; i++) {
    float out = normalize_0_1(closes[i], min, max - min);
    normalizedCloses[i] = out;
  }

  calc_true_range(num_rows);
  calc_atr_10(num_rows);
  min = 0;
  max = 0;
  calc_min_max(&min, &max, atr10s, num_rows, 10, num_rows);
  float* normalizedAtr10s = (float*) malloc(num_rows * sizeof(float));
  for(int i = 10; i < num_rows; i++) {
    float out = normalize_0_1(atr10s[i], min, max - min);
    normalizedAtr10s[i] = out;
  }

  calc_moving_average(num_rows, 20);
  min = 0;
  max = 0;
  calc_min_max(&min, &max, ma, num_rows, 20, num_rows);
  float* normalizedMA20s = (float*) malloc(num_rows * sizeof(float));
  for(int i = 20; i < num_rows; i++) {
    float out = normalize_0_1(ma[i], min, max - min);
    normalizedMA20s[i] = out;
  }

  calc_close_plus_15(num_rows, 15);
  min = 0;
  max = 0;
  calc_min_max(&min, &max, close_plus_15, num_rows, 0, num_rows - 15);
  float* normalizedClosePlus15s = (float*) malloc(num_rows * sizeof(float));
  float last = num_rows - 15;
  for(int i = 0; i < last; i++) {
    float out = normalize_0_1(close_plus_15[i], min, max - min);
    normalizedClosePlus15s[i] = out;
  }

  int num_in_nodes = 2;
  int num_mid_nodes = 4;
  int num_out_nodes = 1;

  // This will be num-mid rows and num-in columns.  The first 3 entries will be
  // i0m0, i1m0, i2m0
  float* weights_in_mid = (float*) malloc(num_in_nodes * num_mid_nodes * sizeof(float));
  float* weights_mid_out = (float*) malloc(num_mid_nodes * num_out_nodes * sizeof(float));

  srand(time(NULL));
  for(int i = 0; i < (num_in_nodes * num_mid_nodes); i++) {
    weights_in_mid[i] = drand48();
  }
  for(int i = 0; i < (num_out_nodes * num_mid_nodes); i++) {
    weights_mid_out[i] = drand48();
  }
  float* mid_nodes = (float*) malloc(sizeof(float) * num_mid_nodes);
  for(int i = 0; i < num_mid_nodes; i++) {
    mid_nodes[i] = 0;
  }
  float* out_nodes = (float*) malloc(sizeof(float) * num_out_nodes);
  for(int i = 0; i < num_out_nodes; i++) {
    out_nodes[i] = 0;
  }

  for(int i = 20; i < (num_rows - 15); i++) {
    // Don't bother with a training vs testing dataset right now.  Just have something run.
    // inputs
    float atr10 = normalizedAtr10s[i];
    float ma20 = normalizedMA20s[i];
    float* in_nodes = (float*) malloc(sizeof(float) * num_in_nodes);
    in_nodes[0] = atr10;
    in_nodes[1] = ma20;

    // outputs
    float closePlus15s = normalizedClosePlus15s[i];

    // Calc output of each mid-node
    for(int j = 0; j < num_mid_nodes; j++) {
      float sum = 0;
      for(int k = 0; k < num_in_nodes; k++) {
	sum += in_nodes[k] * weights_in_mid[(j * num_in_nodes) + k];
      }
      // TODO same with closing price here.
      mid_nodes[j] = tanh(sum);
    }

    // Calc output nodes
    for(int j = 0; j < num_out_nodes; j++) {
      float sum = 0;
      for(int k = 0; k < num_mid_nodes; k++) {
	sum += mid_nodes[k] * weights_mid_out[(j * num_mid_nodes) + 0];
      }
      out_nodes[j] = tanh(sum);
    }

    // Calc err for out-nodes
    // TODO capable of multiple output nodes
    float y = out_nodes[0];
    float output_node_err_gradient = (1 - y*y) * (closePlus15s - y);

    // TODO allow many out-nodes
    // Calc err for mid-nodes
    float *mid_nodes_error_gradients = (float*)malloc(sizeof(float) * num_mid_nodes);
    for(int j = 0; j < num_mid_nodes; j++) {
      float mid_node_output = mid_nodes[j];
      mid_nodes_error_gradients[j] = weights_mid_out[j] *
	(1 - mid_node_output * mid_node_output) *
	output_node_err_gradient;
    }
    printf("%f\t%f\t%f\t%f\n", mid_nodes_error_gradients[0], mid_nodes_error_gradients[1], mid_nodes_error_gradients[2], mid_nodes_error_gradients[3]);

    // TODO:
    // Change weights to mid-nodes
    // Change weights to out-nodes
  }

  printf("done\n");
}
