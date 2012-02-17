# Pleasance
This is a project to build an AI (Artificial Intelligence) system that predicts changes in financial markets.

So far I have built the systems that take in the raw financial data and store it in an array of customized objects.  I also have technical indicators coded that build signals from the raw data.  Then the program parses the signals and re-scales them between -1 and +1 since neural networks (NNs) work best with data that has a common scale.

At present, the system is able to use the genetic algorithm (GA) on the NNs and find a NN which gets a directional accuracy of nearly 70%.

## API
### (refresh-data raw-data data-vector)
'raw-data' is a file-name that contains OHLC data that we are looking at.  'data-vector' is the name of a globally-accessible (Within the pleasance package) vector that will hold the data from the 'raw-data' file.  This macro has to be run to load the data into the program.  If you want the program to do what it does on another data-set, just call this function again.
### (ga length-of-chromosome size-of-population number-of-generations)
This function takes in 3 parameters and applies them onto the NN-function.  The parameters in order are the length of each chromosome, the size of the population, and the number of generations to keep running this GA.

At present we have 12 technical indicators, and you should use that as the value for the length of each chromosome.  I have found that using 30 for the other 2 parameters is good enough to get to the maximum that these NNs can produce (In terms of directional accuracy).

### (nn chromosome)
This function is the basic NN-function.  It takes in a "chromosome" and spits out the stats of that NN's performance after training over 2000 iterations of the data.

A chromosome is a list composed of 1's and 0's.  These are used to select which technical indicators are used as inputs by the NN.  The entire reason for this system and the GA is because we don't know which indicators are actually useful for predictions and which are noise.

### Additional information

As of right now the nn-function is hard-coded to only look at data that is inside the `training-set`, ``validation-set` & `test-set` arrays.  While this can cause confusion when you are dealing with multiple datasets, I don't consider it a big deal since it is so quick to re-calculate all the technical indicators and re-scale the data for each dataset (Especially if we are using daily data; so ~3000 datapoints).

## Todo
### Make the inputs more modular
Right now the system is using hard-coded inputs.  What if I want it to not have only 1 output to compare itself to ("+5close"), but to 2?

What if I want to make a system that predicts the high and low for the next week, as opposed to only the close at the end of the week (As it does right now)?  Etc., etc.

The basic point is that there are a lot of things I could try doing with this.  I need to figure out a way to make this more modular.  I have to get rid of the hard-coding.
### Change the rate of mutation in the GA
Currently I have the GA entering only 1 mutation per generation.  This was because I just wanted the functionality there.  I should change this so that a certain proportion of the population gets mutated, and do the same with the crossover rate.

This is, however, a cosmetic change in the program since it already nearly gets to 70% directional accuracy.

## Choice of name
Pleasance is the home planet of my favourite sci-fi character, Peace Corben.
This is a part of Larry Niven's Known Space universe, and Corben is introduced in "Teacher's Pet", a story by Matthew Joseph Harrington, and published in the book "Man-Kzin Wars XI" (2005).
