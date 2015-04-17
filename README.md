# ideologymodeling

Building the model about the relationship between ideology and speeches

It is often considered that Republicans and Democrats tend to use different words when talking about the same topic. The model is to approach the correlation between ideology and use of words. Two models are tried.

The data is downloaded via Sunlight Foundation API. Downloading codes are adopted from Gaurav Sood athttps://gist.github.com/soodoku/85d79275c5880f67b4c.

The speeches are preprocessed using Gaurav Sood's codes at https://github.com/soodoku/Text-as-Data. Basically removed stop words, stemmed the words, removed special characters, numbers, capitalization etc.

For this repository, modeling.R is the R script for all the steps including producing the ngrams, sorting the frequencies and the modeling. topbigrams.csv lists the bigrams that have the highest chi-square. topbigramsR.csv lists the bigrams that are most frequently used by Republicans. topbigramsD.csv lists the bigrams that are most frequently used by Democrats. toptrigramsR.csv lists the trigrams that are most frequently used by Republicans. toptrigramsD.csv lists the trigrams that are most frequently used by Democrats. writup.pdf is the specific steps and results of the modeling work in text file.
