# NewsClassifier
News Classifier - Project for 732A92: Text Mining

## About

The code in this repository is part of a project aimed at developing a system for the automated classification of German language news articles. It is based on a shallow convolutional neural network using fastText word embeddings. The project has been submitted as the final assignment of the course 732A92: Text Mining (HT 2018) at Linköping University, Sweden. All code provided in this repository is strictly experimental and should be used for scientific purposes only.

## Requirements
**Classifier**
* Python 3
* Python libraries: Keras, TensorFlow, spaCy, scikit-learn, nltk, pandas, textblob, TkInter
* fastText's pre-trained 300 dimensional word embeddings
  * Download [here](https://fasttext.cc/docs/en/pretrained-vectors.html)

**Webscraping script**
* Working installation of R
* Libraries rvest and R.utils
* A stable internet connection

## Preparations
1. Clone or download this repository
2. [Download](https://fasttext.cc/docs/en/pretrained-vectors.html) word embeddings for German language
3. Create a new directory named "embeddings", unpack word embeddings and move the .vec-file in the new folder
4. Run webscraping scripe
5. Run News Classifier
