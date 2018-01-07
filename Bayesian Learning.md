## Most important features as reported by Supervised Machine learning can be really NOT important! Bayesian explanation

## Introduction

Here is a comparitive analsysis of Bayesian Learning vs. Supervised Machine Learning, Accuracy, Variables Importance, Confidence..

## Dataset

Publicly available email marketing campaign dataset from IBM. 

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34472259-c64c011a-ef2b-11e7-9abe-c8c16c25078e.png" width="900"/>
</p>

## Bayesian Networks
**Top** - Bayseian network obtained by averaged bootstrpap of 150 HC (Hill-Climb) models
and 
**Bottom** Naive Bayes. 

Naive Bayes assumes that all the features are independent and only operates with pairwise 
correlations between the target and the feature. 

Bayesian network learning tries to find the best graph. This is NP complete problem and the solution is not 
unique neither the absolutely ground truth.
Notice that many recovered dependecies actually make a lot of sense. 
For example:
*Location.Code* (city, suburb, rural) influences %Edication (ED)*, *Income (Inc)*, *Vehicle Size*, 
*Gender Gr*, *TotalClaimAmount TCA*. 
 For instance, we can see the *Gender* can hardly be dependent on the *Location.Code*.
However, the link from *gender* to *income* (via *Location.Code*) makes sense.  

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646280-a6e82056-f331-11e7-82ca-683501be0acf.jpg" width="900"/>
</p>
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646286-c7b2a23e-f331-11e7-905d-8e0e77eb627c.jpg" width="900"/>
</p>

## Bayesian Networks: classification accurcay

Although the main reason we would make  Bayesian graph is for **inference**, we can also look at 
its **classification** accuracy.

Below I show three classsification acuracies (as plotted gain metric): **Left** is Bayesian, **Center** is Naive-Bayes,
and **Right** is logistic regression - all of course I applied to exactly same datase. (I needed to quantize 
the original data set, which introduced a small quantization error).

The results are consistently similar, which provides another glimse into the same variables 
independence assumption of the logistics regression.
However, classification (and its corresponding accuracy) is just one aspect of Bayesian Learning. 
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646287-cffec17a-f331-11e7-8996-df412b09a7ca.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646289-df0a1110-f331-11e7-900b-fb8201a5236a.jpg" width="270"/>  
  <img src="" width="270"/>  
</p>


## Final Expirement

Now I prodice a new datasey by removing from the original dataset the variables that Bayesian learning reported as unimportant. 

Notice that some of these fatures that I removed - they are actually in the top of the importance list as reported by supervised machine learning!

## Non-important and variables by Bayesian learning

## original dataset: Most important variables by supervised machine learning

## New decimated dataset: Most important variables by supervised machine learning

## Final Expirement: accuracy and confidence comparison

For this two datasets, which happened to be unblanced, I apply a set of supervised machine learning methods. 
Specifically, I employ Random Forest with several unbalanced correction and plot classification accuracy and 
uncertainty intervals.

For more information about how I produce them please see (TBD).

Top raw shows the new dataset after I removed independent features as produced by Bayesian analysis.
Bottom raw shows the original dataset with all the feautres.

Notice the extremely small loss of classification prediction of the decimated dateset vs the original dataset.
And I have removed the top features as reported by Supervised Machine Learning!

Allsamples + Calibration1 + AllGains

Original
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646590-3b2cf1e4-f33a-11e7-9fd0-5651136f8b50.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646601-7196e10e-f33a-11e7-8fb7-f698bbaf34ef.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34646605-99c49d56-f33a-11e7-97c2-f2ea1312a5f8.jpg" width="270"/>  
</p>

Decimated
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646817-345c8252-f340-11e7-970a-eabedf08daf6.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646820-3ed6aa6e-f340-11e7-9294-73e99594feaf.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34646821-459fd3e8-f340-11e7-95f9-6b8fc4fa01cd.jpg" width="270"/>  
</p>

PlotUpper: Original vs. Decimated
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646607-afa40c42-f33a-11e7-9bea-3f79f10a7457.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646825-4c63d15c-f340-11e7-8f03-aaca5a11f1f0.jpg" width="270"/>  
</p>

## Conclusion

We often forget about this "features-independence" assumption that we make in all mot popular Supervised Machine Learning 
methods, from logistics regression and Naive-bayes to many variants of Random-Forest. 
We forget it because for the classification accuracy-confidence it really doesn't make any difference.
we often are happy with Naive-Bayse or Random Forest classification because most of the accuracy gains we
get from various boosting and data upsampling techniques.


However, we can't ignore this assumption when we looking into importance of features!
That is what I wanted to show in this blog.










