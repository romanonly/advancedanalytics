## Most important features as reported by Supervised Machine learning can be really NOT important! Bayesian explanation

## Introduction and Claims

Here is a comparitive analsysis for a very important question: importance of Variables.

I compare Bayesian Learning vs. two methods of Supervised Machine Learning: logistic regression and random Forest. 
Both BL and ML can tell variables importance, 

I show that oftenly forgottern assumption of Logisrtic regression of the "features independence" turns its "variables importance" into errorenoeus/misleading for datasets which vialotes uch assumption (which is probably true for most real-life data).

In addition I show that Random Forest is able to recover and still produce same-quality of clasification prediction /
not susceptible to such "featrures independence" assumption. 

The main conclusion is - for the features importance problem look not at Supervised ML but at Bayesian Learning.



## Keywords and Terms

In my analysis I will use multiple classification prediction accuracy metrics: Area Under Curve (AUC), gain metric, ROC - Sensitivity - Specificity , and also Models Prediction Uncertainty as Resampling and Confidence intervals as Model Calibration.

## Dataset

Publicly available email marketing campaign dataset from IBM (ref (TBD)). 
Typical B2B model with information about clients. Clients are contacted to purchase auto policy. 
We want to estimate clients **Response** from some 23 features (below) which of course have different predictability.
The only parameter marketing manager can really change s the time when to contact the person: it is dictated by
the **Effective To Date** feature: should we contact the client right before the expiration, or we well in advance?

As my analysis below shows, Supervised Machine LAearning uses the Naive-Bayes "features independence" assumption - and may draw suggest conclusion that this (Effective to Date)  feature is important. 

Now, Bayseian Analysis shows this feature is absolutely UNIMPORTANT and Response is independent of this feature.
What I sugget is trust to Bayseian Learning.

Here I show how to make  validation/proof/confirm  of my claim: I remove the important feature as it is reported by Supervised ML (both Random Forest and Logistic Regression)  (but unimportant by Bayesian Learning) from the original dataset and show that the quality of machine learning classification doesn't change!

On the other hand,  when I remove  a few important features based on the the Bayesian Learning (but unimportant by ML/Random Forest) - Logistic regression classification accuracy fails dramatically. That is the indication that the removed feature was  really important. 


<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34472259-c64c011a-ef2b-11e7-9abe-c8c16c25078e.png" width="900"/>
</p>

## Bayesian Networks
**Left** - Bayseian network obtained by averaged bootstrpap of 150 HC (Hill-Climb) models
and 
**Right** Naive Bayes. 

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
  <img src="https://user-images.githubusercontent.com/17115347/34646280-a6e82056-f331-11e7-82ca-683501be0acf.jpg" width="400"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646286-c7b2a23e-f331-11e7-905d-8e0e77eb627c.jpg" width="450"/>
</p>

## Bayesian Networks: classification accurcay

Although the main reason we would make  Bayesian graph is for **inference**, we can also look at 
its **classification** accuracy.

Below I show three classsification acuracies (as plotted gain metric): **Left** is Bayesian, **Center** is Naive-Bayes,
and **Right** is logistic regression - all of course I applied to exactly same datase. (I needed to quantize 
the original data set, which introduced a small quantization error).

The results are consistently similar, which provides another glimse into the same variables 
independence assumption of the logistics regression.
For supervised ML classification is the only outcome. 
However, classification (and its corresponding accuracy) is just one aspect of Bayesian Learning - BL used for 
inference for causality, variables/features independece evaluation. 

Although supervised ML provides importanc ranking for the features, it may turn into meaningless/low pratcial value/misleading when the dataset in question is far from the assumption of the features independence.

In this work I show that Bayesian Learning features ranking should be more accurate.

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646287-cffec17a-f331-11e7-8996-df412b09a7ca.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646289-df0a1110-f331-11e7-900b-fb8201a5236a.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647063-a5020be4-f345-11e7-8eac-370757e312d3.jpg" width="270"/>  
</p>


## Two expirements

Now I prodice a new datasey by removing from the original dataset the variables that Bayesian learning reported as unimportant. 

Notice that some of these fatures that I removed - they are actually in the top of the importance list as reported by  Random Forest learning!

## Non-important variables by Bayesian learning
Those are the variables that outside of the Markov blanket of the target, as well as featyres most far away insides the
Narkov blanket from the target. 

Therefore, in my first expirement I decimate the original dataset by removing independent by Bayesian Learning fatures from the original dataset. Notice that I remove the features that presumably are "noon-important" by the Random Forest learning.
here is the list of features: TBD

I my secoond expirement I remove the features that close to the target, and also "unimportant" by the supervised Random Forest learning: here thet are (TBD)


The ultimate INDEPENDENT test is the follwing.
the expirement that dammages the original dataset the most- that means the removed in such expirement features are happpened to be MORE important.

## original dataset: Most important variables by Random Forest learning
Here is the list of the mostimportant features from the Random Forest learning: TBD

## New decimated dataset 1: 
I apply supervised MK to the new dataset and produce a list of most important variables from this decimated dataset.

## New decimated dataset 2: 
I apply supervised MK to the second new dataset and produce a list of most important variables from this decimated dataset.


## Final Expirement: accuracy and confidence comparison

For this two datasets, which happened to be unblanced, I apply a set of supervised machine learning methods. 
Specifically, I employ Random Forest with several unbalanced correction and plot classification accuracy and 
uncertainty intervals.

For more information about how I produce them please see (TBD).

Top raw shows the new dataset after I removed independent features as produced by Bayesian analysis.
Bottom raw shows the original dataset with all the feautres.

Notice the extremely small loss of classification prediction of the decimated dateset vs the original dataset.
And I have removed the top features as reported by Supervised Machine Learning!

## Conclusion

We often forget about this "features-independence" assumption that we make in all mot popular Supervised Machine Learning 
methods, from logistics regression and Naive-bayes to many variants of Random-Forest. 
We forget it because for the classification accuracy-confidence it really doesn't make any difference.
we often are happy with Naive-Bayse or Random Forest classification because most of the accuracy gains we
get from various boosting and data upsampling techniques.


However, we can't ignore this assumption when we looking into importance of features!
That is what I wanted to show in this blog.

## Comparison Figures
For each dataset (original and decimated) I apply five (5) learning models: Random Forest in 4 different variations and
Boosted Logistic Regression (see reference (TBD).

Then I plot Sensitivity-Specificity accuracy intervals from  resampling, calibration curves, and gain metric curves.
One can see some very extremely small loss in accuracy and confidence for the decimated set.

Original
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646590-3b2cf1e4-f33a-11e7-9fd0-5651136f8b50.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646600-719249e6-f33a-11e7-8c44-28da3794a088.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34646605-99c49d56-f33a-11e7-97c2-f2ea1312a5f8.jpg" width="270"/>  
</p>

Decimated-1: removed non-important by bayesian learning
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646817-345c8252-f340-11e7-970a-eabedf08daf6.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646819-39e1124c-f340-11e7-8afd-cfe631cd5248.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34646821-459fd3e8-f340-11e7-95f9-6b8fc4fa01cd.jpg" width="270"/>  
</p>

Decimated-2: removed important features by bayesian learning
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34647429-b1e252f0-f350-11e7-9727-8f931e74d1c6.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34647433-b86a6c5c-f350-11e7-9819-3730aa392cd9.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647435-c7c1add2-f350-11e7-8b8e-d5ee4fe78fe9.jpg" width="270"/>  
</p>

## Comparison of preidction for three datasets: 
**Left**: original dataset
**Center** - decimated-1
**Right** - decimated-2

Upper-sampling of the data with random Forest
Area Under th  Curve (AUC) shows same quality of classification prediction, even for the decimated-1 dataset 
a few percent better. Decimated-2 looks a bit worse:
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646607-afa40c42-f33a-11e7-9bea-3f79f10a7457.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646825-4c63d15c-f340-11e7-8f03-aaca5a11f1f0.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647442-d574bb90-f350-11e7-9bbf-0b71923d152a.jpg" width="270"/>  
</p>

Confidence Intervals from Calibration: 
**Left**: original dataset
**Center** - decimated-1
**Right** - decimated-2

Hard to notice any difference Left and Center. Perhaps very insignificant/small/week loss of quality for the decimated dataset 
**Center**: it is just a little moved away from the diagonal line in its second from the left bin.  
**Right**: confidence intervals moved from the diaonal significantly:

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646601-7196e10e-f33a-11e7-8fb7-f698bbaf34ef.jpg" width="270" height="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646820-3ed6aa6e-f340-11e7-9294-73e99594feaf.jpg" width="270" height="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647434-bdd7f01a-f350-11e7-9ca7-60618f11dc98.jpg" width="270" height="270"/>  
</p>

Logistic regression AUC metric 
Most explicit indicator. 
**Left** and
**Center** - look almost identical.
**Right** - there is ZERO prediction accuracy by logistic regression for dataset-2. 

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34647452-1bfd91ea-f351-11e7-91ad-1e3df4f9690a.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34647451-0efccbdc-f351-11e7-9a92-ea9bac419916.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647449-fc3ec07c-f350-11e7-995e-104648bac7cb.jpg" width="270"/>  
</p>
