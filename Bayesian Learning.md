## Who is realy important? Features importance by Naive-Bayse Learning (and its Supervised Machine learning peers/sisters/analogues) - can be really NOT important! [Non-Naive] Bayesian explanation

## Introduction and Claims

Here is a comparitive analsysis for a very important question in Machine Learning: importance of Variables.

Often we learn classification model (be it Random Forest, Logistic-regression, Naive-Bayes) and in addition to 
its prediction accuracy we also ask for features priorities/ranking. More important features we may decide to use for 
generating new features, least important we discard. We also want such list to match to some extent to our 'real-life' / 'gut-feeling' expperiece. For example, for predicting Income we expect to be important  Education and Location type (city vs suburb vs rural).

herein I show that oftenly forgottern assumption of Naive-Bayes (and its Suoervised Machine Learning brothers/analogues/variants such as Logisrtic regression and Random Forest) - of the "features independence" turns its "variables importance" into errorenoeus/misleading! It is true for datasets which invalidate such features independence assumption - which is probably most if not all real-life data!


I compare Bayesian Learning (BL) vs. a set of Supervised Machine Learning (ML) models: (1) logistic regression (2) random Forest
and (3) Random Forest with 3 unbalanced data optimization Up-sampling, Down-sampling, and SMOTE.

For more information about how I produce these Figures/results see the link (TBD).

Both BL and ML tell variables importance, in additon to classification. BL is able to infere causes which is also called as Bayeian Inference.

My main conclusion is - for the features importance look not at Supervised ML but at Bayesian Learning!



## Keywords and Terms

In my analysis I will use multiple classification prediction accuracy metrics: Area Under Curve (AUC), Gain metric, ROC - Sensitivity - Specificity , and also Learning Uncertainty (from Resampling) and Confidence intervals (from Model Calibration).

## Dataset 

Publicly available email marketing campaign dataset from IBM (ref (TBD)). 
Typical B2B model with information about clients. Clients are contacted to purchase auto policy. 
We want to estimate clients **Response** from some 23 features (below) which of course have different predictability.
The only parameter marketing manager can really change s the time when to contact the person: it is dictated by
the **Effective To Date** feature: should we contact the client right before the expiration, or we well in advance?

As my analysis below shows, Supervised Machine LAearning uses the Naive-Bayes "features independence" assumption - and may draw suggest conclusion that this (Effective to Date)  feature is important. 

Now, Bayseian Analysis shows this feature is absolutely UNIMPORTANT and Response is independent of this feature.
What I sugget is trust to Bayseian Learning.

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34472259-c64c011a-ef2b-11e7-9abe-c8c16c25078e.png" width="900"/>
</p>

## Expirements
Here is how I  validate/prove/confirm/explain my claim. Both BL and ML tell us features importance. 

In 1st expirement, I remove from the original dataset the features reported as important by Supervised ML - and reported as
NON-mportant by Bayesian Learning. I will show that the ability for classification prediction doesn't change, that means 
what we removed from our data was not really important.

In 2nd expirement On the other hand,   I remove  features reported as important by Bayesian Learning - and reported as
NOT important by ML. Classification accuracy for the new dataset fails dramatically. That is the indication that the removed features were  really important. 

I show in this work that although Supervised ML provides ranking for the features importance, it turns into meaningless/low pratcial value/misleading when the dataset in question is far from the assumption of the features independence.
In this work I show that Bayesian Learning features ranking should be more accurate.

I prodice a new datasey by removing from the original dataset the variables that Bayesian learning reported as unimportant. 
Notice that some of these fatures that I removed - they are actually in the top of the importance list as reported by  Random Forest learning! here they are: (TBD)

The 2nd dataset I produce by decimating the original one by removing least important features based on ML here they are: (TBD). These features are important based on the Bayesian Graph because they have links to the **Response** and located very  close. 

## Bayesian Networks
**Left** - Bayseian network obtained by averaged bootstrpap of 150 HC (Hill-Climb) models
and 
**Right** Naive Bayes. 

Naive Bayes assumes that all the features are independent and only operates with pairwise 
correlations between the target and the feature. 

Bayesian network learning tries to find the best graph that minimizaed some quanitative loss metruic such as maximum likelyhood. Finding a graph is NP-Complete problem, therefore various optimization strategies were developed 
which in general produce a non unique solution for some local minima.

Look at the **Left** graph. Notice that many recovered dependecies actually make a lot of sense. 
For example: person's *Location.Code* (city, suburb, rural) influences (or is influenced by) *Edication (ED)*, *Income (Inc)*, *Vehicle Size*, *TotalClaimAmount TCA*. 
 
Some of it doesn't makes much sense: we can see the person's *Gender Gr* is also linked the *Location.Code*.
However, the two-edge link from *Gender* to *Income* (via *Location.Code*) can be observed in real life.  

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646280-a6e82056-f331-11e7-82ca-683501be0acf.jpg" width="400"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646286-c7b2a23e-f331-11e7-905d-8e0e77eb627c.jpg" width="450"/>
</p>

## Bayesian Networks: classification accurcay

Classification is just one aspect of Bayesian Learning - BL used for inference for causality, variables/features independece evaluation. 
Although the main reason we would make  Bayesian graph is for **Bayesian Inference**, we can also look at 
the graph's **classification** accuracy.

Below I show three classsification acuracies (plotted as Gain metric): **Left** is Bayesian, **Center** is Naive-Bayes,
and **Right** is logistic regression - all of course I applied to exactly same dataset. (I needed to quantize 
the original data set, which introduced a small quantization error).

The results are consistently similar, which provides another glimse into the variables 
independence assumption - of Naive-Bayse and Logistics Regression: such assumption doesn't 
really influence the classification prediction accuracy - and therefore is oftenly forgotten. But, again, this assumption is crucial when evaluatinng the variables importance!

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646287-cffec17a-f331-11e7-8996-df412b09a7ca.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646289-df0a1110-f331-11e7-900b-fb8201a5236a.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647063-a5020be4-f345-11e7-8eac-370757e312d3.jpg" width="270"/>  
</p>



## Features: important by ML vs important by Bayesian Learning.

# Most important variables in Random Forest learning  of the original (quantized) dataset.
Here is the list of the mostimportant features from the Random Forest learning: TBD

## New decimated dataset 1: 

I remove  variables that outside of the *Markov blanket* of the target, as well as featyres far away/distanced from the target which are insides the *Markov blanket*. 

Therefore, in my first expirement I decimate the original dataset by removing independent by Bayesian Learning fatures from the original dataset. Notice that I remove the features that presumably are "important" by the Random Forest learning.
here is the list of features: TBD

## New decimated dataset 2: 

I my secoond expirement I remove the features that close to the target in the BL graph, and also "unimportant" by the supervised Random Forest learning. Here they are (TBD)


## Final Expirement: accuracy and confidence comparison

My expirement is to decimate the original dataset and see how well the new decimated dataset can be used for classification. 

If the classification accuracy (I use several metrics) doesn't change  that means the removed features are happpened to be NOT important.

If the classification accuracy (I use several metrics) fails -  that means the removed features are happpened to be  important.

All the comparisons for the three datasets (original all feaures, expirement-1, expirement-2) are below/the end of this article. 

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

Top raw shows the original dataset and how well if can be utilized for classification learning. 

The second raw is 1st dataset (I removed independent features as reported/shown/told by Bayesian analysis).

The third raw is 2nd dataset (I removed dependent features as reported/shown/told by Bayesian analysis).


Notice the extremely small loss of classification prediction of the 1st decimated dateset vs the original dataset.
One can see some very extremely small loss in accuracy and confidence for the decimated set.
And these are the top features as reported by Supervised Machine Learning - that I removed!

For 2nd expirement (bottom raw) - the dataset was damaged very badly and can not be utilized to classification/its classification is very bad. Notice that I removed LEAST important variables/features as reported/based on/following Supervised ML!

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34651615-93804470-f3a0-11e7-9544-d7410138a5f2.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34651624-a3c9ff06-f3a0-11e7-9c53-ee3cf2099b9c.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34651622-a0ccbcb2-f3a0-11e7-94da-b9a3c1443d05.jpg" width="270"/>  
</p>
expirement-1
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34651642-d08f5bb2-f3a0-11e7-874c-efc933b39c37.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34651646-d5791f32-f3a0-11e7-86a9-258747afd332.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34651644-d3c16406-f3a0-11e7-81c4-936a2df1f5ab.jpg" width="270"/>  
</p>
expirement-2
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34651668-f61805e6-f3a0-11e7-9c77-91af1aada846.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34651672-fcf5dfbe-f3a0-11e7-8b10-13c74324089c.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34651671-fb0fb2c4-f3a0-11e7-8533-bc735a9e858e.jpg" width="270"/>  
</p>

<!---
Non-discretized dataset: original vs decimated1 vs decimated-2
It is not the best way to ompare - Bayesian L can see only discretized variables!
<p align="center">
  <img src="" width="270"/>
  <img src="" width="270"/>  
  <img src="" width="270"/>  
</p>
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
-->

## Comparison of preidction for three datasets: 
**Left**: original dataset
**Center** - decimated-1
**Right** - decimated-2

I show three plots of Random Forest with Up-sampling applied to three data sets.

Area Under the  Curve (AUC) shows same quality of classification prediction for the original and decimated-1 dataset. Decimated-2 looks a much worse and even Over-Fitting for same number of the Trees in Random Forest learning.

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34651632-b285499c-f3a0-11e7-9ed8-fca4326b9c39.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34651650-dafb330a-f3a0-11e7-9944-c1ef27205acf.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34651675-0305d7d8-f3a1-11e7-9a9b-6e3e204ae0f3.jpg" width="270"/>  
</p>
<!---
Non-discretized dataset: original vs decimated1 vs decimated-2
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646607-afa40c42-f33a-11e7-9bea-3f79f10a7457.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646825-4c63d15c-f340-11e7-8f03-aaca5a11f1f0.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647442-d574bb90-f350-11e7-9bbf-0b71923d152a.jpg" width="270"/>  
</p>
-->

Confidence Intervals from Calibration: 
**Left**: original dataset
**Center** - decimated-1
**Right** - decimated-2

Hard to notice any difference **Left** and **Center**. Perhaps very insignificant/small/week loss of quality for the decimated dataset in the 3rd Bin, but its 2nd Bin actually looks closet to the main diagonal. 
**Right**: confidence intervals are much worse as they moved from the main diaonal significantly!
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34651626-a84fb354-f3a0-11e7-9457-108274b285fc.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34651648-d768d1de-f3a0-11e7-8c0f-9255ddac8939.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34651673-fe605f96-f3a0-11e7-92c7-04d9ba83c514.jpg" width="270"/>  
</p>
<!---
Non-discretized dataset: original vs decimated1 vs decimated-2
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34646601-7196e10e-f33a-11e7-8fb7-f698bbaf34ef.jpg" width="270" height="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34646820-3ed6aa6e-f340-11e7-9294-73e99594feaf.jpg" width="270" height="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647434-bdd7f01a-f350-11e7-9ca7-60618f11dc98.jpg" width="270" height="270"/>  
</p>
-->

## Logistic regression AUC metric 
**Left**: original dataset
**Center** - decimated-1
**Right** - decimated-2

**Left** and **Center** - look almost identical.
**Right** - there is NONE prediction by logistic regression for dataset-2. Logistic Regression learning failed completely in classification as the Gain plot is as good as just any random prediction!

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34651635-b72dc3ac-f3a0-11e7-9abc-8f3f122e3f05.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34651651-dde5841c-f3a0-11e7-8e84-0b5f020ecc4c.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34651678-06e6579c-f3a1-11e7-81ab-a39ea2dfcede.jpg" width="270"/>  
</p>
<!---
Non-discretized dataset: original vs decimated1 vs decimated-2
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34647452-1bfd91ea-f351-11e7-91ad-1e3df4f9690a.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34647451-0efccbdc-f351-11e7-9a92-ea9bac419916.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34647449-fc3ec07c-f350-11e7-995e-104648bac7cb.jpg" width="270"/>  
</p>
-->
