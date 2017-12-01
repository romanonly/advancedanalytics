# Data Quality as Modeling uncertainty

Data quality for modeling: what modeling metrics should I look at?

### Getting Started, Prerequisites and Installing

Install R/Rstudio, R modeling packages, such as caret, download repository and run script1. 

## Introduction

### Expirement
I produce three datasets by decimating the minority data points from the original dataset bc_data. The three datasets are unbalanced 20%, 10% and 5%. Let's se which metric is able to capture best, medium and worst datasets.
I also apply six classification models: logistic regression, random forest, and random forest with unbalancedness correction upsampling, downsampling, ROSE, SMOTE. 
I will apply all six models for the three data sets and plot modeling metrics: gain, AUC/ROC, resamples, callibration.

### Gain mettric 
```
Gain metric is widely popular in marketing as it allows to estimate of lift of selecting minority datapoints proposed by modeling vs at random choice. 
```
I plot four gain curves (GLM, Oversampling, Undersampling and SMOTE) for each of the three decimated data sets. 
Same models with the same modeling parameters for three different datasets. 

Which dataset would you pick?

Visual inspection of gain metric produced by four models suggests that the worst dataset is **Left**, and the best dataset is **Right**. 
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456106-600b7654-d5ec-11e7-9a99-30b2c633ef81.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456123-6ead8652-d5ec-11e7-8fc4-635682319cf6.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456137-780a942e-d5ec-11e7-8367-9548d57609fd.jpg" width="270"/>  
</p>

### ROC mettric 
```
ROIC metric in R uses: TBD. 
```
I plot ROC curve for SMOTE classification for training and testing (no overfitting observed) for each of the three decimated data sets. 
Same model with the same modeling parameters for three different datasets. 

Which dataset would you pick?

Visual inspection of the ROC metric suggests that the worst dataset is **LEFT**. Others (**Right** and **Center**) perform slightly and equally better.
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33498143-4fa07d7a-d69e-11e7-992a-d9d225e42c0d.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33498134-49c81c1e-d69e-11e7-9a2e-d98406124ff6.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33498108-3bee96c2-d69e-11e7-9c3c-49aee0c1b700.jpg" width="270"/>  
</p>

### Data uncerttainty 
Resamoling existing pre-trained models can give some initial iglimse into intrinsic uncertainty. 
```
resamples = resampling(models_list)
```
And plot
```
TBD ggplot(resamples)
```
Here are reamplied classifications by the six pre-trained models evaluated with ROC, Sensitivity and Specificity.  

Same model with the same modeling parameters for three different datasets. 

Which dataset would you pick?

Now we start seeing that **LEFT** is apparently oiur worst dataset, as it has highest uncertainty (widest resampled classification). At least for GLM and RandomForest original without data massaging. **Right** has lowest uncrertinty and also slightly higher scores.
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456177-9a7648fa-d5ec-11e7-91f8-ca348ba0fbe3.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456185-9f5cbbd8-d5ec-11e7-896f-d01f52665c3e.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456190-a3cfd420-d5ec-11e7-8c21-6c39932d94f0.jpg" width="270"/>  
</p>

### Data uncerttainty with calibration  
Calibration TBD
```
calibration in R TBD
```
Here are calibration curves produced by the six models for three different datasets. 

Which dataset would you pick now?

**Left** is apparently our best dataset: its zero-percent and one hundreed-percent predictions have the perfect scores. 
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456199-a9527538-d5ec-11e7-87f5-975e3dc65c68.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456201-ac73d6e4-d5ec-11e7-9031-b694caea5e4d.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456206-b1a5d5ae-d5ec-11e7-8c8a-24ae7b62d3fa.jpg" width="270"/>  
</p>

### The three evaluated datasets:
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33499160-d64b103a-d6a1-11e7-8b12-93a8a976b035.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33499161-db8cf306-d6a1-11e7-9fb0-0c0aa3fd183d.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33499165-e0d99918-d6a1-11e7-823f-b4ae348b0956.jpg" width="270"/>  
</p>
