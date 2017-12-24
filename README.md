# Data Quality and Prediction Model uncertainty

Data quality and prediction modeling: what modeling metrics are telling the truth?

### Getting Started, Prerequisites and Installing

Install R/Rstudio, R modeling packages, download repository and run script1. 

## Introduction
The problem of model quality relates essentially to intrinsic data uncertaintly. How trustful if the modeling result? depends on How good is the provided for modeling data? Should I invest in purchasing and processing more data or the data already available just right?
Surprisingly not all modeling metrics are equal, and some of the metrics can actually be misleading. Because any model accuracy has its unceratinty! 

### Expirement
For my benchmark tests I use three data sets produced by decimation from a well known real data set. 
Such decimation essentially damages the original data, and I know which of my three generated  data sets is best and worst.
Let's see how different model metrics are good at guessing the best and worst date set.

I produce three datasets by decimating the minority data points from the original dataset bc_data. The three datasets are unbalanced with a minority class decimated to 20%, 10% and 5%. Let's try and see which metric is able to identify correctly best, medium and worst datasets.
I also deploy six classification models: logistic regression, random forest, and random forest with unbalancedness correction upsampling, downsampling, ROSE, SMOTE. 
I will apply all six models for the three data sets and the following modeling metrics: gain, AUC/ROC, resamples, callibration.

### Gain mettric 
```
Gain metric is widely popular in marketing as it allows to estimate a lift of selecting datapoints proposed by modeling vs. at random choice. 
```
I plot four gain curves (GLM, Oversampling, Undersampling and SMOTE) for each of the three decimated data sets. 
Same models with the same modeling parameters for three different datasets. 

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

Visual inspection of the ROC metric suggests that the worst dataset is **LEFT**. Others (**Right** and **Center**) perform slightly and equally better.
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33498143-4fa07d7a-d69e-11e7-992a-d9d225e42c0d.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33498134-49c81c1e-d69e-11e7-9a2e-d98406124ff6.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33498108-3bee96c2-d69e-11e7-9c3c-49aee0c1b700.jpg" width="270"/>  
</p>

### Data uncerttainty 
Resamoling trained models can give some initial glimse into intrinsic uncertainty. 

Here are resamplied classifications by the six pre-trained models evaluated with ROC, Sensitivity and Specificity.  
Same model with the same modeling parameters for three different datasets. 

Now we start seeing that **LEFT** is apparently the best dataset, as it has lowest uncertainty (short resampled classifications) for GLM and Ooriginal RandomForest . **Right** has highest uncrertinty and also slightly lower scores.
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

Again, **Left** is shown to be the best dataset: notice how its zero-percent and one hundreed-percent predictions have the perfect scores. 
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456199-a9527538-d5ec-11e7-87f5-975e3dc65c68.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456201-ac73d6e4-d5ec-11e7-9031-b694caea5e4d.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456206-b1a5d5ae-d5ec-11e7-8c8a-24ae7b62d3fa.jpg" width="270"/>  
</p>


### The three evaluated datasets
Here are the three unbalanced data sets. **Left** is the least unbalanced, due to low uncertainty models trained using **Left** dataset will produce superior predictions in deployment or for test sets.    
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33499160-d64b103a-d6a1-11e7-8b12-93a8a976b035.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33499161-db8cf306-d6a1-11e7-9fb0-0c0aa3fd183d.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33499165-e0d99918-d6a1-11e7-823f-b4ae348b0956.jpg" width="270"/>  
</p>


## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Authors

* **Roman Kazinnik** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

## Acknowledgments

* R opensource statistical packages 
