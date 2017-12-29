## An Insight into Data Quality: More Data Equals More Work, But Not Necessarily a Better Model

### Getting Started, Prerequisites and Installing

Install R/Rstudio, R modeling packages, download repository and run script1. 

## Introduction
In the field of data quality and predictive modeling, which modeling metrics are best at “telling the truth,” so to speak?

How did I come to this question? I looked at the metric that’s the most well-known and popular in machine learning, and was surprised by what I found. It showed the best result--the best accuracy---for the lowest quality dataset! After that, I used two metrics that aren’t very popular or well-known, but these two metrics were able to correctly point to the best and worst datasets.

If you’re a data scientist and analyst, in your real-life work you don’t have information about the quality of data provided to you beforehand. You just get the data in its raw form. And so, often, you need to decide--from a very wide range of available datasets--which dataset you actually want to use in your modeling and learning. But how can you know which data to choose? In other words, which data will actually improve the quality of your model? 

Here, I’m demonstrating that looking only at the metric of plain prediction accuracy can be misleading. You also want to look at other metrics that reflect the predictive uncertainty.

### Expirement
My benchmark tests involve three datasets produced by decimation from a real, well-known dataset. This process of decimation ends up damaging the original data. 

Going into this, I already know which of my three generated datasets is the best, and which is the worst. With this in mind, let's test some different model metrics out, to see how accurately they’re able to guess which are the best and worst datasets. 

I produce three datasets by decimating the minority data points from the original dataset, “bc_data.” The three datasets are unbalanced, with a minority class decimated to 20%, 10%, and 5%. As I said, we’re going to try and see which metric is able to correctly identify the best, middle-range, and worst datasets. I also deploy six classification models: 
1) Logistic regression
2) Random forest
3) Random forest using upsampling (to learn/model unbalanced data)
4) Downsampling
5) ROSE
6) SMOTE

I will apply all six models to the three datasets, and for the following modeling metrics: gain, AUC/ROC, resamples, and calibration.

### Gain mettric 

Gain metric is popular in the field of marketing because it allows for the estimation of a lift of selecting data points proposed by modeling vs. at random. Let’s say that you use no modeling and know that, historically, you have a 10% response. You can expect that by contacting/emailing/speaking face-to-face with 100 of your clients (selected at random) you will receive a positive response from 10 people. The gain metric tells you that, using a a predictive model, of the first 10 people you will score, say, 6; of the next 10 people you will score 2 more; and of the next 10, another 2. 

So what does this mean for you? You don’t need to call or seek out a face-to-face with all of these 100 people. In real life, this 100 actually represents 100,000 people, because real marketing companies work with hundreds of thousands of people. Realistically, we need a model that means we have to speak to fewer people, because marketing companies can’t afford to call/meet with each person to make a sale/break a deal/sign a contract/etc. In other words, marketing companies need a model that helps them identify the group (of, say, 1,000) with the highest potential for closing a deal.

However, as simple as this may sound, only looking at this particular gain metric is a problem—see the examples below based on real-life data.

I plot four gain curves (GLM, Oversampling, Undersampling, and SMOTE) for each of the three decimated datasets. These are the same models, with the same modeling parameters, for three different datasets. 

Visual inspection of the gain metric produced by the four models suggests that the worst dataset is **Left**, and the best dataset is **Right**.

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456106-600b7654-d5ec-11e7-9a99-30b2c633ef81.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456123-6ead8652-d5ec-11e7-8fc4-635682319cf6.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456137-780a942e-d5ec-11e7-8367-9548d57609fd.jpg" width="270"/>  
</p>

### ROC mettric 

I plot the ROC curve for SMOTE classification for training and testing (with no overfitting observed) for each of the three decimated datasets. These are the same models with the same modeling parameters for three different datasets. 

Visual inspection of the ROC metric suggests that the worst dataset is on the **Left**. The other two (**Right** and **Center**) perform equally well or slightly better.

This popular and widely-used metric tells us that the greatest accuracy would be obtained using the data set on the right, but we know that this is, in fact, the WORST data set. That means that when we deploy such a model, we’ll have a very different accuracy level.
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33498143-4fa07d7a-d69e-11e7-992a-d9d225e42c0d.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33498134-49c81c1e-d69e-11e7-9a2e-d98406124ff6.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33498108-3bee96c2-d69e-11e7-9c3c-49aee0c1b700.jpg" width="270"/>  
</p>

### Data uncerttainty 
Resampling trained models can offer an initial glimpse into intrinsic uncertainty. 

Here are resampled classifications by the six pre‑trained models evaluated with ROC, Sensitivity, and Specificity. These are the same models with the same modeling parameters for three different datasets. 

Here, we see that **Left** is apparently the best dataset, as it has lowest uncertainty (narrow resampled classifications) for GLM and Original RandomForest. The **Right** has the highest uncertainty, as well as slightly lower scores.

Finally, one of them tells us the true story of the data. Yes—the **Left** model is obtained via learning the best data (with the most information about the minority class).
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456177-9a7648fa-d5ec-11e7-91f8-ca348ba0fbe3.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456185-9f5cbbd8-d5ec-11e7-896f-d01f52665c3e.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456190-a3cfd420-d5ec-11e7-8c21-6c39932d94f0.jpg" width="270"/>  
</p>

### Data uncerttainty with calibration  

Here are the calibration curves produced by the six models for the three different datasets. 
Roughly speaking, with the calibration graph you want these intervals be narrow and close to the ideal prediction straight line.

Once again, **Left** is the best dataset: notice how its 0 percent and 100 percent predictions have perfect scores.

How can we tell that the **Left** calibration is better? First, look at the “0 percent” guesses—they correspond to the actual majority class. Similarly, in the case of the “100 percent” guesses, when this model says “100% probability” it’s almost always correct. And now, compare this to the other two **Center** and **Right** models. Their “0 percent” and “100 percent” are not as accurate anymore.
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33456199-a9527538-d5ec-11e7-87f5-975e3dc65c68.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33456201-ac73d6e4-d5ec-11e7-9031-b694caea5e4d.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33456206-b1a5d5ae-d5ec-11e7-8c8a-24ae7b62d3fa.jpg" width="270"/>  
</p>


### The three evaluated datasets

Finally, these are the three unbalanced datasets. Left is the least unbalanced, because low uncertainty models trained using the Left dataset will produce superior predictions both in deployment and for test sets.    
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/33499160-d64b103a-d6a1-11e7-8b12-93a8a976b035.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/33499161-db8cf306-d6a1-11e7-9fb0-0c0aa3fd183d.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/33499165-e0d99918-d6a1-11e7-823f-b4ae348b0956.jpg" width="270"/>  
</p>


## To conclude: my advice

Here’s how I suggest that you take advantage of this work. First, always maintain all the metrics when working on your models, from day one. Any new datasets you want to add to your models should show a combined improvement in respect to multiple metrics, not just one. For instance, if you are suggested using not just the last year of historical records, but many years of data from many different marketing campaigns, now you can see quantitatively if these two years can actually improve the one year.

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Authors

* **Roman Kazinnik** - *Initial work* - [PurpleBooth](https://github.com/PurpleBooth)

## Acknowledgments

* R opensource statistical packages 
