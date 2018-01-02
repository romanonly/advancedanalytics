## Bayesian Networks Learning vs. Supervised Machine Learning: some insights on Accuracy, Variables Importance, Confidence.

## Introduction
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34472259-c64c011a-ef2b-11e7-9abe-c8c16c25078e.png" width="900"/>
</p>

### Expirement

Bayesian Networks: **Left** and **Center** Averaged Bootstrpap 150 HC (Hill-Climb) models with and without whitelisted nodes:
**Right** Naive Bayes:
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34469873-dd107700-eef4-11e7-8eee-9f8afb3d49a4.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34469891-083ca94e-eef5-11e7-8b1d-cc2fb191e7e3.jpg" width="270"/>  
</p>

Gain, AUC (Area Under Curve) for Up sampling Random Forest, and Boosted Logistic regression"
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34469883-f21fabfc-eef4-11e7-8246-035b5ff32994.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34469734-3dee48ec-eef3-11e7-8ada-a4413e2ba503.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34469732-37e21d52-eef3-11e7-9c30-0b87e4cd9968.jpg" width="270"/>
</p>

### Bayseian Networks: classification accuracy with gain mettric 
<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34469874-e191c522-eef4-11e7-872d-efac8d0e3c9e.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34469885-fa865372-eef4-11e7-8e32-b53a5d5fb58c.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34469892-0b12243c-eef5-11e7-8d1f-74aefbc64ed8.jpg" width="270"/>  
</p>

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34469982-2d68cc1a-eef6-11e7-8ed1-76d21f71ccbb.jpg" width="270"/>
  <img src="
https://user-images.githubusercontent.com/17115347/34469991-4a3d3178-eef6-11e7-9cab-fb346c9eb4ca.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34469988-447d5786-eef6-11e7-9e6f-899b3740bef9.jpg" width="270"/>  
</p>



### Classification accuracy with supervised machine learning (original marketing data)

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34469705-ec3c8cc0-eef2-11e7-8ccd-d33449e88e94.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34469708-f461dca2-eef2-11e7-891f-346ad384508c.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34469724-27df1950-eef3-11e7-819e-04544786bef7.jpg" width="270"/>  
</p>
  only 20 most important variables shown (out of 51)
'                      Overall
i1.CLfV               100.000
i1.TCA                 95.903
i1.MoPolInc            90.717
i1.Inc                 89.319
i1.EDnDL               75.806
i4.ROTOffer4           71.891
i3.MpPreAu             68.844
i4.ROTOffer3           68.125
i3.MoLaClm             65.623
i3.ESRetired           59.270
i4.ROTOffer2           35.198
i6.MSSingle            25.208
i3.NoP                 23.932
Location.CodeSuburban  23.481
i6.EdMaster            18.103
i5.NoOC                17.604
i6.MSMarried           14.564
i6.SCBranch            13.030
i6.SCWeb               11.842
VehcSzSmall             9.953
'

### Remove Nodes based on Bayseian Networks: classification accuracy with supervised machine learning
'
cols_remove = names(d0) %in% c( "i1.EDnDL", "i1.MoPolInc", "i3.MoLaClm", "i5.NoOC", "i6.Ed", "i6.Gr", "VehcSz", "PoT, Po", "STATE")
'

<p align="center">
  <img src="https://user-images.githubusercontent.com/17115347/34469980-2853edea-eef6-11e7-93a4-2cca8510ea49.jpg" width="270"/>
  <img src="https://user-images.githubusercontent.com/17115347/34469982-2d68cc1a-eef6-11e7-8ed1-76d21f71ccbb.jpg" width="270"/>  
  <img src="https://user-images.githubusercontent.com/17115347/34469973-1cbd198e-eef6-11e7-96bb-316ed6454d6f.jpg" width="270"/>  
</p>
'
[1] "==== up ==== "
  only 20 most important variables shown (out of 40)

                      Overall
i1.CLfV               100.000
i1.TCA                 88.280
i1.Inc                 80.357
i3.MpPreAu             64.766
i3.ESRetired           40.837
i4.ROTOffer4           40.633
i4.ROTOffer3           39.220
i3.NoP                 23.134
i4.ROTOffer2           22.670
i6.MSSingle            15.977
i6.SCBranch            14.250
i6.MSMarried           13.972
Location.CodeSuburban  12.419
i6.SCCall Center       11.260
i6.SCWeb                8.437
i6.CovExtended          6.763
i6.VCSUV                6.236
i6.VCTwo-Door Car       6.095
i3.ESUnemployed         4.356
i6.CovPremium           3.535
'

## To conclude: my advice

### Getting Started, Prerequisites and Installing

Install R/Rstudio, R modeling packages, download repository and run script1. 

## Contributing

Please read [CONTRIBUTING.md](https://gist.github.com/PurpleBooth/b24679402957c63ec426) for details on our code of conduct, and the process for submitting pull requests to us.

## Authors

* **Roman Kazinnik** - *Initial work* - [AdvancedAnalytics](https://github.com/)

## Acknowledgments

* R opensource statistical packages 
