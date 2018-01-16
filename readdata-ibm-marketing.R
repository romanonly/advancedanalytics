library("bnlearn")
#setwd("~/R/romaprojects/BayesianNetwork/bnlearn/")

readdata_ibm_marketing <- function(nbreaks = 3)
{
  d <- read.table("datasets/WA_Fn-UseC_-Marketing-Customer-Value-Analysis.csv", 
                        header = TRUE, 
                        sep = ","
                        , as.is = FALSE)
  #setkey
  dorig = d
  
  d$Effective.To.Date2 = strptime(as.character(d$Effective.To.Date), "%m/%d/%Y")
  t0 = d$Effective.To.Date2[1]
  d$Effective.To.Date.NumDaysLeft = 60 + as.numeric( difftime(as.POSIXct(d$Effective.To.Date2), as.POSIXct(t0), units="days") )
  cols_select = c("Customer"
                  , "State"
                  , "Customer.Lifetime.Value"
                  , "Response"
                  , "Coverage"
                  , "Education"
                  , "EmploymentStatus"
                  , "Gender"
                  , "Income"
                  #, "Location.Code"
                  , "Marital.Status"
                  , "Monthly.Premium.Auto"
                  , "Months.Since.Last.Claim"
                  , "Months.Since.Policy.Inception"
                  , "Number.of.Open.Complaints"
                  , "Number.of.Policies"
                  , "Policy.Type"
                  , "Policy"
                  , "Renew.Offer.Type" #             : Factor w/ 4 levels "Offer1","Offer2",."
                  , "Sales.Channel"
                  , "Total.Claim.Amount"
                  , "Vehicle.Class"
                  , "Vehicle.Size"
                  , "Effective.To.Date.NumDaysLeft"
  
  ) 
  #d <- d[ cols_select ]
  cols_remove = names(d) %in% c("Customer", "Effective.To.Date", "Effective.To.Date2"
                                #, "Customer.Lifetime.Value" #These variables have zero variances: CLfV
                                #, "Total.Claim.Amount" #These variables have zero variances: CLfV
                                )#"State", 
  d <- d[ !cols_remove ]
  
  d_nodes = names(d)
  #survey.dag = empty.graph(nodes = d_nodes)
  
  d$Months.Since.Last.Claim <- as.numeric(d$Months.Since.Last.Claim)
  d$Months.Since.Policy.Inception <- as.numeric(d$Months.Since.Policy.Inception)
  d$Monthly.Premium.Auto = as.numeric(d$Monthly.Premium.Auto)
  d$Income = as.numeric(d$Income)
  d$Number.of.Open.Complaints = as.numeric(d$Number.of.Open.Complaints)
  d$Number.of.Policies = as.numeric(d$Number.of.Policies)
  
  colnames(d)[colnames(d) == 'Customer.Lifetime.Value'] <- 'i1.CLfV'
  colnames(d)[colnames(d) == 'Total.Claim.Amount'] <- 'i1.TCA'  
  colnames(d)[colnames(d) == 'Coverage'] <- 'i6.Cov' #CovPremium
  colnames(d)[colnames(d) == 'Education'] <- 'i6.Ed' #EDCollege EdHigh School or Below EdMaster
  colnames(d)[colnames(d) == 'EmploymentStatus'] <- 'i3.ES' #ES-Retired
  colnames(d)[colnames(d) == 'Income'] <- 'i1.Inc'
  colnames(d)[colnames(d) == 'Marital.Status'] <- 'i6.MS' #MSSingle MSMarried
  colnames(d)[colnames(d) == 'Number.of.Open.Complaints'] <- 'i5.NoOC'
  colnames(d)[colnames(d) == 'Number.of.Policies'] <- 'i3.NoP'
  colnames(d)[colnames(d) == 'Policy.Type'] <- 'PoT'
  colnames(d)[colnames(d) == 'Policy'] <- 'Po'
  colnames(d)[colnames(d) == 'Renew.Offer.Type'] <- 'i4.ROT' #ROTOffer3 ROTOffer4
  colnames(d)[colnames(d) == 'Sales.Channel'] <- 'i6.SC' #SCBranch SCCall Center SCWeb
  colnames(d)[colnames(d) == 'Vehicle.Class'] <- 'i6.VC' #VCSUV VCTwo-Door Car VCSports Car
  colnames(d)[colnames(d) == 'Effective.To.Date.NumDaysLeft'] <- 'i1.EDnDL'
  colnames(d)[colnames(d) == 'State'] <- 'State'
  colnames(d)[colnames(d) == 'Months.Since.Last.Claim'] <- 'i3.MoLaClm'
  colnames(d)[colnames(d) == 'Months.Since.Policy.Inception'] <- 'i1.MoPolInc'
  colnames(d)[colnames(d) == 'Monthly.Premium.Auto'] <- 'i3.MpPreAu'
  colnames(d)[colnames(d) == 'Gender'] <- 'i6.Gr' #GrM
  colnames(d)[colnames(d) == 'Vehicle.Size'] <- 'VehcSz'
  
  d$Response = as.factor( as.numeric(d$Response == 'Yes') )
  
  d_discretize = discretize(d, breaks = nbreaks, method = "interval") ##quantile
  
  #Basically does its best to reflect the dependence structure of the original data
  #d_discretize$EDnDL<-as.factor(discretize(data.frame(d$EDnDL), method = "hartemink", breaks = 3, 
  #                         ibreaks= 30, idisc= "quantile"))
  'method = a character string, either interval for interval discretization, quantile for quantile discretization 
  (the default) or hartemink for Harteminks pairwise mutual information method.'  

  #Basically does its best to reflect the dependence structure of the original data
  
  d_discretize3 = data.frame("i3.MpPreAu" = d$i3.MpPreAu)
  d_discretize_fc = data.frame("State" = d$State)
  #d_discretize_fc = data.frame("i1.Inc" = d_discretize$i1.Inc)
  
  for (nm in names(d)) { 
    if (!is.factor(d[, nm])) { 
      print ( nm ) #paste( " numeric:", nm))
      d_discretize3$nm = d[,nm]
      #colnames(d_discretize3)[colnames(d_discretize3) == 'nm'] <- nm
      #d_discretize3.rename(index=str, columns={"nm": nm})
      names(d_discretize3) <- sub("^nm$", nm, names(d_discretize3))
    }
    d_discretize_fc$nm = d[,nm] # cbind(x,State=c("NY","FL"))
    #colnames(d_discretize2)[colnames(d_discretize2) == 'nm'] <- nm
    #d_discretize2.rename(index=str, columns={"nm": nm})
    names(d_discretize_fc) <- sub("^nm$", nm, names(d_discretize_fc))
  }
  #Error in complete.cases(dt) : invalid 'type' (list) of argument
  #I converted the data.frame of lists into an actual data frame using the following command:
  #DF <- data.frame(matrix(unlist(DF), nrow=nrow(DF)),stringsAsFactors=FALSE)
  ret = complete.cases(d_discretize3)
  
  'unable to discretize i1.Inc in 30 intervals, some quantiles are not unique.
  cols_remove = names(d_discretize3) %in% c("i1.Inc")
  d_discretize3 = d_discretize3[! cols_remove]
  d_discretize_fc$i1.Inc = d_discretize$i1.Inc
  
  d_discretize3<-discretize(d_discretize3, method = "hartemink", breaks = 3,  ibreaks= 30, idisc= "quantile")
  
  d_discretize_fc = rbind(d_discretize3, d_discretize_fc)
  '
  
  return (list(data=d, data_discrete = d_discretize, data_discrete_hartemink = d_discretize_fc))
}
  
'
setwd("~/RstudioProjects/2017/2017/Bayesian/bak")
setwd("./")
d = readdata_ibm_marketing() 
'