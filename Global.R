library(dplyr)
library(stringr)
library(tidyverse)
library(tidyr)
library(dlookr)
library(table1)
library(alphaOutlier)
library(boot) 
library(htmlTable)
library(scales)
library(factoextra)
library(lubridate)
library(gtsummary)
library(tm)
library(tidytext)
library(data.table)
library(wordcloud2) 

library(knitr)
library(broom)
library(devtools)
library(gtsummary)
library(tidyverse)
library(tidyr)
library(gtsummary)

# Core Tidyverse
library(glue)
library(forcats)

# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)

# Visualization
library(cowplot)

# Preprocessing
library(recipes)

# Sampling / Accuracy
library(rsample)
library(yardstick) 

# Modeling
library(keras)

## Information gain

#remotes::install_github("davidsjoberg/ggsankey")
library(ggsankey)

#library(FSelector)
#library(FSelectorRcpp)

## Process Mining

library(bupaR)

library(FeatureHashing)

# install.packages("stevetemplates")
## https://libscie.github.io/rmarkdown-workshop/handout.html

library(PCAtools)
library(ggplot2)

library(gbm)
library(caret)
library(glmnet)
set.seed(123)
library(parsnip)
library(recipes)
library(workflows)
library(tune)
library(ranger)
library(vip)
library(xgboost)
library(kknn) #Knn model
library(keras) #ANN
library(workflows)
library(caret)

keyword<- c("stemi","acute coronary syndrome","angina","tachycardia","aortic aneurysm","pericardi","ortic dissection",
            "coronary artery dissection","cardiomyopathy","heart failure","mitral valve disease","mitral stenosis",
            "coronary artery disease","chf","congestive heart failure","heart failure","telemetry","myocardial infaction",
            "cardiac arrest","myocardial infarction","aortic stenosis","st elevated","pericardial effusion", "cardiomyopathy",
            "cath lab","tamponade","tamponede")

## List of the Meds:
medList<- c("aspirin","morphine","nitroglycerine","nitroglycerin","heparin")
P2y12<-c("clopidogrel","prasugrel","ticlopidine","ticagrelor")
HMGCoA<-c("altoprev","amlodipine","atorvastatin","caduet",
          "crestor","ezallor","fluvastatin","lescol","lipitor","livalo","lovastatin",
          "mevacor","pitavastatin","pravachol","pravastatin","rosuvastatin","simcor",
          "simvastatin","simvastatin","ezetimibe","simvastatin","niacin","vytorin","zocor","zypitamag")
ACE<-c("benazepril", "captopril", "enalapril","enalaprilat", 
       "fosinopril", "lisinopril", "moexipril", "perindopril", "quinapril", "ramipril","trandolapril")
betaBlockers<-c("acebutolol","atenolol","betaxolol","bisoprolol",
                "carteolol","carvedilol","labetalol","metoprolol","nadolol",
                "nebivolol","penbutolol",
                "pindolol","propanolol","sotalol","timolol")

glycoproteinInhibitors<-c("abciximab","eptifibatide","tirofiban","roxifiban","orbofiban")

subset<- c(medList, P2y12,HMGCoA,ACE,betaBlockers,glycoproteinInhibitors)



# git add . && git commit -am "Report" && git push 
