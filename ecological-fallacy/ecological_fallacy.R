#https://pubmed.ncbi.nlm.nih.gov/1104150/
#https://www.r-bloggers.com/open-data-and-ecological-fallacy/

library(tidyverse)

# country <- factor(rep(LETTERS, each=100))
# n <- length(country)
# fat_intake <- abs(rpois(n, as.numeric(country)) + rnorm(n, 0))
# mort_rate <-   
#   
# 
#   0-200
# correlation <- -0.55
# 
# mort_rate <- 0-20


# A couple of days ago, on Twitter, @alung mentioned an old post I did publish on this blog about open-data, explaining how difficult it was to get access to data in France (the post, published almost 18 months ago can be found here, in French). And  @alung was wondering if it was still that hard to access nice datasets. My first answer was that actually, people were more receptive, and I now have more people willing to share their data. And on the internet, amazing datasets can be found now very easily. For instance in France, some detailed informations can be found about qualitifications, houses and jobs, by small geographical areas, on http://www.recensement.insee.fr (thanks @coulmont for the link). And that is great for researchers (and anyone actually willing to check things by himself).
# 
# But one should be aware that those aggregate data might not be sufficient to build up econometric models, and to infere individual behaviors. Thinking that relationships observed for groups necessarily hold for individuals is a common fallacy (the so-called " ecological fallacy"). 
# 
# 
# In a popular paper, Robinson (1950) discussed "ecological inference", stressing the difference between ecological correlations (on groups) and individual correlations (see also Thorndike (1937)) He considered two aggregated quantities, per american state: the percent of the population that was foreign-born, and the percent that was literate. One dataset used in the paper was the following


# ap <- available.packages()
# "eco" %in% rownames(ap)
# 
# library(remotes)
# install_version("eco", NULL)

library(eco)
library(lgarch)
library(ggpubr)
data(forgnlit30)
# tail(forgnlit30)
# Y          X         W1          W2 ICPSR
# 43 0.076931986 0.03097168 0.06834300 0.077206504    66
# 44 0.006617641 0.11479052 0.03568792 0.002847920    67
# 45 0.006991899 0.11459207 0.04151310 0.002524065    68
# 46 0.012793782 0.18491515 0.05690731 0.002785916    71
# 47 0.007322475 0.13196654 0.03589512 0.002978594    72
# 48 0.007917342 0.18816461 0.02949187 0.002916866    73
# 
# The correlation between  foreign-born and literacy was


cor(forgnlit30$X,1-forgnlit30$Y)

# So it seems that there is a positive correlation, so a quick interpretation could be that in the 30's, amercians were iliterate, 
#but hopefully, literate immigrants got the idea to come in the US. But here, it is like in Simpson's paradox, because actually, 
#the sign should be negative, as obtained on individual studies. In the state-based-data study, 
#correlation was positive mainly because foreign-born people tend to live in states where the native-born 
#are relatively literate.
# Hence, the problem is clearly how individuals were grouped. Consider the following set of individual observations,


n=1000
r=-.5
Z=rmnorm(n,c(0,0),matrix(c(1,r,r,1),2,2))
X=Z[,1]
E=Z[,2]
Y=3+2*X+E
cor(X,Y)


#Consider now some regrouping, e.g.


I=cut(Z[,2],qnorm(seq(0,1,by=.05)))
Yg=tapply(Y,I,mean)
Xg=tapply(X,I,mean)


#Then the correlation is rather different,
 cor(Xg,Yg)

#Here we have a strong positive individual correlation, and a small (positive correlation) on grouped data, but almost anything is possible.
#Models with random coefficients have been used to make ecological inferences. But that is a long story, andI will probably come back with a more detailed post on that topic, since I am still working on this with @coulmont (following some comments by @frbonnet on his post on recent French elections on http://coulmont.com/blog/).

 
 
 
 cor(forgnlit30$X,1-forgnlit30$Y)
 
 d <- forgnlit30 %>% mutate(Y=1-Y) %>%
   rename(foreign=X, literacy=Y) 
 ggplot(d, aes(x=foreign, y=literacy)) +geom_point() + geom_smooth(method="lm")
 
 
 
 
 set.seed(12345)
 n=1000
 r=-.5
 Z=rmnorm(n,c(0,0),matrix(c(1,r,r,1),2,2))
 X=Z[,1]
 E=Z[,2]
 Y=3+2*X+E
 cor(X,Y)

 
 I=cut(Z[,2],qnorm(seq(0,1,by=.05)))
 Yg=tapply(Y,I,mean)
 Xg=tapply(X,I,mean)
 
 df <- data.frame(X,Y, Yg, Xg)
 df <- group_by()
 
 ggplot(df, aes(x=X, y=Y)) +geom_point() + geom_smooth(method="lm")
 ggplot(df, aes(x=Xg, y=Yg)) +geom_point() + geom_smooth(method="lm")
 
 
 
 r=-.5
 
 set.seed(12345)
 n=1000
 
 X= rnorm(n, 5, 1)
 Y= 3+2*X+ rnorm(n)
 E= r*Y*X
 
 Z=rmnorm(n,c(0,0),matrix(c(1,r,r,1),2,2))
 X=Z[,1]
 E=Z[,2]
 Y=3+2*X+E
 cor(X,Y)
 
 
 I=cut(Z[,2],qnorm(seq(0,1,by=.05)), labels=LETTERS[1:20])
 Yg=tapply(Y,I,mean)
 Xg=tapply(X,I,mean)
 
 df <- data.frame(X,Y, Yg, Xg,  I)

 ggplot(df, aes(x=X, y=Y)) +geom_point() + geom_smooth(method="lm")
 ggplot(df, aes(x=Xg, y=Yg)) +geom_point() + geom_smooth(method="lm") 
 ggplot(df, aes(x=X, y=Y)) +geom_point() + geom_smooth(method="lm") + facet_wrap(~I)
 