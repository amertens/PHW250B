
library(epitools)
library(treemap)


RR <- 2
prev <- .3

AR <- (RR-1)/RR
PAF <- (prev*(RR-1))/(prev*(RR-1)+1)

prop_cases_exp <- PAF/AR

#RR= (a*d)/(b*c)
# RR= (a/(prev))/(c/(1-prev))
# a_div_c = (RR * prev)/(1-prev) 
# 
# exp_cases = a_div_c
# exposed_attr <- PAF/exp_cases


unexposed <- 1-prop_cases_exp
exposed_unattr <- 1 - unexposed - PAF


# Create data
group <- c("Unexposed", "Exposed" , "Exposed and attributable")
value <- c(unexposed, exposed_unattr, exposed_attr)
data <- data.frame(group,value)

# treemap
treemap(data,
        index="group",
        vSize="value",
        type="index")