## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----functions, include=FALSE-------------------------------------------------
# A function for captioning and referencing images
fig <- local({
    i <- 0
    ref <- list()
    list(
        cap=function(refName, text) {
            i <<- i + 1
            ref[[refName]] <<- i
            paste("Figure ", i, ": ", text, sep="")
        },
        ref=function(refName) {
            ref[[refName]]
        })
})

## ---- eval=TRUE, echo=FALSE, include=FALSE------------------------------------
#install.packages("kableExtra")
require(knitr)
library(gdtools)
library(kableExtra)
library(clusterCrit)
library(dplyr)

## ---- echo=FALSE, include=FALSE-----------------------------------------------
col1 <- c("1", "2","3","4", "5", "6")
col2 <- c("`data_imputation`","`rates`", "`props`", "`outlier_detect`","`w_spaces`", "`remove_rows_n`")
col3 <- c("Data imputation for longitudinal data", "Conversion of 'counts' to 'rates'", "Conversion of 'counts' (or 'rates') to 'Proportion'", "Outlier detection and replacement","Whitespace removal", "Incomplete rows removal")
col4 <- c("Calculates any missing entries (`NA`, `Inf`, `null`) in a longitudinal data, according to a specified method","Calculates rates from observed 'counts' and its associated denominator data", "Converts 'counts' or 'rates' observation to 'proportion'", "Identifies outlier observations in the data, and replace or remove them","Removes all the leading and trailing whitespaces in a longitudinal data", "Removes rows which contain 'NA' and 'inf' entries")
tble <- data.frame(col1, col2, col3, col4)
tble <- tble

## ----table1, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------
knitr::kable(tble, caption = "Table 1. `Data manipulation` functions", col.names = c("SN","Function","Title","Description")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "8em", background = "white") %>%
  column_spec(3, width = "12em", background = "white") %>%
  column_spec(4, width = "16em", background = "white")#%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")

## ---- eval=FALSE--------------------------------------------------------------
#  #installing the `akmedoids` packages
#  install.packages("devtools")
#  devtools::install_github("manalytics/akmedoids")
#  

## ---- eval=TRUE---------------------------------------------------------------

#loading the package
library(akmedoids)

## ---- eval=TRUE---------------------------------------------------------------

#import and preview the first 6 rows of 'traj' object
data(traj)

head(traj)

#no. of rows
nrow(traj) 

#no. of columns
ncol(traj) 


## ---- eval=TRUE---------------------------------------------------------------
imp_traj <- data_imputation(traj, id_field = TRUE, method = 2, 
               replace_with = 1, fill_zeros = FALSE)

imp_traj <- imp_traj$CompleteData
#viewing the first 6 rows
head(imp_traj)

## ----figs1, echo=FALSE, fig.width=5,fig.height=6,fig.align="center", fig.cap=fig$cap("figs1", "Data imputation with regression")----

par(mar=c(2,2,2,2)+0.1)
par(adj = 0)
par(mfrow=c(6,2))
dev.new()
dat <- as.data.frame(traj)
t_name <- as.vector(traj[,1])
dat <- dat[,2:ncol(dat)]
#if(k==nrow(dat)){
  #}
#head(dat)
for(k in seq_len(nrow(dat))){ #k<-2
  y <- suppressWarnings(as.numeric(as.character(dat[k,])))
  x <- seq_len(length(y))
  known <- data.frame(x, y)
  known_1 <- data.frame(known[is.na(known[,2])|is.infinite(known[,2]),])  #
  known_2 <- data.frame(known[!is.na(known[,2])&!is.infinite(known[,2]),])
  #train the available data using linear regression
  model.lm <- lm(y ~ x, data = known_2)
  # Use predict the y value for the removed data
  newY <- predict(model.lm, newdata = data.frame(x = known_1[,1]))
   l_pred <- predict(model.lm, newdata = data.frame(1:9)) #line
  #add to the original data.
  dat[k, known_1[,1]] <- newY
  #Add the predicted points to the original data
  #dev.new()
  #plot(1:10, col=2)
  #options(rgl.useNULL = TRUE)
  plot (known$x, known$y, type="o", main=paste("traj_id:",t_name[k], sep=" "), font.main = 1)
  if(!length(newY)==0){#plot only if it has elements
  lines(l_pred, lty="dotted", col="red", lwd=2)
  }
  points(known_1[,1], newY, col = "red")
}
#point legend
plot_colors <- c("black","red")
text <- c("Observed points", "Predicted points")
#options(rgl.useNULL = TRUE)
par(xpd=TRUE)
legend("center",legend = text, text.width = max(sapply(text, strwidth)),
       col=plot_colors, pch = 1, cex=1, horiz = FALSE)
par(xpd=FALSE)

#line legend
plot_colors <- c("black","red")
text <- c("line joining observed points", "regression line predicting missing points")
#options(rgl.useNULL = TRUE)
plot.new()
par(xpd=TRUE)
legend("center",legend = text, text.width = max(sapply(text, strwidth)),
       col=plot_colors, lwd=1, cex=1, lty=c(1,2), horiz = FALSE)
par(xpd=FALSE)

## ---- eval=TRUE---------------------------------------------------------------

#import population data
data(popl)

#preview the data
head(popl)

nrow(popl) #no. of rows

ncol(popl) #no. of columns

## ---- echo=FALSE--------------------------------------------------------------
#create a matrix of the same rows and column as the `traj` data
pop <- as.data.frame(matrix(0, nrow(popl), ncol(traj)))
colnames(pop) <- names(traj) 
pop[,1] <- as.vector(as.character(popl[,1]))
pop[,4] <- as.vector(as.character(popl[,2]))
pop[,8] <- as.vector(as.character(popl[,3]))
list_ <- c(2, 3, 5, 6, 7, 9, 10)
for(u_ in seq_len(length(list_))){ #u_<-1
  pop[,list_[u_]] <- "NA"
}

head(pop)

population2 <- pop

## ---- eval=TRUE---------------------------------------------------------------

pop_imp_result <- data_imputation(population2, id_field = TRUE, method = 2, 
               replace_with = 1, fill_zeros = FALSE)

pop_imp_result <- pop_imp_result$CompleteData

#viewing the first 6 rows
head(pop_imp_result)


## ---- eval=TRUE---------------------------------------------------------------

#example of estimation of 'crimes per 200 residents'
crime_per_200_people <- rates(imp_traj, denomin=pop_imp_result, id_field=TRUE, 
                              multiplier = 200)

#view the full output
crime_per_200_people <- crime_per_200_people$rates_estimates

#check the number of rows
nrow(crime_per_200_people)


## ---- eval=TRUE---------------------------------------------------------------

#Proportions of crimes per 200 residents
prop_crime_per200_people <- props(crime_per_200_people, id_field = TRUE, scale = 1, digits=2)

#view the full output
prop_crime_per200_people


#A quick check that sum of each column of proportion measures adds up to 1.  
colSums(prop_crime_per200_people[,2:ncol(prop_crime_per200_people)])



## ----figs2, echo=TRUE, fig.width=6,fig.height=3,fig.align="center", fig.cap=fig$cap("figs2", "Identifying outliers")----

#Plotting the data using ggplot library
library(ggplot2)
#library(reshape2)

#converting the wide data format into stacked format for plotting
#doing it manually instead of using 'melt' function from 'reshape2'

#imp_traj_long <- melt(imp_traj, id="location_ids") 

coln <- colnames(imp_traj)[2:length(colnames(imp_traj))]
code_ <- rep(imp_traj$location_ids, ncol(imp_traj)-1)
d_bind <- NULL
  for(v in seq_len(ncol(imp_traj)-1)){
    d_bind <- c(d_bind, as.numeric(imp_traj[,(v+1)]))
  }

code <- data.frame(location_ids=as.character(code_))
variable <- data.frame(variable=as.character(rep(coln,
                        each=length(imp_traj$location_ids))))
value=data.frame(value = as.numeric(d_bind))

imp_traj_long <- bind_cols(code, variable,value) 
  
#view the first 6 rows
head(imp_traj_long)

#plot function
p <-  ggplot(imp_traj_long, aes(x=variable, y=value,
            group=location_ids, color=location_ids)) + 
            geom_point() + 
            geom_line()

#options(rgl.useNULL = TRUE)
print(p)


## ----figs3, echo=TRUE, fig.width=6,fig.height=3,fig.align="center", fig.cap=fig$cap("figs3", "Replacing outliers with mean observation")----

imp_traj_New <- outlier_detect(imp_traj, id_field = TRUE, method = 2, 
                              threshold = 20, count = 1, replace_with = 2)

imp_traj_New <- imp_traj_New$Outliers_Replaced 

#options(rgl.useNULL = TRUE)
print(imp_traj_New)

#imp_traj_New_long <- melt(imp_traj_New, id="location_ids") 

coln <- colnames(imp_traj_New)[2:length(colnames(imp_traj_New))]
code_ <- rep(imp_traj_New$location_ids, ncol(imp_traj_New)-1)

d_bind <- NULL
  for(v in seq_len(ncol(imp_traj_New)-1)){
    d_bind <- c(d_bind, as.numeric(imp_traj_New[,(v+1)]))
  }

code <- data.frame(location_ids=as.character(code_))
variable <- data.frame(variable=as.character(rep(coln,
              each=length(imp_traj_New$location_ids))))
value=data.frame(value = as.numeric(d_bind))

imp_traj_New_long <- bind_cols(code, variable,value)

#plot function
#options(rgl.useNULL = TRUE)
p <-  ggplot(imp_traj_New_long, aes(x=variable, y=value,
            group=location_ids, color=location_ids)) + 
            geom_point() + 
            geom_line()

#options(rgl.useNULL = TRUE)
print(p)


## ----figs4, echo=FALSE, fig.cap=fig$cap("figs4", paste("Long-time linear trends of relative (`proportion`, `p`) crime exposure. Three inequality trends: trajectory i1: crime exposure is falling faster, i2, crime exposure is falling at the same rate, and i3, crime exposure is falling slower or increasing, relatively to the citywide trend. (Source:", "Adepeju et al. 2021)", sep=" ")), out.width = '60%', fig.align="center"----
knitr::include_graphics("inequality.png")

## ---- echo=FALSE, include=FALSE-----------------------------------------------

col1 <- c("1", "2", "3")
col2 <- c("`akclustr`","`print_akstats`", "`plot_akstats`")
col3 <- c("`Anchored k-medoids clustering`","`Descriptive (Change) statistics of clusters`", "`Plots of cluster groups`")
col4 <- c("Clusters trajectories into a `k` number of groups according to the similarities in their long-term trend and determines the best solution based on the Silhouette width measure or the Calinski-Harabasz criterion","Generates the descriptive and change statistics of groups, and also plots the groups performances", "Generates different plots of cluster groups")
tble2 <- data.frame(col1, col2, col3, col4)
tble2 <- tble2


## ----table2, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------
knitr::kable(tble2, caption = "Table 2. `Data clustering` functions", col.names = c("SN","Function","Title","Description")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "8em", background = "white") %>%
  column_spec(3, width = "12em", background = "white") %>%
  column_spec(4, width = "16em", background = "white")#%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")


## ----figs5, echo=TRUE, fig.width=6,fig.height=3,fig.align="center", fig.cap=fig$cap("figs5",  "Trajectory of crime proportions over time")----

#Visualizing the proportion data

#view the first few rows
head(prop_crime_per200_people)

#prop_crime_per200_people_melt <- melt(prop_crime_per200_people, id="location_ids") 
coln <- colnames(prop_crime_per200_people)[2:length(colnames(prop_crime_per200_people))]
code_ <- rep(prop_crime_per200_people$location_ids, ncol(prop_crime_per200_people)-1)
d_bind <- NULL
  for(v in seq_len(ncol(prop_crime_per200_people)-1)){
    d_bind <- c(d_bind, prop_crime_per200_people[,(v+1)])
  }

prop_crime_per200_people_melt <- data.frame(cbind(location_ids=as.character(code_), variable =
                        rep(coln,
                        each=length(prop_crime_per200_people$location_ids)), value=d_bind))

#plot function
#options(rgl.useNULL = TRUE)
p <-  ggplot(prop_crime_per200_people_melt, aes(x=variable, y=value,
            group=location_ids, color=location_ids)) + 
            geom_point() + 
            geom_line()

#options(rgl.useNULL = TRUE)
print(p)


## ---- echo=TRUE, include=TRUE-------------------------------------------------

#clustering
akObj <- akclustr(prop_crime_per200_people, id_field = TRUE, 
                                method = "linear", k = c(3,8), crit = "Calinski_Harabasz", verbose=TRUE)


## ---- echo=TRUE, message=TRUE, eval=TRUE--------------------------------------

names(akObj)


## ----figs6, echo=FALSE, fig.cap=fig$cap("figs6", "Clustering performance at different values of k"), out.width = '80%', fig.align="center"----
knitr::include_graphics("caliHara.png")

## ---- echo=TRUE, include=TRUE-------------------------------------------------

#5-group clusters
akObj$solutions[[3]] #for `k=5` solution


## ----figs7, echo=FALSE, fig.cap=fig$cap("figs7", "Quantile sub-divisions of most-diverging groups (n_quant=4)"), out.width = '80%', fig.align="center"----

knitr::include_graphics("Nquant.png")


## ---- echo=TRUE, include=TRUE-------------------------------------------------

#Specifying the optimal solution, output$optimal_k (i.e. `k = 5`) and using `stacked` type graph
prpties = print_akstats(akObj, k = 5, show_plots = FALSE)

prpties

## ----figs8, echo=FALSE, eval=FALSE, include=FALSE, fig.cap=fig$cap("figs8","group memberships"), out.width = '85%', fig.align="center"----
#  
#  knitr::include_graphics("traj_perfm.png")
#  

## ---- echo=TRUE, include=TRUE, fig.width=5,fig.height=5,fig.align="center"----

  #options(rgl.useNULL = TRUE)
  plot_akstats(akObj, k = 5, type="lines", y_scaling="fixed")


## ---- echo=TRUE, include=TRUE, fig.width=5,fig.height=5,fig.align="center"----
  #options(rgl.useNULL = TRUE)
  plot_akstats(akObj, k = 5, reference = 1, n_quant = 4, type="stacked")


## ----figs9, echo=FALSE, eval=FALSE, include=FALSE, fig.cap=fig$cap("figs9", "group quality over time"), out.width = '60%', fig.align="center"----
#  
#  knitr::include_graphics("traj_perfm2.png")
#  

## ---- echo=FALSE, include=FALSE-----------------------------------------------

col1 <- c("1", "2","3","4","5","6", "7","8","9","10")
col2 <- c("`group`", "`n`", "`n(%)`", "`%Prop.time1`", "`%Prop.timeT`", "`Change`", "`%Change`", "`%+ve Traj.`", "`%-ve Traj.`", "`Qtl:1st-4th`")
col3 <- c("`group membershp`", "`size (no.of.trajectories.)`", "`% size`", "`% proportion of obs. at time 1 (2001)`", "`proportion of obs. at time T (2009)`", "`absolute change in proportion between time1 and timeT`", "`% change in proportion between time 1 and time T`", "`% of trajectories with positive slopes`", "`% of trajectories with negative slopes`", "`Position of a group medoid in the quantile subdivisions`")
tble3 <- data.frame(col1, col2, col3)
tble3 <- tble3


## ----table3, results='asis', echo=FALSE, tidy.opts=list(width.cutoff=50)------

knitr::kable(tble3, caption = "Table 3. field description of clustering outputs", col.names = c("SN","field","Description")) %>%
  kable_styling(full_width = F) %>%
  column_spec(1, bold = T, border_right = T) %>%
  column_spec(2, width = "8em", background = "white") %>%
  column_spec(3, width = "12em", background = "white") #%>%
  #row_spec(3:5, bold = T, color = "white", background = "#D7261E")


