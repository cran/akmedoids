
#' @title Descriptive (Change) statistics and plots
#' @description This function perform two tasks: (i) it generate the descriptive and change statistics of groups, particularly suited for the outputs form the \code{\link{akmedoids.clust}} function, and (ii) generates the plots of the groups (performances).
#' @param clustr [vector (charater)] A vector of cluster membership (labels). For instance, the result extracted from the \code{\link{akmedoids.clust}} function.
#' @param traj [matrix (numeric)]: corresponding longitudinal data used to generate \code{clustr} (with rows corresponding to each label of \code{clustr}). For example, the first label of \code{clustr} is the group label of the first row of \code{traj} matrix, and so on.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time step.
#' @param type [character] plot type. Available options are: \code{"lines"} and \code{"stacked"}.
#' @param y.scaling [character] works only if \code{type="lines"}. \code{y.scaling} set the vertical scales of the cluster panels. Options are: \code{"fixed"}: uses uniform scale for all panels, \code{"free"}: uses variable scales for panels.
#' @param bandw [numeric] A small probability (quantile) value between \code{[0,1]} to partition the trajectories into three classes, i.e. \code{lower}, \code{central}, and the \code{uppper} classes. The middle of the \code{central} class is defined by the average slope of all trajectories. The upper and the lower limits of the \code{central} class is determined by the value of \code{bandw}. Default value is \code{0.25}, indicating that all slopes within 25th quantiles of the maximum slopes on both sides of the average slope are categorised as \code{central} class.
#' @examples
#' print(traj)
#' traj <- dataImputation(traj, id_field = TRUE, method = 1, replace_with = 1,
#' fill_zeros = FALSE)
#' print(traj)
#' traj <- props(traj, id_field = TRUE)
#' clustr <- akmedoids.clust(traj, id_field = TRUE, method = "linear", k = c(3,6))
#' clustr <- as.vector(clustr$optimSolution)
#' print(statPrint(clustr, traj, id_field=TRUE, type="lines", y.scaling="fixed"))
#' print(statPrint(clustr, traj, id_field=TRUE, bandw = 0.30, type="stacked"))
#' @details Generates the descriptive and change statistics of the trajectory groupings. Given a vector of group membership (labels) and the corresponding data matrix (or data.frame) indexed in the same order, this function generates all the descriptive and change statistics of all the groups.
#' The function can generate a line and an area stacked plot drawing from the functionalities of the \code{ggplot2} library. For a more customised visualisation, we recommend that users deploy \code{ggplot2} directly (\code{Wickham H. (2016)}).
#' @return A plot showing group membership or sizes (proportion) and statistics.
#' @keywords plot, clusters
#' @rawNamespace import(reshape2, ggplot2, stats)
#' @references \code{Wickham H. (2016). Elegant graphics for Data Analysis. Spring-Verlag New York (2016)}
#' @export

statPrint <- function(clustr, traj, id_field=TRUE, bandw = 0.25, type = "lines", y.scaling="fixed"){

  #joining the data with the clusters
  clustr <- data.frame(cbind(traj, clusters=clustr))

  dat <- traj #backing up the data

  #check if 'id_field' is a unique field
  if(id_field==TRUE){
    dat <- dat[,2:ncol(dat)]
    n_CL <- colnames(clustr)[1]
    col_names <- as.vector(clustr[,1])
    if(!length(col_names)==length(unique(col_names))){
      stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
    }
  }

  if(id_field==FALSE){
    clustr <- cbind(1:nrow(clustr), clustr)  #head(clustr)
  }

  #cluster list
  clusters <- as.vector(clustr[,ncol(clustr)])
  data_subset <- clustr[,1:(ncol(clustr)-1)] #head(data_subset)
  data_subset <- as.data.frame(data_subset) #head(data_subset)
  colnames(data_subset) <- c("code", 1:(ncol(data_subset)-1))
  data.subset.melted <- suppressWarnings(melt(data_subset, id="code"))  #head(data_subset.melted)
  #append cluster list
  data.subset.melted <- cbind(data.subset.melted, rep(clusters, ncol(data_subset)-1))#nrow(data.long.melted)
  #some global variables
  colnames(data.subset.melted) <- c("id","Year","value", "clusters")

  #----------------------------------------------------
  #preparing the data to generate descriptive statitics
  year_uni <- as.vector(unique(data.subset.melted$Year))
  order_Cluster <- as.vector(unique(data.subset.melted$clusters))
  clusters_uni <- order_Cluster[order(as.vector(unique(data.subset.melted$clusters)))]

  change_ave_yr_ALL <- NULL
  for(q in 1:length(clusters_uni)){#p<-1
    all_clust_list <- data.subset.melted[which(data.subset.melted$clusters==clusters_uni[q]),]
    ave_yr <- NULL
    for(m in 1:length(year_uni)){ #m<-1
      yr_ <- all_clust_list[which(as.vector(all_clust_list$Year)==year_uni[m]),]
      ave_yr <- c(ave_yr, sum(yr_$value))
    }
    change_ave_yr_ALL <- rbind(change_ave_yr_ALL,  ave_yr)
  }
  #change_ave_yr_ALL  #for generating various descriptive statistics

  #----------------------------------------------------
  #plotting
  #----------------------------------------------------
  ggplot <- aes <- Year <- value <- id <- geom_line <- facet_wrap <- geom_smooth <- theme_minimal <- variable <- NULL
  #plot option 1:
  if(type=="lines"){
  if(y.scaling=="fixed"){
    p <- (ggplot(data.subset.melted, aes(x=Year, y=value,
                                             group=id, color=clusters)) +
                geom_line() +
                stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=1) +
                facet_wrap(~clusters, scales = "fixed") +
                facet_wrap(~clusters) +
                scale_colour_brewer(palette = "Set1")) #clusters
          }

  if(y.scaling=="free"){
    p <- (ggplot(data.subset.melted, aes(x=Year, y=value,
                                         group=id, color=clusters)) +
            geom_line() +
            stat_summary(fun.y=mean, geom="line", aes(group=clusters), color="black", size=1) +
            facet_wrap(~clusters, scales = "free") +
            facet_wrap(~clusters) +
            scale_colour_brewer(palette = "Set1") +
            theme_light()) #clusters
    }
  }#end of type 1
  #----------------------------------------------------

  #----------------------------------------------------
  #plot option 2:
  if(type=="stacked"){
  change_ave_yr_ALL_transpose <- t(change_ave_yr_ALL)
  grp.dat<-data.frame(change_ave_yr_ALL_transpose,row.names=1:nrow(change_ave_yr_ALL_transpose))
  names(grp.dat)<-clusters_uni
  p.dat<-data.frame(Year=row.names(grp.dat),grp.dat,stringsAsFactors=F) #p.dat
  p.dat<-melt(p.dat,id='Year')
  p.dat$Year<-as.numeric(p.dat$Year) #head(p.dat)
  class(p.dat$Year)

  p <- (ggplot(p.dat,aes(x=Year,y=value)) + theme(legend.position="none") +
    geom_area(aes(fill=variable), colour = "gray30", position='fill') + #scale_fill_manual(values = colours) +
    scale_x_continuous(breaks=1:nrow(change_ave_yr_ALL_transpose), labels=Year) +
    scale_fill_brewer(palette = "Set1") +
    theme_light())
  }

  #plot
  flush.console()
  dev.new(width=3, height=3)
  print(p)
  #----------------------------------------------------

  #----------------------------------------------------
  #To generate descriptive statistics
  all_statistics <- list()
  #Change statistics
  desc_Stats <- NULL #
  ll_ <- clusters_uni
  group <- clusters_uni
  for(n in 1:length(ll_)){ #n<-1
    #Calculating the number of trajectories
    a1 <- length(which(clusters%in%ll_[n]))
    a2 <- round((length(which(clusters%in%ll_[n]))/length(clusters))*100,digits = 1)
    a3 <- round((change_ave_yr_ALL[n,1] / sum(change_ave_yr_ALL[,1]))*100, digits = 1)
    a4 <- round((change_ave_yr_ALL[n,ncol(change_ave_yr_ALL)] / sum(change_ave_yr_ALL[,ncol(change_ave_yr_ALL)]))*100, digits = 1)
    a5 <- round((a4-a3), digits=1)
    a6 <- round(((a4-a3)/a4)*100, digits=1)
    desc_Stats <-  rbind(desc_Stats, cbind(a1, a2, a3, a4, a5, a6))
  }
  colnames(desc_Stats) <- c("n","n(%)","%Prop.time1","%Prop.timeT", "Change", "%Change")
  rownames(desc_Stats) <- 1:nrow(desc_Stats)
  #desc_Stats <- as.data.frame(cbind(desc_Stats[,1], group, desc_Stats[,2:ncol(desc_Stats)]))
  desc_Stats <- as.data.frame(cbind(group, desc_Stats))
  attrib1 <- c("'n'->size (number of traj.); 'n(%)'->%size; '%Prop.time1'->% proportion of obs. at time 1; '%Prop.timeT'-> % proportion of obs. at time T; 'Change'-> absolute change in proportion between time1 and timeT; '%Change'-> % change in proportion between time 1 and timeT")
  #attr(desc_Stats,"field description") <- c("'n'->size (number of traj.): 'n(%)'->%size: '%Prop.time1'->% proportion of obs. at time 1: '%Prop.timeT'-> % proportion of obs. at time T: 'Change'-> absolute change in proportion between time1 and timeT: '%Change'-> % change in proportion between time 1 and timeT")
  #attr(desc_Stats,"field_description") <- 6
  #all_statistics[[1]] <- desc_Stats
  #----------------------------------------------------

  #----------------------------------------------------
  #To generate slope statistics

  #required
  sl_List <- NULL
  time <- as.numeric(1:ncol(dat))
  for(i in 1:nrow(dat)){ #i<-1
    b=coefficients(lm(as.numeric(as.character(dat[i,]))~as.numeric(as.character(time))))
    sl_List <- rbind(sl_List, cbind(as.numeric(b[1]), as.numeric(b[2])))
  }

  sl_List <- as.data.frame(cbind(1:nrow(sl_List), sl_List))
  colnames(sl_List) <- c("sn", "intersect","slope")  #head(sl_List)

  #Generate the linear trendlines for all trajectories (dropping all intersects)
  dat_slopp<- NULL
  for(n in 1:nrow(sl_List)){ #k<-1
    dat_slopp <- rbind(dat_slopp, (0 + (sl_List[n,3]*(1:ncol(dat)))))  #head(dat_slopp)
  }

  change_Stats <- NULL
  for(d_ in 1:length(clusters_uni)){ #d_ <- 1
    ids_ <- which(clusters==clusters_uni[d_])
    slope_sign_ <- NULL
    for(v in 1:2){ #v=1
      if(v==1){
        all_1 <- round((length(which(dat_slopp[ids_, ncol(dat_slopp)]>0))/length(ids_))*100, digits = 1)  #head(dat_slopp)
      }
      if(v==2){
        all_2 <- round((length(which(dat_slopp[ids_, ncol(dat_slopp)]<0))/length(ids_))*100, digits = 1)  #head(dat_slopp)
      }
    }
    change_Stats  <- rbind(change_Stats , cbind(d_, all_1, all_2))
  }
  colnames(change_Stats) <- c("sn","%+ve Traj.","%-ve Traj.")
  rownames(change_Stats) <- 1:nrow(change_Stats)
  #change_Stats <- as.data.frame(cbind(change_Stats[,1], group, change_Stats))
  change_Stats <- as.data.frame(cbind(group, change_Stats))
  attrib2 <- c("'%+ve Traj.'-> % of trajectories with positive slopes; '%+ve Traj.'-> % of trajectories with negative slopes")

  #---------------------------------------------------------------------
  #determining the average slope and slopes of all groups (determining the classes of each groups)
  #citywide slope
  ave_slope <- mean(as.vector(sl_List$slope)) #citywide average.
  ave_slope <- data.frame(cbind("City", round(ave_slope, digits = 8)))
  colnames(ave_slope) <- c("gr", "slope")

  clas_ <- matrix(0, length(clusters_uni), 1)
  #group slopes
  gr_slopes <- NULL
  for(h_ in 1:length(clusters_uni)){ #h_<-1
    gr_slopes <- rbind(gr_slopes, cbind(clusters_uni[h_], round(mean(as.vector(sl_List$slope)[which(clusters==clusters_uni[h_])]), digits=8)))
  }
  gr_slopes <- data.frame(gr_slopes)
  colnames(gr_slopes) <- c("gr","slope")

  #split the group slope into three classe using the 'bandw' value)
  temp_upper_classes <- gr_slopes[which(as.numeric(as.character(gr_slopes$slope)) < as.numeric(as.character(ave_slope$slope))),] #separating the slopes into two
  #append the citywide slope
  all_min <- c(as.numeric(as.character(temp_upper_classes$slope)), as.numeric(as.character(ave_slope$slope)))
  #what's the quantile value
  upper_clas <- which(all_min < as.vector(round(quantile(all_min, (1-bandw)) , digits=8)))  #bandw =
  clas_[upper_clas] <- "Rising"

  #determining the 'lower' class from the remaining groups.
  temp_lower_classes <- gr_slopes[which(as.numeric(as.character(gr_slopes$slope)) > as.numeric(as.character(ave_slope$slope))),] #separating the slopes into two
  #append the citywide slope
  all_max <- c(as.numeric(as.character(ave_slope$slope)), as.numeric(as.character(temp_lower_classes$slope)))
  #what's the quantile value
  lower_clas <- which(all_max > as.vector(round(quantile(all_max, bandw) , digits=8)))  #bandw =
  clas_[nrow(gr_slopes) - (0:(length(lower_clas)-1))] <- "Falling"

  #input for the Stable group
  clas_[which(!clas_%in%c("Rising", "Falling"))] <- "Stable"
  class <- data.frame(class=clas_)
  change_Stats <- cbind(change_Stats, class)

  #-------------------

  #all_statistics <- list(descriptiveStats = desc_Stats, attrib.descr = attrib1, changeStats = change_Stats, attrib.slopes = attrib2)
  all_statistics <- list(descriptiveStats = desc_Stats, changeStats = change_Stats)

  #-------------------

return(all_statistics)

}



