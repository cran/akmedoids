
#' @title Anchored k-medoids clustering
#' @description Given a list of trajectories and a functional method, this function clusters the trajectories into a \code{k} number of groups. If a vector of two numbers is given, the function determines the best solution from those options based on the Calinski-Harabatz criterion.
#' @param traj [matrix (numeric)]: longitudinal data. Each row represents an individual trajectory (of observations). The columns show the observations at consecutive time steps.
#' @param id_field [numeric or character] Whether the first column of the \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}. If \code{TRUE} the function recognises the second column as the first time points.
#' @param method [character] The parametric initialisation strategy. Currently, the only available method is a \code{linear} method, set as \code{"linear"}. This uses the time-dependent linear regression lines and the resulting groups are order in the order on increasing slopes.
#' @param k [integer or vector (numeric)] either an exact integer number of clusters, or a vector of length two specifying the minimum and maximum numbers of clusters to be examined from which the best solution will be determined. In either case, the minimum number of clusters is \code{3}. The default is \code{c(3,6)}. The best solution is determined using the Calinski-Harabatz criterion \code{(Calinski T. & Harabasz J. 1974)}.
#' @usage akmedoids.clust(traj, id_field = FALSE, method = "linear", k = c(3,6))
#' @details This function works by first approximating the trajectories based on the chosen parametric forms (e.g. linear), and then partitions the original trajectories based on the form groupings, in similar fashion to k-means clustering \code{(Genolini et al. 2015)}. The key distinction of \code{akmedoids} compared with existing longitudinal approaches is that both the initial starting points as well as the subsequent cluster centers (as the iteration progresses) are based the selection of observations (medoids) as oppose to centroids.
#' @examples
#' traj <- traj
#' print(traj)
#' traj <- dataImputation(traj, id_field = TRUE, method = 2, replace_with = 1, fill_zeros = FALSE)
#' traj <- props(traj, id_field = TRUE)
#' print(traj)
#' output <- akmedoids.clust(traj, id_field = TRUE, method = "linear", k = c(3,6))
#' print(output)  #type 'as.vector(output$optimSolution)'
#' @return The key output is a vector of cluster labels of length equal to the number of trajectories. Each label indicates the group membership of the corresponding trajectory in the \code{traj} object. In addition, a plot of the Calinski-Harabatz scores is shown if a vector of \code{k} is provided.
#' @references \code{1}. Genolini, C. et al. (2015). kml and kml3d: R Packages to Cluster Longitudinal Data. Journal of Statistical Software, 65(4), 1-34. URL http://www.jstatsoft.org/v65/i04/.
#' \code{2}. Calinski T, Harabasz J (1974) A dendrite method for cluster analysis. Commun Stat 3:1-27.
#' \code{3}. Genolini, C.et al. (2016) Package ‘longitudinalData’
#' @rawNamespace import(kml, grDevices, reshape2, Hmisc, stats, utils, ggplot2, longitudinalData)
#' @export


akmedoids.clust <- function(traj, id_field = FALSE, method = "linear", k = c(3,6)){


CaliHara <- 0

#create a vector if an interger of k if provided.
  if(length(k)==1){
    k <- rep(k, 2)
  }

#linear medoid method
if(method=="linear"){

  #check if unaccepted value of k in inputted
  k_ <- k[1]:k[2]
  #checks
  if(k[1] < 3 | k[1] > nrow(traj) | k[2] > nrow(traj) |
     k[1] > 20 | k[2] > 20 | k[1]>k[2]){
    flush.console()
    print("*******Error!********")
    if(k[1] < 3 | k[1] > nrow(traj) | k[2] > nrow(traj) |
       k[1] > 20 | k[2] > 20){
      flush.console()
      print("Enter a number GREATER than 2 but NOT GREATER 20 or the number of trajectories.")
    }

    if(k[1]>k[2]){
      flush.console()
      print("Initial number of clusters can not be less than subsequent number of clusters")
    }
    stop("(: Program terminated!!! :)")
  } else {

    dat <- traj
    #check if there is id_field
    #check if there is unique(id) field
    if(id_field==TRUE){
      n_CL <- colnames(dat)[1]
      col_names <- as.vector(dat[,1])
      dat <- dat[,2:ncol(dat)]
      #check if the 'id_field' is a unique field
      if(!length(col_names)==length(unique(col_names))){
        stop("(: The 'id_field' is not a unique field. Function terminated!!! :)")
      }
    }


    #create a time list
    time = 1:ncol(dat) #clean.time   #mode(time)
    #-----------------------------------------------------
    #looping through to calculate the coefficients
    sl_List <- NULL
    time <- as.numeric(1:ncol(dat))
    for(i in 1:nrow(dat)){ #i<-1
      b=coefficients(lm(as.numeric(as.character(dat[i,]))~as.numeric(as.character(time))))
      sl_List <- rbind(sl_List, cbind(as.numeric(b[1]), as.numeric(b[2])))
    }

    sl_List <- as.data.frame(cbind(1:nrow(sl_List), sl_List))
    colnames(sl_List) <- c("sn", "intersect","slope")  #head(sl_List)

    #-----------------------------------------------------------
    #split the slopes into 'k' partitions to determine the medioids for different value of k
    all_cluster_center_List <- list()
    i_counter <- 0
    for(s_ in k[1]:k[2]){   #s_<-3
      i_counter <- i_counter + 1
      split_slopes <- split(sl_List, cut2(sl_List$slope, g=s_))

      #determine the medoids
      median_slopes_ <- list()
      for(j in 1:length(split_slopes)){ #j=1
        #s_dty <- as.data.frame(split_slopes[j][1],,2)[,3]
        m_dty<-median(split_slopes[j][[1]]$slope)
        median_slopes_ <- rbind(median_slopes_, m_dty)
      }

      #generate regression lines based on the medoid slopes (to create the initial centroids)
      centers_List <- NULL
      for(m in 1:nrow(median_slopes_)){ #m<-1
        centers_List <- rbind(centers_List, (0 + (median_slopes_[[m,1]]*(1:ncol(dat)))))  #mean_Slopes[k,1]
      }
      centers_List <- as.data.frame(centers_List)

      all_cluster_center_List[[i_counter]] <- centers_List

    }#

    #Generate the linear trendlines for all trajectories (dropping all intersects)
    dat_slopp<- NULL
    for(n in 1:nrow(sl_List)){ #k<-1
      dat_slopp <- rbind(dat_slopp, (0 + (sl_List[n,3]*(1:ncol(dat)))))  #head(dat_slopp)
    }

  #looping through list of k and calculate the Calinski and Harabatz criterion
  final_result <- list()


    #initialise holders
    calinski <- NULL  #holder for the quality scores
    result_ <- list() #holder for the results

    #loop through k
    for(r_ in 1:length(k_)){ #r_<-1

      #1st iteration
      part2 <- affectIndivC(dat_slopp, all_cluster_center_List[[r_]])

      #temporary holder for a preceeding solution
      distF_backup <- list()

      #get a list of unique cluster labels
      c_count <- unique(part2)[order(unique(part2))]

      #a vector of time steps
      time_1 <- 1:ncol(traj)

      #variable to store the similarity scores
      simil_ <- matrix(0, 100, length(c_count))

      #number of iterations #fixed as 20
      for(z in 1:20){  #z<-2

        #recalculate the cluster centrure and do the affection
        if(z > 1){

          #pick the last
          #sort the median of the slopes of all the groups
          centers <- NULL
          for(h_ in 1:length(c_count)){#
            dat_slopp_ <- as.data.frame(dat_slopp)[which(part2==c_count[h_]),]
            #sort the last column to determine the medoid trajectory
            indexSort_ <- order(dat_slopp_[,ncol(dat_slopp_)])
            le_ <- indexSort_[ceiling(length(indexSort_)/2)]
            #pull out the medoid trajectory
            centers <- rbind(centers, dat_slopp_[le_, ])
          }
          linear_centers <- as.data.frame(centers)
          #determine the affection of each trajectory to the medoids
          part2 <- affectIndivC((dat_slopp), linear_centers)
        }

        #detetermine the similarity of consecutive solutions
        if(z > 1){
          for(y in 1:length(c_count)){  #y<-1
            #compare
            simil_[z,y] <- (length(distF_backup[[y]]%in%which(part2==c_count[y]))/length(which(part2==c_count[y])))*100
          }
        }

        #executed only in the 1st iteration
        if(z==1){
          for(y in 1:length(c_count)){
            distF_backup[[y]] <- which(part2==c_count[y])
          }
        }

        #back up the current solution
        if(z > 1){
          for(y in 1:length(c_count)){  #
            distF_backup[[y]] <- which(part2==c_count[y]) ##---yes, using x and y here is okay.
          }
        }
      }

      #solution
      sol_k <- alphaLabel(part2)
      attr(sol_k,"k") <- k_[r_]

      result_[[r_]] <- sol_k

      #get the longitudinal trends
      ld <- longData((dat_slopp)) #convert to longitudinal data
      part3 <- partition(part2)
      cr1 <- qualityCriterion(ld,part3) #calculate the quality
      calinski <- c(calinski, round(cr1$criters[1], digits=4))
      flush.console()
      print(paste("solution of k =", k_[r_], "determined!"))

    }#end of k loop

    #return the solution if a single value of k is provided
    if(k[1]==k[2]){
      solution_ <- list()
      solution_[[1]] <- result_[[1]]
      final_result <- list(solution=solution_[[1]])   #
      return(final_result)
    }

    #if a range of value is provided
    if(k[1]!=k[2]){
      qualit<- data.frame(k=k[1]:k[2], CaliHara=calinski)
      id_opt <- (which(qualit[,2]==max(qualit))[1] +(k[1]-1))
      #plot
      plt <- ggplot(qualit, aes(x = k, y = CaliHara)) +
        geom_line(linetype = "dotdash") + geom_point(shape=0)+
        ggtitle(paste("Optimal solution based on the Calinski-Harabatz criterion: k = ", id_opt, sep=" ")) +
        geom_vline(xintercept = (which(qualit[,2]==max(qualit))[1] +(k[1]-1)), linetype="dashed", color = "red", size=0.5)

      qualiCriterion=c("Quality criterion: Calinski-Harabatz criterion")
      #determine optimal solution
      optimal_solution <- result_[[(which(qualit[,2]==max(qualit))[1])]] #all_solutions[[4]]

      flush.console()
      dev.new(width=3, height=3)
      print(plt)

      #combining the results
      final_result <- list(plt,
                           qualitycriterion =  qualiCriterion, optimSolution=optimal_solution)

      return(final_result)

    }


  } #end of else if
}

} #end of function
