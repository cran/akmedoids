
#' @title Anchored k-medoids clustering
#' @description Given a list of trajectories and a functional method,
#' this function clusters the trajectories into a \code{k} number of
#' groups. If a vector of two numbers is given, the function determines
#' the best solution from those options based on the Caliński-Harabasz
#' criterion.
#' @param traj [matrix (numeric)]: longitudinal data. Each row represents
#' an individual trajectory (of observations). The columns show the
#' observations at consecutive time steps.
#' @param id_field [numeric or character] Whether the first column of the
#' \code{traj} is a unique (\code{id}) field. Default: \code{FALSE}.
#' If \code{TRUE} the function recognizes the second column as the first
#' time points.
#' @param method [character] The parametric initialization strategy.
#' Currently, the only available method is a \code{linear} method, set as
#' \code{"linear"}. This uses the time-dependent linear regression lines
#' and the resulting groups are order in the order on increasing slopes.
#' @param k [integer or vector (numeric)] either an exact integer number
#' of clusters, or a vector of length two specifying the minimum and
#' maximum numbers of clusters to be examined from which the best
#' solution will be determined. In either case, the minimum number
#' of clusters is \code{3}. The default is \code{c(3,6)}.
#' @param crit [character] a string specifying the type of the criterion
#' to use for assessing the quality of the cluster solutions, when
#' \code{k} is a vector of two values (as above). Default:
#' \code{crit="Silhouette"}, use the average Silhouette width
#' (\code{Rousseeuw P. J. 1987}). Using the \code{"Silhouette"} criterion,
#' the optimal value of \code{k} can be determined as the elbow point of
#' the curve. Other valid criterion is the "Calinski_Harabasz"
#' (\code{Caliński T. & Harabasz J. 1974}) in which the maximum score
#' represents the point of optimality. Having determined the optimal
#' \code{k}, the function can then be re-run, using the exact (optimal)
#' value of \code{k}.
#' @param verbose to suppress output messages (to the console)
#' during clustering. Default: \code{TRUE}.
#' @param quality_plot Whether to show plot of quality criteria across
#' different values of \code{k}. Default: \code{FALSE}.
#' @usage akclustr(traj, id_field = FALSE, method = "linear",
#' k = c(3,6), crit="Silhouette", verbose = TRUE, quality_plot=FALSE)
#' @details This function works by first approximating the trajectories
#' based on the chosen parametric forms (e.g. linear), and then partitions
#' the original trajectories based on the form groupings, in similar
#' fashion to k-means clustering \code{(Genolini et al. 2015)}. The key
#' distinction of \code{akmedoids} compared with existing longitudinal
#' approaches is that both the initial starting points as well as the
#' subsequent cluster centers (as the iteration progresses) are based
#' the selection of observations (medoids) as oppose to centroids.
#' @examples
#'
#' data(traj)
#'
#' trajectry <- data_imputation(traj, id_field = TRUE, method = 2,
#' replace_with = 1, fill_zeros = FALSE)
#'
#' trajectry <- props(trajectry$CompleteData, id_field = TRUE)
#'
#' print(trajectry)
#'
#' output <- akclustr(trajectry, id_field = TRUE,
#' method = "linear", k = c(3,7), crit='Calinski_Harabasz',
#' verbose = FALSE, quality_plot=FALSE)
#'
#' print(output)
#'
#' @return generates an \code{akobject} consisting of the
#' cluster solutions at the specified values of \code{k}. Also,
#' the graphical plot of the quality scores of the cluster
#' solutions.
#' @references \code{1}. Genolini, C. et al. (2015) kml and kml3d:
#' R Packages to Cluster Longitudinal Data. Journal of Statistical
#' Software, 65(4), 1-34. URL http://www.jstatsoft.org/v65/i04/.
#' @references \code{2}. Rousseeuw P. J. (1987) Silhouettes: A graphical aid
#' to the interpretation and validation of cluster analysis.
#' J. Comput. Appl. Math 20:53–65.
#' @references \code{3}. Caliński T, Harabasz J (1974) A dendrite method for
#' cluster analysis. Commun. Stat. 3:1-27.
#' @importFrom kml affectIndivC
#' @importFrom stats lm median coefficients
#' @importFrom Hmisc cut2
#' @importFrom utils flush.console
#' @importFrom grDevices dev.new
#' @importFrom ggplot2 ggplot aes geom_line geom_point
#' ggtitle geom_vline geom_smooth theme_light
#' @importFrom clusterCrit intCriteria
#' @export


akclustr <- function(traj, id_field = FALSE, method = "linear",
                            k = c(3,6), crit = "Silhouette",
                            verbose = TRUE, quality_plot=FALSE){

  qualityCrit <- 0

  #create a vector of two elements,
  #if k is a single value

  if(length(k)==1){
    k <- rep(k, 2)
  }

  #linear medoid method

  if(method=="linear"){

    #list k
    k_ <- k[1]:k[2]

    #to check that quality_plot' is used correctly.
    if(k[1]==k[2] & quality_plot == TRUE){
      stop(paste("...'quality_plot==TRUE' argument not applicable.",
                 "Provide a set of k values (See documentation of",
                 "'akclustr' function).", sep=" "))}

    #check if unacceptable value of k in inputted

    if(k[1] < 3 | k[1] > nrow(traj) | k[2] > nrow(traj) |
    k[1] > 20 | k[2] > 20 | k[1]> k[2]){

      # flush.console()
      # print("*******Error!********")

      if(k[1] < 3 | k[1] > nrow(traj) | k[2] > nrow(traj) |
      k[1] > 20 | k[2] > 20){

        flush.console()
        print(paste("Enter a number GREATER than 2 but LESS",
                    "THAN 20 or the total number of trajectories.", sep=" "))
      }

      if(k[1]>k[2]){
        flush.console()
        print(paste("Initial number of clusters can not be less than",
                    "subsequent number of clusters"))
      }

      stop("(: Program terminated!!! :)")

    } else {

    dat <- traj

    #check if there is id_field
    #check if id field  is unique

    if(id_field==TRUE){
      n_CL <- colnames(dat)[1]
      col_names <- as.vector(dat[,1])
      dat <- dat[,2:ncol(dat)]

      #check if the 'id_field' is a unique field

      if(!length(col_names)==length(unique(col_names))){
        stop(paste("(: The 'id_field' does not contain unique",
        "elements. Function terminated!!! :)", sep=" "))
      }
    }

    #get the 'time' vector

    time <- seq_len(ncol(dat))
    #-----------------------------------------------------
    #get the linear coefficients

    sl_List <- NULL
    time <- as.numeric(seq_len(ncol(dat)))

    for(i in seq_len(nrow(dat))){ #i<-1
      b <- coefficients(lm(as.numeric(as.character(dat[i,]))~
                          as.numeric(as.character(time))))
      sl_List <- rbind(sl_List, cbind(as.numeric(b[1]),
                                      as.numeric(b[2])))
    }

    sl_List <- as.data.frame(cbind(seq_len(nrow(sl_List)), sl_List))
    colnames(sl_List) <- c("sn", "intersect","slope")

    #-----------------------------------------------------------
    #split the slopes into 'k' partitions to determine
    #the medoids for different value of k

    all_cluster_center_List <- list()
    i_counter <- 0

    for(s_ in k[1]:k[2]){   #s_<-3

      i_counter <- i_counter + 1
      split_slopes <- split(sl_List, cut2(sl_List$slope, g=s_))

      #collate the medoids

      median_slopes_ <- list()

      for(j in seq_len(length(split_slopes))){ #j=1
        m_dty <- median(split_slopes[j][[1]]$slope)
        median_slopes_ <- rbind(median_slopes_, m_dty)
      }

      #generate regression lines based on the medoid
      #slopes (to create the initial centroids)

      centers_List <- NULL

      for(m in seq_len(nrow(median_slopes_))){ #m<-1
        centers_List <- rbind(centers_List,
                              (0 + (median_slopes_[[m,1]]*
                                      (seq(ncol(dat))))))
      }

      centers_List <- as.data.frame(centers_List)
      all_cluster_center_List[[i_counter]] <- centers_List
    }

    #Generate the trendlines for all
    #trajectories (dropping all intersects)

    dat_slopp <- NULL

    for(n in seq_len(nrow(sl_List))){ #k<-1
      dat_slopp <- rbind(dat_slopp, (0 + (sl_List[n,3]*
                                            (seq_len(ncol(dat))))))
    }

    #looping through list of k,
    #get the clusters,
    #and calculate two quality criteria (Silhouette $ Calinski_Harabasz)

    final_result <- list()

    #initialize holders

    criterValue1 <- NULL
    criterValue2 <- NULL

    result_ <- list()

    #loop through k,
    #compute the clusters for all values of k

    #initialise a counter
    counter <- 0

    if(verbose == TRUE){
      flush.console()
      print("Processing....")
      print("..............")
    }

    for(r_ in seq_len(length(k_))){

      counter <- counter + 1
      #1st iteration

      part2 <- affectIndivC(dat_slopp,
                            all_cluster_center_List[[r_]])

      #temporary holder for immediate past solution

      distF_backup <- list()

      #get the unique cluster labels

      c_count <- unique(part2)[order(unique(part2))]

      #get the vector of time steps

      time_1 <- seq_len(ncol(traj))

      #matrix to store the similarity scores
      #for 100 iterations

      simil_ <- matrix(0, 100, length(c_count))

      #number of iterations #fixed as 20

      for(z in seq_len(100)){  #z<-2

        #recalculate the cluster centrure and do the affection

        if(z > 1){

          #pick the last
          #sort the median of the slopes of all the groups

          centers <- NULL

          for(h_ in seq_len(length(c_count))){
            dat_slopp_ <- as.data.frame(dat_slopp)[which(part2==c_count[h_]),]
            #sort the last column to determine the medoid trajectory
            indexSort_ <- order(dat_slopp_[,ncol(dat_slopp_)])
            le_ <- indexSort_[ceiling(length(indexSort_)/2)]
            #pull out the medoid trajectory
            centers <- rbind(centers, dat_slopp_[le_, ])
          }

          linear_centers <- as.data.frame(centers)

          #determine the affection of each trajectory to the medoids
          #for next iteration

          part2 <- affectIndivC((dat_slopp), linear_centers)
        }

        #determine the similarity of consecutive solutions
        if(z > 1){
          for(y in seq_len(length(c_count))){
            #compare
            simil_[z,y] <- (length(distF_backup[[y]]%in%
                                     which(part2==c_count[y]))/
                                       length(which(part2==c_count[y])))*100
          }
        }

        #only executed for 1st iteration

        if(z==1){
          for(y in seq_len(length(c_count))){
            distF_backup[[y]] <- which(part2==c_count[y])
          }
        }
        #back up the current solution
        if(z > 1){
          for(y in seq_len(length(c_count))){
            distF_backup[[y]] <- which(part2==c_count[y])
          }
        }
      }

      #convert cluster labels to alphabets

      sol_k <- alpha_label(part2)
      sol_k_integers <- part2
      attr(sol_k,"cluster labels for k =") <- k_[r_]
      result_[[counter]] <- sol_k

      #-------------------------------------
      #get the slopes

      slp_ <- sl_List$slope #slopes
      slp_x <- rep(0, length(slp_))

      f_cal <- matrix(cbind(slp_x, slp_),,2)
      cl <- as.integer(sol_k_integers)
      #compute quality criterion 1
      vals1 <- as.numeric(intCriteria(f_cal,cl,
                                                   "Silhouette"))
      criterValue1 <- c(criterValue1, vals1)
      #compute quality criterion 2
      vals2 <- as.numeric(intCriteria(f_cal,cl,
                                                   "Calinski_Harabasz"))
      criterValue2 <- c(criterValue2, vals2)
      #-------------------------------------
      if(verbose==TRUE){
      flush.console()
        print(paste("solution of k =", k_[r_], "determined!"))
      }
    }

    #return solution if a single value of k is set
    # if(k[1]==k[2]){
    #   solution_ <- list()
    #   solution_[[1]] <- result_[[1]]
    #   final_result <- list(memberships=solution_[[1]])
    #   return(final_result)
    # }

    #if a range of value is provided
    ##if(k[1]!=k[2]){

      if(crit=="Silhouette"){
        criterValues <- criterValue1
        crit=="Silhouette"
      }

      #"Calinski_Harabasz" is always applicable!
      if(crit=="Calinski_Harabasz"){
        criterValues <- criterValue2
        crit <- "Calinski_Harabasz"
      }

      #if no valid criterion is specified. terminate!!
      if(!crit %in% c("Silhouette", "Calinski_Harabasz")){
        flush.console()
        stop(paste("*----*(: Quality criterion specified is NOT RECOGNISED!!",
                   "Execution terminated!!! :)*----*", sep= " "))
      }

      #for 'Silhouette' criterion. Generate quality plot
      if(crit=="Silhouette"){
        qualit <- data.frame(k=k[1]:k[2], qualityCrit=criterValues)

        #terminate if missing or infinite values exist
        if(any(is.na(qualit$qualityCrit))){
          stop(paste("*----*(: 'Silhouette' criterion is not applicable",
                     "to this dataset!. Try 'Calinski_Harabasz':)*----*",
                     sep=" "))
        }

        #determine the 'elbow' point, using 'linearity' method
        elbP <- elbow_point(qualit$k,qualit$qualityCrit)
        #options(rgl.useNULL = TRUE)
        plt <- ggplot(qualit, aes(x = k, y = qualityCrit)) +
          #geom_line(linetype = "dotdash") +
          geom_point(shape=0)+
          geom_smooth(method = "loess") +
          ggtitle(paste("Optimal solution based on the", crit,
                        "criterion: k = ",
                        round(elbP$x, digits=0), sep=" ")) +
          geom_vline(xintercept = elbP$x, linetype="dashed",
                     color = "red", size=0.5) +
          theme_light()
          qualiCriterion  <- paste("Quality criterion:", crit, sep=" ")
        #if(quality_plot==FALSE){
        # out_msg <- paste("Suggested optimal solution contains",
        #             round(elbP$x, digits=0),
        #             "clusters. See the plot for further examination!",
        #             sep=" ")

        if(k[1]==k[2]){
         final_result <- list(traj=traj,
                               id_field = id_field,
                               solutions=result_,
                               qualitycriterion =  qualiCriterion,
                               qualityCrit.List=qualit)
        }

        if(k[1]!=k[2]){
          final_result <- list(traj=traj,
                             id_field = id_field,
                             solutions=result_,
                             qualitycriterion =  qualiCriterion,
                             optimal_k=(elbP$x),
                             qualityCrit.List=qualit,
                             qltyplot=plt)
        }

        #}
        #----------------------------------
        if(quality_plot==TRUE){
          #flush.console()
          #dev.new(width=3, height=3)
          options(rgl.useNULL = TRUE)
          print(plt)
        }

        class(final_result) <- c("akobject", class(final_result))

        return(final_result)

      }

      #for 'Calinski_Harabasz' criterion. Generate quality plot
      if(crit=="Calinski_Harabasz"){
      qualit <- data.frame(k=k[1]:k[2],
                           qualityCrit=criterValues)
      id_opt <- (which(qualit[,2]==max(qualit))[1] + (k[1]-1))

      #plot
      #options(rgl.useNULL = TRUE)
      plt <- ggplot(qualit, aes(x = k, y = qualityCrit)) +
      geom_line(linetype = "dotdash") + geom_point(shape=0)+
      ggtitle(paste("Optimal solution based on the", crit,
                      "criterion: k = ", id_opt, sep=" ")) +
      geom_vline(xintercept = (which(qualit[,2]==max(qualit))[1] +(k[1]-1)),
                   linetype="dashed", color = "red", size=0.5)+
                  theme_light()

      qualiCriterion <- paste("Quality criterion:", crit, sep=" ")

      #determine optimal solution
      ##optimal_solution <- result_[[(which(qualit[,2]==max(qualit))[1])]]
      optimal_k <- qualit[,1][which(qualit[,2]==max(qualit))][1]


      ##out_msg <- paste("Suggested optimal solution contains",
                       ##round(optimal_solution, digits=0),
                       ##"clusters. See the plot for further examination!",
                       ##sep=" ")

      # #combine the results
      # final_result <- list(plt,
      #                        qualitycriterion =  qualiCriterion,
      #                        membership_optimalSolution=optimal_solution,
      #                        qualityCrit.List=qualit)
      #                       ##qualityCrit.List=qualit, message=out_msg)

      #combine the results
      if(k[1]==k[2]){
       final_result <- list(traj=traj,
                           id_field = id_field,
                           solutions=result_,
                           qualitycriterion =  qualiCriterion,
                           qualityCrit.List=qualit)
      }

      if(k[1]!=k[2]){
       final_result <- list(traj=traj,
                           id_field = id_field,
                           solutions=result_,
                           qualitycriterion =  qualiCriterion,
                           optimal_k=optimal_k,
                           qualityCrit.List=qualit,
                           qltyplot=plt)
      }

      ##qualityCrit.List=qualit, message=out_msg)


      if(quality_plot==TRUE & k[1]!=k[2]){
        #flush.console()
        #dev.new(width=3, height=3)
        options(rgl.useNULL = TRUE)
        print(plt)
      }

      class(final_result) <- c("akobject", class(final_result))

      return(final_result)

      }
    ##}
  }
}

# final_result$traj <- traj
# class(final_result) <-
#   c(
#     class(final_result),
#     'akclustr'
#   )
#
# return(final_result)
}


