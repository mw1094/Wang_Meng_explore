library(ggplot2)
explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  
  Freq_table <- freq_table (data)
  Summary_num <- summary_num (data)
  R_squared <- r_squared (data)
  Pearson_coe <- pearson_coe (data, threshold)
  
  Num <- data[sapply(data, is.numeric)]
  plotsBlue <- plotsNum (Num, plotswitch, bins)
  Plot_gray <- plot_gray (data, plotswitch)
  
  return (c(Freq_table, Summary_num, R_squared, Pearson_coe, plotsBlue, Plot_gray))
  
}




##1.Create a frequency table for every categorical and logical variable
freq_table <- function(data){
  data_cat <- c(data[sapply(data,is.factor)],data[sapply(data,is.logical)])
    #select categorical and logical variable
  return (sapply(data_cat,table)) #make a table
}

##2
#a).Create a summary statistics table for each numerical variable
summary_num <- function(data){
  data_num=data[sapply(data,is.numeric)] #select numeric data
  return (summary(data_num)) #summary statistics
}

#b).Create a data frame that contains each pair of column names in
#the first column (name the column "Variable Pairs") and the
#associated r-square value in the second column (name the
#column "R-Square").
r_squared <- function(data) {
  data_num <- data[sapply(data, is.numeric)] # Select numeric data
  colname <- colnames(data_num) # extract column names
  pairwise_rsquared <- c() # new empty r-squre data
  pairwise_names <- c() # new empty pairnames
  for (i in 1:(length(colname)-1)) {
    for (j in (i+1):length(colname)) { #two column names
      num_rsqaured <- summary(lm(data_num[,i]~data_num[,j]))$r.squared
        # get r-squared data using linear model r.squared
      pairwise_names <- c(pairwise_names, paste(colname[i], colname[j], sep="-"))
        # add pairnames to pairwise_names
      pairwise_rsquared <- c(pairwise_rsquared, num_rsqaured)
        # add r-squared data to pairwise_r_squared
    }
  }
  data_rsquared <- data.frame(pairwise_names, pairwise_rsquared)
  colnames(data_rsquared) <- c("Variable Pairs", "R-squared")
  return (data_rsquared)
}  

#c).Create Pearson Correlation Coefficient
pearson_coe <- function(data, threshold = 0) {
  data_num <- data[sapply(data, is.numeric)] # select numeric data
  comb_names <- combn(colnames(data_num), 2) # combinations of all names
  pairwise_names <- paste(comb_names[1,], comb_names[2, ], sep = "-") 
    # add "-" in names e.g. x-y
  temp <- cor(data_num, method = "pearson")
    # derive pearson correlation coefficient data using cor function
  cor_data <- temp[which(lower.tri(temp))]  
    # use data in lower triangular of matrix to aviod double-use same data
  dfm_new <- data.frame(pairwise_names, cor_data)
    # create a new dataframe data_coe
  dfm_new <- subset(dfm_new, abs(cor_data) > threshold)
    # select absolute value of correlation greater than threshold
  colnames(dfm_new) <- c("Variable Pairs", "Pearson Exceeds Threshold")
  return(dfm_new)
}

#3.plot a pair of blue histograms with a vertical red line at the mean (one using
#counts and the other density) for every numerical variable at each number of bins
#integer specified in the bin vector parameter.
plotsNum <- function(data, plotswitch, bins=NULL) {
  data_frame <- data[sapply(data, is.numeric)]
  for(j in 1:length(bins)){
    if (plotswitch == "on") {
      for(i in 1:ncol(data_frame)){
        print(ggplot(data_frame,aes(x=data_frame[,i],..density..))+
                geom_histogram(bins=bins[j],fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
        # plot a density histogram for each bin size
        
        print(ggplot(data_frame,aes(x=data_frame[,i]))+
                geom_histogram(bins=bins,fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
        # plot a count histogram for each bin size
      }
    }
    else if (plotswitch == "grid") {
      plot.new()
      layout(matrix(c(1:10), 2, 5, byrow = TRUE))
      for(i in 1:ncol(data_frame)){
        print(ggplot(data_frame,aes(x=data_frame[,i],..density..))+
                geom_histogram(bins=bins[j],fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
      }
      
      plot.new()
      layout(matrix(c(1:10), 2, 5, byrow = TRUE))
      for(i in 1:ncol(data_frame)){
        print(ggplot(data_frame,aes(x=data_frame[,i]))+
                geom_histogram(bins=bins[j],fill="blue")+
                geom_vline(xintercept=mean(data_frame[,i]),color="red")+
                xlab(colnames(data_frame)[i]))
      }
      
    }
    else {
    }
  }
}

#4. If the plot switch parameter is "on" or "grid", plot a gray bar
#graph for every categorical and binary variable.
plot_gray <- function(data, plotswitch = "off") {
  dfm_cb <- data[,sapply(data,is.factor)|sapply(data,is.logical)]
  #select categorical and binary variable
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(dfm_cb)){
      grid.newpage()
      p <- ggplot(dfm_cb,aes(x=data[,i]),colour="gray")+
        geom_bar()+ xlab(colnames(dfm_cb[i]))
        #plot gray bar for every categorial and binary variable
      print(p)
    }
  }
}