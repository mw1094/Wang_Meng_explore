library(ggplot2)
explore <- function(data, plotswitch = "off", threshold = 0, bins = NULL) {
  
  Freq_table <- freq_table (data)
  Summary_num <- summary_num (data)
  R_squared <- r_squared (data)
  Pearson_coe <- pearson_coe (data, threshold)
  plot_gray (data, plotswitch)
  num <- data[sapply(data, is.numeric)]
  plot_density_count(num,plotswitch,vector)
  return (list(Freq_table, Summary_num, R_squared, Pearson_coe))
  
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

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

plot_density_count <- function(num,switch,vector){
  if(switch == "on"){
    for(j in 1:length(vector)){
      for(i in 1:ncol(num)){
        mean <- mean(num[,i])
        p1 <- ggplot(num,aes(x=num[i]),color = "blue")+
          geom_histogram(fill="blue",bins=vector[j])+
          ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        p2 <- ggplot(num,aes(x=num[i],..density..))+
          geom_histogram(fill="blue",bins=vector[j])+
          ggtitle(paste(colnames(num[i]),vector[j],sep=" bins="))+
          xlab(colnames(num[i]))+
          geom_vline(xintercept = mean,col="red")
        grid.newpage()
        pushViewport(viewport(layout = grid.layout(2, 2, heights = unit(c(1, 8), "null"))))
        title <- paste(colnames(num[i]),vector[j],sep=" bin=")
        grid.text(title, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:2))
        print(p1, vp = viewport(layout.pos.row = 2, layout.pos.col = 1))
        print(p2, vp = viewport(layout.pos.row = 2, layout.pos.col = 2))
      }
    }
  }else{
    if(switch == "grid"){
      for(j in 1:length(vector)){
        grid.newpage()
        his_count <-list()
        his_density <- list()
        for(i in 1:ncol(num)){
          his_count[[i]] <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(fill="blue", bins = vector[j])+ 
            labs(title= paste(vector[j], "bins"))
        }
        multiplot(plotlist = his_count, cols = 2)  
        for(i in 1:ncol(num)){
          his_density <- ggplot(num, aes_string(colnames(num[i])), color = "blue") + 
            geom_histogram(aes(y= ..density..), fill="blue", bins = vector[j])+ 
            labs(title= paste(vector[j], "bins"))
        }
        multiplot(plotlist = his_density, cols = 2)  
      }
    }
  }
}


#4. If the plot switch parameter is "on" or "grid", plot a gray bar
#graph for every categorical and binary variable.
is.binary <- function(v) {
  x <- unique(v)                    
    #x contains all unique values in v
  length(x) - sum(is.na(x)) == 2L         
    #check to see if x only contains 2 distinct values
}
plot_gray <- function(data, plotswitch = "off") {
  dfm_cb <- data[,sapply(data,is.factor)|sapply(data,is.logical)|sapply(data,is.binary)]
  #select categorical and binary variable
  if(plotswitch=="on"|plotswitch=="grid"){
    for(i in 1:ncol(dfm_cb)){
      p <- ggplot(dfm_cb,aes(x=dfm_cb[,i]),colour="gray")+
        geom_bar()+ xlab(colnames(dfm_cb[i]))
        #plot gray bar for every categorial and binary variable
      print(p)
    }
  }
}