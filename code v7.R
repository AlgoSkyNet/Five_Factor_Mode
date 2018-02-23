setwd("Z:/Ferguson 13 march/other assignment/DS/Five Factor Model/Data")
ff <- read.csv("data_2016.csv", skip = 4)

# remove first empty row

ff <- ff[-1,]

# change the name of the forst column in date 

colnames(ff)[1] <- "date"

ff$date <- as.Date(ff$date,"%d-%m-%y")


#Remove column where the rows contains "INVALID CODE' or 'NO DATA FOUND'

ff2 <- Filter(function(x) !any(grepl("NO DATA VALUES",x) |
                                 grepl("INVALID CODE",x) |
                                 grepl("NO WORLDSCOPE",x) |
                                 grepl("NO DATA",x)), ff)

ff3 <- lapply(colnames(ff2), function(x) gsub("PK.","",x))

colnames(ff2) <- ff3


#move the factor into columns and companys into a single row

library(dplyr)
library(tidyr)

ff4 <- gather(ff2, vars, value, -date )

library(stringr)
ff5 <- separate(ff4, vars, c("company", "factor"),sep ="\\.")

#spread the output (http://stackoverflow.com/questions/8093839/reshape-data-for-values-in-one-column)

ff6 <- reshape(ff5, direction = 'wide', idvar = c("company","date"), timevar = 'factor', 
               v.names = 'value', sep = "_")

#clean the variable names

col_clean <- gsub("value_","", colnames(ff6))
colnames(ff6) <- col_clean

# remove the last two unwanted columns

ff6 <- ff6[,c(-12,-13)]


#Generate variable required for fama and french five factor model

#convert the variables into numeric

ff6[,3:11] <- sapply(ff6[,3:11], function(x) as.numeric(as.character(x)))

#market capitalizaion

ff6 <- ff6 %>% mutate(mc = NOSH * P)

#book to market equity

ff6 <- ff6 %>% mutate(btmv = 1/MTBV)

#profitability

ff6 <- ff6 %>% mutate(op = (WC01001 - WC01051 - WC01101)/(WC05476 * NOSH))
library(plyr)
#Investment
ff7 <- ddply(ff6,.(company), transform, inv_prev = lag(WC02999,1))

ff7 <- ff7 %>% mutate(inv = (WC02999 - inv_prev)/inv_prev)

# for consistency in the inv column

ff7$inv <- ifelse(ff7$inv == 0, NA, ff7$inv)

#carry previous value by company
library(zoo)
ff8 <- ddply(ff7, .(company), na.locf)

# calculate mean of market capitalization, book to market ratio, 
# operating profit and investment

avg_size <- mean(ff8$mc, na.rm = TRUE)

ff8[,3:16] <- sapply(ff8[,3:16], function(x) as.numeric(as.character(x)))

#To compute SMB factor
avg_size <- mean(ff8$mc, na.rm =  TRUE)

BIG <- unique(as.character(ff8$company[ff8$mc > avg_size]))
SMALL <- unique(as.character(ff8$company[ff8$mc < avg_size]))

#Get the prices into a different data frame for returns

prices <- ff8[,c(1,2,4)]

#remove the price column from the factors data frame 

ff8 <- ff8[,c(-4)]

#Make prices into wide format

prices <- spread(prices, company, P)

#remove symbol rows where the missing values are more than 50%
prices <- prices[apply(prices ,2 ,function(x) sum(!is.na(x))/nrow(prices) > .5)]


#calculate returns
returns <- cbind.data.frame(prices[-1,c(1)], lapply(prices[,c(-1)],
                                                    function(x) diff(log(x))))

colnames(returns)[1] <- 'date'

#for SMB
size <- ff8 %>% select(date,company, MV) %>% spread(date, MV)

avg_size <- lapply(size[,c(-1)], function(x) mean(x, na.rm = TRUE))


#for book to market ratio

bmRatio <- ff8 %>% select(date,company, btmv) %>% spread(date, btmv)

#calculate quantiles

bm_quan_low <- lapply(bmRatio[,-1],  function(x)
  quantile(x,probs = c(.3), na.rm = TRUE))

bm_quan_high <- lapply(bmRatio[,-1],  function(x)
  quantile(x,probs = c(.7), na.rm = TRUE))


# size and b/m bivariate sorting

#For small and high portfolio
small_high <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size 
                               & bmRatio[,c(-1)] > bm_quan_high)
colnames(small_high)[1] <- "company"

#For small and neutral portfolio
small_neutral <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                    bmRatio[,c(-1)] > bm_quan_low &
                                    bmRatio[,c(-1)] < bm_quan_high )
colnames(small_neutral)[1] <- "company"

#For small and low portfolio
small_low <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                bmRatio[,c(-1)] < bm_quan_low)
colnames(small_low)[1] <- "company"

#For big and high portfolio
big_high <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                               bmRatio[,c(-1)] > bm_quan_high)
colnames(big_high)[1] <- "company"

#For big and neutral portfolio
big_neutral <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                                  bmRatio[,c(-1)] > bm_quan_low &
                                  bmRatio[,c(-1)] < bm_quan_high )
colnames(big_neutral)[1] <- "company"

#For big and low portfolio
big_low <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                              bmRatio[,c(-1)] < bm_quan_low)
colnames(big_low)[1] <- "company"

###############For SMB for profit#############

#for operating profit ratio

opRatio <- ff8 %>% select(date,company, op) %>% spread(date, op)

#calculate quantiles

op_quan_weak <- lapply(opRatio[,-1],  function(x)
  quantile(x,probs = c(.3), na.rm = TRUE))

op_quan_robust <- lapply(opRatio[,-1],  function(x)
  quantile(x,probs = c(.7), na.rm = TRUE))




#For small and robust portfolio
small_robust <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size & opRatio[,c(-1)] > op_quan_robust)
colnames(small_robust)[1] <- "company"

#For small and neutral portfolio
small_neutral_op <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                       opRatio[,c(-1)] > op_quan_weak &
                                       opRatio[,c(-1)] < op_quan_robust )
colnames(small_neutral_op)[1] <- "company"

#For small and week portfolio
small_weak <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                 opRatio[,c(-1)] < op_quan_weak)
colnames(small_weak)[1] <- "company"
#########################################################################


#For big and robust portfolio
big_robust <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size & opRatio[,c(-1)] > op_quan_robust)
colnames(big_robust)[1] <- "company"

#For big and neutral portfolio
big_neutral_op <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                                     opRatio[,c(-1)] > op_quan_weak &
                                     opRatio[,c(-1)] < op_quan_robust )
colnames(big_neutral_op)[1] <- "company"

#For big and weak portfolio
big_weak <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                               opRatio[,c(-1)] < op_quan_weak)
colnames(big_weak)[1] <- "company"


###############For SMB for investment#############

#for investment ratio

invRatio <- ff8 %>% select(date,company, inv) %>% spread(date, inv)

#calculate quantiles

inv_quan_conservative <- lapply(invRatio[,-1],  function(x)
  quantile(x,probs = c(.3), na.rm = TRUE))

inv_quan_aggressive <- lapply(invRatio[,-1],  function(x)
  quantile(x,probs = c(.7), na.rm = TRUE))




#For small and aggresive portfolio
small_aggressive <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                       invRatio[,c(-1)] > inv_quan_aggressive)
colnames(small_aggressive)[1] <- "company"

#For small and neutral portfolio
small_neutral_inv <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                        invRatio[,c(-1)] > inv_quan_conservative &
                                        invRatio[,c(-1)] < inv_quan_aggressive )
colnames(small_neutral_inv)[1] <- "company"

#For small and conservative portfolio
small_conservative <- cbind.data.frame(size[,c(1)],size[,c(-1)] < avg_size &
                                         invRatio[,c(-1)] < inv_quan_conservative)
colnames(small_conservative)[1] <- "company"
#########################################################################

#For big and aggresive portfolio
big_aggressive <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                                     invRatio[,c(-1)] > inv_quan_aggressive)
colnames(big_aggressive)[1] <- "company"

#For big and neutral portfolio
big_neutral_inv <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                                      invRatio[,c(-1)] > inv_quan_conservative &
                                      invRatio[,c(-1)] < inv_quan_aggressive )
colnames(big_neutral_inv)[1] <- "company"

#For big and conservative portfolio
big_conservative <- cbind.data.frame(size[,c(1)],size[,c(-1)] > avg_size &
                                       invRatio[,c(-1)] < inv_quan_conservative)
colnames(big_conservative)[1] <- "company"




# 
# #get symbols
# g <- sapply(sh[,-1], function(x) sh$company[x])
# g <- lapply(g, function(x) x[!is.na(x)])
# 
# #remove the first list of g because of returns
# 
# g <- g[c(-1)]
# 
# #transpose returns
# 
# returns2 <- setNames(data.frame(t(returns[,-1])), returns[,1])
# 
# 
# #calculate the mean of the portfolio
# 
# sh <- data.frame(t(
#   sapply(g, function(x) sapply(returns2[rownames(returns2) %in% x,], function(y) mean(y, na.rm = TRUE)))[,1])
# )

#create a function to calculate the mean 
mean_return <- function(df){
  g <- sapply(df[,-1], function(x) df$company[x])
  g <- lapply(g, function(x) x[!is.na(x)])
  
  #remove the first list of g because of returns
  
  g <- g[c(-1)]
  
  #transpose returns
  
  returns2 <- setNames(data.frame(t(returns[,-1])), returns[,1])
  
  
  #calculate the mean of the portfolio
  
  
  df <- data.frame(t(
    sapply(g, function(x) sapply(returns2[rownames(returns2) %in%
                                            x,], function(y) mean(y, na.rm = TRUE)))[,1])
  ) 
  
  colnames(df) <- colnames(returns2)
  return(df)
}

#calculate the mean for each sorting
#small bm ratio
small_high <- mean_return(small_high[,-c(2:12)])
small_neutral <- mean_return(small_neutral[,-c(2:12)])
small_low <- mean_return(small_low[,-c(2:12)])

#big bm ratios
big_high <- mean_return(big_high[,-c(2:12)])
big_neutral <- mean_return(big_neutral[,-c(2:12)])
big_low <- mean_return(big_low[,-c(2:12)])
########################################################

#small op ratio
small_robust <- mean_return(small_robust[,-c(2:12)])
small_neutral_op <- mean_return(small_neutral_op[,-c(2:12)])
small_weak <- mean_return(small_weak[,-c(2:12)])

#big op ratios
big_robust <- mean_return(big_robust[,-c(2:12)])
big_neutral_op <- mean_return(big_neutral_op[,-c(2:12)])
big_weak <- mean_return(big_weak[,-c(2:12)])

######################################################

#given


#small inv ratio
small_aggressive <- mean_return(small_aggressive[,-c(2:12)])
small_neutral_inv <- mean_return(small_neutral_inv[,-c(2:12)])
small_conservative <- mean_return(small_conservative[,-c(2:12)])

#big inv ratios
big_aggressive <- mean_return(big_aggressive[,-c(2:12)])
big_neutral_inv <- mean_return(big_neutral_inv[,-c(2:12)])
big_conservative <- mean_return(big_conservative[,-c(2:12)])



#For SMB
small_bm <- rbind(small_high, small_neutral, small_low)
small_bm <- sapply(small_bm, mean)

big_bm <- rbind(big_high, big_neutral, big_low)
big_bm <- sapply(big_bm, mean)

#For SMB_BM 
smb_bm <- small_bm - big_bm


small_op <- rbind(small_weak,small_neutral_op,small_robust)
small_op <- sapply(small_op, mean)

big_op <- rbind(big_weak,big_neutral_op,big_robust)
big_op <- sapply(big_op, mean)

#For SMB_OP
smb_op <- small_op - big_op


small_inv <- rbind(small_conservative, small_neutral_inv, small_aggressive)
small_inv <- sapply(small_inv, mean)

big_inv <- rbind(big_conservative, big_neutral_inv, big_aggressive)
big_inv <- sapply(big_inv, mean)
                  
#For SMB
smb <- small_inv - big_inv


#For HML
hml_high <- rbind(small_high, big_high)
hml_high <- sapply(hml_high, mean)

hml_low <- rbind(small_low, big_low)
hml_low <- sapply(hml_low, mean)

hml <- hml_high - hml_low

#For RWN(profit)

rwn_robust <- rbind(small_robust, big_robust)
rwn_robust <- sapply(rwn_robust, mean)

rwn_weak <- rbind(small_weak, big_weak)
rwn_weak <- sapply(rwn_weak, mean)

rwn <- rwn_robust - rwn_weak

#For CMA(investmnet)

cma_conservative <- rbind(small_conservative, big_conservative)
cma_conservative <- sapply(cma_conservative, mean)


cma_aggressive <- rbind(small_aggressive, big_aggressive)
cma_aggressive <- sapply(cma_aggressive, mean)

sma <- cma_conservative - cma_aggressive

five_factor <- data.frame(t(rbind(smb, hml, rwn, sma)))

# Get the market returns and tbill

