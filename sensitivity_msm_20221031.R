# open packages
library(msm)

# create dataset
df_to_work_with <- df_to_impute[!is.na(df_to_impute$status_all),]

## first obs
msmdf_start <- df_to_work_with[,c("ergoid", "log2nfl")]
msmdf_start$time <- 0
msmdf_start$state <- 1
msmdf_start$firstobs <- 1

## none
msmdf_no <- df_to_work_with[df_to_work_with$status_all == 0 & df_to_work_with$status_all == 0,c("ergoid", "log2nfl", "time")]
msmdf_no$state <- 1
msmdf_no$firstobs <- 0

## got depression
msmdf_dep <- df_to_work_with[df_to_work_with$status_all == 1,c("ergoid", "log2nfl", "time")]
msmdf_dep$state <- 2
msmdf_dep$firstobs <- 0

## got dementia
msmdf_dem <- df_to_work_with[df_to_work_with$later_dementia == 1,c("ergoid", "log2nfl", "time_dementia")]
names(msmdf_dem)[3] <- "time"
msmdf_dem$state <- 3
msmdf_dem$firstobs <- 0

# bind
msmdf <- msmdf1 <- rbind(msmdf_start, msmdf_no, msmdf_dep, msmdf_dem)

## define those who got both dementia and depression
inboth <- msmdf_dep$ergoid[msmdf_dep$ergoid %in% msmdf_dem$ergoid]

## define what started first
inboth_firstdepression <- df_to_work_with[df_to_work_with$ergoid %in% inboth & df_to_work_with$time < df_to_work_with$time_dementia, "ergoid"]
inboth_firstdementia <- df_to_work_with[df_to_work_with$ergoid %in% inboth & df_to_work_with$time > df_to_work_with$time_dementia, "ergoid"]

## change their state 3 to state 4
msmdf$state <- ifelse(msmdf$ergoid %in% inboth_firstdepression & msmdf$state == 3, 4, msmdf$state)
msmdf$state <- ifelse(msmdf$ergoid %in% inboth_firstdementia & msmdf$state == 2, 4, msmdf$state)

## order data
msmdf <- msmdf[order(msmdf$time),]
msmdf <- msmdf[order(msmdf$ergoid),]

# state table
statetable.msm(state, ergoid, data = msmdf)

# get q matrix
qmat <- rbind(c(1,1,1,1),
              c(0,0,0,1),
              c(1,0,0,1),
              c(0,0,0,0))

# get msm
df.msm <- msm(state ~ time, subject = ergoid, data = msmdf, qmatrix = qmat, gen.inits = T, exacttimes = TRUE, covariates = ~ log2nfl)

Q.crude <- crudeinits.msm(state ~ years, PTNUM, data = cav, qmatrix = qmat)
