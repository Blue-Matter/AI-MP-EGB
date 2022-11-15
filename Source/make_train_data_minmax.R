# Make training data

TD<-readRDS("C:/temp/Sim_Data/simdataL4.rda")
keep<-apply(TD,1,function(x)!is.na(sum(x)))&apply(TD,1,function(x)!any(x==-Inf))
sum(keep)/length(keep)
TD<-TD[keep,]
high<-array(rep(apply(TD,2,quantile,p=0.995),each=nrow(TD)),dim(TD))
keep2<-apply(TD<high,1,sum)==ncol(TD)
sum(keep2)/length(keep2)
TD<-TD[keep2,]

keep3<-TD[,1]<40000 & TD[,1]>100
sum(keep3)/length(keep3)
TD<-TD[keep3,]
TD[,1]<-TD[,1]/1000
hist(TD[,1])

# Log independent and dependent variables
TD<-log(TD)
hist(TD[,1])

if(!is.null(TDsmall))if(TDsmall)TD<-TD[1:100,]

nr<-nrow(TD)
nc<-ncol(TD)
ind<-(1:nr)%in%sample(1:nr,floor(nr*0.05),replace=FALSE)

train_data<-TD[!ind,2:nc]
train_labels<-TD[!ind,1]
test_data<-TD[ind,2:nc]
test_labels<-TD[ind,1]

# Check for NAs # for(i in 1:ncol(train_data))print(sum(is.na(train_data)))

column_names <- colnames(TD)[2:nc]


train_df <- train_data %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(column_names) %>%
  mutate(label = train_labels)

test_df <- test_data %>%
  as_tibble(.name_repair = "minimal") %>%
  setNames(column_names) %>%
  mutate(label = test_labels)

spec <- feature_spec(train_df, label ~ . ) %>%
  step_numeric_column(all_numeric(), normalizer_fn = scaler_min_max()) %>%
  fit()

layer <- layer_dense_features(
  feature_columns = dense_features(spec),
  dtype = tf$float32
)

layer(train_df)

print("Returning train_df and test_df")
