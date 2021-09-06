#Clean all
rm(list = ls())
before_vars<-ls()

iam='main_fs'
#Dependencia basica
if(!exists("dependencyLoader")){
  if(!file.exists('functions_common/dependencyLoader.R')) return(list(error=TRUE,data=NULL,msg=paste0("\n",iam,":Missing dependency function: functions_common/dependencyLoader.R")));
  source('functions_common/dependencyLoader.R')
}

# Sources
# devtools::install_github("rstudio/keras")
# install_keras()
libraries<-c('plotly','ggplot2','xtable','keras','tensorflow','RColorBrewer','RMySQL')
sources_common<-paste0("functions_common/",c('formatter_get_tableinfo.R','db_get_event_description.R','load_wtdata.R','close_protocol.R','db_query.R','filter_custom.R','feature_selection.R'))
sources_cast<-paste0("functions_cast/",c('check_acciona_rules.R','check_acciona_rules_cm3.R'))
dep<-dependencyLoader(c(libraries,sources_common,sources_cast))
if(dep$error)  stop(paste0("\n",iam,":on call dependencyLoader\n",dep$msg))
debug_mode<-FALSE

db_config<-if(Sys.info()['nodename']=='alexsobremesa') data.frame(user="user",password="password",dbname="yourHistoricalBD",host="yourHost",port=10003) else data.frame(user="user",password="password",dbname="yourHistoricalBD",host="127.0.0.1",port=3306)

id<-9  #id model 12 izco gen day disptemp

query<-paste0('select * from 1_cast_config_compatible where id=',id)
rs<-db_query(query=query,db_config=db_config)
if(rs$error)  stop(paste0("\n",iam,":on call db_query\n",dep$msg))
wt_query<-rs$data

if(!is.null(wt_query$additional_model_params)) eval(parse(text = wt_query$additional_model_params[1]))

#LSTM
use_lstm<-ifelse(wt_query$type[1]=='deep_lstm',T,F)
#anticipation<-ifelse(is.null(wt_query$anticipation[1]),0,wt_query$anticipation[1])
use_alarms<-grepl(pattern = 'alarm',x = wt_query$target_name[1])
use_ots<-grepl(pattern = 'ot',x = wt_query$target_name[1])
use_health<-grepl(pattern = 'health',x = wt_query$target_name[1])
date_time_name<-'date_time'
if(!exists('tmpfolder',inherits = F)) tmpfolder<-'tmp_fs'

ifelse(!dir.exists(tmpfolder), dir.create(tmpfolder), FALSE)

#anticipation<-ifelse(is.null(wt_query$anticipation[1]),0,wt_query$anticipation[1])
rs  <-  load_wtdata(wt_query=wt_query,
                    date_time_name=date_time_name,
                    target_name=NULL,
                    filter_exclude=paste(date_time_name,"nrecords,ot_block_code,ot_all,ot_all_block_code,production,weekly_production,ld_id,alarm,alarm_block_code,alarm_all,alarm_all_block_code,ot,ot_block_code,ot_all,ot_all_block_code,n1,weekly_n1,weekly_power",sep=","),
                    update_filter_ranges=F,
                    filter_verbose=F,
                    db_config=db_config)
if(rs$error) stop(paste0("\n",iam,":on call load_wtdata\n\t",rs$msg))

wtdata<-rs$data$wtdata
outliers<-rs$data$outliers
rm(rs)
target_name<-wt_query$target_name[1]

#OT block code code to string
if('ot_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$ot_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  ot_table_dic_name<-rs$data$ot_table_dic_name
  if(!is.null(ot_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), ot_table_dic_name,all_info=TRUE, target = "ot", db_config=db_config)
    ot_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$ot_block_code<-gsub(wtdata$ot_block_code,pattern = c,replacement = ot_desc$descripcio_walm[c==ot_desc$id_ot])
    }
  }
}

if('ot_all_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$ot_all_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  ot_table_dic_name<-rs$data$ot_table_dic_name
  if(!is.null(ot_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), ot_table_dic_name,all_info=TRUE, target = "ot", db_config=db_config)
    ot_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$ot_all_block_code<-gsub(wtdata$ot_all_block_code,pattern = c,replacement = ot_desc$descripcio_walm[c==ot_desc$id_ot])
    }
  }
}

#alarm block code to string
if('alarm_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$alarm_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  alarms_table_dic_name<-rs$data$alarms_table_dic_name
  if(!is.null(alarms_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), alarms_table_dic_name,all_info=TRUE, target = "alarm", db_config=db_config)
    alarm_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$alarm_block_code<-gsub(wtdata$alarm_block_code,pattern = c,replacement = alarm_desc$descripcio_walm[c==alarm_desc$id_walm])
    }
  }
}

#alarm block code to string
if('alarm_all_block_code' %in% names(wtdata)){
  #Get descriptions
  codes<-unique(unlist(strsplit(wtdata$alarm_all_block_code,split = ',')))
  codes<-codes[codes!="NA"]
  rs<-formatter_get_tableinfo(table_cast_park_dic='1_cast_park_table_dic',wp_id=wt_query$wp_id[1],db_config=db_config)
  if(rs$error) stop('downloading formatter_get_tableinfo')
  alarms_table_dic_name<-rs$data$alarms_table_dic_name
  if(!is.null(alarms_table_dic_name)){
    rs<-db_get_event_description(paste0(codes,collapse = ','), alarms_table_dic_name,all_info=TRUE, target = "alarm", db_config=db_config)
    alarm_desc<-rs
    #Replace id with descriptions
    for(c in codes){
      wtdata$alarm_all_block_code<-gsub(wtdata$alarm_all_block_code,pattern = c,replacement = alarm_desc$descripcio_walm[c==alarm_desc$id_walm])
    }
  }
}

if(scale_reference_ld){
  #Scale versus reference and move min values to offset 0.
  scale_excluded_columns<-c('ld_id','date_time','ot','alarm','ot_all','alarm_all','ot_block_code','alarm_block_code','ot_all_block_code','alarm_all_block_code')
  reference_ld_id<-abs(as.numeric(unlist(strsplit(wt_query$train_machines[1],split = ','))[1]))
  others_ld_id<-unique(wtdata$ld_id[wtdata$ld_id!=reference_ld_id])
  scale_factors<-data.frame(ld_id=as.numeric(),variable=as.character(),offset=as.numeric(),min=as.numeric(),max=as.numeric())
  for(c in 1:ncol(wtdata)){
    if(!(colnames(wtdata)[c] %in% scale_excluded_columns)&&is.numeric(wtdata[,c])){
      min_ref<-min(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      max_ref<-max(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      original_min_ref<-min_ref
      original_max_ref<-max_ref
      offset_ref<-ifelse(min_ref<0,abs(min_ref),-1*min_ref)
      scale_factors<-rbind(scale_factors,data.frame(reference_ld_id=reference_ld_id,ld_id=reference_ld_id,variable=colnames(wtdata)[c],offset=offset_ref,min=min_ref,max=max_ref))
      wtdata[wtdata$ld_id==reference_ld_id,c]<-wtdata[wtdata$ld_id==reference_ld_id,c]+offset_ref
      min_ref<-min(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      max_ref<-max(wtdata[wtdata$ld_id==reference_ld_id,c],na.rm = T)
      for(ld in others_ld_id){
        min_cur<-min(wtdata[wtdata$ld_id==ld,c],na.rm = T)
        max_cur<-max(wtdata[wtdata$ld_id==ld,c],na.rm = T)
        offset_cur<-ifelse(min_cur<0,abs(min_cur),-1*min_cur)
        scale_factors<-rbind(scale_factors,data.frame(reference_ld_id=reference_ld_id,ld_id=ld,variable=colnames(wtdata)[c],offset=offset_cur,min=min_cur,max=max_cur))
        wtdata[wtdata$ld_id==ld,c]<-wtdata[wtdata$ld_id==ld,c]+offset_cur
        wtdata[wtdata$ld_id==ld,c]<-((((wtdata[wtdata$ld_id==ld,c]*100)/max_cur)*max_ref)/100)
      }
    }
  }
}

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
#to_save<-c('wtdata','wt_query','outliers','date_time_name','query','scale_reference_ld','scale_factors','anticipation','use_alarms','use_ots','use_health','tmpfolder','target_name')
save(list=to_save,file=paste0(tmpfolder,'/wtdata_',id,'_',wt_query$wp_code[1],'_',wt_query$seconds_to_aggregate[1],'s_ant',anticipation,ifelse(use_health,'_health',''),ifelse(use_alarms,'_alarms',''),ifelse(use_ots,'_ots',''),'.RData'),compress = 'xz')
rm(list=to_save)
gc(verbose = F)

############################## Train Test Separation ###################################
#Por temperatura izco
#Grupo1 nov,dic,Enero,febrero,marzo,abril,mayo Grupo 2 Junio,julio,agosto,sept,oct
#wtdata2<-wtdata[,'date_time',drop=F] %>% mutate(month = format(date_time,"%m"))
#wtdata2$month<-as.numeric(wtdata2$month)
#wtdata_inv<-wtdata[wtdata2$month %in% c(1,2,3,4,5,11,12),]
setwd("~/Dropbox/phd/experimentos/deep_learning")
if(!exists('tmpfolder',inherits = F)) tmpfolder='tmp_fs_moncaymulti_rbm'
before_vars<-ls()
tmp_env<-new.env()
load(file =paste0(tmpfolder,'/wtdata.RData') ,envir = tmp_env)
wtdata<-tmp_env$wtdata
target_name<-tmp_env$target_name
anticipation<-tmp_env$anticipation
margin<-if(is.null(tmp_env$margin)) 30 else tmp_env$margin
use_alarms<-tmp_env$use_alarms
use_ots<-tmp_env$use_ots
use_health<-tmp_env$use_health
seconds_to_aggregate<-tmp_env$wt_query$seconds_to_aggregate[1]
use_lstm<-tmp_env$use_lstm
if(!is.null(tmp_env$wt_query$train_machines[1]) && grepl(pattern = '[-]?[0-9]+,',x = tmp_env$wt_query$train_machines[1])){
  train_machines<-as.numeric(unlist(strsplit(tmp_env$wt_query$train_machines[1],split = ',')))
}else{
  train_machines<-tmp_env$wt_query$train_machines[1]
}
balance<-if(exists('balance')) tmp_env$balance else tmp_env$balance_per
balance_by<-tmp_env$balance_by
rm(tmp_env)
gc(verbose = F)

#train_machines<-50 #50% for train 50% for test.
#train_machines<-abs(train_machines)

if(!exists('preprocess_data')) source('preprocess_data.R')

rs<-preprocess_data(wtdata=wtdata,selected_turbines=train_machines,
                    target_name='pre_alarm',use_alarms=use_alarms,
                    use_ots=use_ots,use_health=use_health,anticipation=anticipation,
                    margin=margin,seconds_to_aggregate=seconds_to_aggregate,
                    marging_after_pre_alarm=60*60*24*7,
                    seconds_ot_marging=60*60*24,
                    registers_before_pre_alarm=F,
                    na_columns_per_exclude=5)
if(rs$error) stop('error in preprocess_data')  
wtdata<-rs$data$wtdata
selected_rows<-rs$data$selected_rows
selected_turbines<-rs$data$selected_turbines
zero_sdv_columns_names<-rs$data$zero_sdv_columns_names
na_columns_names<-rs$data$na_columns_names
selected_columns<-rs$data$selected_columns
normalized<-rs$data$normalized
stat<-rs$data$stat

### Code for debug selected.
# wtdata$pre_alarm<-F
# for(ld in unique(pre_alarm$ld_id)){
#   dates<-pre_alarm$date_time[pre_alarm$ld_id==ld&pre_alarm$pre_alarm==1]
#   if(length(dates)>0){
#     wtdata$pre_alarm[(wtdata$ld_id==ld)&(wtdata$date_time %in% dates)]<-T
#   }
# }
# wtdata$selected<-F
# for(s in rs$data$selected_rows){
#   wtdata$selected[(wtdata$date_time == rs$data$wtdata$date_time[s])&(wtdata$ld_id==rs$data$wtdata$ld_id[s])]<-T
# }
# wtdata$status<-'no_selected'
# wtdata$status[(wtdata$selected==T)]<-'selected'
# wtdata$status[(wtdata$pre_alarm==T)]<-'pre_alarm'
# wtdata$status[(wtdata$ot==T)]<-paste0(wtdata$status[(wtdata$ot==T)],'_ot')
# df<-wtdata[,c('date_time','ld_id','status')]
# df<-df[df$ld_id %in% abs(train_machines),]
# df$ld_id<-as.factor(df$ld_id)
# ggplot(df,aes(x=date_time,y=ld_id,color=status))+geom_point()
# ggplotly(ggplot(df,aes(x=date_time,y=ld_id,color=status))+geom_point())

to_save<-ls()[sapply(ls(),function(var) !is.function(get(var)))]
to_save<-to_save[!(to_save %in% before_vars)]
#save(list=to_save,file=paste0(tmpfolder,'/prepare_train.RData'),compress = 'xz')
#rm(list=ls()[!(ls() %in% before_vars)])
#gc(verbose = F)

############################################ VAE #################################################

#' This script demonstrates how to build a variational autoencoder with Keras.
#' Reference: "Auto-Encoding Variational Bayes" https://arxiv.org/abs/1312.6114

library(keras)
K <- keras::backend()

# Parameters --------------------------------------------------------------

batch_size <- 100L
original_dim <- length(selected_columns)
latent_dim <- 2L
intermediate_dim <- length(selected_columns)/2
epochs <- 100L
epsilon_std <- 1.0

# Model definition --------------------------------------------------------
backend_normalize_shape <- function(shape) {
  # if it's a Python object or a list with python objects then leave it alone
  if (inherits(shape, "python.builtin.object"))
    return(shape)
  
  if (is.list(shape)) {
    if (any(sapply(unlist(shape), function(x) inherits(x, "python.builtin.object"))))
      return(shape)
  }
  normalize_shape(shape)
}

as_nullable_integer <- function(x) {
  if (is.null(x))
    x
  else
    as.integer(x)
}

k_random_normal <- function(shape, mean = 0.0, stddev = 1.0, dtype = NULL, seed = NULL) {
  K$random_normal(
    shape = backend_normalize_shape(shape),
    mean = mean,
    stddev = stddev,
    dtype = dtype,
    seed = as_nullable_integer(seed)
  )
}

k_shape <- function(x) {
  K$shape(
    x = x
  )
}

k_exp <- function(x) {
  K$exp(
    x = x
  )
}

k_mean <- function(x, axis = NULL, keepdims = FALSE) {
  K$mean(
    x = x,
    axis = as_axis(axis),
    keepdims = keepdims
  )
}

as_axis <- function(axis) {
  if (length(axis) > 1) {
    sapply(axis, as_axis)
  } else {
    axis <- as_nullable_integer(axis)
    if (is.null(axis))
      axis
    else if (axis == -1L)
      axis
    else
      axis - 1L
  }
}

k_square <- function(x) {
  K$square(
    x = x
  )
}

x <- layer_input(shape = c(original_dim))
h <- layer_dense(x, intermediate_dim, activation = "relu")
z_mean <- layer_dense(h, latent_dim)
z_log_var <- layer_dense(h, latent_dim)

sampling <- function(arg){
  z_mean <- arg[, 1:(latent_dim)]
  z_log_var <- arg[, (latent_dim + 1):(2 * latent_dim)]
  
  epsilon <- k_random_normal(
    shape = c(k_shape(z_mean)[[1]]), 
    mean=0.,
    stddev=epsilon_std
  )
  
  z_mean + k_exp(z_log_var/2)*epsilon
}

# note that "output_shape" isn't necessary with the TensorFlow backend
z <- layer_concatenate(list(z_mean, z_log_var)) %>% 
  layer_lambda(sampling)

# we instantiate these layers separately so as to reuse them later
decoder_h <- layer_dense(units = intermediate_dim, activation = "relu")
decoder_mean <- layer_dense(units = original_dim, activation = "sigmoid")
#decoder_mean <- layer_dense(units = original_dim, activation = "sig")
h_decoded <- decoder_h(z)
x_decoded_mean <- decoder_mean(h_decoded)

# end-to-end autoencoder
vae <- keras_model(x, x_decoded_mean)

# encoder, from inputs to latent space
encoder <- keras_model(x, z_mean)

# generator, from latent space to reconstructed inputs
decoder_input <- layer_input(shape = latent_dim)
h_decoded_2 <- decoder_h(decoder_input)
x_decoded_mean_2 <- decoder_mean(h_decoded_2)
generator <- keras_model(decoder_input, x_decoded_mean_2)


vae_loss <- function(x, x_decoded_mean){
  xent_loss <- (original_dim/1.0)*loss_binary_crossentropy(x, x_decoded_mean)
  kl_loss <- -0.5*k_mean(1 + z_log_var - k_square(z_mean) - k_exp(z_log_var), axis = -1L)
  xent_loss + kl_loss
}

vae %>% compile(optimizer = "adam", loss = vae_loss)


# Data preparation --------------------------------------------------------

# mnist <- dataset_mnist()
# x_train <- mnist$train$x/255
# x_test <- mnist$test$x/255
# x_train <- x_train %>% apply(1, as.numeric) %>% t()
# x_test <- x_test %>% apply(1, as.numeric) %>% t()
x_train<-as.matrix(wtdata[selected_rows,selected_columns])

x_train<-apply(x_train,2,function(x) (x-min(x))/(max(x)-min(x)))

test_selected<-which(!((1:nrow(wtdata)) %in% selected_rows))
x_test<-wtdata[!((1:nrow(wtdata)) %in% selected_rows),selected_columns]

complete_cases_test<-complete.cases(x_test)
x_test<-as.matrix(x_test[complete_cases_test,])
test_selected<-test_selected[complete_cases_test]

x_test<-apply(x_test,2,function(x) (x-min(x))/(max(x)-min(x)))
# Model training ----------------------------------------------------------

vae %>% fit(
  x_train, x_train, 
  shuffle = TRUE, 
  epochs = epochs, 
  batch_size = batch_size,
  validation_data = list(x_test, x_test),
  verbose=1  
)


# Visualizations ----------------------------------------------------------

library(ggplot2)
library(dplyr)
#x_test_encoded <- predict(encoder, x_test, batch_size = batch_size)
x_test_encoded<-encoder$predict(x_test,batch_size = batch_size)

x_test_e<-x_test_encoded %>%
  as_data_frame() %>% 
  mutate(class = as.factor(wtdata$pre_alarm[test_selected]))
p<-ggplot(x_test_e,aes(x = V1, y = V2, colour = class)) + geom_point()

ggplotly(p)

v1<-x_test_e$V1
sdv<-sd(v1)
m<-mean(v1)
out<-which((v1<(m-sdv))|(v1>(m+sdv)))


a<-generator$predict(x_test)

# display a 2D manifold of the digits
n <- 15  # figure with 15x15 digits
digit_size <- 28

# we will sample n points within [-4, 4] standard deviations
grid_x <- seq(-4, 4, length.out = n)
grid_y <- seq(-4, 4, length.out = n)

rows <- NULL
for(i in 1:length(grid_x)){
  column <- NULL
  for(j in 1:length(grid_y)){
    z_sample <- matrix(c(grid_x[i], grid_y[j]), ncol = 2)
    column <- rbind(column, predict(generator, z_sample) %>% matrix(ncol = 28) )
  }
  rows <- cbind(rows, column)
}
rows %>% as.raster() %>% plot()
