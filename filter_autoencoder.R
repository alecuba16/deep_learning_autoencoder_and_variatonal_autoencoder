filter_autoencoder<-function(model=NULL,wtdata=NULL,epochs=100,batch_size=20,date_time_name='date_time',target_name='alarm',exclude_variables=NULL,sdv=NULL,mean=NULL,zero_sdv_columns=NULL,view_metrics=T,verbose=T){
    iam=match.call()[[1]]
    library(keras)
    
    #Bk date_time
    if(!is.na(date_time_name)&&!is.null(date_time_name)){
        date_time_bk<-wtdata[,date_time_name]
        wtdata[,date_time_name]<-NULL
    }
    #Bk target
    if(!is.na(target_name)&&!is.null(target_name)&&(target_name %in% names(wtdata))){
        target_variable_bk<-wtdata[,target_name]
        wtdata[,target_name]<-NULL
    }
    #bk ld_id
    if('ld_id' %in% names(wtdata)){
        ld_id_column<-wtdata$ld_id
        wtdata$ld_id<-NULL
    }
    
    #Exclude columns
    if(!is.null(exclude_variables)&&!is.na(exclude_variables)){
        if(length(exclude_variables)==1) exclude_variables<-unlist(strsplit(exclude_variables,split = ','))
        if(any(names(wtdata) %in% exclude_variables)){
            excluded_variables<-wtdata[,(names(wtdata) %in% exclude_variables),drop=F]
            wtdata[,(names(wtdata) %in% exclude_variables)]<-NULL
        }
    }
    
   
    #Check if all columns all numeric
    classes<-unlist(lapply(wtdata,function(x) is.numeric(x)))
    if(!all(classes)) return(list(error=TRUE,data=NULL,msg=paste0('\nColumns: ',paste0(colnames(wtdata)[!classes],collapse = ','),' are not numeric type')))
    
    #Standarize save for test
    if(is.null(sdv)||is.null(mean)){
        sdv<-apply(wtdata,2,sd,na.rm=T)
        zero_sdv_columns<-(sdv==0)
        sdv<-sdv[!zero_sdv_columns]
        mean<-apply(wtdata[,!zero_sdv_columns],2,base::mean,na.rm=T)
    }
    
    if(sum(zero_sdv_columns)>0) cat(paste0("\nThis variables have zero standard deviation: ",paste0(colnames(wtdata)[zero_sdv_columns],collapse=',')))
    #Standarize save for test
    wtdata<-wtdata[,!zero_sdv_columns]
    for(c in 1:ncol(wtdata)){
        wtdata[,c]<-(wtdata[,c]-mean[c])/sdv[c]
    }
    
    
    #Remove columns with >10% NA
    per<-0.1
    to_remove<-apply(wtdata,2,function(c){sum(is.na(c))>(per*length(c))})
    if(sum(to_remove)>0){
      cat(paste0("\nThis variables have more than ",100*per," of NA's: ",paste0(colnames(wtdata)[to_remove],collapse=',')))
      wtdata[,to_remove]<-NULL
    }
    
    
    cat(paste0("\nInput features: ",paste0(colnames(wtdata),collapse=','),'\n'))
    colnames_bk<-colnames(wtdata)
    
    wtdata<-as.matrix(wtdata)
    selected_rows<-complete.cases(wtdata)
    wtdata<-wtdata[selected_rows,]
    
    #Update selected_rows
    if(exists("date_time_bk",inherits = F)) date_time_bk<-date_time_bk[selected_rows]
    if(exists("target_variable_bk",inherits = F)) target_variable_bk<-target_variable_bk[selected_rows]
    if(exists("ld_id_column",inherits = F)) ld_id_column<-ld_id_column[selected_rows]
    if(exists("excluded_variables",inherits = F)) excluded_variables<-excluded_variables[selected_rows,]
    
    
    if(is.null(model)||is.na(model)){ #Create new model
        
        set.seed(1)
        library(tensorflow)
        
        # TensorFlow session configuration that uses only a single thread. Multiple threads are a 
        # potential source of non-reproducible results, see: https://stackoverflow.com/questions/42022950/which-seeds-have-to-be-set-where-to-realize-100-reproducibility-of-training-res
        session_conf <- tf$ConfigProto(intra_op_parallelism_threads = as.integer(parallel:::detectCores()/1.5), inter_op_parallelism_threads = as.integer(parallel:::detectCores()/1.5))
        
        # Set TF random seed (see: https://www.tensorflow.org/api_docs/python/tf/set_random_seed)
        tf$set_random_seed(1)
        
        # Create the session using the custom configuration
        sess <- tf$Session(graph = tf$get_default_graph(), config = session_conf)
        
        # Instruct Keras to use this session
        K <- backend()
        K$set_session(sess)
        
        
        #Modelo
        model <- keras_model_sequential() 
        
        model %>% 
            layer_dense(units = ceiling(ncol(wtdata)/2), activation='relu', input_shape=c(ncol(wtdata)),name = 'encoder1') %>%
            layer_dense(units = ceiling(ncol(wtdata)/4), activation='relu',name = 'encoder2') %>%
            layer_dense(units = ceiling(ncol(wtdata)/8), activation='relu',name = 'encoder3') %>%
            layer_dense(units = ceiling(ncol(wtdata)/4), activation='relu',name = 'dencoder1') %>%
            layer_dense(units = ceiling(ncol(wtdata)/2), activation='relu',name = 'dencoder2') %>%
            layer_dense(units = ncol(wtdata), activation='tanh',name = 'dencoder3')
        summary(model)
        
        metric_rmse<-function(y_true, y_pred){
            return(K$sqrt(K$mean(K$square(y_pred - y_true))))
        }
        
        model %>% compile(
            loss =metric_rmse,
            optimizer = keras::optimizer_adadelta(),
            metrics ='mape'
        )
        
        #Train
        history <- model %>% fit(wtdata,wtdata,epochs = epochs,batch_size = batch_size,view_metrics=view_metrics,verbose=verbose)
    }
    
    #Reconstruct data
    wtdata<-model %>% predict(wtdata)
    
    #De normalize
    for(c in 1:ncol(wtdata)) wtdata[,c]<-(wtdata[,c]*sdv[c])+mean[c]
    
    wtdata<-as.data.frame(wtdata)
    colnames(wtdata)<-colnames_bk
    
    #Add the backup columns
    if(exists("date_time_bk",inherits = F)) wtdata[,date_time_name]<-date_time_bk
    if(exists("target_variable_bk",inherits = F)) wtdata[,target_name]<-target_variable_bk
    if(exists("ld_id_column",inherits = F)) wtdata$ld_id<-ld_id_column
    #Exclude columns
    if(exists("excluded_variables",inherits = F)) wtdata[,colnames(excluded_variables)]<-excluded_variables
    
    return(list(error=FALSE,data=list(model=model,wtdata=wtdata,sdv=sdv,mean=mean,history=history)))
}