run_models <- function(training, testing,
                       response.name, pred.names=setdiff(names(training), response.name),
                       models = c("knn","nnet","rf","ctree","LogitBoost","plr","bayesglm","rl","ada","gbm"),
                       indicator.class="T", listpar){
  library(caret)
  # data(credit)
  # credit$bad <- ifelse(credit$bad, "T", "F")
  # run_models(credit, credit, response.name="bad")
  
  listparo <- list(rf = list(method = "rf", par = data.frame(mtry=3)),
                   rl = list(method = "plr", par = data.frame(cp="aic", lambda=1e-4)),
                   plr = list(method = "plr", par = data.frame(cp="aic", lambda=10)),
                   nnet = list(method = "nnet", par =data.frame(size=5, decay=0.1)),
                   LogitBoost = list(method = "LogitBoost", par =data.frame(nIter=100)),
                   ctree = list(method = "ctree", par =data.frame(mincriterion=0.95)),
                   #evtree = list(method = "evtree", par =data.frame(alpha=1)),
                   bayesglm = list(method = "bayesglm", par =NULL),
                   ada = list(method = "ada", par =data.frame(iter = 100, maxdepth = 10, nu = 1)),
                   knn = list(method = "knn", par =data.frame(k = 1)),
                   gbm = list(method = "gbm", par =data.frame(n.trees=20,interaction.depth=20 , shrinkage=0.5)))
  
  if(!missing(listpar)){
    for(n in names(listpar)){
      listparo[[n]] <- listpar[[n]]
    }
  }
  
  res <- data.frame(nvars = rep(seq(length(pred.names)), each=length(models)),
                    model = rep(models, length(pred.names)), stringsAsFactors=FALSE)
  res <- dlply(res, .variables= .(nvars, model))
  
  res <- ldply(res, function(x){
    message(sprintf("########## Modelo %s con %s vars", x$model, x$nvars))
    t <- Sys.time()   
    f <- as.formula(paste(response.name, paste(pred.names[seq(x$nvars)], collapse=" + "), sep =" ~ "))
    message("\t\t\t\tAjustando")
    if(x$model %in% c("ada","rf") & x$nvars == 1){ return(x) }
    if(!x$model %in% c("ada")){
      # x <- data.frame(nvars=2, model = "gbm", stringsAsFactors=FALSE)
      # f <- as.formula("DESERCION~COMU_RES_TRUNC+DESC_CARRERA")     
      mod <- train(f, method=listparo[[x$model]][["method"]], data = training,
                   tuneGrid=listparo[[x$model]][["par"]], trControl=trainControl(method="none"))
      preds <- predict(mod, newdata=testing, type ="prob")[, 2]
    }else{
      mod<-ada(f,data=training)  
      preds <- predict(mod, newdata=testing, type ="prob")[, 2]
    }
    
    message("\t\t\t\tCalculando Indicadores")
    sp <- summary_predictions(predictions=preds,
                              targets = ifelse(as.character(testing[[response.name]])==indicator.class, 1, 0))
    message(sprintf("\t\t\t\tTiempo %s segundos", round(t <- Sys.time()-t,digits=1)))
    cbind(x, sp, data.frame(tiempo = t))
  })
  
  res <- join(res, data.frame(nvars=seq(pred.names), namevar = pred.names), by="nvars", type="left")
  res$namevar <- factor(res$namevar, levels=pred.names)
  
  res
}


pred_ranking <- function(df, response = .(desercion_1)){
  library(ROCR)
  response_var <- df[[names(response)]]
  df2 <- df[,-which(names(df)==names(response))]
  df2 <- df2[,laply(df2, function(v){ if(length(unique(na.omit(v)))==1){ FALSE } else { TRUE } })]
  
  res <- ldply(names(df2), function(namevar){
    message(namevar)
    pred_var <- df[[namevar]]
    daux <- data.frame(response_var = response_var, pred_var = pred_var)
    daux_naomit <- na.omit(daux)
    
    model <- glm(response_var ~ pred_var, data = daux_naomit, family = binomial(link = logit))
    
    pred <- prediction(model$fitted.values, daux_naomit$response)
    perf <- performance(pred, "tpr","fpr")
    
    auc <- attr(performance(pred,"auc"),"y.values")[[1]]
    ks <- max(abs(attr(perf,'y.values')[[1]]-attr(perf,'x.values')[[1]]))
    return(data.frame(Variable = namevar, AUCROC = auc, KS = ks, NA.prop = 1-nrow(daux_naomit)/nrow(daux), log = ""))  
  }, .progress="text")
  res <- res[order(res$AUCROC, decreasing=TRUE),]
  res
}

char2factor <- function(df) {
  data.frame(lapply(df, function (v) {
    if (is.character(v)) factor(v)
    else v
  }))
}

pred_ranking_rrf <- function(data, response.name, pred.names = setdiff(names(data), response.name), ...){
  
  library(RRF)
  
  formula <- as.formula(paste(response.name, paste(pred.names, collapse="+"), sep= " ~ "))  
  daux <- subset(data, select=c(response.name, pred.names))
  daux <- char2factor(daux)
  daux <- na.roughfix(daux)
  daux[[response.name]] <- factor(daux[[response.name]])
  rrf <- RRF(formula, data=daux, ...)
  imp <- data.frame(variable = rownames(RRF::importance(rrf)), mdg = as.numeric((RRF::importance(rrf))))
  imp <- imp[order(imp$mdg, decreasing=TRUE),]
  imp
}


