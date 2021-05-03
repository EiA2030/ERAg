#' Calculate Outcome Stability
#'
#' `StabCalc` calculates relative and absolute outcome stability for MYOs based on the methods of \href{https://doi.org/10.1038/s41467-018-05956-1}{Knapp *el al.* (2018)}
#'
#' @param Data *To be described*
#' @param Do.Weight *To be described*
#' @param Weight.by.Study *To be described*
#' @param Rm.Out *To be described*
#' @param Transform *To be described*
#' @param Control *To be described*
#' @param Responses *To be described*
#' @return `StabCalc` returns a `data.table` *to be described*
#' @export
StabCalc<-function(Data,
                   Do.Weight=T,
                   Weight.by.Study=T,
                   Rm.Out=T,
                   Transform=T,
                   Control=list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000),
                   Responses=c("lnRR","lnVR","lnCVR")){
  covariance_commonControl <- function (aDataFrame, control_ID, X_t, SD_t, N_t, X_c, SD_c, N_c, metric = "lnRR") {

    ## generate list of control groups in dataframe
    controlList <- split(aDataFrame, as.factor(aDataFrame[, control_ID]))
    listV <- list(); dataAlignedWithV <- data.frame();

    for(i in 1:length(controlList)) {
      ## stack dataframes in V order
      dataAlignedWithV <- rbind(dataAlignedWithV, controlList[[i]])

      if(metric == "lnRR") {
        ## common control covariance and variance of response ratio based Lajeunesse 2011
        covar <-       (controlList[[i]][, SD_c] ^ 2) / (controlList[[i]][, N_c] * (controlList[[i]][, X_c] ^ 2))
        var <- covar + (controlList[[i]][, SD_t] ^ 2) / (controlList[[i]][, N_t] * (controlList[[i]][, X_t] ^ 2))
      }
      ####### added by SK    (start)
      if(metric == "lnVR") {
        covar <-        1 / (2*(controlList[[i]][, N_c] -1))
        var <- covar +  1 / (2*(controlList[[i]][, N_t] -1))
      }
      if(metric == "lnCVR") {
        covar <- (controlList[[i]][, SD_c] ^ 2) / (controlList[[i]][, N_c] * (controlList[[i]][, X_c] ^ 2))+(1 / (2*(controlList[[i]][, N_c] -1)))
        var <- covar + ((controlList[[i]][, SD_t] ^ 2) / (controlList[[i]][, N_t] * (controlList[[i]][, X_t] ^ 2))+(1 / (2*(controlList[[i]][, N_t] -1))))
      }
      ####### added by SK    (end)

      ## calculate V for ith element in the controlList
      V <- matrix(covar, nrow = length(var), ncol = length(var))
      diag(V) <- var

      ## collect V's with a list
      listV <- unlist(list(listV, list(V)), recursive = FALSE)
    }

    ## convert list of V's into single matrix
    V <- as.matrix(bdiag(listV))

    ## return V matrix paired with aligned dataset
    return(list(V, dataAlignedWithV))
  }
  addmeassure <- function(obstable,metric,lajeunesse){
    # rename metric for escalc() function
    if (metric=="lnRR"){measure="ROM"}
    if (metric=="lnVR"){measure="VR"}
    if (metric=="lnCVR"){measure="CVR"}

    if (lajeunesse)
    {

      # 1) observations that share common control treatment
      sub <- obstable[!is.na(obstable$cluster_cont),]
      if(nrow(sub)>0){
        commoncont <- covariance_commonControl (aDataFrame=sub,
                                                control_ID="cluster_cont",
                                                X_t="yieldexp",SD_t="sdexp",N_t="nryears",
                                                X_c="yieldcont",SD_c="sdcont",N_c="nryears",
                                                metric = metric)
        commoncont[[2]] <- escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,
                                  sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=commoncont[[2]])
      } else{
        commoncont <- list(
          matrix(nrow=0,ncol=0),
          sub
        )
      }

      # 2) observations that share common experimental treatment
      sub <- obstable[!is.na(obstable$cluster_exp),]
      if(nrow(sub)>0){
        # NB: as function is written for common control, t and c have to be interchanged here
        commonexp <- covariance_commonControl (aDataFrame=sub,
                                               control_ID="cluster_exp",
                                               X_c="yieldexp",SD_c="sdexp",N_c="nryears",
                                               X_t="yieldcont",SD_t="sdcont",N_t="nryears",
                                               metric = metric)
        commonexp[[2]] <- escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,
                                 sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=commonexp[[2]])
      } else{
        commonexp <- list(
          matrix(nrow=0,ncol=0),
          sub
        )
      }
      # 3) "normal" obs, not sharing any common control or experimental treatment (no Lajeunesse )
      sub <- obstable[is.na(obstable$cluster_exp) & is.na(obstable$cluster_cont),]
      if(nrow(sub)>0){
        vi<-escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=sub)$vi
        nocommon <- list(
          if(length(vi)>1){diag(vi)}else{vi},   # Amendment to Knapp code - if length was 1 it either output 1 if vi>1 or a zero length matrix if vi<1
          escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=sub))
      } else{
        nocommon <- list(
          matrix(nrow=0,ncol=0),
          sub
        )
      }
      ####### combine
      out <- list(
        bdiag(commoncont[[1]],commonexp[[1]],nocommon[[1]]),  # v matrix
        rbind.fill(commoncont[[2]],commonexp[[2]],nocommon[[2]]) # obs table
      )

    } else {    # when no lajeunese correction demanded, lajeunesse=FALSE in function call
      out <- list(
        diag(escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=obstable)$vi),
        escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=obstable))
    }
    return(out)
  }
  psymbol<-function(X){
    Y<-rep("N.S.",length(X))
    Y[X<0.05]<-"*"
    Y[X<0.01]<-"**"
    Y[X<0.001]<-"***"
    return(Y)
  }
  M.details<-function(model,Transform){

    ### extract back-transformed mean and 95% confidence interval (CI) for intercept
    if(!"rlm" %in% class(model)){
      if(Transform){
        # 1) Mean = without correction for Jensen Inequality
        # 2) Norm = Jensen inequality corrected assuming data follows a normal distribution
        # 3) Smear = Jensen inequality corrected using the Smearing estimate (non-normal distrbution)
        sigma.sq <- sum(summary(model)$sigma2)

        Model<-data.table(Mean=model$b[,1],
                          CI.low=model$ci.lb,
                          CI.high=model$ci.ub,
                          Mean.Jen=exp(model$b[,1] + sigma.sq / 2),
                          CI.low.Jen=exp(model$ci.lb + sigma.sq / 2),
                          CI.high.Jen=exp(model$ci.ub + sigma.sq / 2),
                          Sigma=sigma.sq,
                          Mean.Smear=exp(model$b[,1] * (1 / nobs(model) * sum(exp(resid(model))))),
                          CI.low.Smear=exp(model$ci.lb * (1 / nobs(model) * sum(exp(resid(model))))),
                          CI.high.Smear=exp(model$ci.ub * (1 / nobs(model) * sum(exp(resid(model))))),
                          P.Vals=model$pval,
                          SE=model$se,
                          Mean.exp=exp(model$b[,1]),
                          CI.low.exp=exp(model$ci.lb),
                          CI.high.exp=exp(model$ci.ub),
                          Z.val=model$zval,
                          Model=class(model)[1])
      }else{
        Model<-data.table(Mean=model$b[,1],SE=model$se,Z.val=model$zval,CI.low=model$ci.lb,CI.high=model$ci.ub,P.Vals=model$pval,Model=class(model)[1])
      }}else{
        sig<-f.robftest(model, var = "(Intercept)")$p.value

        ci.lb<-confint.default(object = model, parm = "(Intercept)", level = 0.95)[1]
        ci.ub<-confint.default(object = model, parm = "(Intercept)", level = 0.95)[2]

        ### extract back-transformed mean and 95% confidence interval (CI) for intercept
        if(Transform){
          # 1) Mean = without correction for Jensen Inequality
          # 2) Norm = Jensen inequality corrected assuming data follows a normal distribution
          # 3) Smear = Jensen inequality corrected using the Smearing estimate (non-normal distrbution)
          sigma.sq <-glance(model)$sigma^2


          Model<-data.table(Mean=model$coefficients,
                            CI.low=ci.lb,
                            CI.high=ci.ub,
                            Mean.Jen=exp(model$coefficients + sigma.sq / 2),
                            CI.low.Jen=exp(ci.lb + sigma.sq / 2),
                            CI.high.Jen=exp(ci.ub + sigma.sq / 2),
                            Sigma=sigma.sq,
                            Mean.Smear=exp(model$coefficients) * (1 / nobs(model) * sum(exp(resid(model)))),
                            CI.low.Smear=exp(ci.lb * (1 / nobs(model) * sum(exp(resid(model))))),
                            CI.high.Smear=exp(ci.ub * (1 / nobs(model) * sum(exp(resid(model))))),
                            P.Vals=sig,
                            SE=coef(summary(model))[,2],
                            Mean.exp=exp(model$coefficients),
                            CI.low.exp=exp(ci.lb),
                            CI.high.exp=exp(ci.ub),
                            Z.val=NA,
                            Model=class(model)[1])
        }else{
          Model<-data.table(Mean=model$coefficients,SE=coef(summary(model))[,2],Z.val=NA,CI.low=ci.lb,CI.high=ci.ub,P.Vals=sig,Model=class(model)[1])
        }
      }
    return(Model)
  }

  Data<-data.frame(Data)
  X<-lapply(Responses,FUN=function(Response){
    #print(Response)
    if(Rm.Out){
      rm.outliers<-function(Data,Vals){
        return(Data[!(Vals < quantile(Vals)[2] - 3 *  IQR(Vals)  | Vals > quantile(Vals)[4] + 3 * IQR(Vals)),])
      }

      # Remove outliers
      if(Response=="lnRR"){
        Data<-rm.outliers(Data=Data,Vals=log(Data$yieldratio))
      }
      if(Response=="lnVR"){
        Data<-rm.outliers(Data=Data,Vals=log(Data$sdratio))
      }
      if(Response=="lnCVR"){
        Data<-rm.outliers(Data=Data,Vals=log(Data$cvratio))
      }
    }

    # add the respective response and variance-covariance matrix using the addmeasure() function (Knapp et al. 2018)
    tempmat <- addmeassure(obstable=Data,metric=Response,lajeunesse = T)
    respmat <- tempmat[[2]] # the actual data, with the the respective response
    varmat <- forceSymmetric(tempmat[[1]]) # the variance-covariance matrix (VCV)

    # fit mixed-model with rma.mv() from the metafor package

    if(Do.Weight){
      if(Weight.by.Study){
        model <- rma.mv(yi~1,V=varmat,W=Weight.Study,data=respmat,control=Control)
      }else{
        model <- rma.mv(yi~1,V=varmat,W=Weight,data=respmat,control=Control)
      }
    }else{
      model <- rma.mv(yi~1,V=varmat,data=respmat,control=Control)
    }

    Model<-M.details(model=model,Transform=Transform)[,Robust:=F
    ][,Response:=Response]

    if(length(unique(respmat$ID))>1){
      # Robust model with cluster as the study (better p-values) - a vector specifying a clustering variable to use for constructing the sandwich estimator of the variance-covariance matrix.
      r.model<-robust(model,cluster=respmat$ID,adjust=TRUE)
      R.Model<-M.details(model=r.model,Transform=Transform)[,Robust:=T
      ][,Response:=Response]
    }else{
      r.model<-rlm(yi~1,weights=Weight.Study,data=respmat,maxit=200)
      R.Model<-M.details(model=r.model,Transform=Transform)[,Robust:=T
      ][,Response:=Response]
    }

    Coefs<-rbind(Model,R.Model)[,N.Studies:=length(unique(respmat[!is.na("yi"),"Code"]))][,N.Seq:=nrow(respmat[!is.na("yi")])][,N.Obs:=sum(respmat[!is.na("yi"),"N.Obs"])]
    return(list(Model=model,R.Model=r.model,Coefs=Coefs))
  })

  Y<-data.table(Data)[yieldratio!=0 & sdratio!=0 & cvratio!=0]

  Model<-lm(log(yieldratio)~log(sdratio),weights=Weight.Study,(data=Y))
  lm.VR<-data.table(summary(Model)$coefficients)[,Coefficient:=c("Intercept","ln(VR)")][,Variable:="VR"][,Robust:=F][,Sigma.sq:=sum(summary(Model)$sigma)]


  Model<-lm(log(yieldratio)~log(cvratio),weights=Weight.Study,(data=Y))
  lm.CVR<-data.table(summary(Model)$coefficients)[,Coefficient:=c("Intercept","ln(CVR)")][,Variable:="CVR"][,Robust:=F][,Sigma.sq:=sum(summary(Model)$sigma)]

  rlm.VR<-rlm(log(yieldratio)~log(sdratio),weights=Weight.Study,(data=Y),maxit =200)
  rlm.VR<-data.table(summary(rlm.VR)$coefficients,`Pr(>|t|)`=c(f.robftest(rlm.VR, var = "(Intercept)")$p.value,f.robftest(rlm.VR, var = "log(sdratio)")$p.value))[,Coefficient:=c("Intercept","ln(VR)")][,Variable:="VR"][,Robust:=T][,Sigma.sq:=glance(rlm.VR)$sigma^2]

  rlm.CVR<-rlm(log(yieldratio)~log(cvratio),weights=Weight.Study,(data=Y),maxit =200)
  rlm.CVR<-data.table(summary(rlm.CVR)$coefficients,`Pr(>|t|)`=c(f.robftest(rlm.CVR, var = "(Intercept)")$p.value,f.robftest(rlm.CVR, var = "log(cvratio)")$p.value))[,Coefficient:=c("Intercept","ln(CVR)")][,Variable:="CVR"][,Robust:=T][,Sigma.sq:=glance(rlm.CVR)$sigma^2]
  LMs<-rbind(lm.VR,lm.CVR,rlm.VR,rlm.CVR,use.names=F)
  LMs[,Practice:=Y[1,"Practice"]
  ][,Practice.Code:=Y[1,"Practice.Code"]
  ][,Outcome:=Y[1,"Outcome"]
  ][,PSymbol:=psymbol(`Pr(>|t|)`)
  ][,N.Obs:=nrow(Y)
  ][,N.Studies:=Y[,length(unique(Code))]]

  LMs[,Mean.Jen:=exp(Estimate+Sigma.sq/2)
  ][,CI.low.Jen:=exp(Estimate-`Std. Error`+Sigma.sq/2)
  ][,CI.high.Jen:=exp(Estimate+`Std. Error`+Sigma.sq/2)]

  Tests2<-rbindlist(lapply(X,"[[","Coefs"))
  Tests2[,N.Studies:=round(mean(N.Studies))
  ][,N.Seq:=round(mean(N.Seq))
  ][,N.Obs:=round(mean(N.Obs))
  ][,PSymbol:=psymbol(P.Vals)]

  Tests2<-dcast(Tests2,Model+Robust+N.Studies+N.Seq+N.Obs~Response,value.var = c("Mean","Mean.Jen","SE","Z.val","CI.low","CI.high","CI.low.Jen","CI.high.Jen","P.Vals","PSymbol","Sigma"))

  Tests2[,Practice:=Data[1,"Practice"]
  ][,Practice.Code:=Data[1,"Practice.Code"]
  ][,Outcome:=Data[1,"Outcome"]]

  W<-lapply(X,"[[","Model")
  names(W)<-Responses
  Y<-lapply(X,"[[","R.Model")
  names(Y)<-Responses
  Z<-rbindlist(lapply(X,"[[","Coefs"))
  Z[,Practice:=Data[1,"Practice"]
  ][,Practice.Code:=Data[1,"Practice.Code"]
  ][,Outcome:=Data[1,"Outcome"]]

  return(list(Coefs=Z,Models=W,R.Models=Y,Tests=LMs,Tests2=Tests2))

}
