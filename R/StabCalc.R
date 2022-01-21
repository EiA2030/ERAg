#' Calculate Outcome Stability
#'
#' `StabCalc` calculates relative and absolute outcome stability for MYOs based on the methods of \href{https://doi.org/10.1038/s41467-018-05956-1}{Knapp *el al.* (2018)}. This
#' function is designed for use within the `ERAg::StabCalc2` function.
#'
#' In the `Coefs`, `Models`, `Tests2` outputs of this function class estimates of the response variables `lnRR` = natural log of response ratio, `lnVR` =
#' natural log of absolute variability ratio, and `lnCVR` = natural log of relative variability ratio are obtained with the rma.mv function with
#' formula `response.variable~1`. Confidence intervals at P<0.95 are obtained by default for all (non-fixed) variance and correlation components of
#' the models. Cluster-robust tests, confidence intervals, and significance of the model coefficients from  `rma.mv` models are additionally calculated using the
#' \link[metafor]{robust} and \link[sfsmisc]{f.robftest} functions.
#'
#' Natural log ratios are back-transformed with and without a corrections for the Jensen inequality. Corrections are applied as per
#' \href{https://www.biorxiv.org/content/10.1101/179358v1}{Tandini & Mehrabi 2017} using two methods for back-transformation:
#' 1) `exp(fitted(model) + summary(model)$sigma^2 / 2)`  situable for normally distributed data
#' 2) a smearing estimate as `exp(fitted(model) * (1 / nobs(model) * sum(exp(resid(model))))`
#'
#' The `Tests` output contains the results of a weighted linear model of form `log(y) = a + b × log(x)` where `y` is a stability ratio and
#' `x` is the mean yield ratio. The robust results in this table use a weighted robust linear model, see \link[MASS]{rlm}.
#'
#' @param Data A data.table output by the `ERAg::PrepareStabData` function
#' @param Do.Weight logical, if `TRUE` coefficient estimates are weighted acccording to the supplied weightings in the `Data` object supplied(default = T)
#' @param Weight.by.Study logical, depreciated (default = `T`)
#' @param Rm.Out logical, if `TRUE` extreme outliers are removed withing each Practice x Outcome combination as per the method detailed in `ERAg::OutCalc` (default = `T`)
#' @param Transform logical, if `TRUE` back-transformed coefficient estimates and confidence intervals are appended to outputs  (default = `T`)
#' @param Control list, optional list of control values for the `rma.mv` estimation algorithms. If unspecified, default values are defined inside the function (default = `list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000)`)
#' @param Responses character vector, this argument is depreciated do edit (default=`c("lnRR","lnVR","lnCVR")`)
#' @param Use.acv logical, if `TRUE` scale-adjusted coefficient of variation, acv, is substituted for the coefficient of variation (cv).
#' @return `StabCalc` returns a  list is containing following data:
#' \enumerate{
#' \item **`[[Coefs]]`** A `data.table` of model coefficients, test statistics, confidence intervals:
#' \itemize{
#' \item`Mean` numeric, response variable test coefficient (see `Response` field) given the model used (see `Model` field)
#' \item`CI.low` numeric, response variable test coefficient lower confidence limit see \link[metafor]{confint.rma}
#' \item*`CI.high` numeric, response variable test coefficient upper confidence limit see \link[metafor]{confint.rma}
#' \item`Mean.Jen` numeric, back-transformed response variable test coefficient correcting for the Jensen inequality as `exp(model$b[,1] + sigma.sq / 2)`
#' \item`CI.low.Jen` numeric, back-transformed response variable test coefficient lower confidence limit correcting for the Jensen inequality as `exp(model$ci.lb[,1] + sigma.sq / 2)`
#' \item`CI.high.Jen` numeric, back-transformed response variable test coefficient upper confidence limit correcting for the Jensen inequality as `exp(model$ci.ub[,1] + sigma.sq / 2)`
#' \item`Sigma` numeric, estimated sigma^2 value(s)
#' \item`Mean.Smear` numeric, back-transformed response variable test coefficient correcting for the Jensen inequality using the Smearing estimate as `exp(model$b[,1] * (1 / nobs(model) * sum(exp(resid(model)))))`
#' \item`CI.low.Smear` numeric, back-transformed response variable test coefficient lower confidence limit correcting for the Jensen inequality using the Smearing estimate as `exp(model$ci.lb[,1] * (1 / nobs(model) * sum(exp(resid(model)))))`
#' \item`CI.high.Smear` numeric, back-transformed response variable test coefficient upper confidence limit correcting for the Jensen inequality using the Smearing estimate as `exp(model$ci.ub[,1] * (1 / nobs(model) * sum(exp(resid(model)))))`
#' \item`P.Vals` numeric, p-value for test statistic
#' \item`SE` numeric, standard error of the coefficients
#' \item`Mean.exp` numeric, back-transformed response variable test coefficient not correcting for the Jensen inequality as `exp(model$b[,1])`
#' \item`CI.low.exp` numeric, back-transformed response variable test coefficient lower confidence limit not correcting for the Jensen inequality as `exp(model$ci.lb[,1])`
#' \item`CI.high.exp` numeric, back-transformed response variable test coefficient upper confidence limit not correcting for the Jensen inequality as `exp(model$ci.ub[,1])`
#' \item`Z.val` numeric, test statistic of the coefficient
#' \item`Model` numeric, test used to generate test statistics and confidence intervals `rma.mv` =  Multivariate/Multilevel Linear (Mixed-Effects) Model see \link[metafor]{rma.mv} or `robust.rma` = Cluster-Robust Tests and Confidence Intervals for 'rma' objects see  \link[metafor]{robust}
#' \item`Robust` logical, when `T` robust tests are used, when `F` they are not
#' \item`Response` character, `lnRR` = natural log of response ratio, `lnVR` = natural log of absolute variability ratio, `lnCVR` = natural log of relative variability ratio see `ERAg::PrepareStabData` function for more information.
#' \item`N.Studies` integer, number of studies contributing to the analysis
#' \item`N.Seq` integer, number of unique temporal sequences contributing to the analysis
#' \item`N.Obs`  integer, depreciated field
#' \item`Practice` character, ERA practice
#' \item`Practice.Code` character, ERA practice code
#' \item`Outcome` character, ERA outcome
#' }
#' \item**`[[Models]]`** Multivariate Meta-Analysis Model objects
#' \itemize{
#' \item`lnRR`= model where response variable is `lnRR` = natural log of response ratio
#' \item`lnVR`= model where response variable is `lnVR` = natural log of absolute variability ratio
#' \item`lnCVR`= model where response variable is `lnCVR` = natural log of relative variability ratio
#' }
#' \item**`[[R.Models]]`** Cluster-Robust Tests and Confidence Intervals applied to the Multivariate Meta-Analysis objects
#' \itemize{
#' \item`lnRR`= model where repose variable is `lnRR` = naturl log of response ratio
#' \item`lnVR`= model where response variable is `lnVR` = natural log of absolute variability ratio
#' \item`lnCVR`= model where response variable is `lnCVR` = natural log of relative variability ratio
#' }
#' \item**`[[Tests]]`** A `data.table` containing the results of a weighted linear model of form `ln(y) = a + b × ln(x)` where `y` is a stability ratio and
#' `x` is the mean yield ratio. Robust results use a weighted robust linear model, see \link[MASS]{rlm}
#' \itemize{
#' \item`Estimate` numeric, coefficient estimate
#' \item`Std.Error`numeric, standard error of the coefficient estimate
#' \item`t value` numeric, test statistic of the coefficient estimate
#' \item`Pr(>|t|)` numeric, p-value for test statistic
#' \item`Coefficient`character, coefficient (intercept or beta)
#' \item`Variable` character, `lnRR` = natural log of response ratio, `lnVR` = natural log of absolute variability ratio, `lnCVR` = natural log of relative variability ratio see `ERAg::PrepareStabData` function for more information.
#' \item`Robust` logical, when `T` robust tests are used, when `F` they are not
#' \item`Sigma.sq` numeric, estimated sigma^2 value(s)
#' \item`Practice` character, ERA practice
#' \item`Practice.Code` character, ERA practice code
#' \item`Outcome` character, ERA outcome
#' \item`PSymbol` character, `*` P<=0.05, `**` P<=0.01, `***` P<=0.001, N.S. P>0.05.
#' \item`N.Obs`  integer, depreciated field
#' \item`N.Studies`integer, number of studies contributing to the analysis
#' \item`Mean.Jen` numeric, back-transformed coefficient estimate correcting for the Jensen inequality as `exp(Estimate + sigma.sq / 2)`
#' \item`CI.low.Jen` numeric, back-transformed coefficient estimate less standard error correcting for the Jensen inequality as `exp(Estimate - Std.Error + Sigma.sq / 2)`
#' \item`CI.high.Jen` numeric, back-transformed coefficient estimate plus standard error correcting for the Jensen inequality as `exp(Estimate + Std.Error + Sigma.sq / 2)`
#' }
#' \item**`[[Tests2]]`** = as per the `Coeffs` data.table but transformed to be a wide format for Response variables.
#' \itemize{
#' \item`Model` numeric, test used to generate test statistics and confidence intervals `rma.mv` =  Multivariate/Multilevel Linear (Mixed-Effects) Model see \link[metafor]{rma.mv} or `robust.rma` = Cluster-Robust Tests and Confidence Intervals for 'rma' objects see  \link[metafor]{robust}
#' \item`Robust` logical, when `T` robust tests are used, when `F` they are not
#' \item`N.Studies`integer, number of studies contributing to the analysis
#' \item`N.Seq` integer, number of unique temporal sequences contributing to the analysis
#' \item`N.Obs`  integer, depreciated field
#' \item`Mean_lnCVR` numeric, lnCVR test coefficient
#' \item`Mean_lnRR` numeric, lnRR test coefficient
#' \item`Mean_lnVR` numeric, lnVR test coefficient
#' \item`Mean.Jen_lnCVR` numeric, back-transformed lnCVR test coefficient correcting for the Jensen inequality as `exp(model$b[,1] + sigma.sq / 2)`
#' \item`Mean.Jen_lnRR` numeric, back-transformed lnRR test coefficient correcting for the Jensen inequality as `exp(model$b[,1] + sigma.sq / 2)`
#' \item`Mean.Jen_lnVR` numeric, back-transformed lnVR test coefficient correcting for the Jensen inequality as `exp(model$b[,1] + sigma.sq / 2)`
#' \item`SE_lnCVR` numeric, standard error of lnCVR test coefficient
#' \item`SE_lnRR` numeric, standard error of lnRR test coefficient
#' \item`SE_lnVR` numeric, standard error of lnVR test coefficient
#' \item`CI.low_lnCVR` numeric,  lnCVR test coefficient lower confidence limit
#' \item`CI.high_lnCVR` numeric, lnCVR test coefficient upper confidence limit
#' \item`CI.low_lnRR` numeric, lnRR test coefficient lower confidence limit
#' \item`CI.high_lnRR` numeric, lnRR test coefficient upper confidence limit
#' \item`CI.low_lnVR` numeric, lnVR test coefficient lower confidence limit
#' \item`CI.high_lnVR` numeric, lnVR test coefficient upper confidence limit
#' \item`CI.low.Jen_lnCVR` numeric, back-transformed lnCVR test coefficient lower confidence limit correcting for the Jensen inequality as `exp(model$ci.lb[,1] + sigma.sq / 2)`
#' \item`CI.high.Jen_lnCVR` numeric, back-transformed lnCVR test coefficient lower confidence limit correcting for the Jensen inequality as `exp(model$ci.b[,1] + sigma.sq / 2)`
#' \item`CI.low.Jen_lnRR` numeric, back-transformed lnRR test coefficient upper confidence limit correcting for the Jensen inequality as `exp(model$ci.lb[,1] + sigma.sq / 2)`
#' \item`CI.high.Jen_lnRR` numeric, back-transformed lnRR test coefficient lower confidence limit correcting for the Jensen inequality as `exp(model$ci.b[,1] + sigma.sq / 2)`
#' \item`CI.low.Jen_lnVR` numeric, back-transformed lnVR test coefficient lower confidence limit correcting for the Jensen inequality as `exp(model$ci.lb[,1] + sigma.sq / 2)`
#' \item`CI.high.Jen_lnVR` numeric, back-transformed lnVR test coefficient lower confidence limit correcting for the Jensen inequality as `exp(model$ci.b[,1] + sigma.sq / 2)`
#' \item`P.Vals_lnCVR` numeric, p-value for lnCVR test statistic
#' \item`P.Vals_lnRR` numeric, p-value for lnRR test statistic
#' \item`P.Vals_lnVR` numeric, p-value for lnVR test statistic
#' \item`PSymbol_lnCVR` character, lnCVR `*` P<=0.05, `**` P<=0.01, `***` P<=0.001, N.S. P>0.05.
#' \item`PSymbol_lnRR` character, lnRR `*` P<=0.05, `**` P<=0.01, `***` P<=0.001, N.S. P>0.05.
#' \item`PSymbol_lnVR` character, lnVR `*` P<=0.05, `**` P<=0.01, `***` P<=0.001, N.S. P>0.05.
#' \item`Sigma_lnCVR ` numeric, lnCVR estimated sigma^2 value(s)
#' \item`Sigma_lnRR ` numeric, lnRR estimated sigma^2 value(s)
#' \item`Sigma_lnVR ` numeric, lnVR estimated sigma^2 value(s)
#' \item`Practice` character, ERA practice
#' \item`Practice.Code` character, ERA practice code
#' \item`Outcome` character, ERA outcome
#' }
#' }
#' @export
#' @importFrom metafor escalc rma.mv
#' @importFrom Matrix bdiag forceSymmetric
#' @importFrom plyr rbind.fill
#' @importFrom sfsmisc f.robftest
#' @importFrom broom glance
#' @importFrom MASS rlm
#' @importFrom data.table dcast rbindlist
#' @import data.table
StabCalc<-function(Data,
                   Do.Weight=T,
                   Weight.by.Study=T,
                   Rm.Out=T,
                   Transform=T,
                   DoRandom=T,
                   Control=list(optimizer="optim",optmethod="Nelder-Mead",maxit=10000),
                   Responses=c("lnRR","lnVR","lnCVR"),
                   Use.acv=F){

  if(Use.acv){
    Data[,cvexp:=acvexp
    ][,cvcont:=acvcont
    ][,cvratio:=acvratio]
  }

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
  addmeasure <- function(obstable,metric,lajeunesse){
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
        commonexp[[2]] <- metafor::escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,
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
        vi<-metafor::escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=sub)$vi
        nocommon <- list(
          if(length(vi)>1){diag(vi)}else{vi},   # Amendment to Knapp code - if length was 1 it either output 1 if vi>1 or a zero length matrix if vi<1
          metafor::escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=sub))
      } else{
        nocommon <- list(
          matrix(nrow=0,ncol=0),
          sub
        )
      }
      ####### combine
      out <- list(
        Matrix::bdiag(commoncont[[1]],commonexp[[1]],nocommon[[1]]),  # v matrix
        plyr::rbind.fill(commoncont[[2]],commonexp[[2]],nocommon[[2]]) # obs table
      )

    } else {    # when no lajeunese correction demanded, lajeunesse=FALSE in function call
      out <- list(
        diag(metafor::escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=obstable)$vi),
        metafor::escalc(measure=measure,m1i=yieldexp,m2i=yieldcont,sd1i=sdexp,sd2i=sdcont,n1i=nryears,n2i=nryears,data=obstable))
    }
    return(out)
  }

  psymbol<-function(X){
    Y<-rep("N.S.",length(X))
    Y[X<=0.05]<-"*"
    Y[X<=0.01]<-"**"
    Y[X<=0.001]<-"***"
    return(Y)
  }
  M.details<-function(model,Transform){

    ### extract back-transformed mean and 95% confidence interval (CI) for intercept
    if(!"rlm" %in% class(model)){
      if(Transform){
        # 1) .exp = without correction for Jensen Inequality
        # 2) .Jen = Jensen inequality corrected assuming data follows a normal distribution
        # 3) .Smear = Jensen inequality corrected using the Smearing estimate (non-normal distrbution)
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
        sig<-sfsmisc::f.robftest(model, var = "(Intercept)")$p.value

        ci.lb<-confint.default(object = model, parm = "(Intercept)", level = 0.95)[1]
        ci.ub<-confint.default(object = model, parm = "(Intercept)", level = 0.95)[2]

        ### extract back-transformed mean and 95% confidence interval (CI) for intercept
        if(Transform){
          # 1) Mean = without correction for Jensen Inequality
          # 2) Norm = Jensen inequality corrected assuming data follows a normal distribution
          # 3) Smear = Jensen inequality corrected using the Smearing estimate (non-normal distrbution)
          sigma.sq <-broom::glance(model)$sigma^2


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
    tempmat <- addmeasure(obstable=Data,metric=Response,lajeunesse = T)
    respmat <- tempmat[[2]] # the actual data, with the the respective response
    varmat <- Matrix::forceSymmetric(tempmat[[1]]) # the variance-covariance matrix (VCV)

    # fit mixed-model with rma.mv() from the metafor package

    if(Do.Weight){
      if(Weight.by.Study){
        if(DoRandom){
          model <- metafor::rma.mv(yi~1,V=varmat,W=Weight.Study,data=respmat,control=Control,random = ~1|Code )
        }else{
          model <- metafor::rma.mv(yi~1,V=varmat,W=Weight.Study,data=respmat,control=Control)
        }
      }else{
        if(DoRandom){
          model <- metafor::rma.mv(yi~1,V=varmat,W=Weight,data=respmat,control=Control,random = ~1|Code )
        }else{
          model <- metafor::rma.mv(yi~1,V=varmat,W=Weight,data=respmat,control=Control)
        }
      }
    }else{
      if(DoRandom){
        model <- metafor::rma.mv(yi~1,V=varmat,data=respmat,control=Control,random = ~1|Code)
      }else{
        model <- metafor::rma.mv(yi~1,V=varmat,data=respmat,control=Control)
      }
    }

    Model<-M.details(model=model,Transform=Transform)[,Robust:=F
                                                      ][,Response:=Response]

    if(length(unique(respmat$ID))>1){
      # Robust model with cluster as the study (better p-values) - a vector specifying a clustering variable to use for constructing the sandwich estimator of the variance-covariance matrix.
      r.model<-metafor::robust(model,cluster=respmat$ID,adjust=TRUE)
      R.Model<-M.details(model=r.model,Transform=Transform)[,Robust:=T
      ][,Response:=Response]
    }else{
      r.model<-MASS::rlm(yi~1,weights=Weight.Study,data=respmat,maxit=200)
      R.Model<-M.details(model=r.model,Transform=Transform)[,Robust:=T
      ][,Response:=Response]
    }

    Coefs<-rbind(Model,R.Model)[,N.Studies:=length(unique(respmat[!is.na("yi"),"Code"]))
                                ][,N.Seq:=nrow(respmat[!is.na("yi")])
                                  ][,N.Obs:=sum(respmat[!is.na("yi"),"N.Obs"])]
    return(list(Model=model,R.Model=r.model,Coefs=Coefs))
  })

  Y<-data.table(Data)[yieldratio!=0 & sdratio!=0 & cvratio!=0]

  Model<-lm(log(yieldratio)~log(sdratio),weights=Weight.Study,(data=Y))
  lm.VR<-data.table(summary(Model)$coefficients)[,Coefficient:=c("Intercept","ln(VR)")][,Variable:="VR"][,Robust:=F][,Sigma.sq:=sum(summary(Model)$sigma)]

  Model<-lm(log(yieldratio)~log(cvratio),weights=Weight.Study,(data=Y))
  lm.CVR<-data.table(summary(Model)$coefficients)[,Coefficient:=c("Intercept","ln(CVR)")][,Variable:="CVR"][,Robust:=F][,Sigma.sq:=sum(summary(Model)$sigma)]

  rlm.VR<-MASS::rlm(log(yieldratio)~log(sdratio),weights=Weight.Study,(data=Y),maxit =200)
  rlm.VR<-data.table(summary(rlm.VR)$coefficients,`Pr(>|t|)`=c(sfsmisc::f.robftest(rlm.VR, var = "(Intercept)")$p.value,sfsmisc::f.robftest(rlm.VR, var = "log(sdratio)")$p.value))[,Coefficient:=c("Intercept","ln(VR)")][,Variable:="VR"][,Robust:=T][,Sigma.sq:=broom::glance(rlm.VR)$sigma^2]

  rlm.CVR<-MASS::rlm(log(yieldratio)~log(cvratio),weights=Weight.Study,(data=Y),maxit =200)
  rlm.CVR<-data.table(summary(rlm.CVR)$coefficients,`Pr(>|t|)`=c(sfsmisc::f.robftest(rlm.CVR, var = "(Intercept)")$p.value,sfsmisc::f.robftest(rlm.CVR, var = "log(cvratio)")$p.value))[,Coefficient:=c("Intercept","ln(CVR)")][,Variable:="CVR"][,Robust:=T][,Sigma.sq:=broom::glance(rlm.CVR)$sigma^2]
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
