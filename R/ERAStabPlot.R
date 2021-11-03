#' Stability Plot List
#'
#' This function creates a list of stability plots for all combinations of Practice x Outcome from the output of the `StabCalc2` function.
#'
#' @param Data a list object output by the `StabCalc2` function.
#' @param Intercept logical `(T/F)`; default = `F`; if `T` an intercept is added to the Beta line on each plot, if `F` the Beta has intercept = 0
#' @param Robust logical `(T/F)`; default = `F`; values for the relationship between x and y variables come from a robust linear regression when `T` and from linear
#' regression when `F `
#' @return A list of stability plots.
#' @export
#' @importFrom ggplot2 ggplot aes geom_point geom_vline geom_hline geom_bin2d scale_fill_continuous geom_text labs theme_bw theme element_blank xlim ylim geom_abline
#' @import data.table
ERAStabPlot<-function(Data,Intercept=F,Robust=F){

  StabCalc<-data.table(Data$StabStats.Test)
  StabList<-Data$StabList

  xlimFun<-function(lnRR){
    xlim<-c(min(floor(lnRR)),max(ceiling(lnRR)))
    if(xlim[1]>-1){xlim[1]<- -1}
    if(xlim[2]<1){xlim[2]<- 1}
    return(xlim)
  }
  ylimFun<-function(lnVR,lnCVR){
    ylim<-c(min(floor(c(lnVR,lnCVR))),max(ceiling(c(lnVR,lnCVR))))
    if(ylim[1]>-1){ylim[1]<- -1}
    if(ylim[2]<1){ylim[2]<- 1}
    return(ylim)
  }

  PrxOut<-StabCalc[,unique(paste0(Outcome,".",Practice))]

  X<-pblapply(PrxOut,FUN=function(PxO){
    X<-StabList[PxO][[1]]
    if(Robust==T){
      Y<-StabCalc[Prac.x.Out==PxO & Robust==T]
    }else{
      Y<-StabCalc[Prac.x.Out==PxO & Robust==F]
    }
    Xlimits<-xlimFun(X[,log(yieldratio)])
    Ylimits<-ylimFun(X[,log(sdratio)],X[,log(cvratio)])
    LabelVR<-paste0("b = ",Y[,round(`Estimate_VR_ln(VR)`,2)]," ",Y[,`PSymbol_VR_ln(VR)`],"\nn = ",nrow(X))
    LabelCVR<-paste0("b = ",Y[,round(`Estimate_CVR_ln(CVR)`,2)]," ",Y[,`PSymbol_CVR_ln(CVR)`],"\nn = ",nrow(X))

    MeanYR<-round(X[,mean(log(yieldratio))],2)
    MeanAVR<-round(X[,mean(log(sdratio))],2)
    MeanRVR<-round(X[,mean(log(cvratio))],2)
    LabelVR2<-paste0("ln(YR) mean = ",MeanYR ,"\nln(AVR) mean = ",MeanAVR)
    LabelCVR2<-paste0("ln(YR) mean = ", MeanYR,"\nln(CVR) mean = ",MeanRVR)


    geom_point(aes(x=5.6, y=3.9), colour="blue")

    lnVR.plot<-ggplot(X,aes(x=log(yieldratio),y=log(sdratio)))+
      geom_vline(xintercept=0,lty="dashed")+
      geom_hline(yintercept=0,lty="dashed")+
      geom_bin2d(bins = 10)+
      scale_fill_continuous(type = "viridis",alpha=0.5) +
      geom_text(aes(x = min(Xlimits), y = max(Ylimits), label = LabelVR,hjust=0,vjust=1),col="grey33")+
      geom_text(aes(x = max(Xlimits), y = max(Ylimits), label = LabelVR2,hjust=1,vjust=1),col="grey33")+
      geom_text(aes(x =-0.1, y = min(Ylimits), label = "Cont. higher yield",vjust=0,hjust=1),col="grey33")+
      geom_text(aes(x =0.1, y = min(Ylimits), label = "Treat. higher yield",vjust=0,hjust=0),col="grey33")+
      geom_text(aes(x = min(Xlimits), y = -0.1, label = "Treat. more stable",vjust=1,hjust=0),col="grey33")+
      geom_text(aes(x = min(Xlimits), y = 0.1, label = "Cont. more stable",vjust=0,hjust=0),col="grey33")+
      geom_point(alpha=0.6)+
      geom_point(aes(x=MeanYR, y=MeanAVR), colour="red",pch=3,size=5,alpha=0.75)+
      labs(y="ln (absolute variability ratio)",x=" ln (mean yield ratio)",fill="No. MYOs",title=X[1,Practice])+
      theme_bw()+
      theme(panel.grid = element_blank())+
      xlim(Xlimits)+
      ylim(Ylimits)

    if(Intercept==T){
      lnVR.plot<-lnVR.plot+geom_abline(slope=Y[,`Estimate_VR_ln(VR)`],intercept = Y[,`Estimate_VR_Intercept`])
    }else{
      lnVR.plot<-lnVR.plot+geom_abline(slope=Y[,`Estimate_VR_ln(VR)`])
    }

    lnCVR.plot<-ggplot(X,aes(x=log(yieldratio),y=log(cvratio)))+
      geom_vline(xintercept=0,lty="dashed")+
      geom_hline(yintercept=0,lty="dashed")+
      geom_bin2d(bins = 10)+
      scale_fill_continuous(type = "viridis",alpha=0.5) +
      geom_text(aes(x = min(Xlimits), y = max(Ylimits), label = LabelCVR,hjust=0,vjust=1),col="grey33")+
      geom_text(aes(x = max(Xlimits), y = max(Ylimits), label = LabelCVR2,hjust=1,vjust=1),col="grey33")+
      geom_text(aes(x =-0.1, y = min(Ylimits), label = "Cont. higher yield",vjust=0,hjust=1),col="grey33")+
      geom_text(aes(x =0.1, y = min(Ylimits), label = "Treat. higher yield",vjust=0,hjust=0),col="grey33")+
      geom_text(aes(x = min(Xlimits), y = -0.1, label = "Treat. more stable",vjust=1,hjust=0),col="grey33")+
      geom_text(aes(x = min(Xlimits), y = 0.1, label = "Cont. more stable",vjust=0,hjust=0),col="grey33")+
      geom_point(alpha=0.6)+
      geom_point(aes(x=MeanYR, y=MeanRVR), colour="red",pch=3,size=5,alpha=0.75)+
      labs(y="ln (relative variability ratio)",x=" ln (mean yield ratio)",fill="No. MYOs",title=X[1,Practice])+
      theme_bw()+
      theme(panel.grid = element_blank())+
      xlim(Xlimits)+
      ylim(Ylimits)

    if(Intercept==T){
      lnCVR.plot<-lnCVR.plot+geom_abline(slope=Y[,`Estimate_CVR_ln(CVR)`],intercept = Y[,`Estimate_CVR_Intercept`])
    }else{
      lnCVR.plot<-lnCVR.plot+geom_abline(slope=Y[,`Estimate_CVR_ln(CVR)`])
    }

    list(lnVR=lnVR.plot,lnCVR=lnCVR.plot)

  })
  names(X)<-PrxOut
  return(X)
}
