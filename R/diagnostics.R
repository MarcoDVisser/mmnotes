##' Create 'residual plots' to check non-linearity of lme4 objects 
##'
##' Builds a residual plot from the information provided
##' within the lme4 object. Automatically using the first
##' covariate to plot the residuals against when no other
##' arguments are supplied. 
##' 
##' @param mod lme4 model object 
##' @param covariate optional covariate to plot the residuals
##' against
##' @param gam Logical. Fit a gam and at it to the plot? This
##' requires the package gam to be installed.
##' @param \dots additional printing arguments pased to plot
##' @author Marco D. Visser
##' @return A residual plot
##' @concept diagnostics
##' @export
resPlot <- function(mod=NULL,covariate=NULL,gam=FALSE,...){

  if(is.null(mod)|!is.element("lmerMod",class(mod))){
    stop("supply a lme4 object")}

  if(is.null(covariate)) covariate <- all.vars(as.formula(mod@call))[2]
  if(!is.element(covariate,all.vars(as.formula(mod@call)))&
     !is.element(covariate,names(mod@frame))) {
  stop("covariate not found in model call")}

  if(!gam) if(!"gam"%in%rownames(installed.packages())) {
    stop("package gam not installed")}

  Residuals <- residuals(mod)
  x <- mod@frame[,covariate]
  
  plot(x,Residuals,main="residual plot",...)

  
  if(gam) {
      if(!class(x)=="numeric"){
    stop("covariate is not numeric, try setting gam=FALSE") }

  Mod1<-gam::gam(Residuals~lo(x))
  xran <- range(x)
  x50 <- seq(xran[1],xran[2],length.out=50)
  pred<-gam::predict.gam(Mod1,newdata=data.frame(x=x50))
  abline(h=0,col=rgb(.1,.1,.1,alpha=0.5),lwd=2,lty=2)
  lines(x50,pred,col=rgb(0,0,1,alpha=0.5),lwd=4)
  
} 

  
}
