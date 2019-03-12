

#-------------------------------------------------------------------------------

# Méthodes de représentation graphiques des contenus des objets de sortie 'iamOutput' et 'iamOutputRep'

#-------------------------------------------------------------------------------



    #---------------------------------------------------------------------------
    
    # 1. 'iamOutputRep'
    
    #---------------------------------------------------------------------------



#on redéfinit la manière de calculer les box (quantiles : 0.025,0.25,0.75,0.975)
boxplot.stats.custom <-function (x,coef = 1.5, do.conf=TRUE, do.out=TRUE) {
      
      Mean <- mean(x,na.rm = TRUE)
      lci <- (quantile(x,0.25, na.rm = TRUE)[[1]])
      uci <- (quantile(x,0.75, na.rm = TRUE)[[1]])
      lp <- (quantile(x,0.025, na.rm = TRUE)[[1]])
      up <- (quantile(x,0.975, na.rm = TRUE) [[1]])
      sizen <- sum(!is.na(x))
      lconf <- 0
      uconf <- 0
      stats <- c(lp,lci,Mean,uci,up)
      n<-sizen
      conf <-c(lconf,uconf)
      result <- list(stats = stats,n=n,conf=conf,out = NA)
      return(result)
      
}  



IAM.bwplot <- function(formula, data, ...) {

#require(lattice)

#paramètres graphiques par défaut
  default.args <- list( 
        type = "b",
        pch = 1,
        cex.pch = 1, 
        lty = 1,
        lwd = 1,
        relation = "same", #(ou "free" ou "sliced")
        key=TRUE,    #affichage légende??
        pch.leg = 15,
        pchSize.leg = 1, 
        lty.leg = 1,
        lwd.leg = NA,
        txt.leg = "",
        font.leg = 1,
        cex.leg = 0.8,
        space = "right",
        alpha = 0.3,
        add.v = NA,
        add.h = NA,
        add.lwd = 1,
        add.lty = 2,
        add.col = "black",
        cex.lab.x = 1,
        cex.lab.y = 1,
        font.lab.x = 4,
        font.lab.y = 4,
        cex.axis.x = 1,
        cex.axis.y = 1,
        font.axis.x = 4,
        font.axis.y = 4,
        rot.x = 0,
        rot.y = 0,
        cex.strip = 0.8,
        font.strip = 4,
        col.strip = "lightblue1",
        col = rainbow(6),
        fill = "lightgrey",
        xlab="X",
        ylab="Y"
  )

  argum <- list(...)
  
  #if ("groups"%in%names(argum)) grps <- argum$groups else grps <- NULL
  
  for (i in names(default.args)) {
    if (i %in% names(argum)) default.args[[i]] <- argum[[i]] 
  }
  
  
    grp <- grepl("+",as.character(formula)[2],fixed =TRUE)
    lgth <- length(strsplit(as.character(formula)[2],"+",fixed=TRUE)[[1]])
                                          
  
  both <- !is.na(default.args$pch.leg) & !is.na(default.args$lwd.leg)

  bwplot(formula, data=data, allow.m = TRUE, outer = FALSE, 
  
         panel =function(x, y, subscripts, groups, ...) {
             opar <- trellis.par.get()
             x <- as.numeric(x)
             y <- as.numeric(y)
             COL <- rep(default.args$col,length=lgth)
             FILL <- rep(default.args$fill,length=lgth)
             PCH <- rep(default.args$pch,length=lgth)
             PCHsize <- default.args$cex.pch
  
             settings <- lapply(1:lgth,function(x) list(box.rectangle = list(col = COL[x],fill=FILL[x],lty=1),
                                                     box.umbrella = list(col = COL[x],lty=1),
                                                     plot.symbol = list(col = "transparent",cex=0.6,lty=1),
                                                     box.dot = list(col = COL[x],cex=PCHsize,pch=PCH[x])))
  
             if (grp) vals <- levels(groups)
             for (i in 1:lgth)
             {
                 trellis.par.set(settings[[i]])
                 if (grp) {
                  id <- groups[subscripts] == vals[i]
                  panel.bwplot(x = x[id], y = y[id], stats=boxplot.stats.custom,...)
                 } else {
                  panel.bwplot(x = x, y = y, stats=boxplot.stats.custom,...)
                 }
                 if (!is.na(default.args$add.v)) panel.abline(v=default.args$add.v,lty=default.args$add.lty,
                                                              lwd=default.args$add.lwd,col=default.args$add.col)
                 if (!is.na(default.args$add.h)) panel.abline(h=default.args$add.h,lty=default.args$add.lty,
                                                              lwd=default.args$add.lwd,col=default.args$add.col)
                 trellis.par.set(opar)
             }
         },
         
         scale=list(x=list(rot=default.args$rot.x, cex=default.args$cex.axis.x, font=default.args$font.axis.x),
                    y=list(rot=default.args$rot.y, cex=default.args$cex.axis.y, font=default.args$font.axis.y),
                    relation=default.args$relation),
         
         key=if (default.args$key) {
         
              if (both) {
                    list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                             lty=rep(default.args$lty.leg,length=lgth),
                             col=rep(default.args$col,length=lgth),
                             pch=rep(default.args$pch.leg,length=lgth),
                             cex=rep(default.args$pchSize.leg,length=lgth)),
                         text=list(rep(default.args$txt.leg,length=lgth)),
                         type="b",columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                         cex=default.args$cex.leg)
              } else {
                  if (!is.na(default.args$lwd.leg)){
                           list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                             lty=rep(default.args$lty.leg,length=lgth),
                                             col=rep(default.args$col,length=lgth)),
                                text=list(rep(default.args$txt.leg,length=lgth)),
                                columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                cex=default.args$cex.leg)
                  } else {
                           list(points = list(pch=rep(default.args$pch.leg,length=lgth),
                                              cex=rep(default.args$pchSize.leg,length=lgth),
                                              col=rep(default.args$col,length=lgth)),
                                text=list(rep(default.args$txt.leg,length=lgth)),
                                columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                cex=default.args$cex.leg)
                  }
              }
         
         } else {
          
          NULL},
         par.strip.text=list(font=default.args$font.strip,cex=default.args$cex.strip),strip=strip.custom(bg=default.args$col.strip),
         xlab=list(default.args$xlab,font=default.args$font.lab.x,cex=default.args$cex.lab.x),
         ylab=list(default.args$ylab,font=default.args$font.lab.y,cex=default.args$cex.lab.y))
 
 }   
 
#data(DFiam) 
#IAM.bwplot(gp + un + trois + quatre~t|scen,data=DFiam_iter)

 
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

 
IAM.zone <- function(formula, data,...) {

#require(lattice)

#paramètres graphiques par défaut
  default.args <- list( 
        lty = 1,
        lwd = 1,
        relation = "same", #(ou "free" ou "sliced")
        key=TRUE,    #affichage légende??
        pch.leg = 15,
        pchSize.leg = 1, 
        lty.leg = 1,
        lwd.leg = NA,
        txt.leg = "",
        font.leg = 1,
        cex.leg = 0.8,
        space = "right",
        alpha = 0.3,
        add.v = NA,
        add.h = NA,
        add.lwd = 1,
        add.lty = 2,
        add.col = "black",
        cex.lab.x = 1,
        cex.lab.y = 1,
        font.lab.x = 4,
        font.lab.y = 4,
        cex.axis.x = 1,
        cex.axis.y = 1,
        font.axis.x = 4,
        font.axis.y = 4,
        rot.x = 0,
        rot.y = 0,
        cex.strip = 0.8,
        font.strip = 4,
        col.strip = "lightblue1",
        col = rainbow(6),
        xlab="X",
        ylab="Y"
  )
  
  argum <- list(...)
  
  #if ("groups"%in%names(argum)) grps <- argum$groups else grps <- NULL
  
  for (i in names(default.args)) {
    if (i %in% names(argum)) default.args[[i]] <- argum[[i]] 
  }
  
  
    grp <- grepl("+",as.character(formula)[2],fixed =TRUE)
    lgth <- length(strsplit(as.character(formula)[2],"+",fixed=TRUE)[[1]])
                                          
  
  both <- !is.na(default.args$pch.leg) & !is.na(default.args$lwd.leg)

  xyplot(formula, data=data, allow.m = TRUE, outer = FALSE, 
  
         panel =function(x, y, subscripts, groups, ...) {
             opar <- trellis.par.get()
             x <- as.numeric(x)
             y <- as.numeric(y)
             COL <- rep(default.args$col,length=lgth)
             FILL <- rep(default.args$fill,length=lgth)
  
             if (grp) vals <- levels(groups)
             
             for (i in 1:lgth)
             {

                 id <- groups[subscripts] == vals[i]
                 qtl <- do.call("cbind",lapply(tapply(y[id],list(x[id]),function(w) quantile(w,c(0.05,0.95))),function(z) z)) 
                 mn <- tapply(y[id],list(x[id]),mean)
                 lpolygon(c(names(qtl[1,]),rev(names(qtl[2,]))),c(qtl[1,],rev(qtl[2,])),alpha=default.args$alpha,
                          angle=rep(c(45,135),length=lgth)[i],col=COL[i])
                 llines(names(qtl[2,]),qtl[2,],col=COL[i])
                 llines(names(qtl[1,]),qtl[1,],col=COL[i])
                 llines(names(mn),mn,col=COL[i])
                 
                 if (!is.na(default.args$add.v)) panel.abline(v=default.args$add.v,lty=default.args$add.lty,
                                                              lwd=default.args$add.lwd,col=default.args$add.col)
                 if (!is.na(default.args$add.h)) panel.abline(h=default.args$add.h,lty=default.args$add.lty,
                                                              lwd=default.args$add.lwd,col=default.args$add.col)
             }
         },
         
         scale=list(x=list(rot=default.args$rot.x, cex=default.args$cex.axis.x, font=default.args$font.axis.x),
                    y=list(rot=default.args$rot.y, cex=default.args$cex.axis.y, font=default.args$font.axis.y),
                    relation=default.args$relation),
         
         key=if (default.args$key) {
         
              if (both) {
                    list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                             lty=rep(default.args$lty.leg,length=lgth),
                             col=rep(default.args$col,length=lgth),
                             pch=rep(default.args$pch.leg,length=lgth),
                             cex=rep(default.args$pchSize.leg,length=lgth)),
                         text=list(rep(default.args$txt.leg,length=lgth)),
                         type="b",columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                         cex=default.args$cex.leg)
              } else {
                  if (!is.na(default.args$lwd.leg)){
                           list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                             lty=rep(default.args$lty.leg,length=lgth),
                                             col=rep(default.args$col,length=lgth)),
                                text=list(rep(default.args$txt.leg,length=lgth)),
                                columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                cex=default.args$cex.leg)
                  } else {
                           list(points = list(pch=rep(default.args$pch.leg,length=lgth),
                                              cex=rep(default.args$pchSize.leg,length=lgth),
                                              col=rep(default.args$col,length=lgth)),
                                text=list(rep(default.args$txt.leg,length=lgth)),
                                columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                cex=default.args$cex.leg)
                  }
              }
         
         } else {
          
          NULL},
         par.strip.text=list(font=default.args$font.strip,cex=default.args$cex.strip),strip=strip.custom(bg=default.args$col.strip),
         xlab=list(default.args$xlab,font=default.args$font.lab.x,cex=default.args$cex.lab.x),
         ylab=list(default.args$ylab,font=default.args$font.lab.y,cex=default.args$cex.lab.y))
 
 }   
 
 
 
 
#IAM.zone(gp + un + trois + quatre~t|scen,data=DFiam_iter)


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


IAM.barIC <- function(formula, data,...) {

#paramètres graphiques par défaut
  default.args <- list( 
        lty = 1,
        lwd = 1,
        relation = "same", #(ou "free" ou "sliced")
        key=TRUE,    #affichage légende??
        pch.leg = 15,
        pchSize.leg = 1, 
        lty.leg = 1,
        lwd.leg = NA,
        txt.leg = "",
        font.leg = 1,
        cex.leg = 0.8,
        space = "right",
        alpha = 0.3,
        add.v = NA,
        add.h = NA,
        add.lwd = 2,
        add.lty = 1,
        add.col = "grey",
        cex.lab.x = 1,
        cex.lab.y = 1,
        font.lab.x = 4,
        font.lab.y = 4,
        cex.axis.x = 1,
        cex.axis.y = 1,
        font.axis.x = 4,
        font.axis.y = 4,
        rot.x = 0,
        rot.y = 0,
        cex.strip = 0.8,
        font.strip = 4,
        col.strip = "lightblue1",
        col = rainbow(6),
        xlab="X",
        ylab="Y",
        origin=NULL,           
        as.table=TRUE
  )
  
  argum <- list(...)
  
  #if ("groups"%in%names(argum)) grps <- argum$groups else grps <- NULL
  
  for (i in names(default.args)) {
    if (i %in% names(argum)) default.args[[i]] <- argum[[i]]
  }
  
  
    grp <- grepl("+",as.character(formula)[2],fixed =TRUE)
    lgth <- length(strsplit(as.character(formula)[2],"+",fixed=TRUE)[[1]])
                                          
  
  both <- !is.na(default.args$pch.leg) & !is.na(default.args$lwd.leg)

  #il faut d'abord opérer les calculs sur la table 'data' des moyennes et quantiles
  allVarCalc <- all.vars(formula)[1:lgth]
  allVarCross <- all.vars(formula)[-(1:lgth)]
  
  eval(parse('',text=paste("DFmean <- with(data,aggregate(list(",paste(allVarCalc,collapse=","),
                      "),list(",paste(allVarCross,collapse=","),"),mean))",sep="")))
  eval(parse('',text=paste("DFinf <- with(data,aggregate(list(",paste(allVarCalc,collapse=","),
                      "),list(",paste(allVarCross,collapse=","),"),function(x) quantile(x,probs=0.05)))",sep="")))
  eval(parse('',text=paste("DFsup <- with(data,aggregate(list(",paste(allVarCalc,collapse=","),
                      "),list(",paste(allVarCross,collapse=","),"),function(x) quantile(x,probs=0.95)))",sep="")))
  
  
  names(DFmean) <- names(DFinf) <- names(DFsup) <- c(allVarCross,allVarCalc)

  panel.grps <- function(y,x,box.ratio,...){
                        panel.barchart(x,y,box.ratio=box.ratio,...)
                        groupSub <- function(groups, subscripts,...) groups[subscripts]
                        retSub <- function(groups, subscripts,...) return(subscripts)
                        groups <- as.numeric(groupSub(groups,...))
                        subs <- retSub(groups,...)
                        height <- box.ratio/(1+ box.ratio)
                        
                        for (i in 1:length(levels(x))) {
                          ok <- x == levels(x)[i]
                          for (j in sort(unique(groups))){
                              OK <- groups[ok]==j
                              quant <- c(unlist(DFinf[,allVarCalc])[subs][ok][OK],unlist(DFsup[,allVarCalc])[subs][ok][OK])
                              panel.segments((i + height* (1/lgth) *(j-mean(1:lgth))),
                                              quant[1],
                                              (i + height* (1/lgth) * (j-mean(1:lgth))),
                                              quant[2],col=default.args$add.col,
                                      lwd=default.args$add.lwd,lty=default.args$add.lty)
                          }
                        } 
                        
                }
  
  panel.simple <- function(y,x,subscripts,...){
                        panel.barchart(x, y, ...)
                        for (i in 1:length(levels(x))) {
                          ok <- x == levels(x)[i]
                            quant <- c(unlist(DFinf[,allVarCalc])[subscripts][ok],unlist(DFsup[,allVarCalc])[subscripts][ok])
                            panel.segments(i,quant[1],i,quant[2],col=default.args$add.col,
                                      lwd=default.args$add.lwd,lty=default.args$add.lty)
                        } 
                        
                  }

  barchart(formula,data=DFmean,
                prepanel = function(x,y,subscripts,...) {list(ylim = c(1.05*min(c(0,unlist(DFinf[,allVarCalc])[subscripts]),na.rm=TRUE), 1.05*max(unlist(DFsup[,allVarCalc])[subscripts],na.rm=TRUE)))},
                panel = {if (grp) {panel.grps} else {panel.simple}}, col = rep(default.args$col,length=lgth),
                
                scale=list(x=list(rot=default.args$rot.x, cex=default.args$cex.axis.x, font=default.args$font.axis.x),
                    y=list(rot=default.args$rot.y, cex=default.args$cex.axis.y, font=default.args$font.axis.y),
                    relation=default.args$relation),
         
                key=if (default.args$key) {
         
                    if (both) {
                          list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                   lty=rep(default.args$lty.leg,length=lgth),
                                   col=rep(default.args$col,length=lgth),
                                   pch=rep(default.args$pch.leg,length=lgth),
                                   cex=rep(default.args$pchSize.leg,length=lgth)),
                               text=list(rep(default.args$txt.leg,length=lgth)),
                               type="b",columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                               cex=default.args$cex.leg)
                              } else {
                                  if (!is.na(default.args$lwd.leg)){
                                      list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                                   lty=rep(default.args$lty.leg,length=lgth),
                                                   col=rep(default.args$col,length=lgth)),
                                      text=list(rep(default.args$txt.leg,length=lgth)),
                                      columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                      cex=default.args$cex.leg)
                                  } else {
                                      list(points = list(pch=rep(default.args$pch.leg,length=lgth),
                                                    cex=rep(default.args$pchSize.leg,length=lgth),
                                                    col=rep(default.args$col,length=lgth)),
                                      text=list(rep(default.args$txt.leg,length=lgth)),
                                      columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                      cex=default.args$cex.leg)
                                  }
                              }
               
                    } else {
                
                    NULL},
               par.strip.text=list(font=default.args$font.strip,cex=default.args$cex.strip),
               strip=strip.custom(bg=default.args$col.strip),
               xlab=list(default.args$xlab,font=default.args$font.lab.x,cex=default.args$cex.lab.x),
               ylab=list(default.args$ylab,font=default.args$font.lab.y,cex=default.args$cex.lab.y),
               origin=default.args$origin,as.table=default.args$as.table)


}  


#exemple
#IAM.barIC(un+deux+trois+quatre+cinq+gp~t|scen,data=DFiam_iter,txt.leg=c("1 an","2 ans","3 ans","4 ans","5 ans","6 ans et +"))















    #---------------------------------------------------------------------------
    
    # 2. 'iamOutput'
    
    #---------------------------------------------------------------------------



IAM.barplot <- function(formula, data,...) {

#paramètres graphiques par défaut
  default.args <- list( 
        relation = "same", #(ou "free" ou "sliced")
        key=TRUE,    #affichage légende??
        pch.leg = 15,
        pchSize.leg = 1, 
        lty.leg = 1,
        lwd.leg = NA,
        txt.leg = "",
        font.leg = 1,
        cex.leg = 0.8,
        space = "right",
        alpha = 0.3,
        add.v = NA,
        add.h = NA,
        add.lwd = 2,
        add.lty = 1,
        add.col = "grey",
        cex.lab.x = 1,
        cex.lab.y = 1,
        font.lab.x = 4,
        font.lab.y = 4,
        cex.axis.x = 1,
        cex.axis.y = 1,
        font.axis.x = 4,
        font.axis.y = 4,
        rot.x = 0,
        rot.y = 0,
        cex.strip = 0.8,
        font.strip = 4,
        col.strip = "lightblue1",
        col = rainbow(6),
        xlab="X",
        ylab="Y",
        origin=NULL,           
        as.table=TRUE
  )
  
  argum <- list(...)
  
  for (i in names(default.args)) {
    if (i %in% names(argum)) default.args[[i]] <- argum[[i]]
  }
  
  grp <- grepl("+",as.character(formula)[2],fixed =TRUE)
  lgth <- length(strsplit(as.character(formula)[2],"+",fixed=TRUE)[[1]])
  
  #validation pour s'assurer qu'on a bien qu'une seule valeur par croisement de modalités factorielles
  
  fields <- all.vars(formula)[-c(1:lgth)]
  if (nrow(unique(data[,fields,drop=FALSE]))!=nrow(data)) warning("check for replicates in your data!!") 
      
  both <- !is.na(default.args$pch.leg) & !is.na(default.args$lwd.leg)
  
  barchart(formula,data=data,
                col = rep(default.args$col,length=lgth),
                
                scale=list(x=list(rot=default.args$rot.x, cex=default.args$cex.axis.x, font=default.args$font.axis.x),
                    y=list(rot=default.args$rot.y, cex=default.args$cex.axis.y, font=default.args$font.axis.y),
                    relation=default.args$relation),
         
                key=if (default.args$key) {
         
                    if (both) {
                          list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                   lty=rep(default.args$lty.leg,length=lgth),
                                   col=rep(default.args$col,length=lgth),
                                   pch=rep(default.args$pch.leg,length=lgth),
                                   cex=rep(default.args$pchSize.leg,length=lgth)),
                               text=list(rep(default.args$txt.leg,length=lgth)),
                               type="b",columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                               cex=default.args$cex.leg)
                              } else {
                                  if (!is.na(default.args$lwd.leg)){
                                      list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                                   lty=rep(default.args$lty.leg,length=lgth),
                                                   col=rep(default.args$col,length=lgth)),
                                      text=list(rep(default.args$txt.leg,length=lgth)),
                                      columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                      cex=default.args$cex.leg)
                                  } else {
                                      list(points = list(pch=rep(default.args$pch.leg,length=lgth),
                                                    cex=rep(default.args$pchSize.leg,length=lgth),
                                                    col=rep(default.args$col,length=lgth)),
                                      text=list(rep(default.args$txt.leg,length=lgth)),
                                      columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                      cex=default.args$cex.leg)
                                  }
                              }
               
                    } else {
                
                    NULL},
               par.strip.text=list(font=default.args$font.strip,cex=default.args$cex.strip),
               strip=strip.custom(bg=default.args$col.strip),
               xlab=list(default.args$xlab,font=default.args$font.lab.x,cex=default.args$cex.lab.x),
               ylab=list(default.args$ylab,font=default.args$font.lab.y,cex=default.args$cex.lab.y),
               origin=default.args$origin,as.table=default.args$as.table)


}  


#exemple
#dff <- df2[df2$iter==1,]
#IAM.barplot(un+deux+trois+quatre+cinq+gp~t,data=DFiam,txt.leg=c("1 an","2 ans","3 ans","4 ans","5 ans","6 ans et +"))

#IAM.barplot(un+deux+trois+quatre+cinq+gp~t,data=DFiam_iter,txt.leg=c("1 an","2 ans","3 ans","4 ans","5 ans","6 ans et +"))




#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------





IAM.plot <- function(formula, data,...) {


#paramètres graphiques par défaut
  default.args <- list(
        type = "b",
        pch = 16,
        cex.pch = 1,  
        lty = 1,
        lwd = 1,
        relation = "same", #(ou "free" ou "sliced")
        key=TRUE,    #affichage légende??
        pch.leg = 16,
        pchSize.leg = 1, 
        lty.leg = 1,
        lwd.leg = NA,
        txt.leg = "",
        font.leg = 1,
        cex.leg = 0.8,
        space = "right",
        alpha = 0.3,
        add.v = NA,
        add.h = NA,
        add.lwd = 1,
        add.lty = 2,
        add.col = "black",
        cex.lab.x = 1,
        cex.lab.y = 1,
        font.lab.x = 4,
        font.lab.y = 4,
        cex.axis.x = 1,
        cex.axis.y = 1,
        font.axis.x = 4,
        font.axis.y = 4,
        rot.x = 0,
        rot.y = 0,
        cex.strip = 0.8,
        font.strip = 4,
        col.strip = "lightblue1",
        col = rainbow(6),
        xlab="X",
        ylab="Y",
        origin=NULL,           
        as.table=TRUE
  )
  
  argum <- list(...)
  
  for (i in names(default.args)) {
    if (i %in% names(argum)) default.args[[i]] <- argum[[i]]
  }
  
  grp <- grepl("+",as.character(formula)[2],fixed =TRUE)
  lgth <- length(strsplit(as.character(formula)[2],"+",fixed=TRUE)[[1]])
  
  #validation pour s'assurer qu'on a bien qu'une seule valeur par croisement de modalités factorielles
  
  fields <- all.vars(formula)[-c(1:lgth)]
  if (nrow(unique(data[,fields,drop=FALSE]))!=nrow(data)) warning("check for replicates in your data!!") 
      
  both <- !is.na(default.args$pch.leg) & !is.na(default.args$lwd.leg)
  
  xyplot(formula,data=data, type=default.args$type,
                panel =function(x, y, ...) {
                  panel.xyplot(x,y,...)
                  if (!is.na(default.args$add.v)) panel.abline(v=default.args$add.v,lty=default.args$add.lty,
                                                              lwd=default.args$add.lwd,col=default.args$add.col)
                  if (!is.na(default.args$add.h)) panel.abline(h=default.args$add.h,lty=default.args$add.lty,
                                                              lwd=default.args$add.lwd,col=default.args$add.col)
                },
                lwd=rep(default.args$lwd,lgth),
                lty=rep(default.args$lty,length=lgth),

                pch=rep(default.args$pch,length=lgth),
                cex=rep(default.args$cex.pch,length=lgth),
                col=rep(default.args$col,length=lgth),
                
                scale=list(x=list(rot=default.args$rot.x, cex=default.args$cex.axis.x, font=default.args$font.axis.x),
                    y=list(rot=default.args$rot.y, cex=default.args$cex.axis.y, font=default.args$font.axis.y),
                    relation=default.args$relation),
         
                key=if (default.args$key) {
         
                    if (both) {
                          list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                   lty=rep(default.args$lty.leg,length=lgth),
                                   col=rep(default.args$col,length=lgth),
                                   pch=rep(default.args$pch.leg,length=lgth),
                                   cex=rep(default.args$pchSize.leg,length=lgth)),
                               text=list(rep(default.args$txt.leg,length=lgth)),
                               type="b",columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                               cex=default.args$cex.leg)
                              } else {
                                  if (!is.na(default.args$lwd.leg)){
                                      list(lines = list(lwd=rep(default.args$lwd.leg,lgth),
                                                   lty=rep(default.args$lty.leg,length=lgth),
                                                   col=rep(default.args$col,length=lgth)),
                                      text=list(rep(default.args$txt.leg,length=lgth)),
                                      columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                      cex=default.args$cex.leg)
                                  } else {
                                      list(points = list(pch=rep(default.args$pch.leg,length=lgth),
                                                    cex=rep(default.args$pchSize.leg,length=lgth),
                                                    col=rep(default.args$col,length=lgth)),
                                      text=list(rep(default.args$txt.leg,length=lgth)),
                                      columns=1,space=default.args$space,border=TRUE,font=default.args$font.leg,
                                      cex=default.args$cex.leg)
                                  }
                              }
               
                    } else {
                
                    NULL},
               par.strip.text=list(font=default.args$font.strip,cex=default.args$cex.strip),
               strip=strip.custom(bg=default.args$col.strip),
               xlab=list(default.args$xlab,font=default.args$font.lab.x,cex=default.args$cex.lab.x),
               ylab=list(default.args$ylab,font=default.args$font.lab.y,cex=default.args$cex.lab.y),
               origin=default.args$origin,as.table=default.args$as.table)


}  


#exemple
#IAM.plot(un+deux+trois+quatre+cinq+gp~t,data=DFiam,txt.leg=c("1 an","2 ans","3 ans","4 ans","5 ans","6 ans et +"))

#IAM.plot(un+deux+trois+quatre+cinq+gp~t,data=DFiam_iter,txt.leg=c("1 an","2 ans","3 ans","4 ans","5 ans","6 ans et +"))


