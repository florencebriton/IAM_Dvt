

#-------------------------------------------------------------------------------

# Méthodes utilitaires diverses (mise en forme,...)

#-------------------------------------------------------------------------------


#fonction interne d'agrégation et de formatage de la donnée à représenter selon divers paramètres :  
#                                                                                    agg = numeric(<4) : on opère l'agrégation sur ces dim 
#                                                                                    dimAbsc = numeric(1) : dim en abscisse
#                                                                                    subset = character(1): instruction de filtrage final sur l'objet agrégé
#                                                                                    elmt = character(1) : variable à représenter
#                                                                                    spp = character(1) : espèce concernée
#
#                                                                         Sortie   : data.frame avec colonnes : value + dimensions


iamFormat <- function(object, elmt = character(), spp = character(), agg = NA, headers = as.character(NA), subs, index = NA) {

#on identifie la dimension d'abscisse
if (headers%in%"Scen") headers <- as.character(NA)
x <- switch(headers, f = 1, m = 2, a_c = 3, t = 4, NULL)
heads <- TRUE

#on commence par sélectionner la variable

if (!is.null(object@output[[elmt]])) {
  
  vrbl <- object@output[[elmt]]
  if (!is.na(index)) vrbl <- vrbl[[index]]

} else {

  if (!is.null(object@outputSp[[elmt]][[spp]])) {  
  
    vrbl <- object@outputSp[[elmt]][[spp]]
    
  } else {
  
    if (!is.null(object@outputSp[[elmt]][[index]][[spp]])) { 
    
      vrbl <- object@outputSp[[elmt]][[index]][[spp]]
      
    } else {
  
      stop("can't find the specified variable in the input object!! Check 'elmt' or 'spp' parameter!!")
  
    }
  }
}


#il ne faut pas que 'x' soit dans 'agg' 
if (!is.null(x)) {if (x%in%agg) stop("'headers' can't be in 'agg'!! Check your parameters!!")}
#il ne faut pas que 'x' soit une dimension nulle de DimCst
if (!is.null(x)) {if (attributes(vrbl)$DimCst[x]==0) stop("'headers'th dimension is not defined in specified variable!!")}
if (is.null(x)) heads <- FALSE


#nouvelles dimensions après agrégation
if (!is.null(dim(vrbl))) {     

  if (!all(is.na(agg))) { #on doit opérer l'agrégation
  
    newAgg <- agg[attributes(vrbl)$DimCst[agg]>0]
    dimApply <- (1:sum(attributes(vrbl)$DimCst>0))[-((1:4)-cumsum(attributes(vrbl)$DimCst==0))[newAgg]]

    if (length(dimApply)==0) stop("can't aggregate over those indexes!!")

    vrblPerm <- apply(vrbl,dimApply,sum,na.rm=TRUE)
    #dimension de la nouvelle variable correspondant à l'abscisse de représentation 
    dimTemp <- attributes(vrbl)$DimCst ; dimTemp[newAgg] <- 0 ; 

    if (all(dimTemp%in%0)) stop("no dimensions left after agregation!!")

    nam <- c("f","m","a_c","t")[dimTemp>0]
    #on permutte la matrice et on formate si plus de deux dimensions
    if (!is.null(dim(vrblPerm))) {
    
      df <- cbind.data.frame(value=as.vector(vrblPerm),expand.grid(dimnames(vrblPerm)))
      names(df) <- c("value",nam)
    
    } else {
  
      df <- data.frame(value=as.vector(vrblPerm), absc=names(vrblPerm))
      names(df) <- c("value",nam) 
  
    }
  
  } else {
  
    dimTemp <- attributes(vrbl)$DimCst 

    nam <- c("f","m","a_c","t")[dimTemp>0]

    df <- cbind.data.frame(value=as.vector(vrbl),expand.grid(dimnames(vrbl)))
    names(df) <- c("value",nam)
    
    
  }
  
} else {    #pas d'agrégation possible

  dimTemp <- attributes(vrbl)$DimCst 
  nam <- c("f","m","a_c","t")[dimTemp>0]
  
  df <- data.frame(value=as.vector(vrbl), absc=names(vrbl))
  names(df) <- c("value",nam)
} 

  #if (length(subset)>0)
  if (!missing(subs)) df <- df[eval(substitute(subs),df),]  #on subsette

  
  if (heads & (ncol(df)>2) & (nrow(df)>0)) {
    if (!missing(subs)) df <- cbind.data.frame(value=df$value,as.data.frame(as.matrix(df[,2:ncol(df)]))) #on met à jour les levels après subset
    df <- df[,c("value",headers,names(df)[!names(df)%in%c("value",headers)])] #on met la colonne de headers en deuxième position
    mat <- tapply(df[,1],list(do.call('paste',c(df[,3:ncol(df),drop=FALSE],list(sep=":-:-:"))),df[,2]),function(x) x)
    mat1 <- do.call("rbind",lapply(rownames(mat),function(x) strsplit(x,":-:-:")[[1]]))
    df1 <- as.data.frame(mat1) ; names(df1) <- names(df)[-(1:2)]
    df2 <- as.data.frame(mat) ; rownames(df2) <- NULL
    df <- cbind.data.frame(df1,df2)
  }
  
  
  rownames(df) <- 1:nrow(df)
  
  return(df)  

}


# Méthodes associées


setGeneric("IAM.format", function(object, ...){
	standardGeneric("IAM.format")
	}
)


setMethod("IAM.format", signature("iamOutput"),function(object, 
                                                        ...){
	
  iamFormat(object, ...)

})


setMethod("IAM.format", signature("iamOutputRep"),function(object, 
                                                        ...){
	
  lapply(1:object@arguments$Replicates$nbIter,function(x) 
                  iamFormat(object, ..., index=x))

})



#---------------
#Examples
#---------------


#out <- IAM.input("Z:/Projet/Projet SIAD/Param bio_eco/Modele/Inputs_SIAD_SEL_2.xls",t_init=2010,nbStep=21)
#
#arg <- IAM.args(out)
#
#mod <- IAM.model(arg,out)
#
#IAM.format(mod,elmt = "C", spp = "Langoustine", agg = 1:2, headers = "a_c", t%in%(2012:2016))







#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################
#############################################################################################


# Methode composite issue de IAM.format, ajoutant le regroupement des scenarii --> input des méthodes graphiques 


setGeneric("IAM.unite", function(listObj,...){
	standardGeneric("IAM.unite")
	}
)


setMethod("IAM.unite", signature(listObj="list"), function(listObj,...){

	if (!(all(unlist(lapply(listObj,class))%in%"iamOutput") | all(unlist(lapply(listObj,class))%in%"iamOutputRep"))) 
    stop("only 'iamOutput' objects OR (not AND) 'iamOutputRep' objects allowed in input list!!")
  

    
  df <- do.call("rbind",lapply(listObj, function(x) {
                                  if (class(x)%in%"iamOutput") {                                         
                                    return(cbind.data.frame(IAM.format(x,...),
                                                            Scen=ifelse(x@arguments$Scenario$active==0,
                                                                        "Status Quo",
                                                                        x@arguments$Scenario$ALLscenario[x@arguments$Scenario$SELECTscen])))
                                  } else {
                                    ll <- IAM.format(x,...)
                                    return(cbind.data.frame(do.call("rbind",ll),
                                                            iter=rep(1:length(ll),unlist(lapply(ll,nrow))),
                                                            Scen=ifelse(x@arguments$Scenario$active==0,
                                                                        "Status Quo",
                                                                        x@arguments$Scenario$ALLscenario[x@arguments$Scenario$SELECTscen])))
                                  }
                                                                    
                                  }))
                               
                                  
    call <- match.call()                              
    llhead <- as.list(call)[-1]$headers[1]
    
  
    if (!is.null(llhead)) {
      if (llhead%in%"Scen") {
    
    if ((ncol(df)>2) & (nrow(df)>0)) {
    mat <- tapply(df[,1],list(do.call('paste',c(df[,2:(ncol(df)-1),drop=FALSE],list(sep=":-:-:"))),df[,"Scen"]),function(x) x)
    mat1 <- do.call("rbind",lapply(rownames(mat),function(x) strsplit(x,":-:-:")[[1]]))
    df1 <- as.data.frame(mat1) ; names(df1) <- names(df)[-(c(1,ncol(df)))]
    df2 <- as.data.frame(mat) ; rownames(df2) <- NULL
    df <- cbind.data.frame(df1,df2)
  }
 
  rownames(df) <- 1:nrow(df)
   
    }
  } 
                                   
  return(df)

})



#---------------
#Examples
#---------------


#arg2 <- IAM.args(arg)
#
#mod2 <- IAM.model(arg2,out)
#
#IAM.unite(list(mod,mod2),elmt = "C", spp = "Langoustine", agg = 1:2, headers = "a_c", t%in%(2012:2025))




