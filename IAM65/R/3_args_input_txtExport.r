


  #----------------------
  # Objet 'iamInput'
  #----------------------


#on commence par s'occuper du slot 'specific', dans lequel sont renseignées les dimensions

unlSpec <- function(ll,file="specific.txt") {
  if (is.list(ll)) {  #si c'est un liste...
    out <- c("list","character",rep("NA",6),names(ll)) ; out[length(out)] <- paste(out[length(out)],"\n",sep="")
    cat(out,append=TRUE,sep="\t",file=file)
    for (i in 1:length(ll)) unlSpec(ll[[i]],file=file)
  } else {
    out <- c("var",as.character(typeof(ll)),rep("NA",6),as.character(ll)) ; out[length(out)] <- paste(out[length(out)],"\n",sep="")
    cat(out,append=TRUE,sep="\t",file=file)
  }
}


#on a alors toute l'info nécessaire pour procéder aux formatage des objets dans les autres slots 

unl <- function(ll,file="input.txt",spec=list(),flag=NA) {
  if (is.list(ll)) {  #si c'est une liste...
    out <- c("list","character",rep("NA",6),names(ll)) ; out[length(out)] <- paste(out[length(out)],"\n",sep="")
    cat(out,append=TRUE,sep="\t",file=file)
    for (i in 1:length(ll)) {
      if (names(ll)[i]%in%names(spec$Ages)) flag <- names(ll)[i] 
      unl(ll[[i]],file=file,spec=spec,flag=flag)
    }
  } else {
    dimcst <- attributes(ll)$DimCst
    dimMet <- dimAC <- "NA"
    if (!is.null(dimcst)) {
      if (dimcst[3]>0) { #il faut alors distinguer les âges des catégories
        if (identical(dimnames(ll)[[sum(dimcst[1:2]>0)+1]],spec$Ages[[flag]])) dimAC <- "A" else dimAC <- "C"
      }
      if (dimcst[2]>0) { #il faut alors distinguer les métiers bio des métiers éco
        if (identical(dimnames(ll)[[sum(dimcst[1]>0)+1]],spec$Metier)) dimMet <- "B" else dimMet <- "E"
      }
      
    out <- c("var",as.character(typeof(ll)),if(is.null(dimcst)) rep("NA",6) else c(as.character(dimcst),dimMet,dimAC),as.character(ll))
    out[length(out)] <- paste(out[length(out)],"\n",sep="")
    cat(out,append=TRUE,sep="\t",file=file)
    } else {
    #il faut considérer le cas particulier des matrices (ex:'icat') qui n'ont pas d'attributs 'DimCst' mais qui
    #doivent être quand même formatées 
    #pas besoin de formatage poussé, juste la mise en forme suffit  
      if (is.matrix(ll)) {
        out <- c("var",as.character(typeof(ll)),c("NA","NA","NA","NA",as.character(nrow(ll)),as.character(ncol(ll))),as.character(ll))
      } else {
        out <- c("var",as.character(typeof(ll)),rep("NA",6),as.character(ll))     
      }
      out[length(out)] <- paste(out[length(out)],"\n",sep="")
      cat(out,append=TRUE,sep="\t",file=file) 
      }
  }
}

  #----------------------
  # Objet 'iamArgs'
  #----------------------
  
setGeneric("IAM.export", function(object, folder, ...){
	standardGeneric("IAM.export")
	}
)

  # exporter un objet 'iamInput'

setMethod("IAM.export", signature("iamInput"), function(object, folder, ...){

  fullPathFold <- normalizePath(folder)
  
  if (length(object@specific$Species)>0) {      #sthg to export
    
    if ("specific.txt"%in%list.files(fullPathFold)) {
      warning("'specific.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"specific.txt"))
    } 
    
    if ("input.txt"%in%list.files(fullPathFold)) {
      warning("'input.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"input.txt"))
    } 
    
    if ("scenario.txt"%in%list.files(fullPathFold)) {
      warning("'scenario.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"scenario.txt"))
    } 
    
    if ("stochastic.txt"%in%list.files(fullPathFold)) {
      warning("'stochastic.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"stochastic.txt"))
    }   
    
    unlSpec(object@specific,file=file.path(fullPathFold,"specific.txt"))
  
  } else {
  
    stop(" Nothing to export !!")
  
  }
   
  if (length(object@input)>0) unl(object@input,file=file.path(fullPathFold,"input.txt"),spec=object@specific)
  if (length(object@scenario)>0) unl(object@scenario,file=file.path(fullPathFold,"scenario.txt"),spec=object@specific)
  if (length(object@stochastic)>0) unl(object@stochastic,file=file.path(fullPathFold,"stochastic.txt"),spec=object@specific)

})



  # exporter un objet 'iamArgs'

setMethod("IAM.export", signature("iamArgs"), function(object, folder, ...){

  fullPathFold <- normalizePath(folder)
  
  if (length(object@specific$Species)>0) {
    
    if ("specific.txt"%in%list.files(fullPathFold)) {
      warning("'specific.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"specific.txt"))
    } 
    
    if ("args.txt"%in%list.files(fullPathFold)) {
      warning("'args.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"args.txt"))
    } 
    
    if ("argsCPP.txt"%in%list.files(fullPathFold)) {
      warning("'argsCPP.txt' is already in 'folder'. Sorry, but it will be removed!!")
      file.remove(file.path(fullPathFold,"argsCPP.txt"))
    } 
    
    unlSpec(object@specific,file=file.path(fullPathFold,"specific.txt"))
  
  } else {
    
    stop(" Nothing to export !!")
  
  }
   
  if (length(object@arguments)>0) {
    unl(object@arguments,file=file.path(fullPathFold,"args.txt"),spec=object@specific)
    
    if (object@arguments$Scenario$active==1) {
      scenar <- object@arguments$Scenario$ALLscenario[object@arguments$Scenario$SELECTscen]
    } else {
      scenar <- ""
    }	
 
    Rectyp <- unlist(lapply(object@arguments$Recruitment,function(x) x$simuSTOCHactive * x$typeSIMUstoch))

    mOth <- rep(0,length(object@specific$Species)) ; mOth[match(object@arguments$Gestion$espece,object@specific$Species)] <- 1

    cppArgs <- list(scenario=scenar,
                    RecType1=as.integer(Rectyp==1), 
                    RecType2=as.integer(Rectyp==2), 
                    RecType3=as.integer(Rectyp==3),
                    Scenarii=as.integer(object@arguments$Scenario$active), 
                    Bootstrp=as.integer(object@arguments$Replicates$active),
                    nbBoot=as.integer(object@arguments$Replicates$nbIter), 
                    GestInd=as.integer(object@arguments$Gestion$active),
                    mF=as.double(object@arguments$Gestion$mf),
                    mFM=as.double(object@arguments$Gestion$mfm),             #MM : added 4/02/2013
                    mOth=as.double(mOth),
                    bounds=as.double(c(object@arguments$Gestion$inf,object@arguments$Gestion$sup)),
                    TAC=as.double(object@arguments$Gestion$tac),
                    FBAR=as.double(object@arguments$Gestion$fbar),
                    GestParam=as.integer(c(eTemp = match(object@arguments$Gestion$espece,object@specific$Species)-1,
                                 var = match(object@arguments$Gestion$control,c("Nb jdm","Nb navires")),
                                 trgt = match(object@arguments$Gestion$target,c("TAC","Fbar","TAC->Fbar")),
                                 delay = object@arguments$Gestion$delay,
                                 upd = object@arguments$Gestion$upd,
                                 level = object@arguments$Gestion$level)),
                    EcoDcf=as.integer(object@arguments$Eco$type-1),
                    EcoInd=as.integer(c(adj = object@arguments$Eco$adj,
                                 lev = object@arguments$Eco$lev,
                                 ue_choice = object@arguments$Eco$ue_choice,
                                 oths = object@arguments$Eco$oths,
                                 othsFM = object@arguments$Eco$othsFM,
                                 perscCalc = object@arguments$Eco$perscCalc,
                                 report = object@arguments$Eco$report)),
                    dr=as.double(object@arguments$Eco$dr), 
                    SRind=as.integer(unlist(lapply(object@arguments$Recruitment,function(x) x$modSRactive))),
                    listSR=lapply(object@arguments$Recruitment,function(x) as.double(c(x$parAmodSR,x$parBmodSR,x$parCmodSR,x$wnNOISEmodSR,x$noiseTypeSR))),
                    TypeSR=lapply(object@arguments$Recruitment,function(x) 
                                as.integer(match(x$typeMODsr,c("Mean","Hockey-Stick","Beverton-Holt","Ricker","Shepherd","Quadratic-HS","Smooth-HS")))),
                    bootVar=as.character(object@arguments$Replicates$SELECTvar)) 
       
    unl(cppArgs,file=file.path(fullPathFold,"argsCPP.txt"),spec=object@specific)
    }



})


#---------------
#Examples
#---------------

##objets 'iamInput' et 'iamArgs' qu'on va exporter
#out <- IAM.input("Z:/Projet/Projet SIAD/Param bio_eco/Modele/Inputs_SIAD_SEL_2.xls",t_init=2010,nbStep=21)
#argOut <- IAM.args(out)
#
##exportation d'un objet 'iamInput'
#
#IAM.export(out,folder="C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput")
#
##exportation d'un objet 'iamArgs'
#
#IAM.export(argOut,folder="C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expArgs")
          



                       