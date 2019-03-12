#fonction de conversion des inputs au niveau métier Eco (inclus implémentation de l'allocation de mortalité par pêche

convertInput <- function(inp,Fq_fmi=NULL) {

        namF <- inp@specific$Fleet ; nF <- length(namF) 
        namM <- inp@specific$Metier ; nM <- length(namM)
        llF <- list()
        
        
        #1ère étape : ventilation de la mortalité (ATTENTION : ici, la ventilation sur indices non communs n'est pas envisagée)
        #-------------------------------------------------------------------------------
            
            for (i in inp@specific$Species) {
            if (inp@specific$Q[i]==0) {

            namI <- inp@specific$Ages[[i]] ; nI <- length(namI)
            
              Fini <- inp@input[[i]]$F_fmi
              Fmi <- array(NA, dim=c(nM,nI), dimnames=list(namM,namI))
              Ffmi <- array(NA, dim=c(nF,nM,nI), dimnames=list(namF,namM,namI))
            
              Cmi <- inp@input[[i]]$C_mi
              Ci <- inp@input[[i]]$C_i
              Ymi <- inp@input[[i]]$Y_mi
              Yi <- inp@input[[i]]$Y_i
              Lref <- inp@input[[i]]$Lref_f_e
              Lrefm <- inp@input[[i]]$Lref_f_m_e
              FM <- inp@input[[i]]$fm
            
            if (attributes(Fini)$DimCst[1]>0 & attributes(Fini)$DimCst[2]>0) {
               Ffmi[] <- Fini[]
            } else {
            #ventilation métier
              if (attributes(Fini)$DimCst[2]==0) {                                         #ie F = Fi (cas 1, 2)
                if (!all(is.na(Cmi)) & !all(is.na(Ci)) & all(attributes(Cmi)$DimCst[2:3]>0) & attributes(Ci)$DimCst[3]>0) {   #ie C_mi renseigné avec composante métier et âge, et C_i renseigné avec composante âge -> cas 1 ou 2
                  if (attributes(Cmi)$DimCst[1]>0) {                                        #ie C_mi = Cfmi  (cas 2)
                    Ffmi[] <- Cmi[]
                    aggCmi <- apply(Cmi,3,sum,na.rm=TRUE) ; Ci[aggCmi>Ci] <- aggCmi[aggCmi>Ci]  #on remplace dans Ci les valeurs agrégées issues de C_mi supérieures
                    Ffmi <- Ffmi*rep(Fini/Ci,each=nF*nM)
                  } else {                                                                  #ie C_mi = Cmi (cas 1)
                    Fmi[] <- Cmi[]
                    aggCmi <- apply(Cmi,2,sum,na.rm=TRUE) ; Ci[aggCmi>Ci] <- aggCmi[aggCmi>Ci]
                    Fmi <- Fmi*rep(Fini/Ci,each=nM)
                  }
                } else { if (!all(is.na(Ymi)) & !all(is.na(Yi)) & all(attributes(Ymi)$DimCst[2:3]>0) & attributes(Yi)$DimCst[3]>0) {   #ie Y_mi renseigné avec composante métier et âge, et Y_i renseigné avec composante âge -> cas 1 ou 2
                          if (attributes(Ymi)$DimCst[1]>0) {                                #ie Y_mi = Yfmi  (cas 2)
                            Ffmi[] <- Ymi[]
                            aggYmi <- apply(Ymi,3,sum,na.rm=TRUE) ; Yi[aggYmi>Yi] <- aggYmi[aggYmi>Yi]
                            Ffmi <- Ffmi*rep(Fini/Yi,each=nF*nM)
                          } else {                                                          #ie Y_mi = Ymi (cas 1)
                            Fmi[] <- Ymi[]
                            aggYmi <- apply(Ymi,2,sum,na.rm=TRUE) ; Yi[aggYmi>Yi] <- aggYmi[aggYmi>Yi]
                            Fmi <- Fmi*rep(Fini/Yi,each=nM)
                          }
                         } else {                   #
                           Fmi[] <- Fini[]          #  added 18/07/16
                         }                          #
                       }
                } else {
                  Fmi[] <- Fini[]
                }
                
            #on poursuit avec la ventilation flottille si Ffmi non complété (on suppose à ce stade que Fmi a été complété)
            
              if (all(is.na(Ffmi))) {                                                       #ie cas 3 ou 4
                if (!all(is.na(Cmi)) & !all(is.na(Ci)) & all(attributes(Cmi)$DimCst[1:3]>0) & all(attributes(Ci)$DimCst[2:3]>0)) {   #ie cas 4
                    Ffmi[] <- Cmi[]
                    aggCmi <- apply(Cmi,2:3,sum,na.rm=TRUE) ; Ci[aggCmi>Ci] <- aggCmi[aggCmi>Ci]
                    Ffmi <- Ffmi*rep(Fmi/Ci,each=nF)
                } else {                                                                  
                    if (!all(is.na(Ymi)) & !all(is.na(Yi)) & all(attributes(Ymi)$DimCst[1:3]>0) & all(attributes(Yi)$DimCst[2:3]>0)) {   #ie cas 4
                      Ffmi[] <- Ymi[]
                      aggYmi <- apply(Ymi,2:3,sum,na.rm=TRUE) ; Yi[aggYmi>Yi] <- aggYmi[aggYmi>Yi]
                      Ffmi <- Ffmi*rep(Fmi/Yi,each=nF)
                    } else {                                                                # il faut alors utiliser les données de débarquements des feuillets Eco, redistribuées par métierBio via la matrice fm
                      if (!all(is.na(Lref)) & !all(is.na(FM)) & attributes(Ymi)$DimCst[1]==0 & attributes(Ymi)$DimCst[2]>0) { #cas 3 avec Ctot_m calculé à partir de Y_mi
                         CtotM <- apply(Ymi,1,sum,na.rm=TRUE)
                         Cfm <- FM*as.vector(Lref)
                         aggC <- apply(Cfm,2,sum,na.rm=TRUE) ; CtotM[aggC>CtotM] <- aggC[aggC>CtotM]
                         Ffmi[] <- Cfm/rep(CtotM,each=nF)
                         Ffmi <- Ffmi*rep(Fmi,each=nF)
                      } else {
                        if (!all(is.na(Lref)) & !all(is.na(FM)) & attributes(Yi)$DimCst[1]==0 & attributes(Yi)$DimCst[2]>0) { #cas 3 avec Ctot_m calculé à partir de Y_i
                         CtotM <- apply(Yi,1,sum,na.rm=TRUE)
                         Cfm <- FM*as.vector(Lref)
                         aggC <- apply(Cfm,2,sum,na.rm=TRUE) ; CtotM[aggC>CtotM] <- aggC[aggC>CtotM]
                         Ffmi[] <- Cfm/rep(CtotM,each=nF)
                         Ffmi <- Ffmi*rep(Fmi,each=nF) 
                        } else {    # cas ultime : on utilise les débarquements f*m de référence dans les feuillets Fleet sur Fi et Ytot calculé à partir de Yi 
                          if (attributes(Fini)$DimCst[1]==0 & attributes(Fini)$DimCst[2]==0 & !all(is.na(Lrefm)) & !all(is.na(Yi))) {
                            Ffmi[] <- (Lrefm/sum(Yi,na.rm=TRUE))%o%Fini            # fait le 26/07/2013
                          }
                        }
                      }
                    }
                }
              }
              }
              
            #ici, Ffmi devrait être dispo
            
            if (all(is.na(Ffmi))) stop("wrong or missing data for F allocation! Check C_mi, C_i, Y_mi, Y_i, or Lref_f_m inputs!!")  
              
            llF[[i]] <- Ffmi  
              
            } else {

            llF[[i]] <- adrop(Fq_fmi[[i]][1,1,,,,drop=FALSE],1:2)

            }
            }
         
         
        #2ème étape : on redéfinit chaque variable BIO suivant le niveau métier 'ECO' à l'aide de la matrice MM
        # -> ne concerne que la variable d_i  <-- peut évoluer avec l'évolution de 'sr' et l'intégration des variables de prix au niveau métier
        #-------------------------------------------------------------------------------
        
        for (i in inp@specific$Species) {

        namI <- inp@specific$Ages[[i]] ; nI <- length(namI)
        namME <- inp@specific$MetierEco ; nME <- length(namME)
         
        MM <- inp@input[[i]]$mm 
        if (is.null(MM)) MM <- NA
        if (length(MM)==1) tabMM <- NA else {
          tabMM <- cbind.data.frame(expand.grid(dimnames(MM)),value2=as.vector(MM))
          names(tabMM) <- c("fm","mEco","val2") }
        
        #conversion des données de mortalités --> on utilise les valeurs brutes
        tabF <- cbind.data.frame(expand.grid(dimnames(llF[[i]])),value1=as.vector(llF[[i]])) ; names(tabF) <- c("f","m","a","val1")
        tabF$fm <- paste(tabF$f,tabF$m,sep="__") 
        if (all(is.na(tabMM))) TABF <- cbind(tabF,mEco=tabF$m,val2=as.vector(MM)) else TABF <- merge(tabMM,tabF,all=TRUE)
        TABF$val <- TABF$val1*TABF$val2 ; TABF <- TABF[!is.na(TABF$val),] 
        TABF$f <- factor(as.character(TABF$f),levels=namF)
        TABF$mEco <- factor(as.character(TABF$mEco),levels=namME)    
        TABF$a <- factor(as.character(TABF$a),levels=namI)
        FF <- with(TABF,tapply(val,list(f,mEco,a),function(x) x))
        attributes(FF)$DimCst <- as.integer(c(nF,nME,nI,0))  
        
        if (attributes(inp@input[[i]]$F_fmi)$DimCst[1]>0 & attributes(inp@input[[i]]$F_fmi)$DimCst[2]>0) {
          inp@input[[i]]$F_i <- apply(inp@input[[i]]$F_fmi,3,sum,na.rm=TRUE)
          attributes(inp@input[[i]]$F_i)$DimCst <- as.integer(c(0,0,nI,0))
        } else {
          if (attributes(inp@input[[i]]$F_fmi)$DimCst[2]>0) {
            inp@input[[i]]$F_i <- apply(inp@input[[i]]$F_fmi,2,sum,na.rm=TRUE)
            attributes(inp@input[[i]]$F_i)$DimCst <- as.integer(c(0,0,nI,0))
          } else {
            inp@input[[i]]$F_i <- inp@input[[i]]$F_fmi
          }
        }
               
        inp@input[[i]]$F_fmi <- FF
         
        #conversion des variables ratios --> on applique les mêmes valeurs pour tous les métiers correspondants (on les suppose définie aux âges)  
          
        di <- inp@input[[i]]$d_i 
        if (attributes(di)$DimCst[2]>0) {
          if (attributes(di)$DimCst[1]==0) {
            vec_di <- llF[[i]] ; vec_di[] <- NA #matrice au format fmi
            vec_di[] <- rep(di,each=nF)
          } else {
            vec_di <- di
          }
          
          tabD <- cbind.data.frame(expand.grid(dimnames(vec_di)),value1=as.vector(vec_di)) ; names(tabD) <- c("f","m","a","val1")
          tabD$fm <- paste(tabD$f,tabD$m,sep="__") 
          if (all(is.na(tabMM))) TABD <- cbind(tabD,mEco=tabD$m,val2=MM) else TABD <- merge(tabMM,tabD,all=TRUE)
          TABD$val <- TABD$val1*TABD$val2 ; TABD <- TABD[!is.na(TABD$val),] 
          TABD$f <- factor(as.character(TABD$f),levels=namF)
          TABD$mEco <- factor(as.character(TABD$mEco),levels=namME)    
          TABD$a <- factor(as.character(TABD$a),levels=namI)
          FD <- with(TABD,tapply(val,list(f,mEco,a),function(x) x))
          attributes(FD)$DimCst <- as.integer(c(nF,nME,nI,0))  
          
          inp@input[[i]]$d_i <- FD
        }
        }  
  
  return(inp)
}  
  



#petite fonction de recodage interne à CLK
recFun <- function(df,field,rec) {
		Typ <- class(df[,field]) 
		fc <- factor(df[,field]) 
		Lev <- levels(fc)[!levels(fc)%in%rec$from]
		df[,field] <- factor(fc,levels=c(Lev,rec$from),labels=c(Lev,rec$to))
		eval(parse('',text=paste("df[,field] <- as.",Typ,"(as.character(df[,field]))",sep="")))
		return(df)
	}
 





#fonction pour générer des clés catégories/tailles à partir de fichiers d'extraction d'Arpege
CLK <- function(infile, field="ter",l.mult=1,out=NULL,...) {

tab <- read.table(infile,...)
#on peut avoir envie de recoder les occurences, ou subsetter sur quelques unes
requireNamespace("tcltk")
requireNamespace("tcltk2")

#on construit la df
CAT <- switch(field,loc=tab$categorie_locale,ter=tab$categorie_terrain)
df <- data.frame(categorie=CAT,recodage=CAT)
df <- as.matrix(unique(df))
df[is.na(df)] <- ""
recVal <- character(nrow(df))

tt <- tktoplevel()
tkfocus(tt)
tkwm.deiconify(tt)
#tkgrab.set(tt)
tkwm.title(tt, "Recodage Categories Commerciales")
fontHeading <- tkfont.create(family="times",weight="bold")
tkgrid(tklabel(tt,text="          Categorie          ",font=fontHeading),tklabel(tt,text=""),tklabel(tt,text="          Recodage          ",font=fontHeading))
tkgrid(tklabel(tt,text=""))
                   
for (i in 1:nrow(df)) {

  eval(parse('',text=paste("textEntry",i," <- tclVar(as.character(df[",i,",\"recodage\"]))",sep="")))
  eval(parse('',text=paste("textEntryWidget",i," <- tkentry(tt, width = 20, textvariable = textEntry",i,")",sep="")))  
  eval(parse('',text=paste("tkgrid(tklabel(tt,text=as.character(df[",i,",1])),tklabel(tt,text=\"\"),textEntryWidget",i,")",sep="")))
}                                                                                              
  

OnOK <- function()
{
    for (i in 1:nrow(df)) eval(parse('',text=paste("recVal[",i,"] <<- as.character(tclvalue(textEntry",i,"))",sep="")))
    tkgrab.release(tt)
    tkdestroy(tt)
}

OnCancel <- function() {

#    cbVal <<- rep("1",nrow(df))
    recVal <<- as.character(df$rec)
    tkgrab.release(tt)
    tkdestroy(tt)
  
    }
    
OK.but <- tkbutton(tt, text = "   OK   ", command = OnOK)
#Cancel.but <- tkbutton(tt, text = " Cancel ", command = OnCancel)
tkgrid(tklabel(tt,text=""))
tkgrid(tklabel(tt,text=""), OK.but, tklabel(tt,text=""))
tkgrid(tklabel(tt, text = "    "))

tkwait.window(tt)  
  
#on détermine les instructions de recodage en fonction de recVal
REF <- df[,"recodage",drop=FALSE] 
recList <- list(from=REF[REF!=recVal],to=recVal[REF!=recVal]) 
headeR <- switch(field,loc="categorie_locale",ter="categorie_terrain")
if (length(recList$from)>0) tab <- recFun(tab,headeR,recList) 

tab$poids_elevation[is.na(tab$poids_elevation)] <- (tab$poids_utilise*tab$facteur_elevation)[is.na(tab$poids_elevation)]
  
key <- tapply(tab$comptages*tab$poids_elevation/tab$poids_utilise,list(tab$classe*l.mult,tab[,headeR]),sum,na.rm=TRUE)
key[is.na(key)] <- 0

if (!is.null(out)) write.table(key,file=out,append=FALSE,quote=FALSE,sep="\t")
return(key)
}


#CLK(infile="C:/Documents and Settings/mmerzere/Bureau/ANCHOIS 2009.txt",field="ter",l.mult=10,out="C:/Documents and Settings/mmerzere/Bureau/CLK.txt",sep="\t",header=TRUE)
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------


standFormat <- function(DF,nbStep,modF,modM,modI,modC,alk,as.na=NULL) {

if (is.null(ncol(DF))) {

  return(as.numeric(as.character(DF))) 
  
} else {
  
  if (ncol(DF)==1) {

     Mat <- as.numeric(as.character(DF$value))
     attributes(Mat)$DimCst <- as.integer(rep(0,4))
     return(Mat)
     
  } else {
#  if ("t" %in% names(DF)) DF <- expand.time(DF,nbStep)
  
  if ("f" %in% names(DF) & !all(is.na(modF))) {
      DF$f <- factor(as.character(DF$f),levels=modF)
      dimL.f <- length(modF)
    } else {
      dimL.f <- 0
    }
  
  if ("m" %in% names(DF) & !all(is.na(modM))) {
      DF$m <- factor(as.character(DF$m),levels=modM)
      dimL.m <- length(modM)
    } else {
      dimL.m <- 0
    }
    
  if ("i" %in% names(DF)  & !all(is.na(modI))) {
      DF$i <- factor(as.character(DF$i),levels=modI) 
      dimL.i <- length(modI)
    } else {
      dimL.i <- 0
    } 
    
  if ("l" %in% names(DF)  & !all(is.na(DF))) {                           
      DF$l <- factor(as.character(DF$l),levels=paste("l__",dimnames(alk)[[1]],sep="")) 
      dimL.l <- 1
    } else {
      dimL.l <- 0
    }
    
  if ("c" %in% names(DF)  & !all(is.na(DF))) {                           
      DF$c <- factor(as.character(DF$c),levels=modC) 
      dimL.c <- length(modC)
    } else {
      dimL.c <- 0
    }
   
  if ("t" %in% names(DF)) {ve <- as.character(DF$t) 
                           unve <- unique(ve)
                           DF$t <- factor(ve,levels=unve[order(as.numeric(substr(unve,4,1000)))])
                            }
  
  
    dimL <- c(f=dimL.f, m=dimL.m,             # e=length(unique(DF$e)), 
              i=dimL.i, t=length(unique(DF$t)), c=dimL.c)
  
  eval(parse('',text=paste("Mat <- suppressWarnings(with(DF,tapply(as.numeric(as.character(value)),list(",
                            paste(c("l"[dimL.l>0],"f"[dimL["f"]>0],"m"[dimL["m"]>0],#"e"[dimL["e"]>0],  #on organisera les modules bio par espèce, 
                                    "i"[dimL["i"]>0],"c"[dimL["c"]>0],"t"[dimL["t"]>0]),collapse=","),  #et regroupés en liste après traitement 
                            "),function(x) x)))",sep="")))
  
  if (!is.null(as.na)) Mat[is.na(Mat)] <- as.na
  #on enlève des en-têtes les préfixes indicateurs
  dimnames(Mat) <- lapply(dimnames(Mat), function(x) sapply(1:length(x),function(y) substring(x[y],4,nchar(x[y]))))
  
  #on applique la clé taille-âge si besoin
  if ((!is.null(alk)) & (dimL.l>0)) {Mat <- t(alk/apply(alk,1,sum,na.rm=TRUE))%*%Mat
                                     Mat <- aperm(Mat, match(c("f"[dimL["f"]>0],"m"[dimL["m"]>0],"i"[dimL.l>0],"t"[dimL["t"]>0]),
                                                             c("i"[dimL.l>0],"f"[dimL["f"]>0],"m"[dimL["m"]>0],"t"[dimL["t"]>0])))
                                     dimL[3] = length(modI)
                                     }
  
  attributes(Mat)$DimCst <- as.integer(dimL[1:4]) ; if (dimL.c>0) {if (dimL.i>0) {attributes(Mat)$DimCst <- NULL
                                                                  } else {
                                                                   attributes(Mat)$DimCst[3] <- as.integer(dimL.c)
                                                                   }}

  return(Mat)
  
}}
}


#-------------------------------------------------------------------------------

twoDto1D <- function(df,dim="2D") {  
  
if (dim=="2D") {

  df <- as.matrix(df) ; df[is.na(df)] <- ""
  nColHead <- sum(df[1,]=="")
  dfTemp <- expand.grid(apply(df[-1,1:nColHead,drop=FALSE],1,paste,collapse=":@:@:"),df[1,-(1:nColHead)])

  if (nColHead==1){ 

    DF <- cbind.data.frame(as.character(dfTemp[,1]), as.character(dfTemp[,2]), as.numeric(df[-1,-1]))

  } else {

    DF <- cbind.data.frame(do.call("rbind",lapply(as.character(dfTemp[,1]), function(x) strsplit(x,":@:@:")[[1]])),
                         as.character(dfTemp[,2]), as.numeric(df[-1,-(1:nColHead)]))
  }

} else {

  DF <- df

}

  names(DF) <- c(substring(apply(DF,1,as.character)[1:(ncol(DF)-1)],1,1),"value")

  return(DF)  
}  
                
#-------------------------------------------------------------------------------

#extrapole les valeurs aux temps non décrits pour chaque pas de temps 
#(attention, n'est valable que si présence de champs "t" et "value" dans df   

expand.time <- function(df,t_init,nbStep=1,scenario=FALSE){

if (is.null(nbStep)) stop("nbStep parameter is NULL!!") 
#modalités de temps
occ = paste("t__",seq(t_init,length=nbStep),sep="")
#les modalités qui évolueront au cours du temps
mod = unique(df[,-match(c("t","value"),names(df)),drop=FALSE])
DF <- NULL ; TAB = NULL
for (i in 1:nbStep) {
  
  if (ncol(df)>2){
  
    #on créé la portion de table qu'il faudra remplir
    port <- cbind.data.frame(mod,data.frame(t=occ[i]))
    #on merge avec la partie de la table en commun
    tab <- merge(port,df,all.x=TRUE)
    #s'il reste des NA, on va chercher les valeurs de l'instant (t-1) -> table TAB
    if (any(is.na(tab$value)))
     {
      if (scenario) {
      
       #tab$value[is.na(tab$value)] <- 1
      
      } else {
      
       tab$value[is.na(tab$value)] <- merge(
        tab[is.na(tab$value),-match(c("t","value"),names(tab)),drop=FALSE],TAB,all.x=TRUE)$value
       }}
       
  } else { #ie seulement les champs 'temps' et 'valeurs'
 
    tab = merge(df,data.frame(t=occ[i]),all.y=TRUE)
    if (is.na(tab$value)) tab$value <- TAB$value
  
  }

TAB <- tab
DF <- rbind(DF,TAB)

}

return(DF)
}

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

                      
#on aura besoin des objets définis
#source("Z:/Projet/Projet SIAD/Param bio_eco/Modele/Input_object.r")


read.input <- function(file,t_init,nbStep,t_hist_max=t_init,desc="My input",folderFleet=NULL) {


#require(openxlsx)

tbls <- getSheetNames(file)
nam_stock <- tbls[grep("stock",tolower(tbls))]
if (is.null(folderFleet)) {
  nam_fleet <- tbls[substring(tolower(tbls),1,3)=="f__"]
} else {
  nam_fleet <- sort(list.files(folderFleet))
  nam_fleet <- gsub(".csv","",nam_fleet[substring(tolower(nam_fleet),1,3)=="f__"])
}


namList <- sapply(nam_stock,function(x) gsub("Stock__","",x))
#if (is.null(folderFleet)) {
#  namF <- sapply(nam_fleet,function(x) substring(x,1,nchar(x)-1))
#} else {
#  namF <- nam_fleet
#}
namF <- nam_fleet
#LL <- list(historique=list(),input=list(),scenario=list()) ; LL$historique <- LL$input <- vector("list", length(namList))
#names(LL$historique) <- names(LL$input) <- namList
 
#modalités flottilles et métiers (bio et eco)
modF <- NULL                                                                   
modMbio <- NULL
modMeco <- NULL


#yyy <- try(wb <- loadWorkbook(file))
#if (sum(attributes(yyy)$class%in%"try-error")==1) {
# index.openxlsx <- TRUE      #l'importation en utilisant XLConnect a échoué
# print("'openxlsx' package will be used")
#} else {
# print("'XLConnect' package will be used")
#}

#if (index.openxlsx) {
# detach("package:XLConnect", character.only = TRUE)
# require(openxlsx)  #on travaille désormais avec openxlsx
#}


if (length(nam_stock)>0) {
  for (k in nam_stock) {

  result <- read.xlsx(file,sheet=k,rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
  result[] <- lapply(result, function(x) gsub(",",".",as.character(x)))
  result <- as.matrix(rbind2("",result))

  result[is.na(result)] <- ""

  #on commence par analyser les modalités de chaque type de variable
  vec <- as.vector(result)
  vec <- vec[vec!=""]
  #MOD <- lapply(c("f__","m__"),function(x) {gsub(x,"",unique(vec[grepl(x,vec)]))})    #plus lent
  MOD <- lapply(c("f__","m__"),function(x) {gsub(x,"",unique(vec[sapply(vec,function(y) substring(y,1,3)==x)]))})
  modF <- unique(c(modF,MOD[[1]])) ; modMbio <- unique(c(modMbio,MOD[[2]]))
  }
}


FLEET <- NULL

if (!is.null(folderFleet)) {

  for (k in 1:length(namF)) {
  
  if (k==1) { 
  
    FLEET <- as.matrix(read.table(file.path(folderFleet,paste(namF[k],".csv",sep="")),sep=";",quote="\""))
    FLEET <- FLEET[,-match("annee",FLEET[1,])][,1:7]
  
   } else {
    
    FLtemp <- as.matrix(read.table(file.path(folderFleet,paste(namF[k],".csv",sep="")),sep=";",quote="\""))
    FLEET <- rbind2(FLEET,FLtemp[,-match("annee",FLtemp[1,])][-1,1:7])
   
   }
  }

} else {

  for (k in 1:length(namF)) {
  
  if (k==1) { 
  
      FLEET <- read.xlsx(file,sheet=namF[k],rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)[,1:7]
      FLEET[] <- lapply(FLEET, function(x) gsub(",",".",as.character(x)))
      FLEET <- as.matrix(FLEET)

    FLEET[is.na(FLEET)] <- ""
    #read.xls(file,sheet=namF[k],type="character",rowNames=FALSE,colNames=TRUE)[,1:7]
  
  } else {

      FLEETtmp <- read.xlsx(file,sheet=namF[k],rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)[-1,1:7]
      FLEETtmp[] <- lapply(FLEETtmp, function(x) gsub(",",".",as.character(x)))
      FLEETtmp <- as.matrix(FLEETtmp)

    FLEETtmp[is.na(FLEETtmp)] <- ""
  
    FLEET <- rbind2(FLEET,FLEETtmp)#read.xls(file,sheet=namF[k],type="character",rowNames=FALSE,colNames=FALSE)[-1,1:7])
          
  }
  }

}

FLEET[is.na(FLEET)] <- ""
#on en profite pour finaliser modF et créer modMeco
vec <- as.vector(FLEET)
vec <- vec[vec!=""]
#MOD <- lapply(c("f__","m__"),function(x) {gsub(x,"",unique(vec[grepl(x,vec)]))})
MOD <- lapply(c("f__","m__"),function(x) {gsub(x,"",unique(vec[sapply(vec,function(y) substring(y,1,3)==x)]))})
modF <- unique(c(modF,MOD[[1]])) ; modMeco <- unique(c(modMeco,MOD[[2]]))

if (length(modMbio)==0) modMbio <- modMeco

FLEET <- as.data.frame(FLEET[,c(1,4:7)])
if (!is.null(folderFleet)) FLEET <- FLEET[-1,]
FLEET[,5] <- suppressWarnings(as.numeric(as.character(FLEET[,5])))
FLEET[,1] <- gsub("v__","",FLEET[,1])  
FLEET[,4] <- gsub("e__","",FLEET[,4])  
names(FLEET) <- c("v","f","m","e","value")

#on distingue ce qui se décline par espèce --> à intégrer dans les paramètres stocks
Fstock <- FLEET[FLEET[,4]!="",]
Fleet <- FLEET[FLEET[,4]=="",]


nam_stock_bis <- unique(Fstock$e[!Fstock$e%in%c("espece",namList)])
LL <- list(historique=list(),input=list(),scenario=list()) ; LL$historique <- LL$input <- vector("list", length(namList)+length(nam_stock_bis))
names(LL$historique) <- names(LL$input) <- c(namList,nam_stock_bis)

##Scénarii

#scenar <- read.xls(file,sheet="Scénarii",type="character",rowNames=FALSE,colNames=FALSE)


  scenar <- read.xlsx(file,sheet="Scenarii",rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
  scenar[] <- lapply(scenar, function(x) gsub(",",".",as.character(x)))
  scenar <- as.matrix(rbind2("",scenar))

scenar[is.na(scenar)] <- ""

#on ne prend pas en compte les 100 premières lignes (Attention : format fixe à respecter)
scenar <- scenar[101:nrow(scenar),]


#il faut maintenant tenir compte des scénarios couplés ('... & ...') : on duplique afin de n'avoir qu'un scenario par ligne
repVec <- apply(scenar,1,function(y) length(gregexpr(" & ",as.character(y[1]))[[1]]))
count <- apply(scenar,1,function(y) grepl(" & ",as.character(y[1])))
repVec[count] <- repVec[count] + 1
newSc <- strsplit(as.vector(scenar[,1])," & ")
newSc <- lapply(newSc,function(x) if (length(x)==0) "" else x)
scenar <- scenar[rep(1:nrow(scenar),repVec),]
scenar[,1] <- unlist(newSc) 
scenar[scenar[,1]!="",1] <- paste("s__",scenar[scenar[,1]!="",1],sep="")   

scenar1 <- scenar[,1] ; scenar2 <- scenar[,2:ncol(scenar)]

indEmpt <- suppressWarnings(apply(scenar2,1,function(x) min(unlist(sapply(c("v__","t__","i__","f__","m__","l__","e__","c__"),grep,x)))))

#tout ce qui se trouve avant une modalité de variable est passé à ""
invisible(sapply(1:nrow(scenar2),function(x) if (is.finite(indEmpt[x])) {if (indEmpt[x]>1) scenar2[x,1:(indEmpt[x]-1)] <<- ""}))

#on filtre tout ce qui n'est ni numérique, ni paramètre
#conversion en numérique

num <- apply(suppressWarnings(apply(scenar2,1,as.numeric)),1,as.character)

#on ajoute les paramètres
indic <- substring(scenar2,1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")          #attention : depuis ajout openxlsx, scenar2 --> as.matrix(scenar2)
num[indic] <- scenar2[indic] ; num[is.na(num)] <- ""  ; scenar <- cbind(scenar1,num)


#on sépare les tables
indicRow <- apply(scenar,1,function(x) any(substring(x,1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")))
scenar[!indicRow,] <- rep("",ncol(scenar))
indicTbl <- cumsum(apply(scenar,1,function(x) all(x=="")))

sepSc <- split(as.data.frame(scenar)[indicRow,],indicTbl[indicRow])



##on colle le préfixe à la colonne scénario
#sepSc <- lapply(sepSc, function(x) {x[x[,1]!="",1] <- paste("s__",x[x[,1]!="",1],sep="")
#                           return(x)})
#

#on distingue pour commencer les tables 1D des tables 2D
tbl2DindS <- lapply(sepSc,function(x) !substring(as.character(x[1,1]),1,3)%in%c("s__","v__","t__","i__","f__","m__","l__","e__","c__"))
tbl2DS <- sepSc[(1:length(sepSc))[unlist(tbl2DindS)]]
tbl1DS <- sepSc[(1:length(sepSc))[!unlist(tbl2DindS)]]

#règles des tables 1d :

  #une seule colonne de numériques

if (length(tbl1DS)>0) tbl1DS <- lapply(tbl1DS,function(x) x[,1:((1:ncol(x))[!substring(as.matrix(x[1,]),1,3)%in%c("s__","v__","t__","i__","f__","m__","l__","e__","c__")][1])])  
                                                                                                                  

#règles des tables 2d :

  #une colonne de numérique doit être précédée d'une variable
  
if (length(tbl2DS)>0) tbl2DS <- lapply(tbl2DS,function(x) x[,apply(x,2,function(y) any(substring(as.matrix(y),1,3)%in%c("s__","v__","t__","i__","f__","m__","l__","e__","c__")))])  
 

#on peut maintenant séparer les variables
  #pour cela, il faut tout mettre sous forme 1D

if (length(tbl2DS)>0) tbl2S <- lapply(tbl2DS,twoDto1D,"2D") else tbl2S <- NULL
if (length(tbl1DS)>0) tbl1S <- lapply(tbl1DS,twoDto1D,"1D") else tbl1S <- NULL


ListS <- c(tbl1S,tbl2S) 

iCATtab <- NULL


  Market <- read.xlsx(file,sheet="Market",rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
  Market[] <- lapply(Market, function(x) gsub(",",".",as.character(x)))
  Market <- as.matrix(rbind2("",Market))

#Market <- read.xls(file,sheet="Marché",type="character",rowNames=FALSE,colNames=FALSE)

Market[is.na(Market)] <- ""




#paramètres stock
if (length(nam_stock)>0) {

  for (k in 1:length(nam_stock)) {

    result <- read.xlsx(file,sheet=nam_stock[k],rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
    result[] <- lapply(result, function(x) gsub(",",".",as.character(x)))
    result <- as.matrix(rbind2("",result))

  #result <- read.xls(file,sheet=substring(nam,1,nchar(nam)-1),type="character",rowNames=FALSE,colNames=FALSE)
  result[is.na(result)] <- ""

  #on va ajouter la table market
  MarketSp <- Market[Market[,6]%in%paste("e__",namList[k],sep=""),c(1,4:5,7:8),drop=FALSE]
  #on sépare les tables par variables en intercalant une (ou 2) ligne vide
  tabicat <- do.call("rbind",lapply(c("v__OD_e","v__theta_e","v__Pst_e","v__P_fmce","v__Q_fmce","v__alpha_fmce","v__beta_fmce","v__gamma_fmce"),
                      function(x) MarketSp[c(NA,grep(x,MarketSp[,1]),NA),]))
  tabicat[,5][tabicat[,5]==""] <- "-1"

  #on commence par analyser les modalités de chaque type de variable (age et taille)
  vec <- as.vector(result)
  vec <- vec[vec!=""]
  #MOD <- lapply(c("i__","l__","c__"),function(x) {gsub(x,"",unique(vec[grepl(x,vec)]))})
  MOD <- lapply(c("i__","l__"),function(x) {gsub(x,"",unique(vec[sapply(vec,function(y) substring(y,1,3)==x)]))})



  if (k==1) {            #on insère les variables 'fm' et 'icat' et marché

  #FM <- read.xls(file,sheet="fm_matrix",type="character",rowNames=FALSE,colNames=FALSE)
  #MM <- read.xls(file,sheet="mm_matrix",type="character",rowNames=FALSE,colNames=FALSE)
  #ICAT <- read.xls(file,sheet="icat_matrix",type="character",rowNames=FALSE,colNames=FALSE)

    FM <- read.xlsx(file,sheet="fm_matrix",rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
    FM[] <- lapply(FM, function(x) gsub(",",".",as.character(x)))
    FM <- as.matrix(rbind2("",FM))

  FM[is.na(FM)] <- ""


    MM <- read.xlsx(file,sheet="mm_matrix",rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
    MM[] <- lapply(MM, function(x) gsub(",",".",as.character(x)))
    MM <- as.matrix(rbind2("",MM))

  MM[is.na(MM)] <- ""


    ICAT <- read.xlsx(file,sheet="icat_matrix",rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
    ICAT[] <- lapply(ICAT, function(x) gsub(",",".",as.character(x)))
    ICAT <- as.matrix(rbind2("",ICAT))

  ICAT[is.na(ICAT)] <- ""


  #on transforme un peu les deux matrices pour qu'elles aient le même nombre de colonnes
  ncolMax <- max(ncol(result),ncol(FM),ncol(ICAT),ncol(tabicat),ncol(MM))
  result <- rbind2(rbind2(rbind2(rbind2(eval(parse('',text=paste("cbind(",paste(c("result",rep("\"\"",ncolMax-ncol(result))),collapse=","),")",sep=""))),
                   eval(parse('',text=paste("cbind(",paste(c("FM",rep("\"\"",ncolMax-ifelse(length(FM)>0,ncol(FM),0))),collapse=","),")",sep="")))),
                   eval(parse('',text=paste("cbind(",paste(c("MM",rep("\"\"",ncolMax-ifelse(length(MM)>0,ncol(MM),0))),collapse=","),")",sep="")))),
                   eval(parse('',text=paste("cbind(",paste(c("ICAT",rep("\"\"",ncolMax-ifelse(length(ICAT)>0,ncol(ICAT),0))),collapse=","),")",sep="")))),
                   eval(parse('',text=paste("cbind(",paste(c("tabicat",rep("\"\"",ncolMax-ifelse(length(tabicat)>0,ncol(tabicat),0))),collapse=","),")",sep=""))))
  } else {  #on insère seulement les variables marché

  ncolMax <- max(ncol(result),ncol(tabicat))
  result <- rbind2(eval(parse('',text=paste("cbind(",paste(c("result",rep("\"\"",ncolMax-ncol(result))),collapse=","),")",sep=""))),
                   eval(parse('',text=paste("cbind(",paste(c("tabicat",rep("\"\"",ncolMax-ncol(tabicat))),collapse=","),")",sep=""))))

  }

  result[is.na(result)] <- "NA"

  #on termine en analysant les modalités de la dernière variable (catégorie)
  MOD[[3]] <- gsub("c__","",unique(MarketSp[,4][sapply(MarketSp[,4],function(y) substring(y,1,3)=="c__")]))


  #on commence par extraire le tableau de codage des variables
  indEmpt <- suppressWarnings(apply(result,1,function(x) min(unlist(sapply(c("v__","t__","i__","f__","m__","l__","e__","c__"),grep,x)))))
  recode <- result[5:33,1:4] #recode <- result[5:(match(TRUE,is.finite(indEmpt))-1),1:4]
  #on complète les recodages non spécifiés
  recode[recode[,2]%in%c("","NA"),2] <- recode[recode[,2]%in%c("","NA"),1]
  recode[is.na(recode[,2]),2] <- recode[is.na(recode[,2]),1]
  #table de recodage des variables
  rec <- as.data.frame(recode[2:(match("",recode[,1])-1),])
  names(rec) <- recode[1,]


  #tout ce qui se trouve avant une modalité de variable est passé à ""
  invisible(sapply(1:nrow(result),function(x) if (is.finite(indEmpt[x])) {if (indEmpt[x]>1) result[x,1:(indEmpt[x]-1)] <<- ""}))

  #on filtre tout ce qui n'est ni numérique, ni paramètre
  #conversion en numérique

  num <- apply(suppressWarnings(apply(result,1,as.numeric)),1,as.character)

  #on ajoute les paramètres
  indic <- substring(result,1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")
  num[indic] <- result[indic]

  indicRow <- apply(result,1,function(x) any(substring(x,1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")))
  indicTbl <- cumsum(apply(num,1,function(x) all(is.na(x))))
  #on sépare les tables (sauts de lignes)
  sepTabl <- split(as.data.frame(num)[indicRow,],indicTbl[indicRow])
  tbl <- lapply(sepTabl,function(x) x[,!apply(x,2,function(y) all(is.na(y)))])

  #il faut maintenant filtrer toutes les anomalies de format

  #on distingue pour commencer les tables 1D des tables 2D
  tbl2Dind <- lapply(tbl,function(x) !substring(as.character(x[1,1]),1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__"))
  tbl2D <- tbl[(1:length(tbl))[unlist(tbl2Dind)]]
  tbl1D <- tbl[(1:length(tbl))[!unlist(tbl2Dind)]]

  #règles des tables 1d :

    #une seule colonne de numériques

  if (length(tbl1D)>0) {tbl1D <- lapply(tbl1D,function(x) x[,1:((1:ncol(x))[!substring(as.matrix(x[1,]),1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")][1])])
                        invisible(lapply(1:length(tbl1D),function(x) tbl1D[[x]][tbl1D[[x]]==-1] <<- as.numeric(NA)))}

  #règles des tables 2d :

    #une colonne de numérique doit être précédée d'une variable

  if (length(tbl2D)>0) {tbl2D <- lapply(tbl2D,function(x) x[,apply(x,2,function(y) any(substring(as.matrix(y),1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")))])
                        invisible(lapply(1:length(tbl2D),function(x) tbl2D[[x]][tbl2D[[x]]==-1] <<- as.numeric(NA)))}

  #on peut maintenant séparer les variables
    #pour cela, il faut tout mettre sous forme 1D



  if (length(tbl2D)>0) tbl2 <- lapply(tbl2D,twoDto1D,"2D") else tbl2 <- NULL
  if (length(tbl1D)>0) tbl1 <- lapply(tbl1D,twoDto1D,"1D") else tbl1 <- NULL


  List <- c(tbl1,tbl2)

  #on va légèrement retoucher la table mm pour injecter dans la colonne value la place de l'indice métier_eco correspondant
  indMM <- (1:length(List))[unlist(lapply(List,function(x) ("v__mm"%in%x$v)))]
  if (length(indMM)>0) MMmodif <- List[[indMM]]
  #for (i in indMM) {
  #  tempMM <- MMmodif <- List[[i]]                                         #modif MM  23/02/2012
  #  tempMM$value[!is.na(tempMM$value)] <- match(gsub("m__","",tempMM[,5]),modMeco)[!is.na(tempMM$value)]
  #  tempMM <- tempMM[,c(1:4,6)]
  #  List[[i]] <- tempMM[apply(tempMM,1,function(x) !any(is.na(x))),]
  #  }            #modif 17/04/2013


  #si k==1, on n'oublie pas d'extraire les données "fm" et "mm" pour les insérer avec les données flottille par espèce
  if (k==1) {
  TAB_FM <- do.call("rbind",lapply(List,function(x) if ("v__fm"%in%x$v) {x$e <- gsub("e__","",x$e) ; x$v <- gsub("v__","",x$v) ; return(x[x$v%in%"fm",names(Fstock)]) } else NULL))

  TAB_MM <- do.call("rbind",lapply(List,function(x) if ("v__mm"%in%x$v) {x$e <- gsub("e__","",x$e) ; x$v <- gsub("v__","",x$v) ; return(x[x$v%in%"mm",]) } else NULL))

  #List <- lapply(List,function(x) if ("v__fm"%in%x$v) return(NULL) else x)
  iCATtab <- do.call("rbind",lapply(List,function(x) if ("v__icat"%in%x$v) return(x[x$v%in%"v__icat",])  else NULL))


  LLL <- length(List)
  invisible(sapply(LLL:1,function(x) if ("v__icat"%in%List[[x]]$v) List[[x]] <<- NULL)) #on efface les éléments de v_icat
  Fstock <- rbind(Fstock,TAB_FM)#,TAB_MM)
  }

  List <- c(List,list(iCATtab[iCATtab$e%in%paste("e__",namList[k],sep=""),-2]))

  #on peut intégrer ici les tables de scénarios

  #test pour savoir quelles tables intégrer dans la liste déjà construite
  testS <- lapply(ListS,function(x) { tst <- FALSE
                                      if ((as.character(x$v[1])%in%paste("v__",as.character(rec$Alias),sep="")) & !"e"%in%names(x)) {
                                        tst <- TRUE
                                      } else {
                                        if ("e"%in%names(x)) {
                                          if (namList[k]%in%gsub("e__","",as.character(x$e))) tst <- TRUE
                                          }
                                      }
                                        return(tst)})

  #test de donnée flottille (à n'opérer que lors de la première itération)
  if (k==1) testF <- lapply(ListS,function(x) { (!as.character(x$v[1])%in%paste("v__",as.character(rec$Alias),sep="")) &
                                      (!"e"%in%names(x)) })


  ListStemp <- lapply(ListS,function(x) {x$v <- paste(x$v,x$s,sep="") ; return(x)})
  keep <- ListStemp[unlist(testS)]
  #on retire la colonne "espèce" après avoir filtré sur l'espèce
  keep <- lapply(keep,function(x) x[x$e%in%paste("e__",namList[k],sep=""),])
  keep <- lapply(keep,function(x) if ("e"%in%names(x)) x[,-match("e",names(x))])
  if (k==1) keepF <-  ListStemp[unlist(testF)]
  if (k==1) {if (length(keepF)>0) {keepF <- lapply(keepF,function(x) {x$v <- paste(x$v,"f__",sep="") ; return(x)})
                                    keep <- c(keep,keepF)}
                                    } #on balise les infos Fleet

  if (length(keep)>0) List <- c(List,keep)



  List <- lapply(List,function(x) split(x[,-match("v",names(x)),drop=FALSE],as.character(x[,match("v",names(x))])))
  namL <- gsub("v__","",unlist(lapply(List,names)))
  List <- unlist(List,recursive=FALSE,use.names = FALSE)
  names(List) <- namL

  #il faut regrouper les tables de même variable découpées en plusieurs parties
  Nam <- unique(names(List))
  List <- lapply(Nam,function(x) {tt <- do.call("rbind",List[names(List)%in%x])
                                  rownames(tt) <- NULL
                                  return(tt)})
  names(List) <- Nam


  #on en fait maintenant des objets standards accompagnés de leur attribut 'DimCst' pour les inputs, et on laisse sous forme de DF pour l'historique
  #il faut considérer l'historique... (t<=t_init)
  listHisto <- List[!grepl("s__",names(List))]
  invisible(sapply(c("v__","t__","i__","f__","m__","l__","e__","c__") ,
                function(y) listHisto <<- lapply(listHisto,function(x) as.data.frame(gsub(y,"",as.matrix(x))))))
  listHisto <- lapply(listHisto, function(x) {if (ncol(x)==1) {
                                                return(as.numeric(as.character(x$value)))
                                              } else {
                                                rownames(x) <- 1:nrow(x)
                                                if ("t"%in%names(x)) {    #si l'occurence n'est pas présente, on prend toute la table
                                                  rp <- match(as.character(t_hist_max),as.character(x$t))
                                                  if (!is.na(rp)) {
                                                  x <- x[unique(sort(c(1:match(as.character(t_hist_max),as.character(x$t)),
                                                                        (1:nrow(x))[x$t%in%t_hist_max]))),]
                                                  }}
                                                x$value <- as.numeric(as.character(x$value))
                                                return(x)
                                                }})
  #... et les paramètres d'entrée (t>=t_init)
  listInput <- List[!grepl("s__",names(List))]

  listInput <- lapply(listInput, function(x) {if (ncol(x)==1) {
                                                return(x)
                                              } else {
                                                if ("t"%in%names(x)) {
                                                #il faut distinguer ce qui va servir à calculer la valeur initiale (tab), et ce qui sert pour les projections (proj)
                                                  ind <- grep("t__t__",as.character(x$t))
                                                  indic <- length(ind)>0
                                                  #occ <- unique(as.character(x$t)) ; occ <- occ[length(occ)]
                                                  if (indic) tab <- x[ind,] else
                                                              tab <- x[x$t%in%paste("t__",t_init,sep=""),]
                                                  if (indic) {

                                                    if (max(ind)<nrow(x)) {
                                                      proj <- x[(max(ind)+1):nrow(x),]
                                                    } else {
                                                      proj <- NULL }  #pas de donnée de projection

                                                  } else {

                                                  if (match(paste("t__",t_init,sep=""),rev(as.character(x$t)))==1) {
                                                      proj <- NULL
                                                  } else {
                                                      proj <- x[(nrow(x)+2-match(paste("t__",t_init,sep=""),rev(as.character(x$t)))):nrow(x),] }

                                                  }

                                                  if (ncol(tab)==2) {

                                                    if (is.null(proj))
                                                      return(mean(as.numeric(as.character(tab$value)),na.rm=TRUE))
                                                    else {
                                                      intTab <- rbind.data.frame(tab[nrow(tab),],proj)
                                                      intTab$value[1] <- mean(as.numeric(as.character(tab$value)),na.rm=TRUE)
                                                      intTab$t <- gsub("t__t__","t__",intTab$t)
                                                      return(expand.time(intTab,t_init,nbStep))
                                                    }

                                                  } else {

                                                    nams <- names(tab) ; nams <- nams[-match(c("t","value"),nams)]
                                                    eval(parse('',text=paste("TAB <- with(tab,aggregate(as.numeric(as.character(value)),list(",
                                                            paste(nams,collapse=","),"),mean,na.rm=TRUE))",sep="")))

                                                    if (is.null(proj)) {
                                                        names(TAB) <- c(nams,"value")
                                                        return(TAB)
                                                    } else {
                                                        TAB$newField <- paste("t__",t_init,sep="")
                                                        names(TAB) <- c(nams,"value","t")
                                                        return(expand.time(rbind.data.frame(TAB[,names(proj)],proj),t_init,nbStep))
                                                    }
                                                  }
                                                } else {
                                                return(x)
                                                }
                                              }
                                              })

  #on recode les noms de variables conformément à 'rec' (on ajoute les variables SS3 pilotables par le module scenario)
  SS3nam_N <- paste("Ni0_S",1:4,sep="")
  SS3nam_F <- paste("Ffmi_",as.vector(t(outer(paste("S",1:4,sep=""),paste("M",1:4,sep=""),paste,sep=""))),sep="")
  SS3nam_Flanwt <- paste("FLWfmi_",as.vector(t(outer(paste("S",1:4,sep=""),paste("M",1:4,sep=""),paste,sep=""))),sep="")
  SS3nam_Fdiswt <- paste("FDWfmi_",as.vector(t(outer(paste("S",1:4,sep=""),paste("M",1:4,sep=""),paste,sep=""))),sep="")

  renam <- c(as.character(rec$Variable),as.character(rec$Variable),c("OD_e","theta_e","Pst_e"),SS3nam_N,SS3nam_F,SS3nam_Flanwt,SS3nam_Fdiswt) ;
  names(renam) <- c(as.character(rec$Alias),as.character(rec$Variable),c("OD_e","theta_e","Pst_e"),SS3nam_N,SS3nam_F,SS3nam_Flanwt,SS3nam_Fdiswt)
  renam <- renam[!duplicated(names(renam))]
  names(listHisto) <- renam[names(listHisto)] ; names(listInput) <- renam[names(listInput)]
  #et on applique le multiplicateur à chaque variable dans les deux listes
  rec <- rec[suppressWarnings(!is.na(as.numeric(as.character(rec$Multi)))),]
  invisible(sapply(1:nrow(rec),function(x) if (as.character(rec$Variable)[x]%in%names(listHisto))
          try(listHisto[[as.character(rec$Variable)[x]]]$value <<-
                listHisto[[as.character(rec$Variable)[x]]]$value*as.numeric(as.character(rec$Multi))[x],silent=TRUE)))

  #il ne reste plus qu'à ajouter à listInput les paramètres par espèce issus des fichiers flottilles
  Fle <- Fstock[Fstock[,4]%in%namList[k],] ; n <- unique(Fle[,1])
  Fle <- lapply(n,function(x) {df <- as.data.frame(Fle[Fle[,1]%in%x,c(2,3,5)]); rownames(df) <- 1:nrow(df); return(df)})
  Fle <- lapply(1:length(n),function(x) {df <- Fle[[x]] ; if (all(is.na(df[,3]))) df[,1:2] <- "" ; return(df)})
  Fle <- lapply(Fle,function(x) x[,c(apply(x[,1:(ncol(x)-1)],2,function(y) !all(y=="")),TRUE)])   #on ejecte les colonnes vides
  #on gère les constantes
  Fle <- lapply(Fle,function(x) if (is.null(dim(x))) return(x[1]) else return(x))
  names(Fle) <- n
  listInput <- c(listInput,Fle)

  ALK <- NULL
  #on commence par analyser la clé taille-âge si elle existe
  if ("alk"%in%names(listInput)) {

    if (!all(is.na(listInput$alk))) {

      if (all(c("l","i","value")%in%names(listInput$alk))) {

  ALK <- suppressWarnings(with(listInput$alk,tapply(as.numeric(as.character(value)),list(as.character(l),
                                                  factor(as.character(i),levels=paste("i__",MOD[[1]],sep=""))),function(x) x)))
  dimnames(ALK) <- lapply(dimnames(ALK), function(x) sapply(1:length(x),function(y) substring(x[y],4,nchar(x[y]))))

  listInput <- listInput[-match("alk",names(listInput))]

      }
    }
  }

  #on ajoute mm à partir de TAB_MM
  tabMMtemp <- TAB_MM[TAB_MM$e%in%namList[k],c(2,4,5,6)]
  if (is.null(tabMMtemp) || nrow(tabMMtemp)==0) {
   tabMMtemp <- NULL
  } else {
   colnames(tabMMtemp) <- c("f","mBio","mEco","value")
   rownames(tabMMtemp) <- 1:nrow(tabMMtemp)
  }

  #mod_i <- unique(unlist(lapply(listInput,function(x) if (length(x)<2) return(NULL) else if ("i"%in%names(x)) return(as.character(x$i)) else return(NULL) )))

  listInputBio <- lapply(listInput[!names(listInput)%in%c("GVLref_f_m_e","Lref_f_m_e","dd1_f_m_e","dd2_f_m_e","P_fmce","Q_fmce","P_fme","Q_fme","mm")],
                      standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMbio,sep=""),paste("i__",MOD[[1]],sep=""),paste("c__",MOD[[3]],sep=""),ALK)
  listInputEco <- lapply(listInput[c("GVLref_f_m_e","Lref_f_m_e","dd1_f_m_e","dd2_f_m_e","P_fmce","Q_fmce","P_fme","Q_fme")],
                      standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),paste("i__",MOD[[1]],sep=""),paste("c__",MOD[[3]],sep=""),ALK)
  listInput <- c(listInputBio,list(mm=tabMMtemp),listInputEco)

  invisible(sapply(1:nrow(rec),function(x) if (as.character(rec$Variable)[x]%in%names(listInput))
          try(listInput[[as.character(rec$Variable)[x]]] <<-
                listInput[[as.character(rec$Variable)[x]]]*as.numeric(as.character(rec$Multi))[x],silent=TRUE)))

  #on ajoute les occurences âges, tailles et catégories
  listInput$modI <- MOD[[1]] ; listInput$modL <- MOD[[2]] ; listInput$modC <- MOD[[3]] ; listInput$alk <- ALK

  listScenar <- List[grepl("s__",names(List))]
  listScenar <- lapply(listScenar,function(x) if ("t"%in%names(x)) expand.time(x,t_init,nbStep,TRUE) else return(x))

  indEc <- apply(do.call("rbind",lapply(c("nbv_f_m","cnb_f_m","nbds_f_m","effort1_f_m","effort2_f_m","Lref_f_m","Lref_f_m_e","Lref_f_m_e","Lref_f_m_e","GVLref_f_m",
          "GVLref_f_m_e","GVLref_f_m_e","GVLref_f_m_e","gc_f_m","nbh_f_m","nbtrip_f_m","fc_f_m","vf_f_m",
          "ovc_f_m","oilc_f_m","bc_f_m","foc_f_m","icec_f_m","cshr_f_m"),function(x) grepl(x,names(listScenar)))),2,any)

  listScenarBio <- lapply(listScenar[!indEc],standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),paste("i__",MOD[[1]],sep=""),paste("c__",MOD[[3]],sep=""),ALK,NA)
  listScenarEco <- lapply(listScenar[indEc],standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),paste("i__",MOD[[1]],sep=""),paste("c__",MOD[[3]],sep=""),ALK,NA)
  listScenar <- c(listScenarBio,listScenarEco)

  #on ajoute l'attribut 'intervention'
  for (nn in names(listScenar)) {
    if (length(grep("__x__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(1)  #1 -> multiplication
    if (length(grep("__+__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(2)  #2 -> addition
    if (length(grep("__o__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(3)  #3 -> remplacement
    if (length(grep("__x__",nn))==0 & length(grep("__+__",nn))==0 & length(grep("__o__",nn))==0) attributes(listScenar[[nn]])$type <- as.integer(0)  #0 -> par défaut (multiplication ??)
  }

  names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__x__","",NN))
  names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__+__","",NN))
  names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__o__","",NN))

  #if (k==1) {disti <- grepl("f__",names(listScenar)) ; )
  namS <- names(listScenar) ; namV <- sapply(namS,function(x) strsplit(x,"s__")[[1]][1])
  namS <- sapply(namS,function(x) strsplit(x,"s__")[[1]][2])
  #on ne procède au recodage que sur les variables Stock (exceptée les variables internes)    <<----- variables internes à mettre à jour ici  <<<--------
  testSt <- grepl("f__",namS) | grepl("Foth_i",namV) | grepl("F_fmi",namV) | grepl("Froth_i",namV) | grepl("FDWToth_i",namV) | grepl("FRWToth_i",namV) | grepl("Ni0_",namV) | grepl("N_i0t",namV)

  if (length(listScenar)>0) {
    names(listScenar)[!testSt] <- paste(paste(renam[namV],namS,sep="s__"),namList[k],sep="e__")[!testSt]
    names(listScenar)[testSt] <- paste(names(listScenar)[testSt],namList[k],sep="e__")
    }

  #il ne reste plus qu'à transformer les captures en nombres en captures en poids   (on fera de même pour la partie historique)
  if (length(listInput$Y_mi)==0) {
    if (length(listInput$C_mi)!=0) {
      listInput$Y_mi <- listInput$C_mi
      indProd <- attributes(listInput$C_mi)$DimCst[1:2]
      indProd[indProd==0] <- 1
      listInput$Y_mi <- listInput$Y_mi*rep(listInput$wL_i,each=prod(indProd))/1000
  }
  }

  if (length(listInput$Y_i)==0) {
    if (length(listInput$C_i)!=0) {
      listInput$Y_i <- listInput$C_i
      indProd <- attributes(listInput$C_i)$DimCst[1:2]
      indProd[indProd==0] <- 1
      listInput$Y_i <- listInput$Y_i*rep(listInput$wL_i,each=prod(indProd))/1000
    }
  }

  listInput[is.na(names(listInput))] <- NULL

  LL$historique[[k]] <- listHisto ; LL$input[[k]] <- listInput ; LL$scenario <- c(LL$scenario,listScenar)

  }

} else {

#on rattrape le coup sur listScenar si pas d'espèce dynamique

  #on filtre de ListS les scénarios espèces, ils seront pris en charge ensuite dans la boucle des espèces statiques
  ListStmp <- ListS[!unlist(lapply(ListS,function(x) "e"%in%names(x)))]
  List <- lapply(ListStmp,function(x) {x$v <- paste(x$v,x$s,sep="") ; return(x)})
  List <- lapply(List,function(x) split(x[,-match("v",names(x)),drop=FALSE],as.character(x[,match("v",names(x))])))
  namL <- gsub("v__","",unlist(lapply(List,names)))
  List <- unlist(List,recursive=FALSE,use.names = FALSE)
  names(List) <- namL
  Nam <- unique(names(List))
  List <- lapply(Nam,function(x) {tt <- do.call("rbind",List[names(List)%in%x])
                                  rownames(tt) <- NULL
                                  return(tt)})
  names(List) <- Nam
  listScenar <- List[grepl("s__",names(List))]
  listScenar <- lapply(listScenar,function(x) if ("t"%in%names(x)) expand.time(x,t_init,nbStep,TRUE) else return(x))
  listScenar <- lapply(listScenar,standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),NA,NA,NA,NA)

  for (nn in names(listScenar)) {
    if (length(grep("__x__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(1)  #1 -> multiplication
    if (length(grep("__+__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(2)  #2 -> addition
    if (length(grep("__o__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(3)  #3 -> remplacement
    if (length(grep("__x__",nn))==0 & length(grep("__+__",nn))==0 & length(grep("__o__",nn))==0) attributes(listScenar[[nn]])$type <- as.integer(0)  #0 -> par défaut (multiplication ??)
  }

  names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__x__","",NN))
  names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__+__","",NN))
  names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__o__","",NN))

  names(listScenar) <- paste(names(listScenar),"e__Fleet",sep="")

  LL$scenario <- c(LL$scenario,listScenar)

}

#-----------------------------------------------------------------------------------------

if (length(nam_stock_bis)>0) {

for (k in 1:length(nam_stock_bis)) {

nam <- nam_stock_bis[k]

#on va ajouter la table market
MarketSp <- Market[Market[,6]%in%paste("e__",nam,sep=""),c(1,4:5,8),drop=FALSE]
#on sépare les tables par variables en intercalant une (ou 2) ligne vide
tabicat <- do.call("rbind",lapply(c("v__OD_e","v__theta_e","v__Pst_e","v__P_fme","v__Q_fme","v__alpha_fme","v__beta_fme","v__gamma_fme"),
                    function(x) MarketSp[c(NA,grep(x,MarketSp[,1]),NA),]))
tabicat[,4][tabicat[,4]==""] <- "-1"

result <- tabicat
result[is.na(result)] <- "NA"

#on commence par extraire le tableau de codage des variables
indEmpt <- suppressWarnings(apply(result,1,function(x) min(unlist(sapply(c("v__","t__","i__","f__","m__","l__","e__","c__"),grep,x)))))
#tout ce qui se trouve avant une modalité de variable est passé à ""
invisible(sapply(1:nrow(result),function(x) if (is.finite(indEmpt[x])) {if (indEmpt[x]>1) result[x,1:(indEmpt[x]-1)] <<- ""}))

#on filtre tout ce qui n'est ni numérique, ni paramètre
#conversion en numérique

num <- apply(suppressWarnings(apply(result,1,as.numeric)),1,as.character)

#on ajoute les paramètres
indic <- substring(result,1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")
num[indic] <- result[indic]

indicRow <- apply(result,1,function(x) any(substring(x,1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")))
indicTbl <- cumsum(apply(num,1,function(x) all(is.na(x))))
#on sépare les tables (sauts de lignes)
sepTabl <- split(as.data.frame(num)[indicRow,],indicTbl[indicRow])
tbl <- lapply(sepTabl,function(x) x[,!apply(x,2,function(y) all(is.na(y)))])

#il faut maintenant filtrer toutes les anomalies de format

#on distingue pour commencer les tables 1D des tables 2D
tbl2Dind <- lapply(tbl,function(x) !substring(as.character(x[1,1]),1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__"))
tbl2D <- tbl[(1:length(tbl))[unlist(tbl2Dind)]]
tbl1D <- tbl[(1:length(tbl))[!unlist(tbl2Dind)]]

#règles des tables 1d :

  #une seule colonne de numériques

if (length(tbl1D)>0) {tbl1D <- lapply(tbl1D,function(x) x[,1:((1:ncol(x))[!substring(as.matrix(x[1,]),1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")][1])])
                      invisible(lapply(1:length(tbl1D),function(x) tbl1D[[x]][tbl1D[[x]]==-1] <<- as.numeric(NA)))}

#règles des tables 2d :

  #une colonne de numérique doit être précédée d'une variable

if (length(tbl2D)>0) {tbl2D <- lapply(tbl2D,function(x) x[,apply(x,2,function(y) any(substring(as.matrix(y),1,3)%in%c("v__","t__","i__","f__","m__","l__","e__","c__")))])
                      invisible(lapply(1:length(tbl2D),function(x) tbl2D[[x]][tbl2D[[x]]==-1] <<- as.numeric(NA)))}

#on peut maintenant séparer les variables
  #pour cela, il faut tout mettre sous forme 1D



if (length(tbl2D)>0) tbl2 <- lapply(tbl2D,twoDto1D,"2D") else tbl2 <- NULL
if (length(tbl1D)>0) tbl1 <- lapply(tbl1D,twoDto1D,"1D") else tbl1 <- NULL


List <- c(tbl1,tbl2)

#on peut intégrer ici les tables de scénarios

#test pour savoir quelles tables intégrer dans la liste déjà construite
testS <- lapply(ListS,function(x) { tst <- FALSE
                                    if ("e"%in%names(x)) {
                                        if (nam%in%gsub("e__","",as.character(x$e))) tst <- TRUE
                                        }
                                    return(tst)})

ListStemp <- lapply(ListS,function(x) {x$v <- paste(x$v,x$s,sep="") ; return(x)})
keep <- ListStemp[unlist(testS)]
#on retire la colonne "espèce" après avoir filtré sur l'espèce
keep <- lapply(keep,function(x) x[x$e%in%paste("e__",nam,sep=""),])
keep <- lapply(keep,function(x) if ("e"%in%names(x)) x[,-match("e",names(x))])

if (length(keep)>0) List <- c(List,keep)



List <- lapply(List,function(x) split(x[,-match("v",names(x)),drop=FALSE],as.character(x[,match("v",names(x))])))
namL <- gsub("v__","",unlist(lapply(List,names)))
List <- unlist(List,recursive=FALSE,use.names = FALSE)
names(List) <- namL

#il faut regrouper les tables de même variable découpées en plusieurs parties
Nam <- unique(names(List))
List <- lapply(Nam,function(x) {tt <- do.call("rbind",List[names(List)%in%x])
                                rownames(tt) <- NULL
                                return(tt)})
names(List) <- Nam


#on en fait maintenant des objets standards accompagnés de leur attribut 'DimCst' pour les inputs, et on laisse sous forme de DF pour l'historique
#il faut considérer l'historique... (t<=t_init)
listHisto <- List[!grepl("s__",names(List))]
invisible(sapply(c("v__","t__","i__","f__","m__","l__","e__","c__") ,
              function(y) listHisto <<- lapply(listHisto,function(x) as.data.frame(gsub(y,"",as.matrix(x))))))
listHisto <- lapply(listHisto, function(x) {if (ncol(x)==1) {
                                              return(as.numeric(as.character(x$value)))
                                            } else {
                                              rownames(x) <- 1:nrow(x)
                                              if ("t"%in%names(x)) {    #si l'occurence n'est pas présente, on prend toute la table
                                                rp <- match(as.character(t_hist_max),as.character(x$t))
                                                if (!is.na(rp)) {
                                                x <- x[unique(sort(c(1:match(as.character(t_hist_max),as.character(x$t)),
                                                                      (1:nrow(x))[x$t%in%t_hist_max]))),]
                                                }}
                                              x$value <- as.numeric(as.character(x$value))
                                              return(x)
                                              }})
#... et les paramètres d'entrée (t>=t_init)
listInput <- List[!grepl("s__",names(List))]

listInput <- lapply(listInput, function(x) {if (ncol(x)==1) {
                                              return(x)
                                            } else {
                                              if ("t"%in%names(x)) {
                                              #il faut distinguer ce qui va servir à calculer la valeur initiale (tab), et ce qui sert pour les projections (proj)
                                                ind <- grep("t__t__",as.character(x$t))
                                                indic <- length(ind)>0
                                                #occ <- unique(as.character(x$t)) ; occ <- occ[length(occ)]
                                                if (indic) tab <- x[ind,] else
                                                            tab <- x[x$t%in%paste("t__",t_init,sep=""),]
                                                if (indic) {

                                                  if (max(ind)<nrow(x)) {
                                                    proj <- x[(max(ind)+1):nrow(x),]
                                                  } else {
                                                    proj <- NULL }  #pas de donnée de projection

                                                } else {

                                                if (match(paste("t__",t_init,sep=""),rev(as.character(x$t)))==1) {
                                                    proj <- NULL
                                                } else {
                                                    proj <- x[(nrow(x)+2-match(paste("t__",t_init,sep=""),rev(as.character(x$t)))):nrow(x),] }

                                                }

                                                if (ncol(tab)==2) {

                                                  if (is.null(proj))
                                                    return(mean(as.numeric(as.character(tab$value)),na.rm=TRUE))
                                                  else {
                                                    intTab <- rbind.data.frame(tab[nrow(tab),],proj)
                                                    intTab$value[1] <- mean(as.numeric(as.character(tab$value)),na.rm=TRUE)
                                                    intTab$t <- gsub("t__t__","t__",intTab$t)
                                                    return(expand.time(intTab,t_init,nbStep))
                                                  }

                                                } else {

                                                  nams <- names(tab) ; nams <- nams[-match(c("t","value"),nams)]
                                                  eval(parse('',text=paste("TAB <- with(tab,aggregate(as.numeric(as.character(value)),list(",
                                                          paste(nams,collapse=","),"),mean,na.rm=TRUE))",sep="")))

                                                  if (is.null(proj)) {
                                                      names(TAB) <- c(nams,"value")
                                                      return(TAB)
                                                  } else {
                                                      TAB$newField <- paste("t__",t_init,sep="")
                                                      names(TAB) <- c(nams,"value","t")
                                                      return(expand.time(rbind.data.frame(TAB[,names(proj)],proj),t_init,nbStep))
                                                  }
                                                }
                                              } else {
                                              return(x)
                                              }
                                            }
                                            })

#il ne reste plus qu'à ajouter à listInput les paramètres par espèce issus des fichiers flottilles
Fle <- Fstock[Fstock[,4]%in%nam,] ; n <- unique(Fle[,1])
Fle <- lapply(n,function(x) {df <- as.data.frame(Fle[Fle[,1]%in%x,c(2,3,5)]); rownames(df) <- 1:nrow(df); return(df)})
Fle <- lapply(1:length(n),function(x) {df <- Fle[[x]] ; if (all(is.na(df[,3]))) df[,1:2] <- "" ; return(df)})
Fle <- lapply(Fle,function(x) x[,c(apply(x[,1:(ncol(x)-1)],2,function(y) !all(y=="")),TRUE)])   #on ejecte les colonnes vides
#on gère les constantes
Fle <- lapply(Fle,function(x) if (is.null(dim(x))) return(x[1]) else return(x))
names(Fle) <- n
listInput <- c(listInput,Fle)

listInput <- lapply(listInput,standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),paste("i__",MOD[[1]],sep=""),paste("c__",MOD[[3]],sep=""),alk=NULL)

listScenar <- List[grepl("s__",names(List))]
listScenar <- lapply(listScenar,function(x) if ("t"%in%names(x)) expand.time(x,t_init,nbStep,TRUE) else return(x))

listScenar <- lapply(listScenar,standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),paste("i__",MOD[[1]],sep=""),paste("c__",MOD[[3]],sep=""),alk=NULL,NA)

#on ajoute l'attribut 'intervention'
for (nn in names(listScenar)) {
  if (length(grep("__x__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(1)  #1 -> multiplication
  if (length(grep("__+__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(2)  #2 -> addition
  if (length(grep("__o__",nn))>0) attributes(listScenar[[nn]])$type <- as.integer(3)  #3 -> remplacement
  if (length(grep("__x__",nn))==0 & length(grep("__+__",nn))==0 & length(grep("__o__",nn))==0) attributes(listScenar[[nn]])$type <- as.integer(0)  #0 -> par défaut (multiplication ??)
}

names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__x__","",NN))
names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__+__","",NN))
names(listScenar) <- sapply(names(listScenar),function(NN) gsub("__o__","",NN))

#if (k==1) {disti <- grepl("f__",names(listScenar)) ; )
namS <- names(listScenar) ; namV <- sapply(namS,function(x) strsplit(x,"s__")[[1]][1])
namS <- sapply(namS,function(x) strsplit(x,"s__")[[1]][2])

if (length(listScenar)>0) names(listScenar) <- paste(names(listScenar),nam,sep="e__")

listInput[is.na(names(listInput))] <- NULL

LL$historique[[length(nam_stock)+k]] <- listHisto ; LL$input[[length(nam_stock)+k]] <- listInput ; LL$scenario <- c(LL$scenario,listScenar)

}
}





#il manque un élément Fleet à historique --> à voir
LL$historique$Fleet <- list()


##Fleet

FL <- Fleet
n <- unique(FL[,1])
FL <- lapply(n,function(x) {df <- as.data.frame(FL[FL[,1]%in%x,c(2,3,5)]); rownames(df) <- 1:nrow(df); return(df)})
FL <- lapply(1:length(n),function(x) {df <- FL[[x]] ; if (all(is.na(df[,3]))) df[,1:2] <- "" ; return(df)}) 
FL <- lapply(FL,function(x) x[,c(apply(x[,1:(ncol(x)-1)],2,function(y) !all(y=="")),TRUE)])   #on ejecte les colonnes vides
#on gère les constantes 
FL <- lapply(FL,function(x) if (is.null(dim(x))) return(x[1]) else return(x))
names(FL) <- n
LL$input$Fleet <- lapply(FL,standFormat,nbStep,paste("f__",modF,sep=""),paste("m__",modMeco,sep=""),"","",NULL)

#on calcule les valeurs totales à partir des valeurs moyennes sur les champs "nbact_f_tot","nbds_f_tot","nbdf_f_tot",
#"prodtot_f_tot","GR_f_tot","nbh_f_tot","nbds_f_m_tot","nbdf_f_m_tot","prodtot_f_m_tot","GR_f_m_tot","nbh_f_m_tot"
namFtot <- c("effort_f_tot","nbds_f_tot","GVLref_f_tot","nbh_f_tot","effort_f_m_tot","nbds_f_m_tot","GVLref_f_m_tot","nbh_f_m_tot")
Ftot <- LL$input$Fleet[gsub("_tot","",namFtot)]
Ftot[[1]] <- LL$input$Fleet$effort1_f * LL$input$Fleet$effort2_f
Ftot[[5]] <- LL$input$Fleet$effort1_f_m * LL$input$Fleet$effort2_f_m
if (is.null(LL$input$Fleet$nbv_f)) LL$input$Fleet$nbv_f <- NA
if (is.null(LL$input$Fleet$nbv_f_m)) LL$input$Fleet$nbv_f_m <- NA 
Ftot <- lapply(1:length(namFtot),function(x) if (x<5) Ftot[[x]]*as.vector(LL$input$Fleet$nbv_f) else Ftot[[x]]*as.vector(LL$input$Fleet$nbv_f_m))
names(Ftot) <- namFtot 
LL$input$Fleet <- c(LL$input$Fleet,Ftot)

LL$input$Fleet$Yothsue_f_m <- (LL$input$Fleet$Lref_f_m - Reduce("+",lapply(LL$input[-length(LL$input)],function(x) x$Lref_f_m_e)))/Ftot[[5]]
LL$input$Fleet$tripLgthIniMax_f_m <- LL$input$Fleet$tripLgth_f_m * as.vector(LL$input$Fleet$H_f*LL$input$Fleet$nbTrip_f*LL$input$Fleet$nbv_f / LL$input$Fleet$Lref_f)

#on va traiter les inputs scénarios pour les organiser de la même manière que dans input
SC <- LL$scenario
#on recrée une table des occurences des noms
step1 <- t(sapply(names(SC),function(x) strsplit(x,"e__")[[1]])) ; rownames(step1) <- 1:nrow(step1)
repF <- grepl("f__",step1[,1]) ; step1[,1] <- gsub("f__","",step1[,1]) ; step1[repF,2] <- "Fleet"
step2 <- t(sapply(step1[,1],function(x) strsplit(x,"s__")[[1]])) ; rownames(step2) <- 1:nrow(step2)
tabSc <- data.frame(v=step2[,1],s=step2[,2],e=step1[,2],ind=1:nrow(step2))

gg <- lapply(split(tabSc,tabSc$s),function(x) split(x,x$e,drop=TRUE))
gg <- lapply(gg,function(x) lapply(x,function(y) split(y,y$v,drop=TRUE)))
LL$scenario <- lapply(gg,function(x) lapply(x,function(y) lapply(y,function(z) SC[[z$ind]])))


#il ne reste plus qu'à mettre en forme

reformat <- function(x,slotN="stockInput") {
  n <- names(new(slotN)@input)
  ll <- x[n]
  names(ll) <- n 
  ll <- lapply(ll,function(y) {if (is.null(y)) return(as.numeric(NA)) else {if (all(is.na(y))) return(as.numeric(NA)) else return(y)}})
  return(lapply(ll,function(y){if (length(y)==1 & is.null(names(y)) & is.null(dimnames(y)) & is.numeric(y)) attributes(y)$DimCst <- as.integer(c(0,0,0,0))   #added 18/07/16
                                return(y)}))
}    #si on veut plutôt des NAs, on remplace 'll)}' par 'lapply(ll,function(y) if (is.null(y)) NA else y))}'


LL$historique <- c(lapply(LL$historique[1:length(nam_stock)],reformat),
    if (length(nam_stock_bis)>0) lapply(LL$historique[(1:length(nam_stock_bis))+length(nam_stock)],reformat,"staticStockInput") else NULL,
    lapply(LL$historique[length(nam_stock)+length(nam_stock_bis)+1],reformat,"fleetInput"))

LL$input <- c(if (length(nam_stock)>0) lapply(LL$input[1:length(nam_stock)],reformat) else NULL,
    if (length(nam_stock_bis)>0) lapply(LL$input[(1:length(nam_stock_bis))+length(nam_stock)],reformat,"staticStockInput") else NULL,
    lapply(LL$input[length(nam_stock)+length(nam_stock_bis)+1],reformat,"fleetInput"))

#LL$input <- c(lapply(LL$input[1:(length(nam_stock)+length(nam_stock_bis))],reformat),
#    lapply(LL$input[length(nam_stock)+length(nam_stock_bis)+1],reformat,"fleetInput"))

#on laisse l'élément "scénario" tel quel pour le moment

#on remplit la partie "stochastique" avec les variables issues des valeurs historiques de recrutement (seulement ça pour le moment)
STO <- list()

    stoch <- read.xlsx(file,sheet="Stochasticite_Sensibilite",rowNames=FALSE,colNames=FALSE,skipEmptyRows = FALSE,skipEmptyCols = FALSE)
    stoch[] <- lapply(stoch, function(x) gsub(",",".",as.character(x)))
    stoch <- as.matrix(stoch)

stoch[is.na(stoch)] <- "" 
#stoch <- read.xls(file,sheet="Stochasticité_Sensibilité",type="character",rowNames=FALSE,colNames=FALSE)

indexSt <- seq(from=grep("Samples : recruitment" ,stoch[,1]),to=grep("END Samples" ,stoch[,1])-1) 
indexSp <- grep("e__",stoch[,1]) 
tabSto <- stoch[indexSp[indexSp%in%indexSt],]
tabSto[,1] <- gsub("e__","",tabSto[,1])
STO$RecHist <- lapply(as.character(namList),function(x) {tab <- tabSto[tabSto[,1]%in%x,,drop=FALSE] ;
                                          return(rep(as.numeric(tab[,3])*as.numeric(tab[,4]),as.numeric(tab[,5])))})

names(STO$RecHist) <- as.character(namList)
STO$GeoMeanRec <- lapply(STO$RecHist,function(x) if (length(x)>0) prod(x,na.rm=TRUE)^(1/sum(!is.na(x))) else numeric(0))
STO$RecResiduals <- mapply(function(x,y) x-y,STO$RecHist,STO$GeoMeanRec,SIMPLIFY=FALSE)

allSp <- tapply(tabSto[,1],list(tabSto[,2]),function(x) all(namList%in%x))       #années pour lesquelles on a un historique pour toutes les epsèces
allSp <- names(allSp)[allSp]
STO$RecHistLink <- lapply(as.character(namList),function(x) {tab <- tabSto[tabSto[,1]%in%x,,drop=FALSE] ; tab <- tab[match(allSp,tab[,2]),,drop=FALSE];
                                          return(rep(as.numeric(tab[,3])*as.numeric(tab[,4]),as.numeric(tab[,5])))})
names(STO$RecHistLink) <- as.character(namList)
STO$GeoMeanRecLink <- lapply(STO$RecHistLink,function(x) if (length(x)>0) prod(x,na.rm=TRUE)^(1/sum(!is.na(x))) else numeric(0))
STO$RecResidualsLink <- mapply(function(x,y) x-y,STO$RecHistLink,STO$GeoMeanRecLink,SIMPLIFY=FALSE)


#on remplit aussi, toujours pour le recrutement, la loi de distribution désirée avec ses paramètres pour chaque espèce

indexSt2 <- seq(from=grep("Random-variate : recruitment" ,stoch[,1]),to=grep("END Random-variate" ,stoch[,1])-1) 
tabSto2 <- stoch[indexSt2,]
tabSto2 <- tabSto2[grep("e__",tabSto2[,5]),,drop=FALSE]
tabSto2[,5] <- gsub("e__","",tabSto2[,5])

ff <- lapply(1:4,function(x) {
        if (length(as.character(namList))>0) {
          comp <- lapply(as.character(namList),function(y) {tab <- tabSto2[tabSto2[,5]%in%y,,drop=FALSE] ;
                                            if (length(tab)>0) {
                                              if (tab[1,x]!="") {
                                                if (x%in%(2:4)) as.numeric(tab[1,x]) else as.character(tab[1,x])
                                              } else {
                                              if (x%in%(2:4)) as.numeric(NA) else ""
                                              }} else {
                                              if (x%in%(2:4)) as.numeric(NA) else ""}})
          names(comp) <- as.character(namList)
          return(comp)
        } else return(NA)
        })
names(ff) <- c("RecDist","RecDistPar1","RecDistPar2","RecDistPar3")        
STO <- c(STO,ff)
#on retourne alors l'objet bemInput renseigné (manquent encore les paramètres stochastiques et d'optimisation


#modif MM 23/02/2012
#à partir de MMmodif, on modifie mm pour chaque espèce
indDYN <- (length(nam_stock)>0)
if (indDYN) {
  if (exists("MMmodif")) {
   names(MMmodif)[4] <- "mBio"
   MMmodif$f <- gsub("f__","",MMmodif$f)
   MMmodif$e <- gsub("e__","",MMmodif$e)
   MMmodif$mBio <- gsub("m__","",MMmodif$mBio)
   MMmodif$m <- gsub("m__","",MMmodif$m)
  #tabMM <- with(MMmodif,tapply(value,list(paste(f,mBio,sep="__"),m,e),function(x) x[1]))
  #modif MM 27/08/2013 : ligne au dessus engendre un plantage si 1 seule occurence métier (généralisation du drop)
   tabMM <- lapply(unique(MMmodif$e),function(z) with(MMmodif[MMmodif$e%in%z,],tapply(value,list(paste(f,mBio,sep="__"),m),function(x) x[1])))
   names(tabMM) <- unique(MMmodif$e)
   for (spp in as.character(namList)) LL$input[[spp]]$mm <- tabMM[[spp]]
  }
}
Qvec <- as.integer(rep(0,length(namList))) ; names(Qvec) <- as.character(namList)

return(new("iamInput",desc=desc,specific=list(Species=if (indDYN) as.character(namList) else character(0),StaticSpp=as.character(nam_stock_bis),
                                              Fleet=modF,Metier=modMbio,MetierEco=modMeco,
                                              Ages=if (indDYN) lapply(LL$input,function(x) x$modI)[namList] else list(),
                                              Cat=if (indDYN) lapply(LL$input,function(x) x$modC)[namList] else list(),t_init=t_init,
                                              NbSteps=as.integer(nbStep),times=as.integer(as.character(seq(t_init,by=1,length=nbStep))),
                                              Q=if(indDYN) Qvec else integer()), #initialise Q
            historical=LL$historique,input=LL$input,scenario=LL$scenario,stochastic=STO))

}
                     

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------

#Méthodes d'importation des paramètres d'input


  
setGeneric("IAM.input", function(fileIN, fileSPEC, fileSCEN, fileSTOCH, ...){
	standardGeneric("IAM.input")
	}
)

  # à partir d'un fichier .xls

setMethod("IAM.input", signature("character", "missing", "missing", "missing"),
                   function(fileIN, t_init, nbStep=20, t_hist_max=t_init, desc="My Input", folderFleet=NULL,readXLS="openxlsx",
                                    Fq_i=NULL,iniFq_i=NULL,Fq_fmi=NULL,iniFq_fmi=NULL,
                                    FqLwt_i=NULL,iniFqLwt_i=NULL,FqLwt_fmi=NULL,iniFqLwt_fmi=NULL,
                                    FqDwt_i=NULL,iniFqDwt_i=NULL,FqDwt_fmi=NULL,iniFqDwt_fmi=NULL,
                                    Nt0s1q=NULL,Ni0q=NULL,iniNt0q=NULL,matwt=NULL,...) {
                                                                               #Fq_i matrice season*morph*age
                                                                               #Fq_fmi matrice season*morph*flottille*metier*age
                                                                               #Nt0s1q matrice morph*age  (Effectifs initiaux de projection (saison 1 de t=1) par cohorte)
                                                                               #iniNt0q matrice season*morph*age  (Effectifs initiaux (t=0))
                                                                               #Ni0q vecteur season (Recrutement par saison)
                                                                               #matwt poids moyen pondéré pour calcul de la SSB (morph*age)


if (!substring(fileIN,nchar(fileIN)-4,nchar(fileIN))%in%".xlsx") stop("'fileIN' must be an .xlsx file!!")

#require(abind)

out <- read.input(normalizePath(fileIN),t_init=t_init,nbStep=nbStep,t_hist_max=t_hist_max,desc=desc,folderFleet=folderFleet)



nmQ <- names(Fq_i) ; nmQ <- names(Fq_fmi)[names(Fq_fmi)%in%nmQ] ; nmQ <- names(Nt0s1q)[names(Nt0s1q)%in%nmQ]
nmQ <- names(Ni0q)[names(Ni0q)%in%nmQ] ; nmQ <- names(FqLwt_i)[names(FqLwt_i)%in%nmQ] ; nmQ <- names(FqLwt_fmi)[names(FqLwt_fmi)%in%nmQ]
nmQ <- names(FqDwt_i)[names(FqDwt_i)%in%nmQ] ; nmQ <- names(FqDwt_fmi)[names(FqDwt_fmi)%in%nmQ]
nmQ <- names(iniFq_i)[names(iniFq_i)%in%nmQ] ; nmQ <- names(iniFq_fmi)[names(iniFq_fmi)%in%nmQ]
nmQ <- names(iniFqLwt_i)[names(iniFqLwt_i)%in%nmQ] ; nmQ <- names(iniFqLwt_fmi)[names(iniFqLwt_fmi)%in%nmQ]
nmQ <- names(iniFqDwt_i)[names(iniFqDwt_i)%in%nmQ] ; nmQ <- names(iniFqDwt_fmi)[names(iniFqDwt_fmi)%in%nmQ]
nmQ <- names(iniNt0q)[names(iniNt0q)%in%nmQ] ; nmQ <- names(matwt)[names(matwt)%in%nmQ]
out@specific$Q[out@specific$Species%in%nmQ] <- as.integer(1)

if (all(is.na(out@specific$Species))) OUT <- out else OUT <- convertInput(out,Fq_fmi=Fq_fmi)

#sorting
ODpar_list <- unlist(lapply(OUT@input,function(x) x$OD_e))
if (all(ODpar_list%in%0)) OD <- 0 else OD <- min(ODpar_list,na.rm=TRUE)
OUT@input$Fleet$sorting <- as.integer(as.character(OD))

    # Fq_i <- lapply(OUT@specific$Species[1],function(x)
    #                array(0.1,dim=c(4,4,length(OUT@specific$Ages[[x]])),
    #                         dimnames=list(paste("S",1:4,sep=""),paste("M",1:4,sep=""),OUT@specific$Ages[[x]])))
    #
    # Fq_fmi <- lapply(OUT@specific$Species[1],function(x)
    #                array(0.1,dim=c(4,4,length(OUT@specific$Fleet),length(OUT@specific$MetierEco),length(OUT@specific$Ages[[x]])),
    #                         dimnames=list(paste("S",1:4,sep=""),paste("M",1:4,sep=""),OUT@specific$Fleet,
    #                                      OUT@specific$MetierEco,OUT@specific$Ages[[x]])))
    #
    # Ni0q <- lapply(OUT@specific$Species[1],function(x) array(10183000/4,dim=4,dimnames=list(paste("S",1:4,sep=""))))
    #
    # names(Fq_i) <- names(Fq_fmi) <- names(Ni0q) <- OUT@specific$Species[1]

#gestion des indicateurs trimestre
         # mise en commun : seules les espèces renseignées pour les 3 variables sont conservées
FL <- OUT@specific$Fleet
ME <- OUT@specific$MetierEco

if (length(nmQ)>0) {
   for (i in nmQ) {
      lFq_i <- lFq_fmi <- lFothq_i <- lFqLwt_i <- lFqLwt_fmi <- lFothqLwt_i <- lFqDwt_i <- lFqDwt_fmi <- lFothqDwt_i <- lNt0s1q <- lNi0q <- list()
      liniFq_i <- liniFq_fmi <- liniFothq_i <- liniFqLwt_i <- liniFqLwt_fmi <- liniFothqLwt_i <- liniFqDwt_i <- liniFqDwt_fmi <- liniFothqDwt_i <- liniNt0q <- lmatwt <- list()
      AG <- OUT@specific$Ages[[i]]
      Nt0s1q[[i]][2:4,AG[1]] <- 0 ; Nt0s1q[[i]][1,AG[1]] <- Ni0q[[i]][1]      #pas d'individus d'âge 0 et de morph>1 à l'état initial
      for (season in 1:4) {
          temp <- as.double(Ni0q[[i]][season]) ; attributes(temp)$DimCst <- as.integer(c(0,0,0,0))
          lNi0q[[paste("Ni0_S",season,"M",season,sep="")]] <- temp
        for (morph in 1:4) {
          if (season==1) {
             temp <- adrop(Nt0s1q[[i]][morph,AG,drop=FALSE],1) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
             lNt0s1q[[paste("Nt0_S1M",morph,sep="")]] <- temp
             temp <- adrop(matwt[[i]][morph,AG,drop=FALSE],1) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
             lmatwt[[paste("matwt_M",morph,sep="")]] <- temp
          }
          temp <- adrop(Fq_i[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          lFq_i[[paste("Fi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(Fq_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(length(FL),length(ME),length(AG),0))
          lFq_fmi[[paste("Ffmi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(Fq_i[[i]][season,morph,AG,drop=FALSE]-apply(Fq_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],c(1,2,5),sum,na.rm=TRUE),1:2)
          temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          lFothq_i[[paste("Fothi_S",season,"M",morph,sep="")]] <- temp

          temp <- adrop(iniFq_i[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniFq_i[[paste("iniFi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(iniFq_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(length(FL),length(ME),length(AG),0))
          liniFq_fmi[[paste("iniFfmi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(iniFq_i[[i]][season,morph,AG,drop=FALSE]-apply(iniFq_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],c(1,2,5),sum,na.rm=TRUE),1:2)
          temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniFothq_i[[paste("iniFothi_S",season,"M",morph,sep="")]] <- temp

          temp <- adrop(FqLwt_i[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          lFqLwt_i[[paste("FLWi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(FqLwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(length(FL),length(ME),length(AG),0))
          lFqLwt_fmi[[paste("FLWfmi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(FqLwt_i[[i]][season,morph,AG,drop=FALSE]-apply(FqLwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],c(1,2,5),sum,na.rm=TRUE),1:2)
          temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          lFothqLwt_i[[paste("FLWothi_S",season,"M",morph,sep="")]] <- temp

          temp <- adrop(iniFqLwt_i[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniFqLwt_i[[paste("iniFLWi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(iniFqLwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(length(FL),length(ME),length(AG),0))
          liniFqLwt_fmi[[paste("iniFLWfmi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(iniFqLwt_i[[i]][season,morph,AG,drop=FALSE]-apply(iniFqLwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],c(1,2,5),sum,na.rm=TRUE),1:2)
          temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniFothqLwt_i[[paste("iniFLWothi_S",season,"M",morph,sep="")]] <- temp

          temp <- adrop(FqDwt_i[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          lFqDwt_i[[paste("FDWi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(FqDwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(length(FL),length(ME),length(AG),0))
          lFqDwt_fmi[[paste("FDWfmi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(FqDwt_i[[i]][season,morph,AG,drop=FALSE]-apply(FqDwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],c(1,2,5),sum,na.rm=TRUE),1:2)
          temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          lFothqDwt_i[[paste("FDWothi_S",season,"M",morph,sep="")]] <- temp

          temp <- adrop(iniFqDwt_i[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniFqDwt_i[[paste("iniFDWi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(iniFqDwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(length(FL),length(ME),length(AG),0))
          liniFqDwt_fmi[[paste("iniFDWfmi_S",season,"M",morph,sep="")]] <- temp
          temp <- adrop(iniFqDwt_i[[i]][season,morph,AG,drop=FALSE]-apply(iniFqDwt_fmi[[i]][season,morph,FL,ME,AG,drop=FALSE],c(1,2,5),sum,na.rm=TRUE),1:2)
          temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniFothqDwt_i[[paste("iniFDWothi_S",season,"M",morph,sep="")]] <- temp

          temp <- adrop(iniNt0q[[i]][season,morph,AG,drop=FALSE],1:2) ; temp[] <- as.double(temp) ; attributes(temp)$DimCst <- as.integer(c(0,0,length(AG),0))
          liniNt0q[[paste("iniNt0q_S",season,"M",morph,sep="")]] <- temp
        }
          #OUT@specific$Q[OUT@specific$Species%in%i] <- as.integer(1)
      }
      OUT@input[[i]] <- c(OUT@input[[i]],liniNt0q,lNi0q,lNt0s1q,lmatwt,liniFq_i,lFq_i,liniFq_fmi,lFq_fmi,liniFothq_i,lFothq_i,
                     liniFqLwt_i,lFqLwt_i,liniFqLwt_fmi,lFqLwt_fmi,liniFothqLwt_i,lFothqLwt_i,liniFqDwt_i,lFqDwt_i,
                     liniFqDwt_fmi,lFqDwt_fmi,liniFothqDwt_i,lFothqDwt_i)
      OUT@input[[i]]$N_it0 <- as.double(NA) ; attributes(OUT@input[[i]]$N_it0)$DimCst <- as.integer(c(0,0,0,0))
      OUT@input[[i]]$N_i0t <- as.double(NA) ; attributes(OUT@input[[i]]$N_i0t)$DimCst <- as.integer(c(0,0,0,0))
      OUT@input[[i]]$F_i <- as.double(NA) ; attributes(OUT@input[[i]]$F_i)$DimCst <- as.integer(c(0,0,0,0))
      OUT@input[[i]]$F_fmi <- as.double(NA) ; attributes(OUT@input[[i]]$F_fmi)$DimCst <- as.integer(c(0,0,0,0))
    }
}

return(OUT)

})












  # à partir de fichiers .txt
  
setMethod("IAM.input", signature("character", "character", "missing", "missing"),
                   function(fileIN, fileSPEC, desc="My Input", ...){

if (substring(fileIN,nchar(fileIN)-3,nchar(fileIN))!=".txt") stop("'fileIN' must be a .txt file!!")
if (substring(fileSPEC,nchar(fileSPEC)-3,nchar(fileSPEC))!=".txt") stop("'fileSPEC' must be a .txt file!!")

specific <- suppressWarnings(.Call("Fun",normalizePath(fileSPEC),NULL))
input <- suppressWarnings(.Call("Fun",normalizePath(fileIN),specific))

out <- new("iamInput",desc=desc,specific=specific,historical=list(),input=input,scenario=list(),stochastic=list())
return(convertInput(out))

})
 
  
  
setMethod("IAM.input", signature("character", "character", "character", "missing"),
                   function(fileIN, fileSPEC, fileSCEN, desc="My Input", ...){

if (substring(fileIN,nchar(fileIN)-3,nchar(fileIN))!=".txt") stop("'fileIN' must be a .txt file!!")
if (substring(fileSPEC,nchar(fileSPEC)-3,nchar(fileSPEC))!=".txt") stop("'fileSPEC' must be a .txt file!!")
if (substring(fileSCEN,nchar(fileSCEN)-3,nchar(fileSCEN))!=".txt") stop("'fileSCEN' must be a .txt file!!")

specific <- suppressWarnings(.Call("Fun",normalizePath(fileSPEC),NULL))
input <- suppressWarnings(.Call("Fun",normalizePath(fileIN),specific))
scenario <- suppressWarnings(.Call("Fun",normalizePath(fileSCEN),specific))

out <- new("iamInput",desc=desc,specific=specific,historical=list(),input=input,scenario=scenario,stochastic=list())
return(convertInput(out))

})
  
  
  
setMethod("IAM.input", signature("character", "character", "missing", "character"),
                   function(fileIN, fileSPEC, fileSTOCH, desc="My Input", ...){

if (substring(fileIN,nchar(fileIN)-3,nchar(fileIN))!=".txt") stop("'fileIN' must be a .txt file!!")
if (substring(fileSPEC,nchar(fileSPEC)-3,nchar(fileSPEC))!=".txt") stop("'fileSPEC' must be a .txt file!!")
if (substring(fileSTOCH,nchar(fileSTOCH)-3,nchar(fileSTOCH))!=".txt") stop("'fileSTOCH' must be a .txt file!!")

specific <- suppressWarnings(.Call("Fun",normalizePath(fileSPEC),NULL))
input <- suppressWarnings(.Call("Fun",normalizePath(fileIN),specific))
stochastic <- suppressWarnings(.Call("Fun",normalizePath(fileSTOCH),specific))

out <- new("iamInput",desc=desc,specific=specific,historical=list(),input=input,scenario=list(),stochastic=stochastic)
return(convertInput(out))

})



setMethod("IAM.input", signature("character", "character", "character", "character"),
                   function(fileIN, fileSPEC, fileSCEN, fileSTOCH, desc="My Input", ...){

if (substring(fileIN,nchar(fileIN)-3,nchar(fileIN))!=".txt") stop("'fileIN' must be a .txt file!!")
if (substring(fileSPEC,nchar(fileSPEC)-3,nchar(fileSPEC))!=".txt") stop("'fileSPEC' must be a .txt file!!")
if (substring(fileSCEN,nchar(fileSCEN)-3,nchar(fileSCEN))!=".txt") stop("'fileSCEN' must be a .txt file!!")
if (substring(fileSTOCH,nchar(fileSTOCH)-3,nchar(fileSTOCH))!=".txt") stop("'fileSTOCH' must be a .txt file!!")

specific <- suppressWarnings(.Call("Fun",normalizePath(fileSPEC),NULL))
input <- suppressWarnings(.Call("Fun",normalizePath(fileIN),specific))
scenario <- suppressWarnings(.Call("Fun",normalizePath(fileSCEN),specific))
stochastic <- suppressWarnings(.Call("Fun",normalizePath(fileSTOCH),specific))

out <- new("iamInput",desc=desc,specific=specific,historical=list(),input=input,scenario=scenario,stochastic=stochastic)
return(convertInput(out))

})



#---------------
#Examples
#---------------


#out <- IAM.input("Z:/Projet/Projet SIAD/Param bio_eco/Modele/Inputs_SIAD_SEL_2.xls",t_init=2010,nbStep=21)
#
#out <- IAM.input("C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/input.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/specific.txt")
#
#out <- IAM.input("C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/input.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/specific.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/scenario.txt")
#
#out <- IAM.input("C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/input.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/specific.txt",
#		fileSTOCH="C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/stochastic.txt")
#
#out <- IAM.input("C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/input.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/specific.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/scenario.txt",
#		"C:/Documents and Settings/mmerzere/Bureau/COST_R/IAMwd/expInput/stochastic.txt")
#
#str(out)
#
