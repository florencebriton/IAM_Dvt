
#====================================================================
# Définition de l'objet global et test de validité (paramètres requis renseignés,...)
#====================================================================

val.iamInput <- function(object){

#à remplir avec les tests de validité à appliquer aux données de paramétrage
  #if ... stop(...)
  
	return(TRUE)

}

setClass("iamInput",
	representation(
		desc="character",
		specific = "list",
		historical="list",
		input="list",
		scenario="list",
		stochastic="list",
		optimization="list"
	),
	prototype(
		desc="iamInput",
		specific = list(Species=character(),
                    StaticSpp=character(),
    	              Fleet=character(),
                  	Metier=character(),
                  	MetierEco=character(),	
                    Ages=list(),
                   	Cat=list(),
                    t_init=double(),
                    NbSteps=integer(),
                   	times=integer(),
                    Q=integer()),
		historical=list(),
		input=list(),
		scenario=list(),
		stochastic=list(),
		optimization=list()
  ),
	validity=val.iamInput
)


#====================================================================
# Définition de l'objet StockInput et test de validité (paramètres requis renseignés,...)
#====================================================================

val.stockInput <- function(object){

#à remplir avec les tests de validité à appliquer aux données de paramétrage
  #if ... stop(...)
  
	return(TRUE)

}

setClass("stockInput",
	representation(
		stock="character",
		input="list"
	),
	prototype(
		stock="My stock",
		input=list(
		  #modalités
		  modI = NA,
		  modL = NA,
		  modC = NA,
		  #BIO
		  icat	= NA,     #Clé Catégories commerciales/âges	
      alk	= NA,       #Clé tailles-âges (non temporelle pour le moment, et non spatialisée)	
      fm = NA,        #proportion par métier des valeurs de ventilation pour chaque flottille (par espèce)
      mm = NA,        #matrice de redéfinition du niveau métier entre le module bio/marché et le module éco (f_m.bio_e -> m.éco) 
      M_i	= NA,	      #Mortalité naturelle
      mat_i = NA,     #Ogive de maturité	
      wStock_i	= NA, #Poids individuels moyens dans le stock (kg)
      wL_i	= NA,     #Poids individuels moyens dans les débarquements (kg)	
      wD_i	= NA,     #Poids individuels moyens dans les rejets (kg)	
      N_it0	= NA,     #Effectifs population aux âges à l'instant initial 	
      N_i0t	= NA,     #Effectifs population à l'âge 0
      #Ni0_S1	= NA, Ni0_S2	= NA, Ni0_S3	= NA, Ni0_S4	= NA,   #effectifs population à l'âge 0 pour chaque morph (ie saison)
      F_i = NA,	      #Mortalité par pêche aux âges
      F_fmi	= NA,	    #Mortalité par pêche aux âges ventilée
       #mortalité par pêche aux âges pour chaque saison et chaque morph
      #Fi_S1M1 = NA, Fi_S1M2 = NA, Fi_S1M3 = NA, Fi_S1M4 = NA, Fi_S2M1 = NA, Fi_S2M2 = NA, Fi_S2M3 = NA, Fi_S2M4 = NA,
      #Fi_S3M1 = NA, Fi_S3M2 = NA, Fi_S3M3 = NA, Fi_S3M4 = NA, Fi_S4M1 = NA, Fi_S4M2 = NA, Fi_S4M3 = NA, Fi_S4M4 = NA,
       #mortalité par pêche flottille-métier-âges pour chaque saison et chaque morph
      #Ffmi_S1M1 = NA, Ffmi_S1M2 = NA, Ffmi_S1M3 = NA, Ffmi_S1M4 = NA, Ffmi_S2M1 = NA, Ffmi_S2M2 = NA, Ffmi_S2M3 = NA, Ffmi_S2M4 = NA,
      #Ffmi_S3M1 = NA, Ffmi_S3M2 = NA, Ffmi_S3M3 = NA, Ffmi_S3M4 = NA, Ffmi_S4M1 = NA, Ffmi_S4M2 = NA, Ffmi_S4M3 = NA, Ffmi_S4M4 = NA,
       #mortalité par pêche "autres" aux âges pour chaque saison et chaque morph
      #Fothi_S1M1 = NA, Fothi_S1M2 = NA, Fothi_S1M3 = NA, Fothi_S1M4 = NA, Fothi_S2M1 = NA, Fothi_S2M2 = NA, Fothi_S2M3 = NA, Fothi_S2M4 = NA,
      #Fothi_S3M1 = NA, Fothi_S3M2 = NA, Fothi_S3M3 = NA, Fothi_S3M4 = NA, Fothi_S4M1 = NA, Fothi_S4M2 = NA, Fothi_S4M3 = NA, Fothi_S4M4 = NA,
      B_i	= NA,       #Biomasse aux âges (t)	
      Y_mi = NA,      #Capture totale par métier et par a/l en poids pour ventilation de la mortalité par pêche	(t)
      C_mi = NA,      #Capture totale par métier et par a/l en nombres pour ventilation de la mortalité par pêche	
      Y_i = NA,       #Capture totale par a/l en poids pour ventilation de la mortalité par pêche	(t)
      C_i = NA,       #Capture totale par a/l en nombres pour ventilation de la mortalité par pêche	
      d_i= NA,        #Proportion des captures totales rejetées "flottilles modélisées"
      doth_i= NA,     #Proportion des captures totales rejetées "autres flottilles"
      dd1_f_m_e = NA, #taux de rejets exemption en % de la capture totale de l'espèce
      dd2_f_m_e = NA, #taux de rejets exemption en % de la capture totale
      sr = NA,        #Taux de survie des rejets
#      SelRef = NA,	  #Facteur de sélectivité de référence (PSo)
      r = NA,         #SPiCT : Intrinsic growth rate : growth, recruitment, natural mortality
      K = NA,         #SPiCT : Carrying capacity (or equilibrium biomass or virgin stock biomass)
      n = NA,         #SPiCT : Parameter determining the shape of the production curve
      sigmaF = NA,    #SPiCT : Standard deviation of F
      sigmaB = NA,    #SPiCT : Standard deviation of B
      P_fmce = NA,    #Prix moyen par catégorie (euros)
      Pst_e = NA,     #Prix farine
      OD_e = NA,      #obligation de débarquement ? (oui(1)/non(0))
      theta_e = NA,   #multiplicateur de prix pour les rejets débarqués (0<=...<=1)
      alpha_fmce = NA,     #Coefficient modèle de prix	
      beta_fmce = NA,      #Coefficient modèle de prix	
      gamma_fmce = NA,     #Coefficient modèle de prix
      TAC = NA,
      Fbar = NA,
      FmaxTarget = NA,
      #ACTIVITE
      Lref_f_e = NA,  #Quantité moyenne débarquée par navire d'une flottille par an en tonnes par espèce
      Lref_f_m_e = NA,#Quantité moyenne débarquée par navire d'une flottille-métier par an en tonnes par espèce
      GVLref_f_e = NA,	  #Valeur moyenne débarquée par navire d'une flottille par an en milliers d'euro par espèce
      GVLref_f_m_e = NA 	#Valeur moyenne débarquée par navire d'une flottille-métier par an en milliers d'euro par espèce
    )
  ),
	validity=val.stockInput
)


#====================================================================
# Définition de l'objet StaticStockInput et test de validité (paramètres requis renseignés,...)
#====================================================================

val.staticStockInput <- function(object){

#à remplir avec les tests de validité à appliquer aux données de paramétrage
  #if ... stop(...)

	return(TRUE)

}

setClass("staticStockInput",
	representation(
		stock="character",
		input="list"
	),
	prototype(
		stock="My stock",
		input=list(
      LPUE_f_m_e=NA,  #Débarquements moyens par unité d'effort pour les espèces non modélisées (t/nbds)
      d_f_m_e=NA,     #Proportion des captures totales rejetées pour les espèces non modélisées sur les flottilles modélisées
      dd1_f_m_e = NA, #taux de rejets exemption en % de la capture totale de l'espèce
      dd2_f_m_e = NA, #taux de rejets exemption en % de la capture totale
      dst_f_m_e = NA, #taux de rejets débarqués sous-taille en % du tonnage de rejets débarqués de l'espèce
      P_fme = NA,     #Prix moyen espèces non modélisées (euros)
      Pst_e = NA,     #Prix farine
      OD_e = NA,      #obligation de débarquement ? (oui(1)/non(0))
      theta_e = NA,   #multiplicateur de prix pour les rejets débarqués (0<=...<=1)
      alpha_fme = NA,     #Coefficient modèle de prix
      beta_fme = NA,      #Coefficient modèle de prix
      gamma_fme = NA,
      Lref_f_e = NA,
      Lref_f_m_e = NA,
      GVLref_f_e = NA,
      GVLref_f_m_e = NA
    )
  ),
	validity=val.staticStockInput
)

#====================================================================
# Définition de l'objet Fleet et test de validité (paramètres requis renseignés,...)
#====================================================================

val.fleetInput <- function(object){

#à remplir avec les tests de validité à appliquer au données de paramétrage
  #if ... stop(...)
  
	return(TRUE)

}

setClass("fleetInput",
	representation(
		stock="character",
		input="list"
	),
	prototype(
		stock="My fleet",
		input=list(
		  #modalités
		  modF = NA,
		  modMbio = NA,
		  modMeco = NA,
		  #variables 
      sorting = NA,
		  Lref_f = NA,             #Débarquements de référence par flottille
		  Lref_f_m = NA,           #Débarquements de référence par flottille - métier
      GVLref_f = NA,           #CA moyen initial par navire d'une flottille
      GVLref_f_m = NA,         #CA moyen initial par navire d'une flottille - métier
      Yothsue_f_m = NA,
      nbv_f = NA,              #Nombre de navires par flottille
      nbv_f_m = NA,            #Nombre de navire par flottille - métier
      lc_f_m = NA,             #Taxes de débarquement (% CA éco)
      lcd_f_m = NA,            #Taxes de débarquement relatives aux rejets débarqués sous-taille (% CA éco)
      gc_f = NA,               #Coût total engins par navire d'une flottille
      gc_f_m = NA,             #Coût total engins par navire d'une flottille - métier
      nbds_f = NA,             #Nombre moyen de Jours de Mer par navire d'une flottille par an
      nbds_f_m = NA,           #Nombre moyen de Jours de Mer par navire d'une flottille - métier par an
      nbh_f = NA,              #Nombre d'heures moteur par navire d'une flottille par an
      nbh_f_m = NA,            #Nombre d'heures moteur par navire d'une flottille - métier par an
      nbTrip_f = NA,           #Nombre de marées annuel par navire d'une flottille
      nbTrip_f_m = NA,         #Nombre de marées annuel par navire d'une flottile - métier
      tripLgth_f = NA,         #durée moyenne d'une marée par navire d'une flottille
      tripLgth_f_m = NA,       #durée moyenne d'une marée par navire d'une flottile - métier
      tripLgthIniMax_f_m = NA,
      effort1_f = NA,
      effort1_f_m = NA,
      effort2_f = NA,
      effort2_f_m = NA,
      effort1max_nbds_f = NA,
      effort1max_nbTrip_f = NA,
      effort1max_f = NA,
      H_f = NA,
      fc_f = NA,               #Coûts du carburant par navire d'une flottille
      fc_f_m = NA,             #Coûts du carburant par navire d'une flottille - métier
      vf_f = NA,               #Prix du carburant par navire d'une flottille
      vf_f_m = NA,             #Prix du carburant par navire d'une flottille - métier
      ovc_f = NA,              #Autres coûts variables par navire d'une flottille
      ovc_f_m = NA,            #Autres coûts variables par navire d'une flottille - métier
      oilc_f = NA,             #Coûts d'huile par navire d'une flottille
      oilc_f_m = NA,           #Coûts d'huile par navire d'une flottille - métier
      bc_f = NA,               #Coûts d'appâts par navire d'une flottille
      bc_f_m = NA,             #Coûts d'appâts par navire d'une flottille - métier
      foc_f = NA,              #Coûts de vivres par navire d'une flottille
      foc_f_m = NA,            #Coûts de vivres par navire d'une flottille - métier
      cnb_f = NA,              #Effectif moyen par navire d'une flottille
      cnb_f_m = NA,            #Effectif moyen par navire d'une flottille - métier
      icec_f = NA,             #Coûts de glace par navire d'une flottille
      icec_f_m = NA,           #Coûts de glace par navire d'une flottille - métier
      cshr_f = NA,             #Part équipage (ratio du RAP) par navire d'une flottille
      cshr_f_m = NA,           #Part équipage (ratio du RAP) par navire d'une flottille - métier
      eec_f = NA,              #Cotisations salariales par navire d'une flottille
      mwhg_f = NA,             #Salaire brut horaire minimum national
      mwh_f = NA,              #Salaire net horaire minimum national
      altwh_f = NA,            #Salaire net horaire alternatif
      rep_f = NA,              #Coûts entretien et réparation
      onvc_f = NA,             #Autres Coûts Non Variables
      insp_f = NA,             #Primes d'assurance
      ownc_f = NA,             #Autres dépenses d'armement
      mngc_f = NA,             #Cotisation Centre de gestion
      licc_f = NA,             #Coût Total Licences
      comc_f = NA,             #Taxes Comités
      finc_f = NA,             #Coût du capital
      dep_f = NA,              #Amortissement total
      ic_f = NA,               #Intérêt
      K_f = NA,                #Valeur d'assurance
      vc_f = NA,               #Coût total gréement
      persc_f = NA,            #Coûts de personnel
      ecc_f = NA,              #Cotisations patronales
      pl_f = NA,               #Congés payés
      ovcDCF_f = NA,	         #Autres coûts variables DCF par navire d'une flottille
      ovcDCF_f_m = NA,	       #Autres coûts variables DCF par navire d'une flottille - métier
      fixc_f = NA,             #Coûts fixes (DCF)
      FTE_f = NA,
      FTE_f_m = NA,
      inv_f = NA,
      effort_f_tot=NA,
      nbds_f_tot = NA,         #Nombre total de Jours de Mer par navire par an et par flottille
      GVLref_f_tot = NA,       #Valeur totale débarquée par navire par an et par flottille en milliers d'euros (CA nav)
      nbh_f_tot = NA,          #Nombre total d'heures moteur par navire par an et par flottille
      effort_f_m_tot=NA,
      nbds_f_m_tot = NA,       #Nombre total de jour de mer par navire par an et par flottille - métier
      GVLref_f_m_tot = NA,     #Valeur totale débarquée par navire par an et par flottille - métier en milliers d'euro 			
      nbh_f_m_tot = NA         #Nombre total d'heure moteur par navire par an  et par flottille - métier   
    )
  ),
	validity=val.fleetInput
)



#====================================================================
# Définition de l'objet Args contenant les arguments et test de validité (paramètres requis renseignés,...)
#====================================================================

val.iamArgs <- function(object){

arg <- object@arguments
spe <- object@specific

#Recruitments
if (length(arg$Recruitment)!=sum(spe$Q%in%0)) stop("wrong 'Recruitment' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,length)==9))) stop("missing argument in a 'Recruitment' element of iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) x$modSRactive)%in%(0:1)))) stop("wrong 'modSRactive' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) x$typeMODsr)%in%c("Mean","Hockey-Stick","Beverton-Holt","Ricker","Shepherd","Quadratic-HS","Smooth-HS")))) stop("wrong 'typeMODsr' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) is.numeric(x$parAmodSR))))) stop("wrong 'modSRactive' argument in iamArgs object!!")                           
if (!all(unlist(lapply(arg$Recruitment,function(x) is.numeric(x$parAmodSR))))) stop("wrong 'parAmodSR' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) is.numeric(x$parBmodSR))))) stop("wrong 'parBmodSR' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) is.numeric(x$parCmodSR))))) stop("wrong 'parCmodSR' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) is.numeric(x$wnNOISEmodSR))))) stop("wrong 'wnNOISEmodSR' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) x$simuSTOCHactive)%in%(0:1)))) stop("wrong 'simuSTOCHactive' argument in iamArgs object!!")
if (!all(unlist(lapply(arg$Recruitment,function(x) x$typeSIMUstoch)%in%(1:3)))) stop("wrong 'typeSIMUstoch' argument in iamArgs object!!")

#Replicates
if (!arg$Replicates$active%in%(0:1)) stop("wrong 'Rep$active' argument in iamArgs object!!")
if (!is.numeric(arg$Replicates$nbIter)) stop("wrong 'nbIter' argument in iamArgs object!!")                           
if (arg$Replicates$active==1 & length(arg$Replicates$SELECTvar)==0) stop("no specified output variables for replicates in iamArgs object!!") 

#Scenario
ind <- is.null(arg$Scenario$ALLscenario)
if (!arg$Scenario$active%in%(0:1)) stop("wrong 'Scen$active' argument in iamArgs object!!")
if (arg$Scenario$active==1 & ind) warning("no available scenario. 'Scen$active' put to 0!!")
if (!ind) {if (!is.character(arg$Scenario$ALLscenario)) stop("wrong 'ALLscenario' argument in iamArgs object!!")}                           
if (!ind) {if (!arg$Scenario$SELECTscen%in%(c(1:length(arg$Scenario$ALLscenario)))) stop("wrong 'SELECTscen' argument in iamArgs object!!")}                           

#Gestion
if (!arg$Gestion$active%in%(0:1)) stop("wrong 'Gest$active' argument in iamArgs object!!")
if (!arg$Gestion$control%in%c("Nb vessels","Nb trips")) stop("wrong 'control' argument in iamArgs object!!")
if (!arg$Gestion$target%in%c("TAC","Fbar","TAC->Fbar","biomasse")) stop("wrong 'target' argument in iamArgs object!!")
if (!arg$Gestion$espece%in%c(spe$Species,spe$StaticSpp)) stop("wrong 'espece' argument in iamArgs object!!")
if (!arg$Gestion$typeG%in%(0:1)) stop("wrong 'level' argument in iamArgs object!!")
if (!arg$Gestion$delay%in%(1:spe$NbSteps)) stop("wrong 'delay' argument in iamArgs object!!")
if (!arg$Gestion$upd%in%(1:2)) stop("wrong 'upd' argument in iamArgs object!!")
if (!is.numeric(arg$Gestion$sup)) stop("wrong 'sup' argument in iamArgs object!!")
if (!is.numeric(arg$Gestion$inf)) stop("wrong 'inf' argument in iamArgs object!!")
if (length(arg$Gestion$tac)!=length(spe$times)) stop("wrong 'tac' argument in iamArgs object!!")
if (length(arg$Gestion$fbar)!=length(spe$times)) stop("wrong 'fbar' argument in iamArgs object!!")
if (nrow(arg$Gestion$mfm)!=length(spe$Fleet)) stop("wrong 'mfm' argument in iamArgs object!!")
if (ncol(arg$Gestion$mfm)!=length(spe$MetierEco)) stop("wrong 'mfm' argument in iamArgs object!!")
if (length(arg$Gestion$othSpSup)!=length(c(na.omit(spe$Species)))+length(c(na.omit(spe$StaticSpp)))-1) stop("wrong 'othSpSup' argument in iamArgs object!!")
if (nrow(arg$Gestion$effSup)!=length(spe$Fleet)) stop("wrong 'effSup' argument in iamArgs object!!")
if (ncol(arg$Gestion$effSup)!=length(spe$times)) stop("wrong 'effSup' argument in iamArgs object!!")



#Eco
if (!arg$Eco$active%in%(0:1)) stop("wrong 'Eco$active' argument in iamArgs object!!")
if (!arg$Eco$type%in%(1:2)) stop("wrong 'Eco$type' argument in iamArgs object!!")
if (!arg$Eco$adj%in%(1:2)) stop("wrong 'Eco$adj' argument in iamArgs object!!")
#if (!arg$Eco$lev%in%(1:2)) stop("wrong 'Eco$lev' argument in iamArgs object!!")
if (!arg$Eco$ue_choice%in%(1:2)) stop("wrong 'Eco$ue_choice' argument in iamArgs object!!")
if (!arg$Eco$oths%in%(0:1)) stop("wrong 'Eco$oths' argument in iamArgs object!!")
if (!arg$Eco$othsFM%in%(0:1)) stop("wrong 'Eco$othsFM' argument in iamArgs object!!")
if (!arg$Eco$perscCalc%in%(0:4)) stop("wrong 'Eco$perscCalc' argument in iamArgs object!!")
if (!arg$Eco$report%in%(0:1)) stop("wrong 'Eco$report' argument in iamArgs object!!")
if (!is.numeric(arg$Eco$dr)) stop("wrong 'Eco$dr' argument in iamArgs object!!")

	return(TRUE)

}

setClass("iamArgs",
	representation(
		desc="character", 
		arguments="list",
		specific="list"
	),
	prototype(
		desc="My model",
		arguments=list(
		  Recruitment = list(),
      Replicates = list(), 
      Scenario = list(), 
      Gestion = list(), 
      Eco = list()),
    specific = list(Species=character(),
                    StaticSpp=character(),
    	              Fleet=character(),
                  	Metier=character(),
                  	MetierEco=character(),	
                    Ages=list(),
                   	Cat=list(),
                    t_init=double(),
                    NbSteps=integer(),
                   	times=integer(),
                    Q=integer())
  ),
	validity=val.iamArgs
)


