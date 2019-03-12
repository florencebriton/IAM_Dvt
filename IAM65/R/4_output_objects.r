
#==================================================
# Définition de l'objet Output global sans réplicats et test de validité 
#==================================================

val.iamOutput <- function(object){

#à remplir avec les tests de validité à appliquer aux données de sortie
  #if ... stop(...)
  
	return(TRUE)

}



setClass("iamOutput",
	representation(
		desc = "character",
		arguments = "list",
		specific = "list",
		outputSp = "list",       # indicateur avec niveau de définition "espèce"
		output = "list"          # indicateur sans niveau de définition "espèce"
	),
	prototype(
		desc = "My output",
		arguments = list(Recruitment = list(),
                     Replicates = list(), 
                     Scenario = list(), 
                     Gestion = list(), 
                     Eco = list()),
		specific = list(Species = character(),
                    StaticSpp=character(),
    	              Fleet = character(),
                  	Metier = character(),
                  	MetierEco = character(),	
                    Ages = list(),
                   	Cat = list(),
                    t_init = double(),
                    NbSteps = integer(),
                   	times = integer(),
                    Q=integer()),
		outputSp = list(F = list(),               #mortalité par pêche non corrigée (-> captures)
                    Fr = list(),              #mortalité par pêche corrigée (-> morts)
                    Fothi = list(),           #mortalité "autres flottilles, autres métiers"
                    Fbar = list(),            #indice Fbar
                    Z = list(),               #mortalité totale (F+M)
                    N = list(),               #effectifs totaux en nombre
                    B = list(),               #biomasse
                    SSB = list(),             #biomasse reproductrice
                    C = list(),               #captures totales en nombre pour les flottilles et métiers modélisés
                    Ctot = list(),            #captures totales en nombre
                    Y = list(),               #captures totales en poids pour les flottilles et métiers modélisés (composante âge)
                    Ytot = list(),            #captures totales en poids (composante âge)
                    D = list(),               #rejets totaux en poids pour les flottilles et métiers modélisés (composante âge)
                    Li = list(),              #débarquements totaux aux âges en poids pour les flottilles et métiers bio modélisés  (composante âge)
                    Lc = list(),              #débarquements totaux en poids par catégorie pour les flottilles et métiers bio modélisés  (composante âge)
                    Ltot = list(),            #débarquements totaux en poids (composante âge)
                    P = list(),               #prix moyen par espèce et catégorie
                    GVL_f_m_e = list(),       #CA total par espèce, flottille et métier éco
                     GVLcom_f_m_e = list(),
                     GVLst_f_m_e = list(),
                    statY = list(),           #captures par flottille, métier pour les espèces sans dynamique
                    statL = list(),           #débarquements par flottille, métier pour les espèces sans dynamique
                    statD = list(),           #rejets par flottille, métier pour les espèces sans dynamique
                    statP = list(),           #Prix pour les espèces sans dynamique
                    statGVL_f_m = list(),     #CA total par espèce statique , flottille et métier éco
                     statGVLcom_f_m = list(),
                     statGVLst_f_m = list(),
                    PQuot = list(),
                    F_S1M1= list(),F_S1M2= list(),F_S1M3= list(),F_S1M4= list(),
                    F_S2M1= list(),F_S2M2= list(),F_S2M3= list(),F_S2M4= list(),
                    F_S3M1= list(),F_S3M2= list(),F_S3M3= list(),F_S3M4= list(),
                    F_S4M1= list(),F_S4M2= list(),F_S4M3= list(),F_S4M4= list(),
                    Fr_S1M1= list(),Fr_S1M2= list(),Fr_S1M3= list(),Fr_S1M4= list(),
                    Fr_S2M1= list(),Fr_S2M2= list(),Fr_S2M3= list(),Fr_S2M4= list(),
                    Fr_S3M1= list(),Fr_S3M2= list(),Fr_S3M3= list(),Fr_S3M4= list(),
                    Fr_S4M1= list(),Fr_S4M2= list(),Fr_S4M3= list(),Fr_S4M4= list(),
                    Z_S1M1= list(),Z_S1M2= list(),Z_S1M3= list(),Z_S1M4= list(),
                    Z_S2M1= list(),Z_S2M2= list(),Z_S2M3= list(),Z_S2M4= list(),
                    Z_S3M1= list(),Z_S3M2= list(),Z_S3M3= list(),Z_S3M4= list(),
                    Z_S4M1= list(),Z_S4M2= list(),Z_S4M3= list(),Z_S4M4= list(),
                    N_S1M1= list(),N_S1M2= list(),N_S1M3= list(),N_S1M4= list(),
                    N_S2M1= list(),N_S2M2= list(),N_S2M3= list(),N_S2M4= list(),
                    N_S3M1= list(),N_S3M2= list(),N_S3M3= list(),N_S3M4= list(),
                    N_S4M1= list(),N_S4M2= list(),N_S4M3= list(),N_S4M4= list(),
                    DD_efmi= list(),
                    DD_efmc= list(),
                    LD_efmi= list(),
                    LD_efmc= list(),
                    statDD_efm= list(),
                    statLD_efm= list(),
                    statLDst_efm= list(),
                    statLDor_efm= list(),
                    oqD_ef= list(),
                    oqD_e= list(),
                    oqDstat_ef= list(),
                    TACtot = list(),
                    TACbyF = list()),
    output = list(typeGest = integer(),                #type de scénario de gestion appliqué
                  nbv_f = numeric(),                   #Nb de navires par flottille
                  effort1_f = numeric(),               #1ère composante d'effort moyen par an et par flottille
                  effort2_f = numeric(),               #2ème composante d'effort moyen par an et par flottille
                  nbv_f_m = numeric(),                 #Nb de navires par flottille-métier
                  effort1_f_m = numeric(),             #1ère composante d'effort moyen par an et par flottille-métier
                  effort2_f_m = numeric(),             #2ème composante d'effort moyen par an et par flottille-métier
                  #Lbio_f = numeric(),
                  GVLtot_f_m = numeric(),
                  GVLav_f_m = numeric(),
                  GVLtot_f = numeric(),
                  GVLav_f = numeric(),
                  #GVLoths_f = numeric(),
                  NGVLav_f_m = numeric(),
                  NGVLav_f = numeric(),
                  ET_f_m = numeric(),
                  cnb_f_m = numeric(),
                  cnb_f = numeric(),
                  #vcst_f_m = numeric(),
                  #vcst_f = numeric(),
                  rtbs_f_m = numeric(),
                  rtbs_f = numeric(),
                  rtbsAct_f = numeric(),
                  cshrT_f_m = numeric(),
                  cshrT_f = numeric(),
                  ncshr_f = numeric(),
                  ocl_f = numeric(),
                  cs_f = numeric(),
                  csAct_f = numeric(),
                  csTot_f = numeric(),
                  gva_f = numeric(),
                  gvaAct_f = numeric(),
                  gvamargin_f = numeric(),
                  gva_FTE_f = numeric(),
                  ccw_f = numeric(),
                  ccwCr_f = numeric(),
                  wageg_f = numeric(),
                  wagen_f = numeric(),
                  wageg_FTE_f = numeric(),
                  wageg_h_f = numeric(),
                  gp_f = numeric(),
                  gpAct_f = numeric(),
                  gpmargin_f = numeric(),
                  ncf_f = numeric(),
                  np_f = numeric(),
                  npmargin_f = numeric(),
                  prof_f = numeric(),
                  npmargin_trend_f = numeric(),
                  ssTot_f = numeric(),
                  ps_f = numeric(),
                  psAct_f = numeric(),
                  sts_f = numeric(),
                  stsAct_f = numeric(),
                  BER_f = numeric(),
                  CR_BER_f = numeric(),
                  fuelEff_f = numeric(),
                  ratio_fvol_gva_f = numeric(),
                  ratio_gp_gva_f = numeric(),
                  ratio_GVL_K_f = numeric(),
                  ratio_gp_K_f = numeric(),
                  RoFTA_f = numeric(),
                  ROI_f = numeric(),
                  ratio_np_K_f = numeric(),
                  ratio_GVL_cnb_ue_f = numeric(),
                  YTOT_fm = numeric(),
                  reconcilSPP = character())
  ),
	validity=val.iamOutput
)




#==================================================
# Définition de l'objet Output global avec réplicats et test de validité 
#==================================================

val.iamOutputRep <- function(object){

#à remplir avec les tests de validité à appliquer aux données de sortie
  #if ... stop(...)
  
	return(TRUE)

}



setClass("iamOutputRep",
	representation(
		desc = "character",
		arguments = "list",
		specific = "list",
		outputSp = "list",       # indicateur avec niveau de définition "espèce"
		output = "list"          # indicateur sans niveau de définition "espèce"
	),
	prototype(
		desc = "My output with replicates",
		arguments = list(Recruitment = list(),
                     Replicates = list(), 
                     Scenario = list(), 
                     Gestion = list(), 
                     Eco = list()),
		specific = list(Species = character(),
                    StaticSpp=character(),
    	              Fleet = character(),
                  	Metier = character(),
                  	MetierEco = character(),	
                    Ages = list(),
                   	Cat = list(),
                    t_init = double(),
                    NbSteps = integer(),
                   	times = integer(),
                    Q=integer()),
		outputSp = list(F = list(),               #mortalité par pêche non corrigée (-> captures)
                    Fr = list(),              #mortalité par pêche corrigée (-> morts)
                    Fothi = list(),           #mortalité "autres flottilles, autres métiers"
                    Fbar = list(),            #indice Fbar
                    Z = list(),               #mortalité totale (F+M)
                    N = list(),               #effectifs totaux en nombre
                    B = list(),               #biomasse
                    SSB = list(),             #biomasse reproductrice
                    C = list(),               #captures totales en nombre pour les flottilles et métiers modélisés
                    Ctot = list(),            #captures totales en nombre
                    Y = list(),               #captures totales en poids pour les flottilles et métiers modélisés
                    Ytot = list(),            #captures totales en poids
                    D = list(),               #rejets totaux en poids pour les flottilles et métiers modélisés
                    Li = list(),              #débarquements totaux aux âges en poids pour les flottilles et métiers bio modélisés
                    GVL_f_m_e = list(),       #CA total par espèce dynamique, flottille et métier éco
                    statY = list(),           #captures par flottille, métier pour les espèces sans dynamique
                    statL = list(),           #débarquements par flottille, métier pour les espèces sans dynamique
                    statD = list(),           #rejets par flottille, métier pour les espèces sans dynamique
                    statGVL_f_m = list(),     #CA total par espèce statique , flottille et métier éco
                    PQuot = list()),
    output = list(nbv_f = list(),                   #Nb de navires par flottille
                  nbds_f = list(),                  #Nb de jdm moyen par an et par flottille
                  nbv_f_m = list(),                 #Nb de navires par flottille-métier
                  nbds_f_m = list(),                #Nb de jdm moyen par an et par flottille-métier
                  GVLtot_f_m = list(),              #CA total par flottille et métier
                  GVLtot_f = list(),                #CA total par flottille
                  GVLav_f = list(),                 #CA moyen par navire d'une flottille
                  vcst_f = list(),                  #Coûts variables par navire d'une flottille
                  vcst_f_m = list(),                #Coûts variables par navire d'une flottille-métier
                  rtbs_f = list(),                  #Reste à partager par navire d'une flottille
                  rtbsAct_f = list(),               #Reste à partager actualisé par navire d'une flottille
                  cs_f = list(),                    #Surplus de l'équipage par navire d'une flottille
                  csAct_f = list(),                 #Surplus de l'équipage actualisé par navire d'une flottille
                  gva_f = list(),                   #Valeur Ajoutée Brute pour un navire d'une flottille
                  gvaAct_f = list(),                #Valeur Ajoutée Brute actualisée pour un navire d'une flottille
                  ccwCr_f = list(),                 #Coûts du personnel par marin
                  wagen_f = list(),                 #Salaire net par marin
                  gcf_f = list(),                   #Excédent brut d'exploitation par navire d'une flottille
                  gcfAct_f = list(),                #Excédent brut d'exploitation actualisé par navire d'une flottille
                  gp_f = list(),                    #Résultat net d'exploitation par navire d'une flottille
                  ps_f = list(),                    #Surplus total du producteur d'une flottille
                  psAct_f = list(),                 #Surplus total du producteur actualisé d'une flottille
                  sts_f = list(),                   #Surplus de l’État associé à une flottille 
                  stsAct_f = list())                #Surplus de l’État actualisé associé à une flottille 
  ),
	validity=val.iamOutputRep
)


