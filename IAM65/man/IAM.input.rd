\name{IAM.input}
\alias{IAM.input}
\alias{IAM.input-methods}
\alias{IAM.input,character,missing,missing,missing-method}
\alias{IAM.input,character,character,missing,missing-method}
\alias{IAM.input,character,character,character,missing-method}
\alias{IAM.input,character,character,missing,character-method}
\alias{IAM.input,character,character,character,character-method}
\docType{methods}

\title{'iamInput' objects creator}
\description{ToDo}
\section{Methods}{
  \describe{
	\item{IAM.input}{\code{signature(fileIN = "character", fileSPEC = "missing", fileSCEN = "missing", fileSTOCH = "missing")}: fileIN is an .xls standard IAM input file.}
	\item{IAM.input}{\code{signature(fileIN = "character", fileSPEC = "character", fileSCEN = "missing", fileSTOCH = "missing")}: fileIN & fileSPEC are 'input' and 'specific' output .txt files from "IAM.input.out" method.}
	\item{IAM.input}{\code{signature(fileIN = "character", fileSPEC = "character", fileSCEN = "character", fileSTOCH = "missing")}: fileIN, fileSPEC & fileSCEN are 'input', 'specific' and 'scenario' output .txt files from "IAM.input.out" method.}
	\item{IAM.input}{\code{signature(fileIN = "character", fileSPEC = "character", fileSCEN = "missing", fileSTOCH = "character")}: fileIN, fileSPEC & fileSTOCH are 'input', 'specific' and 'stochastic' output .txt files from "IAM.input.out" method.}
	\item{IAM.input}{\code{signature(fileIN = "character", fileSPEC = "character", fileSCEN = "character", fileSTOCH = "character")}: fileIN, fileSPEC, fileSCEN & fileSTOCH are 'input', 'specific', 'scenario' and 'stochastic' output .txt files from "IAM.input.out" method.}
}}


\section{Further arguments :}{
\tabular{ll}{
\bold{\code{t_init}} \tab Only for first signature. Initial year.  \cr
\bold{\code{nbStep}} \tab Only for first signature. Number of timesteps (including initial year).  \cr
\bold{\code{t_hist_max}} \tab Only for first signature. Last year considered for 'historical' slot.  \cr
\bold{\code{desc}} \tab Object descriptor (default value : "My Input").  \cr
\bold{\code{folderFleet}} \tab Folder containing fleets input sheets (Optionnal. Default value : NULL).  \cr
\bold{\code{readXLS}} \tab Character. Package to be used to read parameter Excel file. 'openxlsx' for post-2007 excel version (.xlsx files), 'XLConnect' for pre-2007 excel version (.xls files).(Default value : "openxlsx").  \cr
\bold{\code{Fq_i}} \tab List containing SS3 parameters arrays for quarterly stock dynamic simulation. One element per considered species, all names must match with input species. Fishing mortality per season, morph, and age. (Optionnal. Default value : NULL. Example : list(hake=array(...,dim=c(4,4,nAge))).  \cr
\bold{\code{Fq_fmi}} \tab See above. Fishing mortality per season, morph, fleet, "economic" metiers and age (Optionnal. Default value : NULL).  \cr
\bold{\code{Ni0q}} \tab See above. Recruits numbers per season (Optionnal. Default value : NULL).  \cr
}
}

\keyword{methods}


