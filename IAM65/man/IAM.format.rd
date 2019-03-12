\name{IAM.format}
\alias{IAM.format}
\alias{IAM.format-methods}
\alias{IAM.format,iamOutput-method}
\alias{IAM.format,iamOutputRep-method}
\docType{methods}

\title{Formating method}
\description{ToDo}

\usage{
IAM.format(object,\dots)
}

\arguments{
  \item{object}{An "iamOutput" or "iamOutputRep" object.}
  \item{\dots}{Further arguments (see below for details)}
    
}

\section{Other arguments}{
\tabular{ll}{
\bold{elmt} \tab Character. Name of the operating variable. \cr
\bold{spp} \tab Character. Name of the considered species (relevant only for some variables). \cr
\bold{agg} \tab Numeric. Index(es) describing dimensions on which agregation must be done, referring to 'DimCst' attributes (1=fleet, 2=metier, 3=age/category, 4=time).  \cr
\bold{headers} \tab Character. Optionnal. Dimension that will be developped as headers in the output data.frame. \cr
\bold{subs} \tab Logical. An optionnal 'subset' argument to operate a final subsetting on output dataframe. \cr
}
}

%\details{
%}

\section{Methods}{
  \describe{
	\item{IAM.format}{\code{signature(object = "iamOutput"}: object is an output from \emph{IAM.model} method.}
	\item{IAM.format}{\code{signature(object = "iamOutputRep"}: object is an output from \emph{IAM.model} method.}
}}

\keyword{methods}


