\name{IAM.unite}
\alias{IAM.unite}
\alias{IAM.unite-methods}
\alias{IAM.unite,list-method}
\docType{methods}

\title{Formating method for grouping various iamOutput or iamOutputRep objects}
\description{ToDo}

\usage{
IAM.unite(listObj,\dots)
}

\arguments{
  \item{listObj}{A list containing "iamOutput" OR "iamOutputRep" objects. Mixed lists are not allowed.}
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
	\item{IAM.unite}{\code{signature(listObj = "list"}: \emph{listObj} is a list with 'iamOutput' or 'iamOutputRep' objects.}
}}

\keyword{methods}


