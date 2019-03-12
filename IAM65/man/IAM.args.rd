\name{IAM.args}
\alias{IAM.args}
\alias{IAM.args-methods}
\alias{IAM.args,iamInput,missing-method}
\alias{IAM.args,character,character-method}
\alias{IAM.args,iamArgs,missing-method}
\docType{methods}

\title{'iamArgs' objects creator}
\description{ToDo}

\usage{
IAM.args(object,specific,\dots)
}

\arguments{
  \item{object}{ToDo.}
  \item{specific}{ToDo.}
  \item{\dots}{Further arguments}  
}


\section{Methods}{
  \describe{
	\item{IAM.args}{\code{signature(object = "iamInput", specific = "missing")}: 'object' is the output of \emph{IAM.input} method.}
	\item{IAM.args}{\code{signature(object = "character", specific = "character")}: 'object' & 'specific' are 'args' and 'specific' output .txt files from, respectively, "IAM.args.out" & "IAM.input.out" methods.}
	\item{IAM.args}{\code{signature(object = "iamArgs", specific = "missing")}: 'object' is the output of \emph{IAM.args} method.}
}}


\section{Further arguments :}{
\tabular{ll}{
\bold{\code{desc}} \tab Object descriptor (default value : \code{as.character(NA)}).  \cr
}
}

\keyword{methods}


