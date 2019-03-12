\name{IAM.export}
\alias{IAM.export}
\alias{IAM.export-methods}
\alias{IAM.export,iamInput-method}
\alias{IAM.export,iamArgs-method}
\docType{methods}

\title{Exporting IAM objects as readable .txt file}
\description{ToDo}

\usage{
IAM.export(object,folder,\dots)
}

\arguments{
  \item{object}{ToDo.}
  \item{folder}{Character. The folder in which the output .txt files will be exported.}
  \item{\dots}{Further arguments}  
}

\details{This method will export the input object's slots as .txt file with a generic name corresponding to the name of the slot 
("specific.txt", "input.txt","scenario.txt" & "stochastic.txt" for an \emph{iamInput} object, or "specific.txt" & "args.txt" for an
\emph{iamArgs} object. \\
WARNING : if a file with the same name is already in the folder, it will be replaced with the exported file. So, use it with caution!!  

}


\section{Methods}{
  \describe{
	\item{IAM.export}{\code{signature(object = "iamInput")}: 'object' is the output of \emph{IAM.input} method.}
	\item{IAM.export}{\code{signature(object = "iamArgs")}: 'object' is the output of \emph{IAM.args} method.}
}}


\keyword{methods}


