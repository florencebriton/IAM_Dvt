Instructions Compilation pour architecture simple :

cd C:\Users\fbriton\Documents\Golfe_Gascogne\IAM\IAM_TAC_variables

%R_pgm%/R CMD check IAM65 --no-multiarch

%R_pgm%/R CMD build IAM65 --no-multiarch

%R_pgm%/R CMD INSTALL --build IAM65 --no-multiarch


Debuggage avec gdb :

R -d gdb -f c:\test.r

Compilation 64 bits

%R_pgm%/R CMD INSTALL --build --compile-both IAM65_0.1.tar.gz


