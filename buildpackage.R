pckDIR <- "/home/martin/git/prmisc/"
myRlib <- "/home/martin/R/x86_64-pc-linux-gnu-library/3.4/"
##myRlib <- "/home/martin/R/i686-pc-linux-gnu-library/3.4/"

library("roxyPackage")
roxy.package(
   pck.source.dir=pckDIR,
   pck.version="0.01-0",
   R.libs=myRlib,
   repo.root="~/R/repo/prmisc",
   pck.description=data.frame(
   Package="prmisc",
   Type="Package",
   Title="Miscellaneous printing",
   Author="Martin Papenberg <martin.papenberg@hhu.de>",
   AuthorsR="c(person(given=\"Martin\", family=\"Papenberg\",
   email=\"martin.papenberg@hhu.de\",
   role=c(\"aut\", \"cre\")))",
   Maintainer="Martin Papenberg <martin.papenberg@hhu.de>",
   Depends="R (>= 3.0.0)",
   Description="Miscellaneous printing of stat results in Rmarkdown",
   License="Public domain",
   Encoding="UTF-8",
   LazyLoad="yes",
   URL="https://github.com/m-Py/prmisc",
   stringsAsFactors=FALSE)
)
