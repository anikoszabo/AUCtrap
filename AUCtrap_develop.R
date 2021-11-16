library(devtools)
library(desc)
create_package("../AUCtrap")

use_git()
use_github()
desc_add_author(given="Aniko", family="Szabo", email = "aszabo@mcw.edu",
                role=c("aut","cre"))
use_gpl3_license()

use_r("AUCtrap")

document()
load_all()
check()

AUCtrap(0:4, c(3,4,1,2), method="AUC")
AUCtrap(0:3, c(3,4,1,2), method="AUC")
AUCtrap(0:3, c(3,4,1,2))
AUCtrap(0:3, c(3,4,1,2), method="iAUC")
AUCtrap(0:3, c(3,4,1,2), method="xyz")
