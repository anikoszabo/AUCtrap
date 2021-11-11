library(devtools)
library(desc)
create_package("../AUCtrap")

use_git()
use_github()
desc_add_author(given="Aniko", family="Szabo", email = "aszabo@mcw.edu",
                role=c("aut","cre"))
use_gpl3_license()
