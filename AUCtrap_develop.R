library(devtools)
library(desc)
create_package("../AUCtrap")

use_git()
use_github()
desc_add_author(given="Aniko", family="Szabo", email = "aszabo@mcw.edu",
                role=c("aut","cre"))
desc_add_author(given="Yushu", family="Wang", email = "yuswang@mcw.edu",
                role=c("aut","cre"))
desc_add_author(given="DeXuan", family="Zhang", email = "dezhang@mcw.edu",
                role=c("aut","cre"))
desc_add_author(given="Haoran", family="Teng", email = "hteng@mcw.edu",
                role=c("aut","cre"))
use_gpl3_license()
use_testthat()


use_r("AUCtrap")
use_test("AUCtrap")

document()
test()
load_all()
check()

AUCtrap(0:4, c(3,4,1,2), method="AUC")
ex <- AUCtrap(0:3, c(3,4,1,2), method="AUC")
AUCtrap(0:3, c(3,4,1,2))
AUCtrap(0:3, c(3,4,1,2), method="iAUC")
AUCtrap(0:3, c(3,4,1,2), method="xyz")


plot(ex)
plot(ex, fill.pos="pink", pch=20, xlab="Time", ylab="Value")
