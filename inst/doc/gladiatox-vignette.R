## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ---- eval=TRUE, echo=FALSE, include=FALSE-------------------------------
library(GladiaTOX)
outdir <- getwd()

## ---- eval=FALSE, echo=FALSE, include=FALSE, message=FALSE, warning=FALSE, results='hide'----
#  #NOTE ssh to chp893
#  library(devtools)
#  library(roxygen2)
#  setwd("~/gladiatox")
#  roxygenize()
#  load_all()
#  setwd("~/gladiatox/vignettes")
#  outdir <- getwd()

## ---- eval=TRUE, echo=FALSE, include=FALSE-------------------------------
# Set the options
sqlite_src <- file.path(system.file(package="GladiaTOX"), "sql",
                        "gladiatoxdb.sqlite")
sqlite_trg <- file.path(system.file(package="GladiaTOX"), "sql",
                        "gladiatoxdbtmp.sqlite")
file.copy(sqlite_src, sqlite_trg, overwrite=TRUE)

## ---- eval=TRUE, echo=FALSE, include=FALSE-------------------------------
sqlite_trg <- file.path(system.file(package="GladiaTOX"), "sql",
                        "gladiatoxdbtmp.sqlite")
tcplConf(drvr = "SQLite",
         host = NA,
         user = NA,
         pass = NULL,
         db = sqlite_trg)

## ---- eval=TRUE, include=FALSE, echo=FALSE-------------------------------
load(system.file("extdata", "data_for_vignette.rda", package="GladiaTOX"))

## ---- eval=FALSE, echo=TRUE, include=TRUE--------------------------------
#  library(GladiaTOX)
#  # Set output directory for reporting
#  outdir <- getwd()

## ---- eval=TRUE, echo=TRUE, include=TRUE---------------------------------
tcplLoadAsid()

## ---- eval=TRUE, echo=FALSE, include=TRUE--------------------------------
file.path("/install_dir", "sql", "gladiatoxdb.sqlite")

## ---- eval=FALSE, echo=TRUE, include=TRUE--------------------------------
#  tcplConf(drvr = "SQLite",
#           host = NA,
#           user = NA,
#           pass = NULL,
#           db = "/my_folder_path/gladiatoxdb.sqlite")

## ---- eval=FALSE, echo=TRUE, include=TRUE--------------------------------
#  tcplConf(drvr = "MySQL",
#           host = "local.host",
#           user = "username",
#           pass = "********",
#           db = "my_gl_database")

## ---- eval=FALSE, include=FALSE, echo=TRUE-------------------------------
#  load(system.file("extdata", "data_for_vignette.rda", package="GladiaTOX"))
#  outdir <- getwd()

## ---- eval=FALSE---------------------------------------------------------
#  head(plate)

## ---- eval=TRUE, echo=FALSE, include=TRUE--------------------------------
print(head(plate), row.names = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  head(chnmap, 7)

## ---- eval=TRUE, echo=FALSE, include=TRUE--------------------------------
print(head(chnmap, 7), row.names = FALSE)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
assay <- GladiaTOX:::.buildAssayTab(plate, chnmap)

## ---- eval=FALSE, echo=FALSE, include=FALSE------------------------------
#  head(assay, 4)

## ---- eval=FALSE, echo=FALSE, include=FALSE------------------------------
#  print(head(assay, 4), row.names = FALSE)

## ---- eval=FALSE---------------------------------------------------------
#  head(dat)

## ---- eval=TRUE, echo=FALSE, include=TRUE--------------------------------
print(head(dat), row.names = FALSE)

## ---- eval=TRUE----------------------------------------------------------
## Set study paramenters
std.nm <- "S167320" # study name
phs.nm <- "vignette" # study phase

## ---- echo=TRUE, eval=TRUE, message=FALSE, warning=FALSE-----------------
## List of studies before loading
tcplLoadAsid()
## Load annotation in tcplDB
loadAnnot(plate, assay, NULL)
## List of studies after loading
tcplLoadAsid()

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
# Get assay source ID
asid = tcplLoadAsid(fld = c("asnm", "asph"), val = list(std.nm, phs.nm))$asid
print(asid)

## ---- echo=TRUE, eval=TRUE-----------------------------------------------
# Prepare and load data
dat <- prepareDatForDB(asid, dat)
tcplWriteData(dat[ , list(acid, waid, wllq, rval)], lvl = 0, type = "mc")

## ---- echo=TRUE, eval=TRUE, results='hide', message=FALSE, warning=FALSE----
assignDefaultMthds(asid = asid)

## ---- echo=TRUE, eval=TRUE, include=TRUE, results='hide', message=FALSE, warning=FALSE----
res <- tcplRun(asid = asid, slvl = 1, elvl = 3)
aeids <- tcplLoadAeid(fld = "asid", val = asid)$aeid
tmp <- mapply(function(xx){
    tryCatch(tcplCalcVmad(inputs = xx, aeid = xx, 
                          notes = "computed within study"), 
             error = function(e) NULL)},
    as.integer(aeids))
res <- tcplRun(asid = asid, slvl = 1, elvl = 6)

## ---- echo=TRUE, eval=FALSE, include=TRUE, message=FALSE, warning=FALSE, results='hide'----
#  ## QC report
#  tcplReport(type = "qc", asid = asid, report_author = "report author",
#             report_title = "Vignette QC report", odir = outdir)

## ---- eval=TRUE----------------------------------------------------------
acnm <- "DNA damage (pH2AX)_DNA damage (pH2AX)_4h"
myaid <- tcplLoadApid()[u_boxtrack == "S-000031351", aid]
myaid <- myaid[myaid%in%tcplLoadAid(fld = "asid", val = asid)$aid]
apid <- tcplLoadApid()[u_boxtrack == "S-000031351" & aid == myaid, apid]
acid <- tcplLoadAcid(fld = c("aid", "acnm"), val = list(myaid, acnm))[, acid]
l2 <- tcplLoadData(lvl = 2L, fld = "acid", val = acid)

## ---- eval=TRUE, fig=TRUE, fig.width=8, fig.height=6, fig.cap='Example of heatmap of a plate raw values. Letters in well indicate the well type.'----
tcplPlotPlate(dat = l2, apid = apid, id = acid)

## ---- eval=TRUE----------------------------------------------------------
aeid <- tcplLoadAeid(fld = c("acid", "analysis_direction"), val = list(acid, "up"))[, aeid]
spid <- tcplLoadWaid(fld = c("apid", "wllt"), val = list(apid, "c"))[,unique(spid)]
m4id <- tcplLoadData(lvl = 4L, fld = c("spid", "aeid"), val = list(spid, aeid))[, m4id]

## ---- eval=TRUE, out.width='70%', fig.cap='Example of Positive control (chlorambucil chemical) plot with three concentrations, and three technical replicates per concentration.'----
tcplPlotM4ID(m4id = m4id, lvl = 6, bline = "coff")

## ---- eval=TRUE, echo=TRUE, include=TRUE---------------------------------
apid <- tcplLoadApid()[u_boxtrack%in%"S-000030318", apid] # plate id
waids <- tcplLoadWaid(fld="apid", val=apid)$waid #well ids
m0ids <- tcplLoadData(lvl = 0, fld = "waid", val = waids)$m0id # raw data ids
tcplSetWllq(ids = m0ids, wllq = 0, type = "mc") # set well quality to zero

## ---- echo=TRUE, eval=TRUE, results='hide', message=FALSE, warning=FALSE----
res <- tcplRun(asid = asid, slvl = 1, elvl = 6)

## ---- eval=FALSE, echo=TRUE, include=TRUE, message=FALSE, warning=FALSE, results='hide'----
#  ## Processing report
#  tcplReport(type = "all", asid = asid, report_author = "report author",
#             report_title = "Vignette Processing report", odir = outdir)

## ---- eval=TRUE, fig.width=7---------------------------------------------
## Endpoint to plot
aeids <- tcplLoadAeid(fld=c("asid", "aenm"),
            val=list(asid, "DNA damage (pH2AX)_DNA damage (pH2AX)_24h_up"),
            add.fld="asid")$aeid
## level 4 id to plot
m4id <- tcplLoadData(lvl=4L)[(aeid==aeids & grepl("chromium", spid))]$m4id[1]

## ---- eval=TRUE, out.width='70%', fig.cap='Best fit selection.'----------
tcplPlotM4ID(m4id = m4id, lvl = 6, bline = "coff")

## ---- eval=TRUE, fig.cap='Example of dose-response curves.'--------------
## Get chemical id to plot
chid <- tcplLoadChem(field = "chnm", val = "chromium",
                     include.spid = FALSE)$chid
## Plot dose-response curves
tcplPlotWin(chid = chid, aeid = aeids, bline = "bmad", collapse = TRUE)

## ---- eval=TRUE, echo=TRUE, include=TRUE, message=FALSE, warning=FALSE, results='hide', fig.cap='Example of MEC boxplot.'----
fname <- paste0(format(Sys.Date(), format="%y%m%d"), "_ASID", asid,"_MEC.pdf")
fname <- file.path(outdir, fname)
pdf(fname, width = 11, height = 7)
glPlotStat(asid)
dev.off()

## ---- echo=FALSE, out.width='100%', fig.cap='Example of MEC barplot.'----
knitr::include_graphics('MECfig.png')

## ---- eval=TRUE, echo=TRUE, include=TRUE, warning=FALSE, fig.cap='Example of pie plots. The figure shows three chemicals and all endpoints.'----
chnms <- c("mercury", "o-cresol", "p-cresol")
glPlotPie(asid, chnms = chnms)

