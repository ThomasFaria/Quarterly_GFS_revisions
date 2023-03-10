# Quarterly GFS revisions

[![Licence](https://img.shields.io/badge/Licence-EUPL--1.2-001489)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
[![Onyxia](https://img.shields.io/badge/Launch-Datalab-orange?logo=R)](https://datalab.sspcloud.fr/launcher/ide/rstudio?autoLaunch=true&service.image.pullPolicy=«Always»&service.image.custom.enabled=true&service.image.custom.version=«thomasfaria%2Fquarterly_gfs_revisions%3Alatest»&security.allowlist.enabled=false&onyxia.friendlyName=«QGFS»)
[![Build](https://img.shields.io/github/actions/workflow/status/ThomasFaria/Quarterly_GFS_revisions/build-image.yaml?label=Build
)](https://hub.docker.com/repository/docker/thomasfaria/quarterly_gfs_revisions)
[![Publish](https://img.shields.io/github/actions/workflow/status/ThomasFaria/Quarterly_GFS_revisions/publish.yaml?label=Paper&style=flat)](https://thomasfaria.github.io/Quarterly_GFS_revisions/)

Replication of the research paper ["How well-behaved are revisions to quarterly fiscal data in the euro area?"](https://thomasfaria.github.io/Quarterly_GFS_revisions/) by [Krzysztof Bańkowski](https://www.ecb.europa.eu/pub/research/authors/profiles/krzysztof-bankowski.fr.html), [Thomas Faria](https://www.ecb.europa.eu/pub/research/authors/profiles/thomas-faria.fr.html), [Robert Schall](https://www.ecb.europa.eu/pub/research/authors/profiles/robert-schall.fr.html) (2022).

## Getting Started

To ensure full reproducibility of the results, the project is accompanied by a [Docker image](https://hub.docker.com/r/thomasfaria/quarterly_gfs_revisions) that contains all the necessary packages and dependencies. You can pull the Docker image using the following command in your terminal:

```
docker pull thomasfaria/quarterly_gfs_revisions:latest
```

Alternatively, you can use the [Onyxia instance SSPCloud](https://github.com/InseeFrLab/onyxia-web), a datalab developed by the French National Institute of Statistics and Economic Studies ([INSEE](https://www.insee.fr/fr/accueil)) that provides an easy-to-use interface for running the Docker image.

To get started with SSPCloud:

- Step 0: Go to [https://datalab.sspcloud.fr/home](https://datalab.sspcloud.fr/home). Click on **Sign In** and then **create an account** with your academic or institutional email address.
- Step 1: Click [here](https://datalab.sspcloud.fr/launcher/ide/rstudio?autoLaunch=true&service.image.pullPolicy=«Always»&service.image.custom.enabled=true&service.image.custom.version=«thomasfaria%2Fquarterly_gfs_revisions%3Alatest»&security.allowlist.enabled=false&onyxia.friendlyName=«QGFS») or on the orange badge on top of the page.
- Step 2: **Open** the service and follow the instructions concerning *username* and *credentials*.
- Step 3: **Open a new project** by opening the following file : `~/Quarterly_GFS_revisions/Quarterly_GFS_revisions.Rproj`.
- Step 4: Ensure all necessary packages are installed by executing the ```renv::restore()``` command in the console. You should get the following message: `The library is already synchronized with the lockfile.`

## Database

One of the main contributions of the paper is the construction of a **real-time fiscal quarterly dataset** for the euro area. The dataset contains published releases from 2006Q3 to 2022Q3[^1] and is stored in both Parquet and CSV formats. This dataset contains both fiscal and macroeconomic variables used for this project and is available via the following URLs:

- parquet format: https://minio.lab.sspcloud.fr/tfaria/public/real-time-fiscal-database.parquet
- CSV format: https://minio.lab.sspcloud.fr/tfaria/public/real-time-fiscal-database.csv

| Name      | Retrieval code |
| ----------- | ----------- |
|**Total revenue** |GFS.Q.N.*.W0.S13.S1.P.C.OTR._Z._Z._Z.XDC._Z.S.V.N._T|
|Current taxes on income, wealth, etc. (**Direct taxes**)| GFS.Q.N.*.W0.S13.S1.N.C.D5._Z._Z._Z.XDC._Z.S.V.N._T|
|Taxes on production and imports (**Indirect taxes**) |GFS.Q.N.*.W0.S13.S1.N.C.D2._Z._Z._Z.XDC._Z.S.V.N._T|
|Net social contributions (**Social contributions**) |GFS.Q.N.*.W0.S13.S1.N.C.D61._Z._Z._Z.XDC._Z.S.V.N._T|
|**Other current revenue** = Market output, output for own final use and payments for other non-market output + Other subsidies on production + Property income + Other current transfers |GFS.Q.N.*.W0.S13.S1.N.C.P1O._Z._Z._Z.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.N.C.D39._Z._Z._Z.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.P.C.D4._Z._Z._Z.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.P.C.D7._Z._Z._Z.XDC._Z.S.V.N._T|
|Capital transfers (**Capital revenue**) |GFS.Q.N.*.W0.S13.S1.P.C.D9._Z._Z._Z.XDC._Z.S.V.N._T|
|**Total expenditure**| GFS.Q.N.*.W0.S13.S1.P.D OTE._Z._Z._T.XDC._Z.S.V.N._T|
|Social benefits other than social transfers in kind (**Social transfers**)| GFS.Q.N.*.W0.S13.S1.N.D.D62._Z._Z._T.XDC._Z.S.V.N._T|
|**Purchases** = Social transfers in kind - purchased market production + Intermediate consumption| GFS.Q.N.*.W0.S13.S1.N.D.D632._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.N.D.P2._Z._Z._T.XDC._Z.S.V.N._T|
Interest (**Interest payments**) | GFS.Q.N.*.W0.S13.S1.C.D.D41._Z._Z._T.XDC._Z.S.V.N._T|
|Compensation of employees (**Gov. compensation**)| GFS.Q.N.*.W0.S13.S1.N.D.D1._Z._Z._T.XDC._Z.S.V.N._T|
|**Other current expenditure** = Property income other than interest + Other taxes on production + Current taxes on income, wealth, etc. + Other current transfers + Adjustment for the change in pension entitlements| GFS.Q.N.*.W0.S13.S1.N.D.D4N._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.N.D.D29._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.N.D.D5._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.P.D.D7._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.N.D.D8._Z._Z._T.XDC._Z.S.V.N._T|
|Gross fixed capital formation (**Gov. investment**)| GFS.Q.N.*.W0.S13.S1.N.D.P51G._Z._Z._T.XDC._Z.S.V.N._T|
|**Other capital expenditure** = Changes in inventories and acquisition less disposals of valuables + Acquisitions less disposals of non-produced assets + Capital transfers|GFS.Q.N.*.W0.S13.S1.N.D.P5M._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.N.D.NP._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.*.W0.S13.S1.C.D.D9._Z._Z._T.XDC._Z.S.V.N._T|
|**GDP**| MNA.Q.N.*.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N|
|**Private consumption**| MNA.Q.N.*.W0.S1M.S1.D.P31._Z._Z._T.EUR.V.N|
|**Total investment**| MNA.Q.N.*.W0.S1.S1.D.P5.N1G._T._Z.EUR.V.N|
|**Exports** |MNA.Q.N.*.W1.S1.S1.D.P6._Z._Z._Z.EUR.V.N|
|**Gov. consumption**| MNA.Q.N.*.W0.S13.S1.D.P3._Z._Z._T.EUR.V.N|
|**Compensation of employees** |MNA.Q.N.*.W2.S1.S1.D.D1._Z._T._Z.EUR.V.N|

The asterisk refers to the ISO-2 country code position (i.e. "DE", "FR", "IT"...).

[^1]: The dataset will be automatically updated with future releases.

## Codes

The project is deeply relying on the [target package](https://books.ropensci.org/targets/), which is a tool for creating and running reproducible pipelines in R. target is particularly useful for managing large or complex data sets, as it allows you to define each task in a pipeline as a separate function, and then run the pipeline by calling the ```target()``` function. This ensures that tasks are run in the correct order, and can save time by only running tasks that are out of date or have not been run before.

To reproduce the results of the paper, simply run the ```run.R``` script at the root of the project. This will retrieve the real-time fiscal database and produce the necessary computations following the pipeline defined in the ```_targets.R``` file.

Once the pipeline has been run, you can access the target object using the ```tar_read()``` function. For example, you can view the real-time database by running ```View(tar_read(RTDB))```.

All functions used in the project are organized by theme in the ```R/``` folder :

```
Quarterly_GFS_Revisions
└─── R
     │ data_functions.R
     │ plot_functions.R
     │ preprocessing_functions.R
     │ regression_functions.R

```

## Licence

This work is licenced under the [European Union Public Licence 1.2](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12).
