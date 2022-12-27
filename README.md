# Quarterly GFS revisions

[![Licence](https://img.shields.io/badge/Licence-EUPL--1.2-001489)](https://joinup.ec.europa.eu/collection/eupl/eupl-text-eupl-12)
[![Onyxia](https://img.shields.io/badge/Launch-Datalab-orange?logo=R)](https://datalab.sspcloud.fr/launcher/ide/rstudio?autoLaunch=true&service.image.pullPolicy=«Always»&service.image.custom.enabled=true&service.image.custom.version=«thomasfaria%2Fquarterly_gfs_revisions%3Alatest»&security.allowlist.enabled=false&onyxia.friendlyName=«QGFS»)
[![Publish](https://img.shields.io/github/actions/workflow/status/ThomasFaria/Quarterly_GFS_revisions/publish.yaml?label=Publish&style=flat)](https://thomasfaria.github.io/Quarterly_GFS_revisions/)
[![Build](https://img.shields.io/github/actions/workflow/status/ThomasFaria/Quarterly_GFS_revisions/build-image.yaml?label=Build
)](https://hub.docker.com/repository/docker/thomasfaria/quarterly_gfs_revisions)

Repository replicating the results presented in our paper [Krzysztof Bańkowski](https://www.ecb.europa.eu/pub/research/authors/profiles/krzysztof-bankowski.fr.html), [Thomas Faria](https://www.ecb.europa.eu/pub/research/authors/profiles/thomas-faria.fr.html), [Robert Schall](https://www.ecb.europa.eu/pub/research/authors/profiles/robert-schall.fr.html) (2022) ["How well-behaved are revisions to quarterly fiscal data in the euro area?"](https://www.ecb.europa.eu/pub/pdf/scpwps/ecb.wp2676~65f27f7ac1.fr.pdf?7f2b353ebef0e876f2feb488dbea1079).

## Getting Started

The repository is accompanied by its [docker image](https://hub.docker.com/r/thomasfaria/quarterly_gfs_revisions) to ensure full reproducibility of the results.
If you are familiar with docker you can directly pull the image using the following command in your terminal:

```
docker pull thomasfaria/quarterly_gfs_revisions
```

However, we recommend using [Onyxia](https://github.com/InseeFrLab/onyxia-web), a datalab developed by the French National institute of statistics and economic studies ([INSEE](https://www.insee.fr/fr/accueil)).

- Step 0: Go to [https://datalab.sspcloud.fr/home](https://datalab.sspcloud.fr/home). Click on **Sign In** and then **create an account** with your academic or institutional email address.
- Step 1: Click [here](https://datalab.sspcloud.fr/launcher/ide/rstudio?autoLaunch=true&service.image.pullPolicy=«Always»&service.image.custom.enabled=true&service.image.custom.version=«thomasfaria%2Fquarterly_gfs_revisions%3Alatest»&security.allowlist.enabled=false&onyxia.friendlyName=«QGFS») or on the orange badge on top of the page.
- Step 2: **Open** the service and follow the instruction concerning *username* and *credentials*.
- Step 3: **Open a new project** and **clone** this repository.

All necessary packages have been already installed in the docker image and dependencies are managed by the [renv](https://rstudio.github.io/renv/index.html) package. You only need to call ```renv::restore()``` to reinstall all the packages, as declared in the *lockfile*, in your service.

## Database

One major contribution of our paper is the construction of a **real-time fiscal dataset** for the euro area. The dataset is stored on MinIO, and is made available via this [url](https://minio.lab.sspcloud.fr/tfaria/public/RealTimeDatabase.csv).

The dataset contains published releases from 2006Q3 to 2022Q3[^1] for several fiscal and macroeconomic series as specified in the following table:

| Name      | Retrieval code |
| ----------- | ----------- |
|**Total revenue** |GFS.Q.N.cc.W0.S13.S1.P.C.OTR._Z._Z._Z.XDC._Z.S.V.N._T|
|Current taxes on income, wealth, etc. (**Direct taxes**)| GFS.Q.N.cc.W0.S13.S1.N.C.D5._Z._Z._Z.XDC._Z.S.V.N._T|
|Taxes on production and imports (**Indirect taxes**) |GFS.Q.N.cc.W0.S13.S1.N.C.D2._Z._Z._Z.XDC._Z.S.V.N._T|
|Net social contributions (**Social contributions**) |GFS.Q.N.cc.W0.S13.S1.N.C.D61._Z._Z._Z.XDC._Z.S.V.N._T|
|**Other current revenue** = Market output, output for own final use and payments for other non-market output + Other subsidies on production + Property income + Other current transfers |GFS.Q.N.cc.W0.S13.S1.N.C.P1O._Z._Z._Z.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.N.C.D39._Z._Z._Z.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.P.C.D4._Z._Z._Z.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.P.C.D7._Z._Z._Z.XDC._Z.S.V.N._T|
|Capital transfers (**Capital revenue**) |GFS.Q.N.cc.W0.S13.S1.P.C.D9._Z._Z._Z.XDC._Z.S.V.N._T|
|**Total expenditure**| GFS.Q.N.cc.W0.S13.S1.P.D OTE._Z._Z._T.XDC._Z.S.V.N._T|
|Social benefits other than social transfers in kind (**Social transfers**)| GFS.Q.N.cc.W0.S13.S1.N.D.D62._Z._Z._T.XDC._Z.S.V.N._T|
|**Purchases** = Social transfers in kind - purchased market production + Intermediate consumption| GFS.Q.N.cc.W0.S13.S1.N.D.D632._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.N.D.P2._Z._Z._T.XDC._Z.S.V.N._T|
Interest (**Interest payments**) | GFS.Q.N.cc.W0.S13.S1.C.D.D41._Z._Z._T.XDC._Z.S.V.N._T|
|Compensation of employees (**Gov. compensation**)| GFS.Q.N.cc.W0.S13.S1.N.D.D1._Z._Z._T.XDC._Z.S.V.N._T|
|**Other current expenditure** = Property income other than interest + Other taxes on production + Current taxes on income, wealth, etc. + Other current transfers + Adjustment for the change in pension entitlements| GFS.Q.N.cc.W0.S13.S1.N.D.D4N._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.N.D.D29._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.N.D.D5._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.P.D.D7._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.N.D.D8._Z._Z._T.XDC._Z.S.V.N._T|
|Gross fixed capital formation (**Gov. investment**)| GFS.Q.N.cc.W0.S13.S1.N.D.P51G._Z._Z._T.XDC._Z.S.V.N._T|
|**Other capital expenditure** = Changes in inventories and acquisition less disposals of valuables + Acquisitions less disposals of non-produced assets + Capital transfers|GFS.Q.N.cc.W0.S13.S1.N.D.P5M._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.N.D.NP._Z._Z._T.XDC._Z.S.V.N._T + GFS.Q.N.cc.W0.S13.S1.C.D.D9._Z._Z._T.XDC._Z.S.V.N._T|
|**GDP**| MNA.Q.N.cc.W2.S1.S1.B.B1GQ._Z._Z._Z.EUR.V.N|
|**Private consumption**| MNA.Q.N.cc.W0.S1M.S1.D.P31._Z._Z._T.EUR.V.N|
|**Total investment**| MNA.Q.N.cc.W0.S1.S1.D.P5.N1G._T._Z.EUR.V.N|
|**Exports** |MNA.Q.N.cc.W1.S1.S1.D.P6._Z._Z._Z.EUR.V.N|
|**Gov. consumption**| MNA.Q.N.cc.W0.S13.S1.D.P3._Z._Z._T.EUR.V.N|
|**Wages and salaries** |MNA.Q.N.cc.W2.S1.S1.D.D1._Z._T._Z.EUR.V.N|

[^1]: The dataset will be automatically updated with future releases.

<!-- TODO: describe the variables in the database -->

## Codes

The project is based on the [target package](https://books.ropensci.org/targets/), which is a tool for creating and running reproducible pipelines in R. target is particularly useful for managing large or complex data sets, as it allows you to define each task in a pipeline as a separate function, and then run the pipeline by calling the ```target()``` function. This ensures that tasks are run in the correct order, and can save time by only running tasks that are out of date or have not been run before.

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
