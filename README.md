# STADEM
**ST**ate space **A**dult **D**am **E**scapement **M**odel

The primary goal of this project is to estimate total adult escapement of spring/summer Chinook salmon and steelhead that cross Lower Granite dam (LGD). In addition, to meet desired management and research objectives, total escapement has to include estimates of uncertainty and be parsed into weekly strata by three origin groups; wild, hatchery and hatchery no-clip. To reach this goal, we have developed the **ST**ate space **A**dult **D**am **E**scapement **M**odel (STADEM) model that incorporates fish ladder window counts, data from sampled fish at the LGD adult trap, and observations of previously PIT tagged fish at LGD adult detection sites.

This package contains functions to query and summarise the necessary data to fit a STADEM model, as well as write the JAGS model and run it using the jagsUI package in R. Some of the functions can be applied to other dams, and we are actively working to be able to run STADEM at locations other than Lower Granite Dam.

STADEM is a collaborative project, with the primary contributors being:

* Kevin See (Quantitative Consultants Inc.)
* Ryan N. Kinzer (Nez Perce Tribe)
* Rick Orme (Nez Perce Tribe)
* Mike Ackerman (Quantitative Consultants Inc.)
