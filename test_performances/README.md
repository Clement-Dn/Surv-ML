# test_performances

This folder contains preliminary works on R in order to
grasp the survex method. The heart of the folder are the two python notebooks
in which our function, SurvTreeSHAP(t), is compared to KernelSurvSHAP(t) in 
terms of algorithmic performances, and to Cox model in order to show its
functionalities in terms of explaining Random Forest predictions of survival
functions.

NB_donnees_simul.ipynb contains comparisons regarding complexity between 
SurvTreeSHAP(t) and KernelSurvSHAP(t) on simulated data according to the
proportionnal hazards hypothesis. It can be used as a user's guide for
SurvTreeSHAP(t) as well.

simul non proportionnal.ipynb compares SurvTreeSHAP(t) and Cox model on non proportionnal data, 
meaning we attribute time dependent contributions to our covariates (instead of having 
beta, we have beta(t))
