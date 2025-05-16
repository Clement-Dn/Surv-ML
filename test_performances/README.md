# test_performances
Ce dossier contient des travaux préliminaires pour 
s'approprier les méthodes survex sur R, ainsi que deux
notebooks dans lesquels SurvTreeSHAP(t) est comparé à 
KernelTreeSHAP(t) du point de vue des performances algorithmiques, 
et au modèle de Cox pour montrer ses avantages en terme 
d'explicabilité.

NB_donnees_simul contient les comparaisons en terme de complexité avec 
KernelSurvSHAP(t) sur des données simulées selon l'hypothèse des risques 
proportionnels. Il peut par ailleurs servir de manuel d'utilisation de
SurvtreeSHAP(t).

non_proportionnal compare SurvTreeSHAP(t) et le modèle de Cox sur des données
non proportionnelles, c'est à dire qu'on attribue aux covariables explicatives 
une contribution à la survie qui cette fois dépend du temps (au lieu d'avoir beta
comme dans NB_donnes_simul on a beta(t)). 
