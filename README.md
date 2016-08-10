# Rgeoprofile-1.1.0
Implementation of the Dirichlet Process Mixture (DPM) model of geographic profiling described in Verity et al. (2014) and extended in Faulkner et al (in review). 

Geographic profiling (GP) was originally developed as an analytical tool in criminology, where it uses the spatial locations of linked crimes (for example murder, rape or arson) to create a surface of search priority that is overlaid on a map of the study area to produce a geoprofile, which in turn allows the police to prioritise investigations by systematically checking suspects associated with locations in descending order of the height on the geoprofile (Rossmo 2000). The technique has been extremely successful in this field, and is now widely used by police forces and investigative agencies around the world. 

In criminology, the most commonly used model of GP is the Criminal Geographic Targeting (CGT) algorithm described in Rossmo (2000). More recently, the same method has been applied to biological data, notably in spatial epidemiology, where it uses the locations of disease cases to identify infection sources: the identification of these sources is critical to control efforts of diseases such as malaria, since targeted intervention is more efficient and cost effective than untargeted intervention. In such cases – where there may be multiple sources, and where the number of sources may be unknown – the Dirichlet Process Mixture (DPM) model of geographic profiling introduced by Verity et al. (2014) may be more appropriate.

The package here extends this model to estimate source locations of invasions directly from spatial point pattern data without the need to specify dispersal parameters, as in Verity et al. (2014). This model is currently in review in a paper in Diversity and Distributions.

Selected papers from my research group

Faulkner SC, Stevenson MD, Verity R et al. (2015) . Using geographic profiling to locate elusive nocturnal animals: a case study with spectral tarsiers.Journal of Zoology vol. 295, (4) 261-268.
10.1111/jzo.12203
http://qmro.qmul.ac.uk/xmlui/handle/123456789/12376

Hauge MV, Stevenson MD, Rossmo DK et al. (2016) . Tagging Banksy: using geographic profiling to investigate a modern art mystery.Journal of Spatial Science vol. 61, (1) 185-190.
10.1080/14498596.2016.1138246
http://qmro.qmul.ac.uk/xmlui/handle/123456789/12473

Le Comber SC, Nicholls B, Rossmo DK et al. (2006) . Geographic profiling and animal foraging.J Theor Biol vol. 240, (2) 233-240.
10.1016/j.jtbi.2005.09.012

Le Comber SC, Stevenson MD (2012) . From Jack the Ripper to epidemiology and ecology.Trends Ecol Evol vol. 27, (6) 307-308.
10.1016/j.tree.2012.03.004

Raine NE, Rossmo DK, Le Comber SC (2009) . Geographic profiling applied to testing models of bumble-bee foraging.J R Soc Interface vol. 6, (32) 307-319.
10.1098/rsif.2008.0242

Rossmo DK, Lutermann H, Stevenson MD et al. (2014) . Geographic profiling in Nazi Berlin: fact and fiction.Geospatial Intelligence Review (Fall 2014)
http://qmro.qmul.ac.uk/xmlui/handle/123456789/6578

Smith CM, Downs SH, Mitchell A et al. (2015) . Spatial Targeting for Bovine Tuberculosis Control: Can the Locations of Infected Cattle Be Used to Find Infected Badgers?.PLOS ONE vol. 10, (11) e0142710-e0142710.
10.1371/journal.pone.0142710
http://qmro.qmul.ac.uk/xmlui/handle/123456789/12164

Smith CM, Le Comber SC, Fry H et al. (2015) . Spatial methods for infectious disease outbreak investigations: systematic literature review.Eurosurveillance vol. 20, (39)
10.2807/1560-7917.ES.2015.20.39.30026
http://qmro.qmul.ac.uk/xmlui/handle/123456789/9471

Stevenson M, Le Comber S (2014) . Biological geographical profiling.The Encyclopedia of Criminology and Criminal Justice, Editors: Bruinsma, G, Weisburd, D, Springer

Stevenson MD, Rossmo DK, Knell RJ et al. (2012) . Geographic profiling as a novel spatial tool for targeting the control of invasive species.Ecography vol. 35, (8) 704-715.
10.1111/j.1600-0587.2011.07292.x

Verity R, Stevenson MD, Rossmo DK et al. (2014) . Spatial targeting of infectious disease control: Identifying multiple, unknown sources.Methods in Ecology and Evolution vol. 5, (7) 647-655.
10.1111/2041-210X.12190
http://qmro.qmul.ac.uk/xmlui/handle/123456789/7569
