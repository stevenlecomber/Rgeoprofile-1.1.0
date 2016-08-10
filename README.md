# Rgeoprofile-1.1.0
Implementation of the Dirichlet Process Mixture (DPM) model of geographic profiling described in Verity et al. (2014) and extended in Faulkner et al (in review). 

Geographic profiling (GP) was originally developed as an analytical tool in criminology, where it uses the spatial locations of linked crimes (for example murder, rape or arson) to create a surface of search priority that is overlaid on a map of the study area to produce a geoprofile, which in turn allows the police to prioritise investigations by systematically checking suspects associated with locations in descending order of the height on the geoprofile (Rossmo 2000). The technique has been extremely successful in this field, and is now widely used by police forces and investigative agencies around the world. 

In criminology, the most commonly used model of GP is the Criminal Geographic Targeting (CGT) algorithm described in Rossmo (2000). More recently, the same method has been applied to biological data, notably in spatial epidemiology, where it uses the locations of disease cases to identify infection sources: the identification of these sources is critical to control efforts of diseases such as malaria, since targeted intervention is more efficient and cost effective than untargeted intervention. In such cases – where there may be multiple sources, and where the number of sources may be unknown – the Dirichlet Process Mixture (DPM) model of geographic profiling introduced by Verity et al. (2014) may be more appropriate.

The package here extends this model to estimate source locations of invasions directly from spatial point pattern data without the need to specify dispersal parameters, as in Verity et al. (2014). This model is currently in review in a paper in Diversity and Distributions.
