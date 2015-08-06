%% COSSAN-X Toolbox User Guide
%
%% What is "COSSAN-X"?
%
% COSSAN is a general purpose software package for stochastic and reliability
% analysis taking into account the uncertainties associated to geometric,
% material and loading parameters. COSSAN-X, which is the latest version of
% COSSAN, provides in this regard an efficient approach to integrate third-party
% (deterministic) software (e.g. Finite Element solvers), efficient numeric
% algorithms (e.g. Line Sampling) and High Performance Computing . Although
% nowadays FE codes are quite sophisticated in terms of geometrical and
% mechanical modelling capabilities, they usually neglect the important issue of
% inherent uncertainties. Stochastic methods, on the other hand, offer a much
% more realistic approach for the analysis and design of structural systems. For
% these reasons, it is crucial to link deterministic FE codes with the advanced
% tools of stochastic analysis.           
%
% The software offers the most advanced and recent algorithms for the rational
% quantification and propagation of uncertainties. Furthermore, since stochastic
% methods are in general computationally demanding, the resort of high
% performance computing is required in order to reduce the execution time (wall
% clock time) of the analysis. In this regard, the implementations available in
% COSSAN-X are coupled with the parallel computing features in order to be able
% to analyse realistic problems within feasible computational times. As a
% result, COSSAN-X has evolved as the all-in-one package software for the
% practitioners to perform Uncertainty Quantification (UQ), Simulation-based
% Reliability Analysis, Sensitivity Analysis, Meta-Modelling, Stochastic Finite
% Elements Analysis (SFEM), and Reliability-Based Optimization (RBO) .          
%
%% Overview of the features of COSSAN-X %%
% 
% All-in-one package for your real-life engineering problems covering a wide
% range from uncertainty management to reliability assessment, from optimizing
% designs to sensitivity analysis, model validation and much more.
% Easy modeling of uncertainties using 16 continous and 6 discrete distribution
% types, as well as user defined distributions, Gaussian Mixture Distribution
% and the possibility to define stochastic processes.  
% A wide list of the most robust and efficient algorithms implemented within
% each toolbox. 
% Interaction with Oracle Grid Engine and extendible to other job managers for
% distributing the workload on heterogeneous grids.
%
%% Interaction with 3rd party software
%
% Despite to the continous research and progress achieved in the computational
% stochastic mechanics field, surprisingly the developed methods and tools in
% this regard are not being recognized or appreciated enough by the industry and
% practitioners. This in fact has been one of the driving motivations behind the
% development of COSSAN-X, namely to strengthen the bridge between the industry
% and academic researchers, since nowadays the popularity of any method used on
% real applications is very much dependent on the availability of efficient
% software, which facilitate the use of these methods without an extensive
% training. In other words, it is crucial that the tools of stochastic analysis
% are offered to the industry within a easy-to-use general purpose software, so
% that these algorithms are actually used for practical applications and its
% remedies can be recognized by the authorities.           
%
% The foreseen methodology in order to achieve this goal is to provide the
% industry with the general purpose user-friendly software, which can interact
% with the 3rd party deterministic FE solvers. The advantages in this case are
% twofold: (i) to make ample use of the advanced solution and visualization
% capabilities of the commercial 3rd party FE software, (ii) to offer the
% industry the tools of the stochastic analysis in a platform, which they are
% already familiar with.      
%
% *Interaction methods*
%
% From the point of interaction with 3rd party software, the methods can be
% classified into two groups, namely intrusive and non-intrusive methods,
% respectively. On one hand, non-intrusive methods refer to those, where 3rd
% party programs can be used in a black-box fashion, i.e. no modification is
% required on the 3rd party program. The intrusive methods, on the other hand,
% require calculations with system matrices. Therefore, these matrices have to
% be accessed in the deterministic solver. This may require advanced knowledge
% or even modifications on the solver side. Consequently, the implementation of
% this class of methods is more involved.         
% 
% *Manipulation of Input/Output files*
%
% COSSAN-X allows to interact with the any type of solver. The Input and Output
% files are manipulated using the so called Injector and Extractor objects. 
%
%% See also
% Plese see the complete 
% <http://cossan.cfd.liv.ac.uk/wiki/index.php/Category:User_Manual user manual>.
%
% For more information about Cossan visit out 
% <http://cossan.cfd.liv.ac.uk Web Site>


% Copyright 2011 COSSAN-X, University of Innsbruck, Austria, EU.
