%% Tutorial Turbine Blade - (overview)
% In this tutorial, a turbine blade structure will be presented, where the
% uncertainties in the material properties, e.g. young's modulus, is taken into
% account.   
%
% Description of the deterministic FE model
% 
% The mesh of the FE model consists of 12,933 tetrahedral elements and 3,119
% nodes, leading to a total of 18,714 DOF's. The structure is fully fixed at the
% bottom and subjected to inertial load in +y direction, corresponding to the
% centrifugal force due to the rotation of the turbine disc at a velocity of
% 12,200 rotations per minute. The analyzed blade has a height of 45 mm and the
% assumed material properties of the nominal model correspond to those of a
% nickel-based alloy, with a density of 8.9 g/cm3, a Young's modulus of 200
% kN/mm2 and a Poisson ratio of 0.31.       
%
%
% See also: http://cossan.cfd.liv.ac.uk/wiki/index.php/Turbine_Blade
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Murat~Panayirci$ 

%% Create the model
% The Turbaine blade can be analysed using 2 different solvers: Abaqus and
% Nastran
%
% Please run the appropriate Tutorial
%
% echodemo TutorialTurbineBladeAbaqus
%
% echodemo TutorialTurbineBladeNastran

%% Perform Analysis
% Uncertainty quantification can be performend using one of the following
% methods from the Stochasitic Finite Element Method (SFEM) toolbox
%
% Using Collocation Method
% echodemo TutorialTurbineBladePerformCollocationAnalysis
%
% Using Polynomial Chaos
% echodeno TutorialTurbineBladePerformPCAnalysis
%
% Using Perturbation Analysis
% echodemo TutorialTurbineBladePerformPerturbationAnalysis
