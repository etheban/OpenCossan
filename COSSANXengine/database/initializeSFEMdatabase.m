%% initializeSFEMDATABASE
% This script is use to verify that the database for SFEM is accessible from the
% matlab path

disp('------------------------------------------------------------------------------');
if isdeployed
    disp('Checking SFEM database files')
    % Checking PCterms
    if exist(fullfile('PCterms','vci2jk_coefficients','vci2ik_coeffs_1_1.mat'),'file')
        disp('Polynomial Chaos coefficients available')
    else
        disp('Polynomial Chaos coefficients NOT available')
    end
    
    if exist(fullfile('DMAP','dmapoutputdisplacements'),'file')
        disp('DMAP files present')
    else
        disp('DMAP files NOT available')
    end
    
    
    
else
    Sfullpath = mfilename('fullpath');
    Spathstr = fileparts(Sfullpath);
    disp('Adding SFEM database to Matlab search path.');
    addpath(fullfile(Spathstr,'DMAP')); %#ok<*MCAP>
    addpath(fullfile(Spathstr,'PCterms'));
end
disp('------------------------------------------------------------------------------');


