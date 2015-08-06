%% Unit Test for the SensitivityMeasures object
% EP

function varargout=UnitTestSensitivityMeasures

% Test constructor
Ntest=38;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

% No mandatory objects are required by the SensitivityMeasures object

%% 1
% Constructor and display
% Empty object
itest=1;
try
    Xsm  = SensitivityMeasures;
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Empty Object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2
itest=itest+1;
try
    Xsm  = SensitivityMeasures('Sdescription','My SensitivityMeasure object');
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with only description';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],'CinputNames',{'Input1' 'Input2' 'Input3'});
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with Total indices';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4
% the constructor should check the number of values (i.e. inputs of the
% performance function = length(VDesignPointPhysical)
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],'CinputNames',{'Input1' 'Input3'});
    display(Xsm)
catch ME
    % This call should fail due too wrong length in the CinputNames
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3]);
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with Total indices and CoV';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'SevaluatedObjectName','My evaluated object');
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with Object evaluated name';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty Evaluator
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev);
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with evaluated object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
% Collect Sobol' indices
% The Matrix of Sobol indices can be passed as a full or sparse matrix
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'CsobolComponentsIndices',[11 0 0;0 22 0;0 0 33]);
    display(Xsm)
catch ME
    % This should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolFirstOrder',[11; 22; 33]);
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with Sobol indices (First order)';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7;],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; ], ...
        'XevaluatedObject',Xev,...
        'VsobolFirstOrder',[11; 22;]);
    display(Xsm)
catch ME
    % This should fail
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]});
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with Sobol indices';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'VsobolIndicesCoV',[0.1 0.2 0.12 0.13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]});
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create Object with Sobol indices and CoV';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 13
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'VsobolIndicesCoV',[0.2 0.12 0.13],...
        'VsobolFirstOrder',[10 02 11],...
        'VsobolFirstOrderCoV',[0.2 0.12 0.13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]}, ...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
    Vtest(itest)=true;
    Cmess{itest}='Create full SensitivityMeasures object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 14
% Test methods
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]}, ...
        'SestimationMethod','Testing','SoutputName','Test');
    Xsm  = Xsm.addSobolIndices('CsobolComponentsIndices',{[1 2] [2 3]},'VsobolIndices',[12; 23]);
    Vtest(itest)=true;
    display(Xsm)
    Cmess{itest}='Test addSobolIndices';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 15
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'VsobolIndicesCoV',[0.2 0.12 0.13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]}, ...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
    Xsm  = Xsm.addSobolIndices('CsobolComponentsIndices',{[1 2] [2 3]},'VsobolIndices',[12; 23]);
catch ME
    % This should fail because assSobolIndices requires VsobolIndicesCoV
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 16
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23  12 13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3] [2 4] [3 5]}, ...
        'SestimationMethod','Testing','SoutputName','Test');
catch ME
    % This should fail because Xsm has 3 inputs and Csobol components indices
    % contains 5.
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 17
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndicesCoV',[0.10; 0.20; 0.3], ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]}, ...
        'SestimationMethod','Testing','SoutputName','Test');
    Xsm  = Xsm.addSobolIndices('CsobolComponentsIndices',{[1 2] [2 3]},'VsobolIndices',[12; 23; 11]);
    display(Xsm)
catch ME
    % this should fail: not consistent number of Sobol indices
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 18
itest=itest+1;
% test depent property
try
    Xsm  = SensitivityMeasures(...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VsobolIndices',[23 12 13],...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]});
    CsobolComponentsNames=Xsm.CsobolComponentsNames;
    if strcmp(CsobolComponentsNames{3},'Input1; Input3')
        Vtest(itest)=true;
        Cmess{itest}='Depent field created correctly';
    else
        Cmess{itest}=['Depent field ' CsobolComponentsNames{3} ' NOT created correctly'];
    end
    display(Xsm)
    Vtest(itest)=true;
catch ME
    % this should fail
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% Test plot function
Xsm  = SensitivityMeasures(...
    'CinputNames',{'X1' 'X2' 'X3'},...
    'VtotalIndices',[0.5506015 0.4697538 0.2391275],...
    'Vsobolfirstorder',[3.076874e-01 4.419659e-01 3.442228e-29], ...
    'VupperBounds',[0.6 0.5 0.3]);

%% 19
itest=itest+1;
try
    hf=Xsm.plot;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 20
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 21
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',true,'LtotalIndices',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 22
itest=itest+1;
try
    hf=Xsm.plot('LsobolIndices',true);
    % this return a warning message
    Vtest(itest)=true;
    Cmess{itest}=lastwarn;
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 23
itest=itest+1;
try
    hf=Xsm.plot('LupperBounds',false);
    % this return a warning message
    Vtest(itest)=true;
    Cmess{itest}=lastwarn;
catch ME
    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 24
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',true,'LtotalIndices',false,'LupperBounds',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% Test CoV plot
Xsm  = SensitivityMeasures(...
    'CinputNames',{'X1' 'X2' 'X3'},...
    'VtotalIndices',[0.5506015 0.4697538 0.2391275],...
    'VupperBounds',[0.6 0.5 0.3],...
    'VsobolFirstOrder',[3.076874e-01 4.419659e-01 3.442228e-29],...
    'VtotalIndicesCoV',[0.05 0.1 0.2],...
    'VupperBoundsCoV',[0.05 0.1 0.2],...
    'VsobolFirstOrderCoV',[0.05 0.1 0.2]);

%% 25
itest=itest+1;
try
    hf=Xsm.plot;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 26
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 27
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',true,'LtotalIndices',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 28
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',false,'LupperBounds',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 29
itest=itest+1;
try
    hf=Xsm.plot('LfirstOrder',true,'LtotalIndices',false,'LupperBounds',false);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% Test high order sobol indices
Xsm  = SensitivityMeasures(...
    'CinputNames',{'X1' 'X2' 'X3'},...
    'VsobolIndices',[0.5506015 0.4697538 0.2391275 0.3506015],...
    'VsobolIndicesCoV',[0.05 0.1 0.2 0.01],...,...
    'CsobolComponentsIndices',{[1 2] [2 3] [1 3] [1 2 3]});

%% 30
itest=itest+1;
try
    hf=Xsm.plot('LupperBounds',false,'LsobolIndices',true);
    Vtest(itest)=true;
    Cmess{itest}='Figure plot';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 31
itest=itest+1;
try
    hf=Xsm.plot('LtotalIndices',false);
    Cmess{itest}=lastwarn;
    Vtest(itest)=true;
catch ME
    % this should fail
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 32
itest=itest+1;
try
    hf=Xsm.plot('LsobolfirstIndices',true);
    % this should return an warning message
    Cmess{itest}=lastwarn;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Test confidence intervals
%% 33
itest=itest+1;
try
    Xev=Evaluator; % Create an Empty model
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndices',[23 12 13],...
        'MtotalIndicesCI',[0.10 0.11; 0.20 0.21; 0.3 0.31]', ...
        'XevaluatedObject',Xev,...
        'VsobolIndices',[23 12 13],...
        'MsobolIndicesCI',[23 24; 12 13; 13 14]',...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 3]}, ...
        'VupperBounds',[24 14 15],...
        'MupperBoundsCI',[23 24; 12 13; 13 14]',...
        'Valpha',[0.4 0.96],...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
    Vtest(itest)=true;
catch ME
    % this should fail: not consistent number of Sobol indices
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 34
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VupperBounds',[24 14 15],...
        'MtotalIndicesCI',[23 24 15; 12 13 24; 15 12 16 ],...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
catch ME
    % this should fail: confidence intervals defined for not existing measueres
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 35
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VtotalIndices',[24 14 15],...
        'MtotalIndicesCI',[23 15 45; 12 13 15; 13 14 15],...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
catch ME
    % this should fail: confindence interval must have 2 rows
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 36
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VupperBounds',[24 14 15],...
        'MupperBoundsCI',[23 15 45; 12 13 15; 13 14 15],...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
catch ME
    % this should fail: confindence interval must have 2 rows
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 37
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'VsobolFirstIndices',[24 14 15],...
        'MsobolFirstIndicesCI',[23 15 45; 12 13 15; 13 14 15],...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
catch ME
    % this should fail: confindence interval must have 2 row
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 38
itest=itest+1;
try
    Xsm  = SensitivityMeasures('VtotalIndices',[5; 7; 10],...
        'CinputNames',{'Input1' 'Input2' 'Input3'},...
        'CsobolComponentsIndices',{[2 3] [1 2] [1 2 3]},...
        'VsobolIndices',[24 14 15],...
        'MsobolIndicesCI',[23 15 45; 12 13 15; 13 14 15],...
        'SestimationMethod','Testing','SoutputName','Test');
    display(Xsm)
catch ME
    % this should fail: confindence interval must have 2 row
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 39
itest=itest+1;
try
    % Test CoV and CI plot
    Xsm  = SensitivityMeasures(...
        'CinputNames',{'X1' 'X2' 'X3'},...
        'VtotalIndices',[0.5506015 0.4697538 0.2391275],...
        'MtotalIndicesCI',[0.54 0.56; 0.46 0.47; 0.22 0.25]', ...
        'VupperBounds',[0.6 0.5 0.3],...
        'MupperBoundsCI',[0.54 0.7; 0.46 0.52; 0.22 0.35]', ...
        'VsobolFirstOrder',[3.076874e-01 4.419659e-01 3.442228e-29],...
        'MsobolFirstOrderCI',[0.01 0.1; 0.01 0.1; 0.01 0.1]', ...
        'VtotalIndicesCoV',[0.05 0.1 0.2],...
        'VupperBoundsCoV',[0.05 0.1 0.2],...
        'VsobolFirstOrderCoV',[0.05 0.1 0.2]);
    
    hf=Xsm.plot('LtotalIndices',false);
    Cmess{itest}=lastwarn;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


close all

if nargout>0
    % Export name of the UnitTest
    varargout{1}='SensitivityMeasures';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of SensitivityMeasures (' datestr(now) ')'])
    disp([' Successful: ' num2str(100*(sum(Vtest)/length(Vtest))) '%'])
    disp('--------------------------------------------------------------------')
    for i=1:length(Vtest)
        if Vtest(i)
            disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
        else
            disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
        end
    end
end

