%
%   Car body in white - training of mode-based meta-model
%
%   In this example, a mode-based meta-model of the car-body in white is 
%   performed.
% 
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2009 IfM
% =========================================================================

Lcal_val_samples = 1;
Lcal_val_metamodel = 1;

%% 1.   Data of thicknesses
%in this part, the values of the thickness of the different parts of the
%car body are defined. The different parts correspond to different property
%cards defined in a Nastran input file; these property cards refer to shell
%elements
%1.1.   Id number of the property cards
Vid_shells  = [11,17,19,53,71,79,82,84,86,88,91,98,100,101,102,105,107,109,110];
%1.2.   Values of thicknesses
Vthick  = [1,2,0.9,1.5,2,1.8,1.4,0.8,1.2,2.5,1.5,1.2,1.2,0.9,1,1.8,1.2,5,1.4];
%this vector contains the values of the thicknesses for each of the
%property cards

%% 3.   Create random variables to apply gradient estimation
Nrv         = length(Vid_shells);   %total number of random variables
for i=1:Nrv,    %loop for defining random variables
    Srvname         = ['xrv_pshell_' num2str(Vid_shells(i))];   %string containing name of random variable
    eval( [ Srvname ...
        ' = RandomVariable(''Sdistribution'',''normal'',''mean'',' ...
        num2str(Vthick(i)) ',''std'',' num2str(Vthick(i)*0.05) ');']);   %command to define random variable in workspace
end

%% 4.   Create random variable set
Xrvs    = RandomVariableSet('Cmembers',{'*all*'});  %add all random 
%variables present in the workspace to the set of random variables

%% 5.   Create input object
Xin     = Input;            %create empty output object
Xin     = add(Xin,Xrvs);    %add random variable set

%% 6.   Define Injector
Xi  = Injector('Sscanfilename','car_mcs.cossan',...  %name of the file with identifiers
    'Sfile','car_mcs.dat');               %name of the input file, i.e. once the values have been injected

%% define Extractor

Xe1  = Op4Extractor('Sfile','CAR_LAMDA_0.OP4','Soutputname','Vlambda');
Xe2  = Op4Extractor('Sfile','CAR_PHI_0.OP4','Soutputname','MPhi');

%% 7.   Construct the connector
%7.1.   create the connector
Xc  = Connector('Stype','nastran_x86_64',...    %use Nastran 64
    'Lkeepsimfiles',false,...     %do not keept files after completing simulation
    'Smaininputfile','car_mcs.dat',...  %name of input file
    'Sworkingdirectory','./');    %working directory
%7.2.   add injector to empty connector
Xc  = add(Xc,Xi);
Xc  = add(Xc,Xe1);
Xc  = add(Xc,Xe2);

%% 8.   JobManager object

Scwd        = pwd;
%9.1.   Grid pre-execution commands
Spreexecmd  = ['cp ' Scwd '/843_63100_a.dat ./;'];  %copies input file required for FE analysis
%9.2.   Grid post-execution command
%Spostexecmd =['name_file=${PWD##*/} ; cp run_car2_a.f06 $name_file.f06 ; cp $name_file.f06 ' Scwd '/ ;'];
%9.3.   Define Jobmanager object
%Xg          = JobManager('Stype','GridEngine',...
%    'Squeue','pizzas64.q',...
%    'Spreexecmd',Spreexecmd);%,...

%% 9.   Create Evaluator and Model object
%9.1    Evaluator
Xev         = Evaluator('Xconnector',Xc);
%9.2.   Construct the Model
Xmdl        = Model('Cmembers',{'Xin','Xev'});

if Lcal_val_samples
%% 10.  Run deterministic model

Xout_nominal    = apply(Xmdl,Xin);
Xmodes0.Vlambda = Xout_nominal.Tvalues.Vlambda(:,1);
Xmodes0.MPhi    = Xout_nominal.Tvalues.MPhi;

Xin0 = Xin;
save Xmodes0 Xin0 Xmodes0


%% 11.  Run calibration simulations

Xin_calibration     = sample(Xin,'Nsamples',44);
Xout_calibration    = apply(Xmdl,Xin_calibration);

Xmodes_calibration = Modes;
for isim = 1:Xout_calibration.Nsamples
    Xmodes_calibration(isim).Vlambda = Xout_calibration.Tvalues(isim).Vlambda(:,1);
    Xmodes_calibration(isim).MPhi = Xout_calibration.Tvalues(isim).MPhi;
end

save Xmodes_calibration Xin_calibration Xmodes_calibration

%% 12.  Run validation simulations

Xin_validation     = sample(Xin,'Nsamples',6);
Xout_validation    = apply(Xmdl,Xin_validation);

Xmodes_validation = Modes;
for isim = 1:Xout_validation.Nsamples
    Xmodes_validation(isim).Vlambda = Xout_validation.Tvalues(isim).Vlambda(:,1);
    Xmodes_validation(isim).MPhi = Xout_validation.Tvalues(isim).MPhi;
end

save Xmodes_validation Xin_validation Xmodes_validation
end

if Lcal_val_metamodel
%% Calibration of meta model

%nominal mass matrix
load mass_matrix_nominal
%nominal values
load Xmodes0 
% samples for the calibration (44 set in total)
load Xmodes_calibration
% samples for the validation (6 set in total)
load Xmodes_validation

%NOTE: calibration & validation samples contain 12-13 (changes w.r.t sample)

% 11.  Construct & Calibrate the mode based meta model

% Modes to be approximated
Vmodes = 1:8;

% define metamodel with passing Input and Output of calibration points
Xmm = ModeBased('sdescription','metamodel',...
                'XFullmodel',Xmdl,...
                'Cnamesmodalprop',{'Vlambda','MPhi'},...
                'XtrainingInput',Xin_calibration,...
                'XtrainingOutput',Xmodes_calibration,...
                'Vmodes',Vmodes,...
                'Vmkmodes',3*ones(length(Vmodes),1),...
                'Mmass0',mass_mat,...
                'Xmodes0',Xmodes0);
            
%%  validate with previously calculated validation samples

[Xmm, Xoutput_metamodel_val] = validate(Xmm,'XvalidationInput',Xin_validation,'XvalidationOutput',Xmodes_validation);   

plot_comparison(Xmm.XvalidationOutput,Xoutput_metamodel_val);

Xoutput_metamodel_cal = apply(Xmm,Xin_calibration);
plot_comparison(Xmm.XtrainingOutput,Xoutput_metamodel_cal);
end
