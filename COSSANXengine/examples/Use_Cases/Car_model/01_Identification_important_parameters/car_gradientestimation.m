%
%   Car body in white - gradient estimation
%
%   In this example, the sensitivity of the eigenfrequency of a mechanical
%   model is calculated using a simulation-based gradient algorithm.  
%
%   The physical model comprises a car body in white. The target
%   eigenfrequency corresponds to the first torsional mode of the car body.
%   The sensitivity of this quantity is calculated with respect to the
%   thicknesses of the different parts of the car body; the total number of
%   thicknesses considered is 106.
% 
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2009 IfM
% =========================================================================

%% 1.   Fix flow of random numbers
%first, the flow of random numbers is fixed. This allows reproducing the
%results obtained with the simulation-based gradient algorithm.
stream_rnd_n    = RandStream('mt19937ar','Seed',1);
RandStream.setDefaultStream(stream_rnd_n)

%% 2.   Data of thicknesses
%in this part, the values of the thickness of the different parts of the
%car body are defined. The different parts correspond to different property
%cards defined in a Nastran input file; these property cards refer to shell
%elements
%2.1.   Id number of the property cards
Vid_shells  = [11:75,78:111,115:121];   %this vector contains the Id 
%numbers of the different shells, as declared in the Nastran input file
%2.2.   Values of thicknesses
Vthick  = [1.0,1.5,1.6,1.5,1.4,1.4,2.0,2.0,0.9,3.5,3.5,2.4,0.8,2.0,...
    0.8,2.5,2.2,1.6,2.0,1.2,2.0,1.2,1.2,2.0,1.2,1.2,2.0,1.8,0.7,1.6,...
    1.5,0.9,0.7,0.7,0.7,1.5,1.4,1.0,0.8,0.8,0.8,2.0,1.5,1.5,2.0,1.0,...
    2.0,1.6,1.2,2.0,1.5,2.0,2.0,1.2,1.2,2.0,2.0,0.9,3.0,2.0,2.0,1.2,...
    3.5,1.5,1.5,1.8,1.8,2.5,0.8,1.4,0.7,0.8,2.5,1.2,0.8,2.5,1.5,1.0,...
    1.5,1.0,0.7,1.2,0.8,0.8,1.2,1.2,1.5,1.2,0.9,1.0,1.8,1.5,1.8,1.8,...
    1.2,1.4,5.0,1.4,1.5,2.0,1.5,2.0,1.0,1.0,0.8,0.7];
%this vector contains the values of the thicknesses for each of the
%property cards

%% 3.   Create random variables to apply gradient estimation
Nrv         = length(Vid_shells);   %total number of random variables
for i=1:Nrv,    %loop for defining random variables
    Srvname         = ['xrv_pshell_' num2str(Vid_shells(i))];   %string containing name of random variable
    eval( [ Srvname ...
        ' = RandomVariable(''Sdistribution'',''normal'',''mean'',' ...
        num2str(Vthick(i)) ',''std'',' num2str(Vthick(i)*0.1) ');']);   %command to define random variable in workspace
end

%% 4.   Create random variable set
Xrvs    = RandomVariableSet('Cmembers',{'*all*'});  %add all random 
%variables present in the workspace to the set of random variables

%% 5.   Create input object
Xin     = Input;            %create empty output object
Xin     = add(Xin,Xrvs);    %add random variable set

%% 6.   Create injector
Xi  = Injector('Sscanfilename','run_car2_a.cossan',...  %name of the file with identifiers
    'Sfile','run_car2_a.dat');               %name of the input file, i.e. once the values have been injected
% The position of the variable are stored into the Xi injector
% The format of the variable is the following: 
% <cossan name="I" index="1" format="%1d" original="1"/> 
% name: is the name of the variable in the COSSAN-X workspace
% format: format use to store the variable in the input file
%         (see fscanf for more details about the format string
%          ordinary characters and/or conversion specifications. 
%

%% 7.   Create Response object and Extractor object
%7.1.   Create Response object
Xresp   = Response('Sname', 'freq', ...     %name of response is "freq"
             'Sfieldformat', '%12e%*', ...  %look for a float
             'Clookoutfor',{'R E A L   E I G E N V A L U E '}, ...    %identify position of the output file where eigenvalues are written
             'Svarname','', ...
             'Ncolnum',68, ...
             'Nrownum',4);
%7.2.   Create Extractor
Xe      = Extractor('Sdescription','Extractor for first torsional mode', ...
             'Sfile','run_car2_a.f06',...   %name of the output file
             'Xresponse',Xresp);            %response to be extracted

%% 8.   Construct the connector
%8.1.   create the connector
Xc  = Connector('Stype','nastran_x86_64',...    %use Nastran 64
    'Lkeepsimfiles',false,...     %do not keept files after completing simulation
    'Smaininputfile','run_car2_a.dat',...  %name of input file
    'Sworkingdirectory','/tmp');    %working directory
%8.2.   add injector and extractor to emtpty connector
Xc  = add(Xc,Xi);
Xc  = add(Xc,Xe);


%% 9.   JobManager object
Scwd        = pwd;
Spreexecmd  = ['cp ' Scwd '/843_63100_a.dat ./;'];  %copies input file required for FE analysis
Xjmi = JobManagerInterface('Stype','GridEngine','Squeue','pizzas64.q');
Xg   = JobManager('Spreexecmd',Spreexecmd,'Xjobmanagerinterface',Xjmi);

%% 10.  Create Evaluator and Model object

Xev         = Evaluator('Xconnector',Xc,'Xjobmanager',Xg);
Xmdl        = Model('Cmembers',{'Xin','Xev'});

%% test the model with one sample

%Xin   = sample(Xin);
%Xout  = apply(Xmdl,Xin)

%% find the important parameters with Finite differences

Xgrad1 = Sensitivity.gradientFiniteDifferences('Xtarget',Xmdl);

%save results.mat

%% find the important parameters using gradient MC

%Xgrad2   = Sensitivity.gradientMonteCarlo('Xtarget',Xmdl);


