%
%   Car body in white - mode-based meta-model
%
%   In this script, a mode-based meta-model of the car body in white is
%   generated. This is a hard-coded script intended for producing a demo
%   for the 9th milestone meeting.
%
%   The mode-based meta-model is trained using the source code prepared by
%   LP during the summer of 2009; the quality of this code is unknonwn.
%
%   The parameters of the meta-model are the thicknesses of different shell
%   elements of the FE model of the car.
% 
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2009 IfM
% =========================================================================

clear variables

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

%% 2.   Create input of the model
%in this part of the code, the parameters of the model are created. These
%parameters are related with the thicknesses defined above
%2.1.   Create rv's
Nrv         = length(Vid_shells);
Cmembers    = cell(1,Nrv);
Srvs        = '[';
for i=1:Nrv,
    Srvname         = ['xrv_pshell_' num2str(Vid_shells(i))];
    eval([Srvname ' = RandomVariable(''Sdistribution'',''normal'',''mean'',' num2str(Vthick(i)) ',''std'',' num2str(Vthick(i)*0.05) ');']);
    Cmembers{i}     = Srvname;
    Srvs            = [Srvs ' ' Srvname];
end
Srvs    = [Srvs ']'];
%2.2.   Create random variable set
eval(['Xrvs    = RandomVariableSet(''Cmembers'',Cmembers,''Xrv'',' Srvs ');']);
%2.3.   Define Xinput
Xin     = Input;
Xin     = add(Xin,Xrvs);

%% 3.   Construct Mio object to load data to train meta-model
% the mio will solve for the eigenvalues, eigenvectors and the mass matrix
Xm  = Mio('Sdescription', 'Performance function', ...
                'Spath','./', ...
                'Sfile','gen_car', ...
                'Liostructure',true,...
				'Lfunction',true); % This flag specify if the .m file is a script or a function. 
            
%% 4.   Construct model associated with Mio object
Xeval   = Evaluator('Xmio',Xm,'Sdescription','Evaluator xmio');
Xmdl    = Model('evaluator',Xeval,'Xinput',Xin);

%% 5.   Create the meta-model
Xmm     = ModeBased('sdescription','metamodel','xmodel',Xmdl,...
    'Smassname','mass','Sstiffnessname','stiff','nmodes',8,'Ncalibrationsamples',44,'Xinput',Xin,'Lmodes',1);

%% 6.   Remove information not required from meta-model
Xmm.Xmodes  = [];   %with this command, a lot of information on the data 
%for the calibration of the meta-model (which is no longer required) can be
%deleted

%% 7.   Create a structure with relevant information of the meta-model
Tmm.Nmodes          = Xmm.Nmodes;
Tmm.Xmodes0.Vlambda =Xmm.Xmodes0.Vlambda;
Tmm.Xmodes0.MPhi    = Xmm.Xmodes0.MPhi;
Tmm.Xmodes0.Mmass   = Xmm.Xmodes0.Mmass ;
Tmm.Valpha          = Xmm.Valpha;
Tmm.Vlincomb        = Xmm.Vlincomb;
Tmm.list_mod        = Xmm.list_mod;
save Tmm.mat Tmm

return