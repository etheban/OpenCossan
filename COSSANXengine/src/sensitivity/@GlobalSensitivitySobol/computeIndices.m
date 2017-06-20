function varargout=computeIndices(Xobj,varargin)
%COMPUTEINDICES This method does the Local Sensitivity analysis, and
%computes the local sensitivity indices
%
% $Copyright~1993-2012,~COSSAN~Working~Group,~University~of~Liverpool,~UK$
% $Author: Edoardo-Patelli$

% =====================================================================
% This file is part of openCOSSAN.  The open general purpose matlab
% toolbox for numerical analysis, risk and uncertainty quantification.
%
% openCOSSAN is free software: you can redistribute it and/or modify
% it under the terms of the GNU General Public License as published by
% the Free Software Foundation, either version 3 of the License.
%
% openCOSSAN is distributed in the hope that it will be useful,
% but WITHOUT ANY WARRANTY; without even the implied warranty of
% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
% GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with openCOSSAN.  If not, see <http://www.gnu.org/licenses/>.
% =====================================================================
%% Check inputs
OpenCossan.validateCossanInputs(varargin{:})

%% Process inputs
for k=1:2:length(varargin)
    switch lower(varargin{k})
        case {'xtarget','xmodel'}
            Xobj=Xobj.addModel(varargin{k+1}(1));
        case {'cxtarget','cxmodel'}
            Xobj=Xobj.addModel(varargin{k+1}{1});
        otherwise
            error('openCOSSAN:GlobalSensitivitySobol:computeIndices',...
                'The PropertyName %s is not allowed',varargin{k});
    end
end

% Set the analysis name when not deployed
if ~isdeployed
    OpenCossan.setAnalysisName(class(Xobj));
end
% set the analyis ID
OpenCossan.setAnalysisID;
% insert entry in Analysis DB
if ~isempty(OpenCossan.getDatabaseDriver)
    insertRecord(OpenCossan.getDatabaseDriver,'StableType','Analysis',...
        'Nid',OpenCossan.getAnalysisID);
end

% Get the local indices

Ninput=length(Xobj.Cinputnames);
Noutput=length(Xobj.Coutputnames);
Nsamples=Xobj.Xsimulator.Nsamples;
OpenCossan.cossanDisp(['Total number of model evaluations ' num2str(Nsamples*(Ninput+2))],2)

%% Estimate sensitivity indices
% Generate samples
OpenCossan.cossanDisp(['Generating samples from the ' class(Xobj.Xsimulator) ],4)

% Create two samples object each with half of the samples
OpenCossan.cossanDisp('Creating Samples object',4)
XA=Xobj.Xsimulator.sample('Xinput',Xobj.Xinput);
XB=Xobj.Xsimulator.sample('Xinput',Xobj.Xinput);

% Evaluate the model
OpenCossan.cossanDisp('Evaluating the model ' ,4)
ibatch = 1;
XoutA=Xobj.Xtarget.apply(XA); % y_A=f(A)
if ~isempty(OpenCossan.getDatabaseDriver)
    insertRecord(OpenCossan.getDatabaseDriver,'StableType','Simulation', ...
        'Nid',getNextPrimaryID(OpenCossan.getDatabaseDriver,'Simulation'),...
        'XsimulationData',XoutA,'Nbatchnumber',ibatch)
end
ibatch = ibatch+1;
XoutB=Xobj.Xtarget.apply(XB); % y_B=f(B)
if ~isempty(OpenCossan.getDatabaseDriver)
    insertRecord(OpenCossan.getDatabaseDriver,'StableType','Simulation', ...
        'Nid',getNextPrimaryID(OpenCossan.getDatabaseDriver,'Simulation'),...
        'XsimulationData',XoutB,'Nbatchnumber',ibatch)
end

% Expectation values of the output variables
OpenCossan.cossanDisp('Extract quantity of interest from SimulationData ' ,4)


% Check if the model contains Dataseries
Vindex=strcmp(XoutA.CnamesDataseries,Xobj.Coutputnames);
if sum(Vindex)>0
    warning('It is not possible to compute the Sensitivity Analysis with respect a variable that is a DataSeries')
    fprintf('****** Removing vairables %s from the requested outputs ******\n',Xobj.Coutputnames{Vindex})
    Xobj.Coutputnames=Xobj.Coutputnames(~Vindex);
    Noutput=length(Xobj.Coutputnames);
end

MoutA=XoutA.getValues('Cnames',Xobj.Coutputnames);
MoutB=XoutB.getValues('Cnames',Xobj.Coutputnames);

%OpenCossan.cossanDisp(['Compute Vf02 for ' Coutputnames{nout}],4)
Vf02=(sum([MoutA;MoutB],1)/(2*Nsamples)).^2; % fo²

%% Define a function handle to estimate the parameters
% This function handle is also used by the bootstraping method to estimate
% the variance of the estimators.
hcomputeindices=@(MxA,MxB)sum(MxA.*MxB)/(size(MxA,1))- Vf02;

% Compute the Total variance of the outputs
%VD=sum([MoutA;MoutB].^2,1)/(2*Nsamples) - Vf02;
VD=hcomputeindices([MoutA;MoutB],[MoutA;MoutB]);

if Xobj.Nbootstrap>0
    VDbs=bootstrp(Xobj.Nbootstrap,hcomputeindices,[MoutA;MoutB],[MoutA;MoutB]);
end

% Preallocate memory
Dz=zeros(Ninput,Noutput);
Dy=zeros(Ninput,Noutput);

% Extract matrices of samples
MA=XA.MsamplesHyperCube;
MB=XB.MsamplesHyperCube;
Dybs=zeros(Ninput,Xobj.Nbootstrap,Noutput);
Dzbs=zeros(Ninput,Xobj.Nbootstrap,Noutput);
for irv=1:Ninput
    OpenCossan.cossanDisp(['[Status] Compute Sensitivity indices ' num2str(irv) ' of ' num2str(Ninput)],2)
    Vpos=strcmp(XA.Cvariables,Xobj.Cinputnames{irv});
    MC=MB;
    MC(:,Vpos)=MA(:,Vpos); % Create matrix C_i
    % Construct Samples object
    XC=Samples('Xinput',Xobj.Xinput,'MsamplesHyperCube',MC);
    
    % Evaluate the model
    ibatch = ibatch+1;
    XoutC=Xobj.Xtarget.apply(XC); % y_C_i=f(C_i)
    if ~isempty(OpenCossan.getDatabaseDriver)
        insertRecord(OpenCossan.getDatabaseDriver,'StableType','Simulation', ...
            'Nid',getNextPrimaryID(OpenCossan.getDatabaseDriver,'Simulation'),...
            'XsimulationData',XoutC,'Nbatchnumber',ibatch)
    end
    
    
    MoutC=XoutC.getValues('Cnames',Xobj.Coutputnames);
    
    %estimate V(E(Y|X_i))
    Dy(irv,:)=hcomputeindices(MoutA,MoutC);
    %Dy(irv,:)=sum(MoutA.*MoutC)/Nsamples- Vf02; %
    
    %estimate V(E(Y|X~i))
    %Dz(irv,:)=sum(MoutB.*MoutC)/Nsamples- Vf02; %
    Dz(irv,:)=hcomputeindices(MoutB,MoutC);
    
    if Xobj.Nbootstrap>0
        Dybs(irv,:,:)=bootstrp(Xobj.Nbootstrap,hcomputeindices,MoutA,MoutC);
        Dzbs(irv,:,:)=bootstrp(Xobj.Nbootstrap,hcomputeindices,MoutB,MoutC);
    end
end

%% According to the Saltelli manuscipt (Sensitivity Analysis practices.
% Strategies for model-based inference) the Sobol indices and total
% indices are defined by Dy/D and 1-Dz/D, respectively. However, the
% numerical results tell us that this two relation are inverted.


%% Export results
% Construct SensitivityMeasure object
for n=1:Noutput
    
    % Compute First order Sobol' indices
    MfirstOrder=Dy(:,n)/VD(n);
    
    % Compute the Total Sensitivity indices
    Mtotal=1-Dz(:,n)/VD(n);
    
    if Xobj.Nbootstrap>0
        VfirstOrderCoV=std(squeeze(Dybs(:,:,n))'./repmat(VDbs(:,n),1,Ninput))./abs(MfirstOrder');
        VtotalCoV=std(1-squeeze(Dzbs(:,:,n))'./repmat(VDbs(:,n),1,Ninput))./abs(Mtotal');
        
        varargout{1}(n)=SensitivityMeasures('Cinputnames',Xobj.Cinputnames, ...
            'Soutputname',  Xobj.Coutputnames{n},'Xevaluatedobject',Xobj.Xtarget, ...
            'Sevaluatedobjectname',Xobj.Sevaluatedobjectname, ...
            'VtotalIndices',Mtotal','VsobolFirstOrder',MfirstOrder', ...
            'VtotalIndicesCoV',VtotalCoV,'VsobolFirstOrderCoV',VfirstOrderCoV, ...
            'Sestimationmethod','Sensitivity.sobolIndices'); %#ok<AGROW>
    else
        varargout{1}(n)=SensitivityMeasures('Cinputnames',Xobj.Cinputnames, ...
            'Soutputname',  Xobj.Coutputnames{n},'Xevaluatedobject',Xobj.Xtarget, ...
            'Sevaluatedobjectname',Xobj.Sevaluatedobjectname, ...
            'VtotalIndices',Mtotal','VsobolFirstOrder',MfirstOrder', ...
            'Sestimationmethod','Sensitivity.sobolIndices'); %#ok<AGROW>
    end
end

if nargout>1
    % Merge all the 3 Simulation Data and export the result
    varargout{2}=XoutC.merge(XoutB.merge(XoutA));
end

if ~isdeployed
    % add entries in simulation and analysis database at the end of the
    % computation when not deployed. The deployed version does this with
    % the finalize command
    XdbDriver = OpenCossan.getDatabaseDriver;
    if ~isempty(XdbDriver)
        XdbDriver.insertRecord('StableType','Result',...
            'Nid',getNextPrimaryID(OpenCossan.getDatabaseDriver,'Result'),...
            'CcossanObjects',varargout(1),...
            'CcossanObjectsNames',{'Xgradient'});
    end
end