%function dmcs_rv()
%DMCS_RV This function is developed to interact with PATRAN PLUG-IN. 
%        More specifically, it reads the ASCII file prepared by the
%        Plug-in, generates the samples and output these to a text file.
%        Note that this function is for the RANDOM VARIABLE problem, i.e.
%        only one RV is used to represent a certain random parameter.
%                                
%
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2010 IfM
% =========================================================================

%% Open the file

[fid,~] = fopen('PatranPlugin.txt','r');

if fid == -1
   error('PatranPlugin.txt could not be opened'); 
else
   disp('PatranPlugin.txt opened successfully');
end

%% Read the file

tline = fgetl(fid);
% read the number of simulations
Nsim  = textscan(tline, '%*s %*s %*s %*s %*s %d'); Nsim = Nsim{1};
tline = fgetl(fid); %#ok<*NASGU>
tline = fgetl(fid);
tline = fgetl(fid);
tline = fgetl(fid);
input = textscan(fid, '%s %s %f %f');
% read the RV names, distributions, Mean & CoV values
Crvnames       = input{1};
Cdistributions = input{2};
Vmean          = input{3};
Vcov           = input{4};

disp('Reading PatranPlugin.txt completed successfully');
fclose(fid); 

%% Create the RV objects in COSSAN

Nrvs = length(input{1}); 
for irvno=1:Nrvs
    Xrv1=RandomVariable('Sdistribution',Cdistributions{irvno},'mean',Vmean(irvno),'cov',Vcov(irvno));
    renameVariable(Crvnames{irvno},Xrv1);
end

%% Create the RVSET
    
clear Xrv1
Xrvs1=RandomVariableSet('Cmembers',Crvnames);  
    
%% Generate Samples    
       
Xsamples = sample(Xrvs1,'Nsamples',Nsim);    
Msamples = (Xsamples.MsamplesPhysicalSpace)';     
    
%% Output samples to file   
    
fid=fopen('DMCS_RV_samples.dat','w+');    
    
Srvformat = repmat('%8.5e ',1,Nrvs);    
fprintf(fid,[Srvformat '\n'],Msamples);   
fclose(fid); 

%return

