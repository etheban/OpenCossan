%SFEM_PLUGIN This function is developed to interact with PATRAN PLUG-IN. 
%        More specifically, 
%                                
%
% =========================================================================
% COSSAN - COmputational Stochastic Structural Analysis
% IfM, Chair of Engineering Mechanics, LFU Innsbruck, A
% Copyright 1993-2010 IfM
% =========================================================================

initializeCOSSAN('scossandatabasepath','/home/hmp/matlab/toolbox/COSSANXengine/database/');

%% Open the file

[fid,~] = fopen('PatranPlugin.txt','r');

if fid == -1
   error('PatranPlugin.txt could not be opened'); 
else
   disp('PatranPlugin.txt opened successfully');
end

%% Read the file

tline = fgetl(fid);
tline = fgetl(fid);
tline = fgetl(fid);
tline = fgetl(fid);
Smethod = tline;
tline = fgetl(fid); %#ok<*NASGU>
tline = fgetl(fid);
tline = fgetl(fid);
Simplementation = tline;
tline = fgetl(fid); 
tline = fgetl(fid);
tline = fgetl(fid);
Norder = str2num(tline); %#ok<*ST2NM>
tline = fgetl(fid); 
tline = fgetl(fid);
tline = fgetl(fid);
Nsimulations = str2num(tline);
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

%% Create the INPUT

Xinp1 = Input('Sdescription','Xinput object');       
Xinp1 = add(Xinp1,Xrvs1);

%% Construct the Model

test_con = Connector('Stype','nastran_x86_64','Smaininputfile','dummy');
test_con.Ssolverbinary = '/usr/site/msc/MD_Nastran/R3/x86_64/bin/mdnast2008';
test_eval  = Evaluator('Xconnector',test_con);
test_model = Model('Xevaluator',test_eval,'xinput',Xinp1);

%% Insert the identifiers to the FE input file

fid = fopen('cossan_analysis.bdf','r+');
fid2 = fopen('plugin.cossan','w+');

if fid == -1
   error('cossan_analysis.bdf could not be opened'); 
else
   disp('cossan_analysis.bdf opened successfully');
end

Rv_counter = 0;
for i = 1:Nrvs
    Srv = Crvnames{i};
    for j = 1:length(Crvnames{i})
       if strcmp(Srv(j),'_')
          pos = j; 
       end
    end
    Smatname{i} = Srv(1:(pos-1)); %#ok<*SAGROW>
end

while 1
       mat_found = 0;
       longformat = 0;
       Sline = fgetl(fid);
       if ~ischar(Sline),   break,   end
       if (length(Sline)>18 && strcmp(Sline(1:19),'$ Material Record :') )
           [Smat1 Smat2 Smat3 Smat4 Smat5] = strread(Sline, '%s %s %s %s %s');
           for i = 1:Nrvs
                if strcmp(Smatname{i},Smat5{1}) 
                    mat_found = 1;
                end
           end
           if mat_found == 1
              Sline = fgetl(fid);
              Sline = fgetl(fid);
              if strcmp(Sline(1:5),'MAT1*')
                 longformat = 1; 
              end
              [Smat1 Smat2 Smat3 Smat4 Smat5] = strread(Sline, '%s %s %s %s %s'); 
              Rv_counter = Rv_counter + 1;
              Smatnew = [Smat1{1}(1:4) ',' Smat2{1} ',<cossan name="' Crvnames{Rv_counter} '" format="%10.4e" original="1" />, , ' Smat4{1}  ',' Smat5{1} ];
              fwrite(fid2,Smatnew);
              fprintf(fid2,'\n');
              if (longformat == 1)
                Sline = fgetl(fid);  
              end
           else 
               fwrite(fid2,Sline);
               fprintf(fid2,'\n');
           end 
       else
            fwrite(fid2,Sline);
            fprintf(fid2,'\n');
       end
end

% Close files
fclose(fid);
fclose(fid2);

%  Repeat the above procedure also for the group files (applies only to
%  component-wise)
if strcmp(Simplementation,'Component-wise')
    for i = 1:Nrvs
        % open the files
        Srvname = Crvnames{i};
        fid = fopen([ Srvname '.bdf'],'r+');
        fid2 = fopen([ Srvname '.dat'],'w+');
        while 1
           Sline = fgetl(fid);
           if ~ischar(Sline),   break,   end
           if (length(Sline)>3 && strcmp(Sline(1:4),'MAT1') )
               [Smat1 Smat2 , ~, Smat4 Smat5] = strread(Sline, '%s %s %s %s %s'); 
               Smatnew = [Smat1{1}(1:4) ',' Smat2{1} ',<cossan name="' Crvnames{Rv_counter} '" format="%10.4e" original="1" />, , ' Smat4{1}  ',' Smat5{1} ];
               fwrite(fid2,Smatnew);
               fprintf(fid2,'\n');
               Sline = fgetl(fid);        
           else
                fwrite(fid2,Sline);
                fprintf(fid2,'\n');
           end
        end
        % close the files
        fclose(fid);
        fclose(fid2);
    end
end

%% Perform SFEM Analysis

Sdirectory = pwd;
if strcmp(Smethod,'PERTURBATION')
    
    Xsfem = Perturbation('Xmodel',test_model,'Sinputfile',[Sdirectory filesep 'plugin.cossan'],'Simplementation',Simplementation,...
        'CyoungsmodulusRVs',Crvnames);
    
elseif strcmp(Smethod,'NEUMANN')
    
    Xsfem = Neumann('Xmodel',test_model,'Sinputfile',[Sdirectory filesep 'plugin.cossan'],'Simplementation',Simplementation,...
        'CyoungsmodulusRVs',Crvnames,'Nsimulations',Nsimulations,'Norder',5);
    
elseif strcmp(Smethod,'P-C')
    
    Xsfem = SfemPolynomialChaos('Xmodel',test_model,'Sinputfile',[Sdirectory filesep 'plugin.cossan'],'Simplementation',Simplementation,...
        'CyoungsmodulusRVs',Crvnames,'Norder',Norder);
    
end
                       
Xout = Xsfem.performAnalysis;

Xout = getResponse(Xout,'Sresponse','all');

Xout.prepareReport;

