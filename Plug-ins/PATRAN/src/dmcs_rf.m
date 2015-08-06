function dmcs_rf()
%DMCS_RF This function is developed to interact with PATRAN PLUG-IN. 
%        More specifically, it reads the ASCII file prepared by the
%        Plug-in, generates the samples and output these to a text file.
%        Note that this function is for the RANDOM FIELD problem, i.e.
%        random fields are generated for each selected region (assigned 
%        using the groups).
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
tline = fgetl(fid);
tline = fgetl(fid);
% read the no of RFs
Nrfs  = textscan(tline, '%*s %*s %d'); Nrfs = Nrfs{1};
tline = fgetl(fid);
% read the no of simulations
Nsim  = textscan(tline, '%*s %*s %d'); Nsim = Nsim{1};
tline = fgetl(fid); %#ok<*NASGU>
tline = fgetl(fid);
tline = fgetl(fid);
% read the required parameters to model RFs
for i = 1:Nrfs
    tline = fgetl(fid);
    input = textscan(tline, '%s %s %f %f %s %s %f %d');
    Crfnames{i}       = input{1}; %#ok<*AGROW>
    Cdistributions{i} = input{2};
    Vmean(i)          = input{3};
    Vcov(i)           = input{4};
    Cgeometry{i}      = input{5};
    Ccorrfunctions{i} = input{6};
    Vcorrlengths(i)   = input{7};
    Vnklterms(i)      = input{8};
    Vvar(i)           = (Vmean(i)*Vcov(i))^2;
end
tline = fgetl(fid);
tline = fgetl(fid);
tline = fgetl(fid);
tline = fgetl(fid);
% Read the names of the groups
% NOTE: these had to be output in this way, because of some difficulties
% encountered within PCL. Dont ask me why, but believe it is better this
% way
Cgroupnames = textscan(tline, '%s');
Cgroupnames = Cgroupnames{1};
 
disp('Reading PatranPlugin.txt completed successfully');
fclose(fid);

%% Calculate midpoints for each group

Ngroups = length(Cgroupnames);
% loop over no of groups
for i=1:Ngroups
    % read the node coordinates of the elements in the group
    Sfilename   = ['node_coord_' Cgroupnames{i} '.dat'];
	fid         = fopen( Sfilename,'r');
	Cnodecoords = textscan(fid, '%f %f %f %f ');
    fclose(fid); 
    % get node ids & no of nodes
    Vnodeids = Cnodecoords{1}; 
    Nnodes = length(Cnodecoords{1}); 
    % get the node coordinates
    Vnodexcoord = zeros(Nnodes,1); 
    Vnodeycoord = zeros(Nnodes,1); 
    Vnodezcoord = zeros(Nnodes,1);  
    Vnodexcoord(Vnodeids) = Cnodecoords{2};
    Vnodeycoord(Vnodeids) = Cnodecoords{3};
    Vnodezcoord(Vnodeids) = Cnodecoords{4};
    % Read the connectivities
    Sfilename = ['connectivities_' Cgroupnames{i} '.dat'];
    fid       = fopen(Sfilename,'r');
    % Find out how many columns there are in each line
    Sline = fgetl(fid);
    [~, Ncolumns] =sscanf(Sline,'%d');
    Sread = '';
    for k = 1:Ncolumns
        Sread = [ Sread ' %d']; %#ok<AGROW>
    end
    % go back to the beginning of the file
    frewind(fid);
    Cconnectivities = textscan(fid, Sread);
    fclose(fid);
    % NOTE - HMP (14-Sep-2010)
    % this part is added since PATRAN outputs now an additional column of
    % zeros, which causes error in this function
    if Cconnectivities{end}(1) == 0
        Ncolumns = Ncolumns-1;
    end
    % get the no of elements in the group
    Nelems = size(Cconnectivities{1},1);
    % get the coordinates of each node
    Melemnodes = zeros(Nelems,(Ncolumns-1));
    for k = 1:(Ncolumns-1)
        Melemnodes(:,k) = Cconnectivities{k+1};
    end
    % Calculating the coordinates of the midpoint
    Vcentx = zeros(Nelems,1);
    Vcenty = zeros(Nelems,1);
    Vcentz = zeros(Nelems,1);
    for k = 1:(Ncolumns-1)
        Vcentx = Vcentx + Vnodexcoord(Melemnodes(:,k));
        Vcenty = Vcenty + Vnodeycoord(Melemnodes(:,k));
        Vcentz = Vcentz + Vnodezcoord(Melemnodes(:,k));
    end
    Vcentx = Vcentx/(Ncolumns-1);
    Vcenty = Vcenty/(Ncolumns-1);
    Vcentz = Vcentz/(Ncolumns-1);
    % Calculate the distances between midpoints for PLANE geometry
    Mdistances  = zeros(Nelems,Nelems);
    if strcmpi(Cgeometry{i},'PLANE')
        for j = 1:Nelems
            Mdistances(j,:)=sqrt((repmat(Vcentx(j),1,Nelems)-Vcentx(1:Nelems)').^2 +...
                           (repmat(Vcenty(j),1,Nelems)-Vcenty(1:Nelems)').^2+...
                           (repmat(Vcentz(j),1,Nelems)-Vcentz(1:Nelems)').^2); 
        end                    
    end  
    Cdistances{i} = Mdistances;
    Vnelems(i)    = Nelems;
    % clear these to free memory
    clear Vcentx Vcenty Vcentz Melemnodes Cconnectivities Vnodexcoord Vnodeycoord Vnodezcoord
    clear Vnodeids Nnodes Mdistances Nelems
end

%% Generate the samples for each Random Field

for j = 1:Nrfs
    % following loop is to identify the group name of the RF
    % NOTE: this is necessary because it can be that there are more than
    %       one RF generated on a certain region. In this case, the
    %       information on distances comes from each group (which defines 
    %       the region in the structure)
    for k = 1:Ngroups      
        Sgroupname   = Cgroupnames{k};
        Srfname      = Crfnames{j};
        Srfname      = Srfname{1};
        idx          = strfind(Srfname, '_');
        Srfgroupname = Srfname(1:(idx-1));
        if strcmpi(Srfgroupname,Sgroupname)
            Group_index = k;
        end       
    end
    % get the distances & No of elements for the group of the RF
    Mdistance = Cdistances{Group_index};
    Nelems    = Vnelems(Group_index);
    % Calculate the correlation matrix
    Mcorr = zeros(Nelems,Nelems);
    if strcmpi(Ccorrfunctions{j},'EXPONENTIAL') 
        for i = 1:Nelems
            Mcorr(i,:) = exp(-abs((Mdistance(i,:))/Vcorrlengths(j)));
        end 
    elseif strcmpi(Ccorrfunctions{j},'TRIANGULAR')   
        for i = 1:Nelems
            Mcorr(i,:) = max(0,(1-Mdistance(i,:)/Vcorrlengths(j)));
        end 
    elseif strcmpi(Ccorrfunctions{j},'CONVEX')   
        for i = 1:Nelems
            Mcorr(i,:) = max((2-(2*0.63212*Mdistance(i,:)/Vcorrlengths(j))-exp(-Mdistance(i,:)/Vcorrlengths(j))),0);
        end
    end    
    % Obtain the covariance from the correlations
    Mcovariance =  Vvar(j)*Mcorr;
    % Use the Stochastic Process class to generate the samples of RFs
    SP1 = StochasticProcess('Sdistribution','normal','Vmean',Vmean(j),'Mcoord',Mdistance,...
                            'Mcovariance',Mcovariance,'Lhomogeneous',true);
    SP1 = KL_terms(SP1,'NKL_terms',Vnklterms(j));
    ds1 = SP1.sample('Nsamples',Nsim);
    Csamples{j} = ds1.Xdataseries.Mdata;
    % free the memory
    clear Mcovariance Mdistance
end
    
%% Output samples to file   
    
fid = fopen('DMCS_RF_samples.dat','w');
for l=1:Nsim
    for i=1:Nrfs
        fprintf(fid,'%8.5e\n',Csamples{i}(l,:));
    end
end 
fclose(fid);

return

