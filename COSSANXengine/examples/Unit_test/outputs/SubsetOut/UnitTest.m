%% unit test subset output
clear variables

%% constructor

itest=1;
try
    Xsso1 = SubsetOutput('sperformancefunctionname','Vg','vpfl',[1 2 3],'vcovpfl',[1 2 3],'vrejectionrates',[1 2 3]);
    Xsso2 = SubsetOutput('sperformancefunctionname','Vg','vpfl',[1 2 3],'vcovpfl',[1 2 3]','vrejectionrates',[1 2 3]);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Vtest(itest)=false;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

%% Invalid constructor
itest=itest+1;
try
    Xsso2 = SubsetOutput('sperformancefunctionname','Vg','vpfl',[1 2 3 ;4 5 3],'vcovpfl',[1 2 3],'vrejectionrates',[1 2 3]);
    Vtest(itest)=false;
    Crk{itest}='';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

%
itest=itest+1;
try
    Xsso2 = SubsetOutput('sperformancefunctionname','Vg','vpfl',[1 2 3 ],'vcovpfl',[1 2 3;4 5 3],'vrejectionrates',[1 2 3]);
    Vtest(itest)=false;
    Crk{itest}='';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

%

itest=itest+1;
try
    Xsso2 = SubsetOutput('sperformancefunctionname','Vg','vpfl',[1 2 3 ],'vcovpfl',[1 2 3],'vrejectionrates',[1 2 3;4 5 3]);
    Vtest(itest)=false;
    Crk{itest}='';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

%% merging w/ 1 simulationoutput object
itest=itest+1;

try
    Xout5=SimulationOutput.load('SfileName','SimulationOutput5');
    Xsso2 = merge(Xsso2,Xout5);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Vtest(itest)=false;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

%% invalid merge
itest=itest+1;
try
    Xsso2 = merge(Xsso2,Xsso1);
    Vtest(itest)=false;
    Crk{itest}='';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end


%%
clc

disp('  ')
disp('--------------------------------------------------------------------')
disp([' Unit Test of SubsetOutput (' datestr(now) ')'])
disp('--------------------------------------------------------------------')
Cmess{itest+1}=[]; Crk{itest+1}=[];
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} '), (' Crk{i} ')' ]);
    end
end
