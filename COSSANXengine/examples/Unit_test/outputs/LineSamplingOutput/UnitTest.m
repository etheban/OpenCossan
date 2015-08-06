%% unit test line sampling output
clear variables
%%


itest=1;
try
    Xlso1 = LineSamplingOutput('sperformancefunctionname','Xrv3','vnumpointline',[2 3]);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Vtest(itest)=false;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

%% invalid vnumpointline
itest=itest+1;
try
    Xlso2 = LineSamplingOutput('sperformancefunctionname','Xrv3','vnumpointline',[2 3 4 5; 1 2 3 6]);
    Vtest(itest)=false;
        Crk{itest}='';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message;
        Crk{itest}='';
end
%% accessory data

Xlso2 = LineSamplingOutput('sperformancefunctionname','Xrv3','vnumpointline',[2 3]');
Xlso3 = LineSamplingOutput('sperformancefunctionname','Vg2','vnumpointline',[2 3]');



%% merging two linesamplingoutput objects
itest=itest+1;

try
    merge(Xlso1,Xlso2)
        Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Vtest(itest)=false;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end

% merging two linesamplingoutput w/ different perfun. names
itest=itest+1;
try
    merge(Xlso1,Xlso3)
    Vtest(itest)=false;
        Crk{itest}='';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end
%% merging 1 linesamplingoutput and 1 simulationoutput object
itest=itest+1;

try
    Xout5=SimulationOutput.load('SfileName','SimulationOutput5');
    Xlso2 = merge(Xlso2,Xout5);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Vtest(itest)=false;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end


%% plotlines

%require a Linesampling object
try
Xgrad=Gradient('Sdescription','Test gradient object', ...
    'Vgradient',[0;3],'Cnames',{'RV1', 'RV2'});

Xls=LineSampling('Nlines',2,'Xgradient',Xgrad,'Nbatches',1);

plotLines(Xlso2,Xls)
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Vtest(itest)=false;
    Cmess{itest}=ME.message;
    Crk{itest}='';
end
%%
clc
close all
disp('  ')
disp('  ')
disp('--------------------------------------------------------------------')
disp([' Unit Test of LineSamplingOutput (' datestr(now) ')'])
disp('--------------------------------------------------------------------')
Cmess{itest+1}=[]; Crk{itest+1}=[];
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} '), (' Crk{i} ')' ]);
    end
end
