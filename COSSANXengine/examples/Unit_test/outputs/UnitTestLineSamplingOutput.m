function varargout = UnitTestLineSamplingOutput
%UNITTESTLineSamplingOutput Summary of this function goes here
%   Detailed explanation goes here

% Test LineSamplingOutput
% Perallocate memory


Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;


%% 1 Create an empty object
itest = itest+1;
try
    Xobj = LineSamplingOutput;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Constructor
itest = itest+1;
try
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[2 3]);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest = itest+1;
try
    Xobj = LineSamplingOutput('Sperformancefunctionname','Xrv3',...
        'Vnumpointline',[2 3 4 5; 1 2 3 6]);
    display(Xobj)
     Cmess{itest}='It should fail';
catch ME
    Vtest(itest)=true;
    disp(ME)
end

%% 4 merging two linesamplingoutput objects
itest = itest+1;
try
    Xobj1 =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[2 3]);    
    Xobj2 =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 1]);
    Xobj=merge(Xobj1,Xobj2);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5 pass point distance 
itest = itest+1;
try
   
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 1],...
        'VdistanceOrigin',rand(9,1));
    display(Xobj)
    Vtest(itest)=true;
catch ME
        Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6 pass point distance 
itest = itest+1;
try
   
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 1],...
        'VdistanceOrthogonalPlane',rand(9,1));
    display(Xobj)
    Vtest(itest)=true;
catch ME
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 pass point distance 
itest = itest+1;
try
   
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 2],...
        'VdistanceOrthogonalPlane',rand(10,1),'VdistanceOrigin',rand(10,1));
    display(Xobj)
    Vtest(itest)=true;
catch ME
        Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 pass point distance 
itest = itest+1;
try
   
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 1],...
        'VdistanceOrthogonalPlane',rand(5,1),'VdistanceOrigin',rand(9,1));
    display(Xobj)
    Cmess{itest}='wrong numer of points';
catch ME
        Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end

%% 9 pass point distance 
itest = itest+1;
try
   
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 1],...
        'VdistanceOrthogonalPlane',rand(9,1),'VdistanceOrigin',rand(8,1));
    display(Xobj)
       Cmess{itest}='wrong numer of points';
catch ME
       Cmess{itest}=[ME.identifier ' -- ' ME.message];
       Vtest(itest)=true; 
end


%% 10 plotlines
itest = itest+1;
try
    %definition of required Linesampling object
    for n=1:10
    Tvalues(n).Xrv3=rand;
    end
    Xsd=SimulationData('Tvalues',Tvalues);
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 2],...
        'VdistanceOrthogonalPlane',rand(10,1),'VdistanceOrigin',rand(10,1),...
        'XsimulationData',Xsd);
    h=Xobj.plotLines('Ldistance',true);
    close(gcf)
    h=Xobj.plotLines('Ldistance',false);
    close(gcf);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 
itest = itest+1;
try   
    
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 2]);
     %definition of required Linesampling object
    h=Xobj.plotLines('Ldistance',true);
    close h;    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12
itest = itest+1;
try   
    Xobj =  LineSamplingOutput('Sperformancefunctionname','Xrv3','Vnumpointline',[5 3 2]);
     %definition of required Linesampling object
    h=Xobj.plotLines('Ldistance',false);
    close h;    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 13
itest = itest+1;
try   
    Xobj =  LineSamplingOutput.load('Sfilename','LineSamplingOutput.mat');
    assert(length(Xobj.VdistancePlane)==Xobj.Nsamples,'LinesamplingOutput:load:UnitTest',...
        'LinesamplingOutput object is not correct! check the method *load*')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 
if nargout>0
    % Export name of the UnitTest
    varargout{1}='LineSamplingOutput';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of the LineSamplingOutput (' datestr(now) ')'])
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



