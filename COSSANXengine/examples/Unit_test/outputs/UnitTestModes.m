%% Unit Test for the Modes object
function varargout=UnitTestModes

% Test constructor
Ntest=7;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

% The DesignPoint requires a ProbabilisticModel

%% 1
% Constructor and display
itest=1;
try
    Xout = Modes('Sdescription','Modes for Unit test','Mphi',rand(5,5),'Vlambda',rand(5,1));
    display(Xout)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2
itest=itest+1;
try
    Xout = Modes('Mphi',rand(5,5),'Vlambda',rand(5,1));
    display(Xout)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xout = Modes('Mphi',rand(21,1),'Vlambda',rand(1,1));
    display(Xout)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4
% this should fail
itest=itest+1;
try
    Xout = Modes('Vlambda',rand(3,1));
    display(Xout)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xout = Modes('Mphi',rand(21,1),'Vlambda',rand(21,1));
    display(Xout)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6
% the constructor should return an error
itest=itest+1;
try
    Xout = Modes('Mphi',rand(21,1),'Vlambda',rand(20,1));
    display(Xout)
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% test frf

%% 7

itest=itest+1;
try
    Vzeta = 0.01*ones(1,length(Xout.Vlambda));
    Tout  = Xout.frf('Sfrftype','acc','Vzeta',Vzeta,'Vforce',Xout.MPhi(5,:),'Vexcitationfrequency',1.0:0.01:3);
    display(Tout)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Show summary of the test
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of Modes (' datestr(now) ')'])
OpenCossan.cossanDisp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end



if nargout>0
% Export name of the UnitTest
varargout{1}='Modes';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
