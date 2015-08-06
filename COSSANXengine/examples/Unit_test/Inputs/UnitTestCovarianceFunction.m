function varargout = UnitTestCovarianceFunction
%UNITTESTCovarianceFunction Summary of this function goes here
%   Detailed explanation goes here

% Test CovarianceFunction
% Perallocate memory

Ntest=6;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

StestPath = [OpenCossan.getCossanRoot '/examples/Unit_test/Inputs'];

%% 1. Create an empty object
itest = itest+1;
try
    Xobj = CovarianceFunction;
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2. Create an object
itest = itest+1;
try
    Xobj = CovarianceFunction('Lfunction',true,...
        'Cinputnames',{'t1','t2'},...
        'Spath',StestPath, ...
        'Sfile','expcovfunction.m',...
        'Coutputnames',{'fcov'},...
        'Lfunction',true,...
        'Liostructure',true);
    display(Xobj)
    Vtest(itest)=true;
    Cmess{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3. Wrong use of constructor - more than one output
itest = itest+1;
try
    Xobj = CovarianceFunction('Lfunction',true,...
        'Cinputnames',{'t1','t2'},...
        'Spath',StestPath, ...
        'Sfile','expcovfunction.m',...
        'Coutputnames',{'fcov' 'otherout'},...
        'Lfunction',true,...
        'Liostructure',true);
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4. Wrong use of constructor - more than two inputs
itest = itest+1;
try
    Xobj = CovarianceFunction('Lfunction',true,...
        'Cinputnames',{'t1','t2','other input'},...
        'Spath',StestPath, ...
        'Sfile','expcovfunction.m',...
        'Coutputnames',{'fcov'},...
        'Lfunction',true,...
        'Liostructure',true);
    display(Xobj)
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5. test method evaluate

itest = itest+1;
try
    Xobj = CovarianceFunction('Lfunction',true,...
        'Cinputnames',{'t1','t2'},...
        'Spath',StestPath, ...
        'Sfile','expcovfunction.m',...
        'Coutputnames',{'fcov'},...
        'Lfunction',true,...
        'Liostructure',true);
    MX= [0 0; 0 1; 1 0; 1 1];
    Vcov = Xobj.evaluate(MX);
    assert(length(Vcov)==length(MX),'Wrong dimension of the computed covariance')
    Vtest(itest)=true;
    Cmess{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6. wrong dimension of MX
itest = itest+1;
try
    MX= [0 0 1; 0 1 1; 1 0 1];
    Vcov = Xobj.evaluate(MX);
    Vcov = Xobj.evaluate(MX');
    Cmess{itest}='This test should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% no other tests are done, since they are already tests of Mio

%% finalize the unit test
if nargout>0
    % Export name of the UnitTest
    varargout{1}='CovarianceFunction';
    % Export Results of the UnitTest
    varargout{2}=Vtest;
    varargout{3}=Cmess;
    
else
    %% Show summary of the test
    disp('--------------------------------------------------------------------')
    disp([' Unit Test of the CovarianceFunction (' datestr(now) ')'])
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


return
