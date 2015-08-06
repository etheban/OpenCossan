%% Unit Test for the SfemOutput object

function varargout=UnitTestSFEMoutput
% Test constructor
Ntest=8;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% 1
% Constructor and empty object
itest=1;
try
    Xsfemoutput  = SfemOutput('Sdescription','Test1'); %#ok<*NASGU>
    display(Xsfemoutput)
    Cmess{itest}='empty SFEM output object created successfully';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2 
% Pass only a SFEM object
load([OpenCossan.getCossanRoot '/examples/Unit_test/outputs/Xsfem'])
itest=itest+1;
try
    Xsfemoutput = SfemOutput('Sdescription','Test2','XSfemObject',Xsfem);
    Vtest(itest)=true;
    Cmess{itest}='Object created successfully';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
load([OpenCossan.getCossanRoot '/examples/Unit_test/outputs/Xsfemout'])
Xsfemout.XSfemObject=Xsfem;
itest=itest+1;
try
    getResponse(Xsfemout,'Sresponse','max');
    Vtest(itest)=true;
    Cmess{itest}='Using getResponse method with max option';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 4 
itest=itest+1;
try
    getResponse(Xsfemout,'Sresponse','specific','MresponseDOFs',[150 3;150 1]);
    Vtest(itest)=true;
    Cmess{itest}='Using getResponse method with specific option';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5    
itest=itest+1;
try
    Xsfemout = getResponse(Xsfemout,'Sresponse','specific','MresponseDOFs',[150 3]);
    Xsfemout.prepareReport;
    Vtest(itest)=true;
    Cmess{itest}='testing prepareReport method';
    delete('*.txt');
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 
itest=itest+1;
try
    getResponse(Xsfemout,'Sresponse','all');
    Vtest(itest)=true;
    Cmess{itest}='Using getResponse method with all option';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7 
itest=itest+1;
try
    getResponse(Xsfemout,'Sresponse','specific','MresponseDOFs',[150 3 2]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
try
    getResponse(Xsfemout,'Sresponse','dummy');
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of SFEMoutput  (' datestr(now) ')'])
disp('--------------------------------------------------------------------')
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='SFEMoutput';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
