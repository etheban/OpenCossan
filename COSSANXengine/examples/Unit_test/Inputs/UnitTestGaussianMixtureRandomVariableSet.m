% Unit testing of GaussianMixtureRandomVariableSet
	
function varargout=UnitTestGaussianMixtureRandomVariableSet
	
Ntest=13;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);
itest=0;

%% Prepare the input

N      = 20;
alpha  = 0.05;
c      = 1-alpha^(1/N);
A1     = [2 -1.8 0; -1.8 2 0; 0 0 1];
det_A1 = det(A1);
A2     = [2 -1.9 1.9; -1.9 2 -1.9; 1.9 -1.9 2];
det_A2 = det(A2);
A3     = [2 1.9 0;1.9 2 0; 0 0 1];
det_A3 = det(A3);
p      = [0.03 0.95 0.02];
MU     = [4 4 -4;-3 -5 4;4 -4 0];
SIGMA  = cat(3,A1,A2,A3);
obj    = gmdistribution(MU,SIGMA,p);
r      = random(obj,N);   
r_org  = r;
MXorig = [5 2 1; 2 0 1];

%% 1 Create an empty object

itest = itest+1;
try
    Xobj = GaussianMixtureRandomVariableSet; %#ok<*NASGU>
    Vtest(itest)=true;
    Cmess{itest}='Create an empty object';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 2 Check display

itest = itest+1;
try
    display(Xobj);
    Cmess{itest}='Checking display';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3 Mdataset not provided 

itest = itest+1;
try
    Xobj = GaussianMixtureRandomVariableSet('Cmembers',{'X1' 'X2' 'X3'});
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4 Constructing the object with input parameters 

itest = itest+1;
try
    Xobj = GaussianMixtureRandomVariableSet('MdataSet',r,'Cmembers',{'X1' 'X2' 'X3'});
    Vtest(itest)=true;
    Cmess{itest}='Creating object with the input parameters';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 5 Checking methods 

itest = itest+1;
try
     MS = map2stdnorm(Xobj,MXorig);
     Vtest(itest)=true;
     Cmess{itest}='Checking map2stdnorm method';
catch ME

    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 Checking methods 

itest = itest+1;
try
     MX = map2physical(Xobj,MS);
     Vtest(itest)=true;
     Cmess{itest}='Checking map2physical method';
catch ME

    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 7 Checking methods 

itest = itest+1;
try
     MU = physical2cdf(Xobj,MXorig);
     Vtest(itest)=true;
     Cmess{itest}='Checking physical2cdf method';
catch ME

    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8 Checking methods 

itest = itest+1;
try
     MU = stdnorm2cdf(Xobj,MS);
     Vtest(itest)=true;
     Cmess{itest}='Checking stdnorm2cdf method';
catch ME

    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9 Checking methods 

itest = itest+1;
try
     MX = cdf2physical(Xobj,MU);
     Vtest(itest)=true;
     Cmess{itest}='Checking cdf2physical method';
catch ME

    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 10 Checking methods 

itest = itest+1;
try
     MS = cdf2stdnorm(Xobj,MU);
     Vtest(itest)=true;
     Cmess{itest}='Checking cdf2stdnorm method';
catch ME

    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 11 Correlation matrix with inconsistent size 

itest = itest+1;
try
    Mcorr = [0.5 0.5 0.5 1; 0.5 0.5 0.5 1; 0.5 0.5 0.5 1]; 
    Xobj  = GaussianMixtureRandomVariableSet('MdataSet',r,'Cmembers',{'X1' 'X2' 'X3'},...
            'Mcorrelation',Mcorr);
    Cmess{itest}='should fail - inconsistent size of Mcorrelation not allowed';   
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 Correlation matrix with inconsistent size 
itest = itest+1;
try
    Mcorr = [0.5 0.5 0.5 1; 0.5 0.5 0.5 1; 0.5 0.5 0.5 1]; 
    Xobj  = GaussianMixtureRandomVariableSet('MdataSet',r,'Cmembers',{'X1' 'X2' 'X3'},...
            'Mcovariance',Mcorr);
    Cmess{itest}='should fail - inconsistent size of Correlation matrix';   
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 12 

itest = itest+1;
try
    Xobj  = GaussianMixtureRandomVariableSet('MdataSet',r,'Cmembers',{'X1' 'X2' 'X3'},...
            'Vweights',[0.2 0.3 0.4]);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Finish the test

if nargout>0
% Export name of the UnitTest
varargout{1}='GaussianMixtureRandomVariableSet';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the GaussianMixtureRandomVariableSet (' datestr(now) ')'])
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



