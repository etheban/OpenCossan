function varargout = UnitTestRandomVariableSet

%%  Unit test for RANDOM VARIABLE SET 

Ntest          = 19;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% Testing Random Variable Set

% 1
itest=1;
try
    Xrv1 = RandomVariable('Sdistribution','normal','mean',100,'std',15);
    Xrv2 = RandomVariable('Sdistribution','normal','mean',10,'cov',0.1);
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2});
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 2
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2','Xrv3'}); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 3
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2}); 
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 4
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[0.4,0.5;0.2,0.3]); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 5
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[0.4,0.5;0.5,0.4]); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 6
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[1.0,0.5;0.5,1.0]); 
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 7
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[1.0,0.5,0.3;0.5,1.0,0.3]); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 8
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[1.5,0.5;0.5,1.5]); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 9
itest=itest+1;
try
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[-1.5,0.5;0.5,-1.5]); 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 10
itest=itest+1;
try
    Xrv4  = RandomVariable('Sdistribution','normal','mean',9','std',1);
    Xrvs2 = RandomVariableSet('Xrv',Xrv4,'Nrviid',10);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 11
itest=itest+1;
try
    Xrvs = RandomVariableSet('Xrv',Xrv5,'Nrviid',10);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 12
itest=itest+1;
try
[Vpdf Vpdfrv] = evalpdf(Xrvs,'Mxsamples',[1 2]); %correct syntax
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 13
itest=itest+1;
try
    MX = map2physical(Xrvs,[0  0]);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 14
itest=itest+1;
try
    MU = map2stdnorm(Xrvs,[0  0; 1 1]);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 15
itest=itest+1;
try
    MU = cdf2stdnorm(Xrvs,MU);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 16
itest=itest+1;
try
    Msamples = sample(Xrvs,1000);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 17
itest=itest+1;
try
    MU = jacobianNataf(Xrvs,[0 0]);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 18
itest=itest+1;
try
    Xrv1 = RandomVariable('Sdistribution','unid','lowerbound',-2,'upperbound',2);
    Xrv2 = RandomVariable('Sdistribution','poisson','par1',2);
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[1,0.5;0.5,1]);
    Msamples = sample(Xrvs,1000);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 19
itest=itest+1;
try
    Xrv1 = RandomVariable('Sdistribution','unid','lowerbound',-2,'upperbound',2);
    Xrv2 = RandomVariable('Sdistribution','uniform','lowerbound',-2,'upperbound',2);
    Xrvs = RandomVariableSet('Cmembers',{'Xrv1','Xrv2'},'CXrv',{Xrv1,Xrv2},'MCorrelation',[1,0.5;0.5,1]);
    Msamples1 = sample(Xrvs,10000);
    Msamples2 = Msamples1.MsamplesPhysicalSpace;
    Msamples3 = Msamples1.MsamplesStandardNormalSpace;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


if nargout>0
% Export name of the UnitTest
varargout{1}='Unit Test of Random Variable Set';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else

%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the Random Variable Set (' datestr(now) ')'])
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









