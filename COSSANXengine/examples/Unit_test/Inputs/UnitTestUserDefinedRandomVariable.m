
function varargout=UnitTestUserDefinedRandomVariable

Ntest          = 25;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);


%%  Constructor

x=-5:.01:5;


Xfun = Function('Sexpression','normpdf(<&x&>)');
Xfun2 = Function('Sexpression','normcdf(<&x&>)');


% 1
iTest=1;
try
    Xrv01=UserDefRandomVariable
    Vtest(iTest)=true;
    Cmess{iTest}='Empty object';
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

% 2 - Vdata
iTest=2;
try
    Xrv01=UserDefRandomVariable('Vdata',[1 68 4 844 4 6 41 8 6 4 3  64  4  6 4]);
    Xrv02=UserDefRandomVariable('Vdata',[1 68 4 844 4 6 41 8 6 4 3  64  4  6 4]');
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%3 - pdf
iTest = iTest+1;
try
    Xrv02=UserDefRandomVariable('Sdistribution','pdf','Xpdf',Xfun','par1',1,'par2',50,'par3',.5);
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end

%4 - cdf
iTest = iTest+1;
try
    Xrv03=UserDefRandomVariable('Xcdf',Xfun2','Vdata',x,'Vtails',[0.01 .99]);
    Vtest(iTest)=true;
catch ME
    Cmess{iTest}=[ME.identifier ' -- ' ME.message];
end


%5 - display
iTest = iTest+1;
X = [Xrv01 Xrv02];
for i=1:length(X)
    try
        %         get(X(i),'Sdistribution')
        display(X(i));
    Vtest(iTest)=true;

    catch ME
        display(['test #' num2str(iTest) ' failed:  '  X(i).Sdistribution  ':   ' ME.identifier '  ' ME.message])
        Cmess{iTest}=[ME.identifier ' -- ' ME.message];

    end
end


% 6-7 - evalpdf
for i=1:length(X)
iTest = iTest+1;    
    try
        evalpdf(X(i),0.1);
    Vtest(iTest)=true;
    catch ME
        Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    end
end


%8-9 - sample
for i=1:length(X)
    iTest = iTest+1;
    try
        sample(X(i),'Nsamples', 10);
        sample(X(i),'Vsamples', [2 5]);
    Vtest(iTest)=true;
        
    catch ME
        Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    end
    
end


% 10-11 - map2physical

for i=1:length(X)
    iTest = iTest+1;
    try
        map2physical(X(i),0.1);
        map2physical(X(i),[0.1, 1, 2, 5]);
            Vtest(iTest)=true;
    catch ME
        Cmess{iTest}=[ME.identifier ' -- ' ME.message];
        
    end
end

%12-13 map2stdnorm

for i=1:length(X)
    iTest = iTest+1;
    try
        map2stdnorm(X(i),0);
        map2stdnorm(X(i),[-1 0.1, 1, 2, 5]);
         Vtest(iTest)=true;
    catch ME
        Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    end
end

%14-15 map2physical(map2stdnorm)
for i=1:length(X)
    iTest = iTest+1;
    if abs(map2stdnorm(X(i),map2physical(X(i),1))-1) < 1e-3
                 Vtest(iTest)=true;
    else
         Cmess{iTest}='map2physical(map2stdnorm) with too much unprecision';
    end
end

%16-17 cdf2physical

for i=1:length(X)
    iTest = iTest+1;
    try
        cdf2physical(X(i),0.5);
        cdf2physical(X(i),[.5 .8]);
        Vtest(iTest)=true;
        
    catch ME
        Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    end
    
end

%18-19 cdf2physical w/ invalid argument
for i=1:length(X)
    iTest = iTest+1;
    try
        cdf2physical(X(i),-0.5);
    catch ME
        if strncmp(ME.identifier,'openCOSSAN',6) %error message from cossan
            Vtest(iTest)=true;
        else
            Cmess{iTest}=[ME.identifier ' -- ' ME.message];
        end
    end
end

% 20 cdf2physical
iTest = iTest+1;
try
    RandomVariable.cdf2stdnorm(0.5);
    RandomVariable.cdf2stdnorm([.5 .8]);
            Vtest(iTest)=true;

catch ME
            Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    
end


% 21
iTest = iTest+1;
try
    RandomVariable.cdf2stdnorm(-0.5);
catch ME
    if strncmp(ME.identifier,'openCOSSAN',6) %error message from cossan
            Vtest(iTest)=true;
    else
            Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    end

end



%22-23 physical2cdf
for i=1:length(X)
    iTest = iTest+1;

    try
        physical2cdf(X(i),0.5);
        physical2cdf(X(i),[.5 .8]);
            Vtest(iTest)=true;

    catch ME
            Cmess{iTest}=[ME.identifier ' -- ' ME.message];
    end
end

%24-25 physical2cdf(cdf2physical)

for i=1:length(X)
            iTest = iTest+1;

    if abs(physical2cdf(X(i),cdf2physical(X(i),.5))-.5) < 1e-3
            Vtest(iTest)=true;
    else
         Cmess{iTest}='map2physical(map2stdnorm) with too much unprecision';
    end
end


if nargout>0
% Export name of the UnitTest
varargout{1}=['Unit Test of Userdef RANDOM VARIABLES (' datestr(now) ')'];
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the USER DEFINED RANDOM VARIABLES (' datestr(now) ')'])
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
end



