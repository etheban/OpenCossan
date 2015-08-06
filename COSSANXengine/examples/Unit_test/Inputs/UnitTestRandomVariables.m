function varargout = UnitTestRandomVariables

%%  Unit test for RANDOM VARIABLES 

Ntest          = 61;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% Testing Random Variables

% 1
itest=1;
try
    Xobj = RandomVariable('Sdistribution','normal','mean',100,'std',15); 
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 2
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','normal','mean',100,'std',0);   
    display(Xobj)
    Cmess{itest}='This should fail!';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 3
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','normal','mean',100,'CoV',0);   
    display(Xobj)
    Cmess{itest}='This should fail!';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 4
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','normal','mean',100,'std',-2);   
    display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 5
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','normal','mean',100,'cov',-2); 
    display(Xobj)
    Cmess{itest}='This should fail!';      
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 6
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','lognormal','mean',100,'cov',0.10);   
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 7
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','lognormal','Cpar',{'par1',10;'par2',0.1});   
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 8
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','uniform','Cpar',{'par1',1;'par2',2});   
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 9
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','uniform','Cpar',{'par1',3;'par2',2});   
    display(Xobj)
    Cmess{itest}='This should fail!';    
    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 10
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','uniform','Cpar',{'par1',3});   
    display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 11
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','exponential','Cpar',{'par1',3;'par2',2});   
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 12
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','rayleigh','mean',100,'cov',0.10);   
    display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 13
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','rayleigh','mean','cov',100,0.10);   
    display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 14
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','small-i','mean',100,'cov',0.10);  
        display(Xobj)

    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 15
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','large-i','mean',100,'cov',0.10);   
        display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 16
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','gumbel','mean',100,'cov',0.10); 
        display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 17
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','weibull','mean',100,'cov',0.10);   
    display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 18
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','weibull','Cpar',{'par1',3;'par2',2});   
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 19
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','beta','Cpar',{'par1',3;'par2',2});   
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 20
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','gamma','Cpar',{'par1',3;'par2',2}); 
        display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 21
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','f','Cpar',{'par1',3;'par2',2});
        display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 22
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','student','Cpar',{'par1',3;'par2',2});  
        display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 23
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','logistic','Cpar',{'par1',3;'par2',2}); 
        display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 24
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','logistic','Cpar',{'par1',3;}); 
    display(Xobj)
%     Cmess{itest}='This should fail! but with a meaningful message';    
catch ME
    Vtest(itest)=true;    
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


% 25
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','normal','meanstd',100,'std',10);
        display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 26
itest=itest+1;
try
    Xobj=RandomVariable('Sdistribution','normalzz','mean',100,'std',10);
        display(Xobj)
    Cmess{itest}='This should fail!';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 27 DISCRETE RV - UNIFORM
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','unid','lowerbound',-2,'upperbound',2);
            display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 28 DISCRETE RV - UNIFORM
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','unid','lowerbound',3,'upperbound',2);
            display(Xobj)
       Cmess{itest}='This should fail!';      
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 29 DISCRETE RV - UNIFORM
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','unid','lowerbound',3);
            display(Xobj)
   Cmess{itest}='This should fail!';          
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 30 DISCRETE RV - UNIFORM
itest=itest+1;
try
    Xobj = RandomVariable('mean',5,'std',1);
            display(Xobj)
    Cmess{itest}='it should not be possible to create an RV without specifiying the distribution';
catch ME
    Vtest(itest)=true;
        Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 31 DISCRETE RV - UNIFORM
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','unid','mean',5,'std',1);
     display(Xobj)
     Cmess{itest}='This should fail!'; 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 32 DISCRETE RV - POISSON
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','poisson','par1',12);
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 33 DISCRETE RV - POISSON
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','poisson');
         display(Xobj)
     Cmess{itest}='This should fail!'; 
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 34 DISCRETE RV - POISSON
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','poisson','par1',0);
         display(Xobj)
     Cmess{itest}='This should fail!'; 
catch ME
     Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 35 DISCRETE RV - POISSON
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','poisson','mean',0,'std',1);
         display(Xobj)
     Cmess{itest}='This should fail!'; 
catch ME
     Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% 36
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','unid','lowerbound',3,'upperbound',2);
     display(Xobj)
     Cmess{itest}='This should fail!'; 
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Vtest(itest)=true;
end


% 37 - sample
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','poisson','par1',2);
    a = sample(Xobj,'Nsamples',10);
    display(a)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

% Test getPDF for all the supported distribution
%% 38 weibull   
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','weibull','par1',2,'par2',0.2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='weibull test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 39 chi2   
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','chi2','par1',2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='chi2 test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 40 betaDistribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','beta','par1',2,'par2',0.2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='betaDistribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 41 gamma Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','Gamma','par1',2,'par2',0.2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='Gamma Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 42 exponential Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','exponential','par1',2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='exponential Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 43 generalizedPareto Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','generalizedPareto','par1',0,'par2',0.4,'par3',1);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='generalizedPareto Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 44 large_I Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','large-i','mean',2,'std',4);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='large_I Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 45 lognormal Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','lognormal','par1',2,'par2',1);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='lognormal Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 46 logistic Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','logistic','par1',2,'par2',2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='logistic Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 47 normal Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','normal','mean',2,'std',1.2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='normal Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 48 uniform Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','uniform','par1',2,'par2',4);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='uniform Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 49 rayleigh Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','rayleigh','par1',2,'par2',4);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='rayleigh Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 50 small-i Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','small-i','mean',2,'std',4);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='small-i Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 51 uniformdiscrete Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','uniformdiscrete','par1',2,'par2',4);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='uniformdiscrete Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 52 student Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','student','par1',5);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='student Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 53 poisson Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','poisson','par1',5);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='poisson Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 54 truncnormal Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','truncnormal','lowerBound',2,'upperBound',4,'par1',2,'par2',2);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='truncnormal Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 55 f Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','f','par1',5,'par2',3);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='f Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 56 binomial Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','binomial','par1',100,'par2',0.9);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='binomial Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end  

%% 57 geometric Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','geometric','par1',25,'par2',0.03);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='geometric Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end  

%% 58 hypergeometric Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','hypergeometric','par1',1000,'par2',50,'par3',20);
   [~, B]=Xobj.getPdf('Nsamples',100);
%     Cmess{itest}='This is taking forever!!!!';
    Vtest(itest)=true;
    Cmess{itest}='hypergeometric Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end  

%% 59 negativebinomial Distribution 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','negativebinomial','par1',3,'par2',0.5);
    [~, B]=Xobj.getPdf;
    Vtest(itest)=true;
    Cmess{itest}='negativebinomial Distribution test getPDF';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 

%% 60 Check Fitting distribution
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','normal','Vdata',rand(100,1));
    display(Xobj)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 

%% 61 randomVariable2designVariable 
itest=itest+1;
try
    Xobj = RandomVariable('Sdistribution','negativebinomial','par1',3,'par2',0.5);
    Xobj.randomVariable2designVariable
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end 



if nargout>0
% Export name of the UnitTest
varargout{1}=['Unit Test of RANDOM VARIABLES (' datestr(now) ')'];
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;

else
%% Show summary of the test
disp('--------------------------------------------------------------------')
disp([' Unit Test of the RANDOM VARIABLES (' datestr(now) ')'])
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








