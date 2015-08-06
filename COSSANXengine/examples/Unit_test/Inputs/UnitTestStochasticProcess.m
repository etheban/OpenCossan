%% unit test of Stochastic Process
function varargout=UnitTestStochasticProcess

Ntest          = 18;
Vtest(1:Ntest) = false;
Cmess          = cell(Ntest,1);

%% 1 - CoVfunction
itest=1;              
try
    
    Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
        'Lfunction',true,'Liostructure',true,'Liomatrix',false,...
        'Cinputnames',{'t1','t2'},... % Define the inputs
        'Spath',fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Inputs'),...
        'Sfile','expcovfunction.m',... % external file
        'Coutputnames',{'fcov'}); % Define the outputs
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Crk{itest}='';
end

%% 2 - constructor

itest=itest+1;
try
    
   Vtime =  0:0.25:2;
SP1    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xcovfun,'Mcoord',Vtime);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Crk{itest}='';
end

%% 3 - KL terms
itest=itest+1;
try
SP1    = KL_terms(SP1,'NKL_terms',3,'LcovarianceAssemble',true);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Crk{itest}='';
end


% 4 - LcovarianceAssemble
itest=itest+1;
try
SP1    = KL_terms(SP1,'NKL_terms',3,'LcovarianceAssemble',false);
Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Crk{itest}='NKL_terms > size(Mcovmatrix)';
end

%% 5 - KL terms w/ too many terms
itest=itest+1;
try
    SP1    = KL_terms(SP1,'NKL_terms',30,'LcovarianceAssemble',true);
    Crk{itest}='NKL_terms > size(Mcovmatrix), error is not intercepted';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 6 - sample 
itest=itest+1;
try
    ds1 = SP1.sample('Nsamples',3);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Crk{itest}='';
end

%% invalid CoVfunction
% 7 - only one input argument
itest=itest+1;
try
    Xinvalidcovfunction1  = CovarianceFunction('Sdescription','covariance function', ...
        'Lfunction',true,'Liostructure',true,'Liomatrix',false,...
        'Cinputnames',{'t1'},... % Define the inputs
        'Spath',fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Inputs'),...
        'Sfile','invalidcovfunction1.m',... % external file
        'Coutputnames',{'fcov'}); % Define the outputs
    SP2    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xinvalidcovfunction1,'Mcoord',Vtime);
    SP2    = KL_terms(SP2,'NKL_terms',3,'LcovarianceAssemble',true);
    Crk{itest}='Error is not intercepted';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%negative covariance
itest=itest+1;
try
    Xinvalidcovfunction2  = CovarianceFunction('Sdescription','covariance function', ...
        'Lfunction',true,'Liostructure',true,'Liomatrix',false,...
        'Cinputnames',{'t1'},... % Define the inputs
        'Spath',fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Inputs'),...
        'Sfile','invalidcovfunction2.m',... % external file
        'Coutputnames',{'fcov'}); % Define the outputs
    SP3    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xinvalidcovfunction2,'Mcoord',Vtime);
    SP3    = KL_terms(SP3,'NKL_terms',3,'LcovarianceAssemble',true);
    
   
    Crk{itest}='Error is not intercepted';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    
end




% 9 - inexistant covariance fct
itest=itest+1;
try
    Xinvalidcovfunction3  = CovarianceFunction('Sdescription','covariance function', ...
        'Lfunction',true,'Liostructure',true,'Liomatrix',false,...
        'Cinputnames',{'t1'},... % Define the inputs
        'Spath',fullfile(OpenCossan.getCossanRoot,'/examples/Unit_test/Inputs'), ...
        'Sfile','fleubleubleu.m',... % external file
        'Coutputnames',{'fcov'}); % Define the outputs
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 10 - StochasticProcess object by passing the covariance matrix

itest=itest+1;
try
    Vtime = [1 2 3];
    
    Mcovariance =eye(3);
    
    SP3    = StochasticProcess('Sdistribution','normal','Vmean',5,'Mcovariance',Mcovariance,'Mcoord',Vtime);
    SP3    = KL_terms(SP3,'NKL_terms',2);
    
    ds3 = SP3.sample('Nsamples',1);
    Vtest(itest)=true;
    Crk{itest}='';
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
    Crk{itest}='';
end



%% 11 invalid matrix
itest=itest+1;
try
    Vtime = [1 2 3];
    
    Mcovariance =[1  .9 -.9;
                  .9  1 .9;...
                  -.9 .9  1    ];
    
    SP3    = StochasticProcess('Sdistribution','normal','Vmean',5,'Mcovariance',Mcovariance,'Mcoord',Vtime);
    
    SP3    = KL_terms(SP3,'NKL_terms',2);
    SP3    = KL_terms(SP3,'NKL_terms',3);
    
    ds3 = SP3.sample('Nsamples',1);

catch ME
    Crk{itest}='Mcovariance with a negative eigenvalue';
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 12 - invalid matrix
itest=itest+1;
try
    Vtime = [1 2 3];
    
    Mcovariance =eye(4);
    
    SP3    = StochasticProcess('Sdistribution','normal','Vmean',5,'Mcovariance',Mcovariance,'Mcoord',Vtime);
    
    SP3    = KL_terms(SP3,'NKL_terms',2);
    SP3    = KL_terms(SP3,'NKL_terms',3);
    
    ds3 = SP3.sample('Nsamples',1);
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 13
% triangular matrix
itest=itest+1;
try
    Vtime = [1 2 3];
    
    Mcovariance =[1  .2 -.1;
                  0  1 .1;...
                  0  0   1 ];
    
    SP3    = StochasticProcess('Sdistribution','normal','Vmean',5,'Mcovariance',Mcovariance,'Mcoord',Vtime);
    
    SP3    = KL_terms(SP3,'NKL_terms',2);
    SP3.Mcovariance
    ds3 = SP3.sample('Nsamples',1);
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% Additional tests implemented by EP
%% 14
itest=itest+1;
try
    X1  = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
    Xrvs1     = RandomVariableSet('CSmembers',{'X1'},'CXmembers',{X1});

    Xmio=Mio('Cinputnames',{'X1','SP3'},... % Define the inputs 
          'Sscript', 'for n=1:length(Tinput), Toutput(n).out1 = Tinput(n).X1 + max(Tinput(n).SP3.Vdata); end', ...
          'Coutputnames',{'out1'}); % Define the outputs

    
    Vtime = [1 2 3];
    Mcovariance =[1  .2 -.1;
                  0  1 .1;...
                  0  0   1 ];
    
    SP3    = StochasticProcess('Sdistribution','normal','Vmean',5,'Mcovariance',Mcovariance,'Mcoord',Vtime);
    SP3    = KL_terms(SP3,'NKL_terms',2);
    
    Xinput=Input('CSmembers',{'SP3' 'Xrvs1'},'CXmembers',{SP3 Xrvs1});
    Xev=Evaluator('Xmio',Xmio);
    Xmodel=Model('Xinput',Xinput,'Xevaluator',Xev);
    Xout=Xmodel.deterministicAnalysis;
    display(Xout)

    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 15
itest=itest+1;
try
    Xmc=MonteCarlo('Nsamples',5);
    Xout=Xmc.apply(Xmodel);
    display(Xout)

    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 16
itest=itest+1;
try
    Xds=DesignOfExperiments;
    Xout=Xds.apply(Xmodel);
    display(Xout)

    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 17
itest=itest+1;
try
    Xds=DesignOfExperiments('SdesignType','UserDefined','MdoeFactors',[2;5;8;10]);
    Xout=Xds.apply(Xmodel);
    display(Xout)

    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 18. Samples object with Dataseries
itest=itest+1;
try
    Xds1=Dataseries('Mcoord',1:10,'Mdata',rand(1,10),'Sindexname','index','Sindexunit','myUnits');
    Xds2=Dataseries('Mcoord',1:10,'Mdata',rand(1,10),'Sindexname','index','Sindexunit','myUnits');
    Xsample  = Samples('Xdataseries',[Xds1 Xds2]);

    assert(Xsample.Nsample==1,'UnitTest','Wrong number of samples')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end



%% 19. Samples object with Dataseries
itest=itest+1;
try
    Xds1=Dataseries('Mcoord',1:10,'Mdata',rand(1,10),'Sindexname','index','Sindexunit','myUnits');
    Xds2=Dataseries('Mcoord',1:10,'Mdata',rand(1,10),'Sindexname','index','Sindexunit','myUnits');
    Xsample  = Samples('Xdataseries',[Xds1;Xds2]);

    assert(Xsample.Nsample==2,'UnitTest','Wrong number of samples')
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

disp('  ')
disp('  ')
disp('--------------------------------------------------------------------')
disp([' Unit Test of StochasticProcess (' datestr(now) ')'])
disp('--------------------------------------------------------------------')
Cmess{itest+1}=[]; Crk{itest+1}=[];
for i=1:length(Vtest)
    if Vtest(i)
        disp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);
    else
        disp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} '), (' Crk{i} ')' ]);
    end
end


if nargout>0
% Export name of the UnitTest
varargout{1}='StochasticProcess';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
