% %% Unit Test for the Object FailureProbability
% 
% function varargout=UnitTestFailureProbability
% % Test constructor
% Ntest=26;
% Vtest(1:Ntest)=false;
% Cmess=cell(Ntest,1);
% 
% %% Constructor and display
% itest=1;
% try
%     Xpf=FailureProbability('pf',0.01,'variancePf',2,'secondMoment',10,'Nsamples',10,'Smethod','UserDefined');
%     display(Xpf)
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% %% 2
% itest=itest+1;
% try
%     Xpf=FailureProbability('Sdescription','test2','pf',0.01,'variancePf',0.1,'Nsamples',10,'Smethod','UserDefined');
%     display(Xpf)
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% %% 3
% itest=itest+1;
% try
%     Xpf=FailureProbability('Sdescription','test3','pf',0.01,'variancePf',0.1,'Smethod','UserDefined');
%     display(Xpf)
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% %% 4
% itest=itest+1;
% try
%     Xpf=FailureProbability('Sdescription','test4','Smethod','UserDefined');
%     display(Xpf)
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% %% 5
% itest=itest+1;
% try
%     Xmc=MonteCarlo;
%     Xperfun=ProbabilisticModel;
%     Xpf=FailureProbability('CXmembers',{Xmc Xperfun},'Sdescription','Another test');
%     display(Xpf)
%     Cmess{itest}='This should fail';
% catch ME
%     Vtest(itest)=true;
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 6
% itest=itest+1;
% try
%     % Pass right objects
%     Xmc=MonteCarlo;
%     Xperfun=PerformanceFunction;
%     Xpf=FailureProbability('CXmembers',{Xmc Xperfun},'Sdescription','Another test');
%     display(Xpf)
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 7
% itest=itest+1;
% try
%     Xis=ImportanceSampling;
%     Xperfun=PerformanceFunction;
%     Xpf=FailureProbability('CXmembers',{Xis Xperfun},'Sdescription','Another test');
%     Cmess{itest}='This should fail';
% catch ME
%     Vtest(itest)=true;
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% %% 8
% itest=itest+1;
% try
%     Xis=ImportanceSampling;
%     Xperfun=PerformanceFunction;
%     Xpf=FailureProbability('CXmembers',{Xis Xperfun},'Sdescription','Another test','SweigthsName','out1');
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 9
% % Test with a SimulationData
% itest=itest+1;
% try
%     Xmc=MonteCarlo;
%     Tvalues.out1=5;
%     Xo=SimulationData('Tvalues',Tvalues);
%     XperFun=PerformanceFunction;
%     Xpf=FailureProbability('CXmembers',{Xmc XperFun Xo},'Sdescription','Another test','SperformanceFunction','out1');
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 10
% % Test with a addBatch
% itest=itest+1;
% try
%     Xpf=Xpf.addBatch('XsimulationOutput',Xo);
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 11
% itest=itest+1;
% try
%     Xpf=Xpf.addBatch('pf',0.1,'variancePf',0.3,'Nsamples',3);
%     Vtest(itest)=true;
%     Cmess{itest}='Object created';
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 12
% itest=itest+1;
% try
%     Xpf=Xpf.addBatch('pf',0.1,'variancePf',0.2);
%     Cmess{itest}='Object created';
% catch ME
%     Vtest(itest)=true;
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 13
% itest=itest+1;
% Nsamples=2;
% Nbatches=1000;
% 
% Vrand=rand(Nsamples*Nbatches,1)-1;
% Mrand = reshape(Vrand,Nsamples,Nbatches);
% Totalpf = mean(Vrand);
% Totalpf2 = mean(Vrand.^2);
% Totalstdpf= (Totalpf2-Totalpf^2)/(Nsamples*Nbatches);
% Totalvar = var(Vrand);
% pfs = mean(Mrand,1);
% pfs2 = mean(Mrand.^2,1);
% vars = var(Mrand,1); % Second Moment
% varpf= (pfs2-pfs.^2)/Nsamples; % Variance estimator
% 
% try
%     Xpf=FailureProbability('Sdescription','test','pf',pfs(1),'variancePf',varpf(1),'secondMoment',vars(1),'Smethod','none','Nsamples',Nsamples);
%     
%     for ib=2:Nbatches
%         Xpf=Xpf.addBatch('pf',pfs(ib),'variancePf',varpf(ib),'secondMoment',vars(ib),'Nsamples',Nsamples);
%     end
%     Cmess{itest}='Object constructed';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 14
% itest=itest+1;
% try
%     pfdelta=Totalpf-Xpf.pfhat;
%     assert(abs(pfdelta)<1e-4,'Error in the estimation of the pf')
%     Cmess{itest}='phhat check passed (equal weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 15
% itest=itest+1;
% try
%     vardelta=Totalstdpf.^2-Xpf.variancePfhat;
%     assert(abs(vardelta)<1e-2,'Error in the estimation of the variance of Pf')
%     Cmess{itest}='Variance check passed (equal weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 16
% itest=itest+1;
% try
%     delta=Totalvar-Xpf.variance;
%     assert(abs(delta)<(1e-2),'Error in the estimation of the variance')
%     Cmess{itest}='Variance check passed (equal weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% 
% %% 17
% itest=itest+1;
% Nsamples=100;
% Nbatches=4;
% 
% Vrand=(randn(Nsamples*Nbatches,1)<=0);
% 
% Mrand = reshape(Vrand,Nsamples,Nbatches);
% Totalpf = mean(Vrand);
% Totalstdpf= sqrt(var(Vrand)/(Nsamples*Nbatches));
% Totalvar = var(Vrand);
% pfs = mean(Mrand,1);
% vars = var(Mrand,1); % Second Moment
% varpf=vars/Nsamples;
% 
% 
% try
%     Xpf=FailureProbability('Sdescription','test','pf',pfs(1),'variancePf',varpf(1),'secondMoment',vars(1),'Smethod','none','Nsamples',Nsamples);
%     
%     for ib=2:Nbatches
%         Xpf=Xpf.addBatch('pf',pfs(ib),'variancePf',varpf(ib),'secondMoment',vars(ib),'Nsamples',Nsamples);
%     end
%     Cmess{itest}='Object constructed';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 18
% itest=itest+1;
% try
%     pfdelta=Totalpf-Xpf.pfhat;
%     assert(abs(pfdelta)<1e-4,'Error in the estimation of the pf')
%     Cmess{itest}='phhat check passed (equal weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 19
% itest=itest+1;
% try
%     vardelta=Totalstdpf-Xpf.stdPfhat;
%     assert(abs(vardelta)<1e-3,'Error in the estimation of the variance of Pf')
%     Cmess{itest}='Variance check passed (equal weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 20
% itest=itest+1;
% try
%     delta=Totalvar-Xpf.variance;
%     assert(abs(delta)<(1e-3),'Error in the estimation of the variance')
%     Cmess{itest}='Variance check passed (equal weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% 
% 
% 
% 
% %% 21 Different weights
% itest=itest+1;
% 
% TotS=length(Vrand);
% Samples = [0.1 0.1 0.25 0.45 0.05 0.05]*TotS;
% Ind=cumsum(Samples);
% 
% Totalpf=mean(Vrand);
% 
% pfs=[];
% vars=[];
% varpf=[];
% 
% pfs(1) = mean(Vrand(1:Ind(1))) ;
% vars(1) = var(Vrand(1:Ind(1))) ;
% varpf(1) = var(Vrand(1:Ind(1)))/Samples(1);
% 
% for i=2:length(Samples)
%     pfs(i) = mean(Vrand(Ind(i-1)+1:Ind(i))) ;
%     vars(i) = var(Vrand(Ind(i-1)+1:Ind(i))) ;
%     varpf(i) = var(Vrand(Ind(i-1)+1:Ind(i)))/Samples(i) ;
% end
% 
% 
% try
%     Xpf=FailureProbability('Sdescription','test','pf',pfs(1),'variancePf',varpf(1),'secondMoment',vars(1),'Smethod','none','Nsamples',Samples(1));
%     for ib=2:length(Samples)
%         Xpf=Xpf.addBatch('pf',pfs(ib),'variancePf',varpf(ib),'secondMoment',vars(ib),'Nsamples',Samples(ib));
%     end
%     Cmess{itest}='Object constructed';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 22
% itest=itest+1;
% try
%     pfdelta=Totalpf-Xpf.pfhat;
%     assert(abs(pfdelta)<1e-3,'Error in the estimation of the pf')
%     Cmess{itest}='phhat check passed (diffent weights)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 23
% itest=itest+1;
% try
%     vardelta=Totalvar-Xpf.variance;
%     assert(abs(vardelta)<1e-2,'Error in the estimation of the variance')
%     Cmess{itest}='Variance check passed (diffent weights)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% %% 24
% itest=itest+1;
% try
%     vardelta=Totalstdpf.^2-Xpf.variancePfhat;
%     assert(abs(vardelta)<1e-2,'Error in the estimation of the variance of Pf')
%     Cmess{itest}='Variance check passed (different weigths)';
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end
% 
% 
% %% 25 HJM test
% itest=itest+1;
% nsb = [120,30,80];
% 
% nb = numel(nsb);
% for k=1:nb
%     rv = randn(nsb(k),1);
%     Ineg = find(rv<=0);
%     Ipos = find(rv>0);
%     rv(Ineg) = 1; %#ok<FNDSB>
%     rv(Ipos) = 0; %#ok<FNDSB>
%     if(k==1)
%         x = rv;
%     else
%         x = [x;rv];
%     end
%     
%     my(k) = mean(rv);
%     va(k) = var(rv);
%     stdev_pf = sqrt(va(k)/nsb(k));
% end
% 
% nt0 = size(x,1);
% my0 = mean(x);
% va0 = var(x);
% stdev_pf0 = sqrt(va0/nt0);
% 
% nt1 = sum(nsb);
% s1 = 0;
% s2 = 0;
% for k=1:nb
%     s1 = s1+my(k)*nsb(k);
%     s2 = s2+va(k)*(nsb(k)-1)+my(k)^2*nsb(k);
% end
% my1 = s1/nt1;
% va1 = (s2-s1^2/nt1)/(nt1-1);
% stdev_pf1 = sqrt(va1/nt1);
% 
% try
%     Xpf=FailureProbability('Sdescription','test','pf', my(1),'variancePf',va(1)/nsb(1),'Smethod','none','Nsamples',nsb(1));
%     
%     for ib=2:nb
%         Xpf=Xpf.addBatch('pf', my(ib),'variancePf',va(ib)/nsb(ib),'Nsamples',nsb(ib));
%     end
%     
%     assert(Xpf.pfhat-my1<1e-4,[],'Error in the estimation of the pfhat')
%     assert(Xpf.stdPfhat-stdev_pf1<1e-4,[],'Error in the estimation of the stdPfhat')
%     Vtest(itest)=true;
% catch ME
%     Cmess{itest}=[ME.identifier ' -- ' ME.message];
% end

%% 26
itest=itest+1;
Nsamples=10;
Nbatches=4;

Vrand=(randn(Nsamples*Nbatches,1)<=0);

Mrand = reshape(Vrand,Nsamples,Nbatches);
Totalpf = mean(Vrand);
Totalstdpf= sqrt(var(Vrand)/(Nsamples*Nbatches));
Totalvar = var(Vrand);
pfs = mean(Mrand,1);
vars = var(Mrand,1); % Second Moment
varpf=vars/Nsamples;
Tvalues = cell2struct(num2cell(Vrand-0.5),'Vrand',2);

XsimOut=SimulationData('Tvalues',Tvalues);

Vindices=true(Nsamples,Nbatches);

istart=1;
for n=1:Nbatches
    iend=istart+Nsamples-1;
    XsimOutB(n)=XsimOut.split('Vindices',istart:iend);
    istart=iend+1;
end

try
    Xpf=FailureProbability('Smethod','MonteCarlo','XsimulationData',XsimOutB(1),'SperformanceFunction','Vrand');
    
    for ib=2:Nbatches
        Xpf=Xpf.addBatch('XsimulationData',XsimOutB(ib));
    end
    Cmess{itest}='Object constructed';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% Show summary of the test
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of FailureProbability  (' datestr(now) ')'])
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
varargout{1}='FailureProbability';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
