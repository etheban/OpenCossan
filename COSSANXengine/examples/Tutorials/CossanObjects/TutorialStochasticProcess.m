%% TUTORIALSTOCHASTICPROCESS
%
% In this tutorial it is shown how to construct a StochasticProcess object and
% how to use it as an input to a dynamic, transient FE-analysis
%
%
% See Also:
% http://cossan.cfd.liv.ac.uk/wiki/index.php/@StochasticProcess
%
% $Copyright~1993-2011,~COSSAN~Working~Group,~University~of~Innsbruck,~Austria$
% $Author:~Barbara~Goller$


% Reset the random number generator in order to always obtain the same results.
% DO NOT CHANGE THE VALUES OF THE SEED
OpenCossan.resetRandomNumberGenerator(51125)

% copy FE-input file with COSSAN-identifiers to working directory
StutorialPath = fileparts(which('TutorialStochasticProcess.m'));
copyfile(fullfile(StutorialPath,'Connector','ABAQUS','crane.cossan'),...
    fullfile(OpenCossan.getCossanWorkingPath),'f');
copyfile(fullfile(StutorialPath ,'Connector','ABAQUS','Readresults.py'),...
    fullfile(OpenCossan.getCossanWorkingPath),'f');
copyfile(fullfile(StutorialPath, 'Connector','NASTRAN','plate_SP.cossan'),...
    fullfile(OpenCossan.getCossanWorkingPath),'f');

%% 1D-STOCHASTIC PROCESS

% Define covariance matrix
Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
    'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'t1','t2'},... % Define the inputs
    'Sscript', 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end', ...
    'Coutputnames',{'fcov'}); % Define the outputs


% Define stochastic process
Vtime =  linspace(0,5,101); % time steps
SP1    = StochasticProcess('Sdistribution','normal',...
    'Vmean',0,...
    'Xcovariancefunction',Xcovfun,...
    'Mcoord',Vtime,...
    'Lhomogeneous',true);
SP1    = KL_terms(SP1,'NKL_terms',30,'LcovarianceAssemble',true); % compute 30 Karhunen-Loeve terms

% Generate 3 samples of stochastic process
ds1 = SP1.sample('Nsamples',3);
f1=figure;
plot(ds1.Xdataseries(1,1).Mcoord,cat(1,ds1.Xdataseries.Vdata));
grid on
xlabel('time [s]')
ylabel('function value')
legend('sample no. 1','sample no. 2','sample no. 3')
title('3 samples of 1D-stochastic process')

% compare covariance matrix of samples and analytical covariance matrix
ds1 = SP1.sample('Nsamples',1000); % use 1000 samples
Mcov = cov(cat(1,ds1.Xdataseries.Vdata));
f2=figure;
mesh(ds1.Xdataseries(1,1).Mcoord,ds1.Xdataseries(1,1).Mcoord,Mcov)
xlim([0 5])
ylim([0 5])
xlabel('\Delta t')
ylabel('\Delta t')
zlabel('covariance')
title('Covariance matrix of stationary 1D-stochastic process estimated from 1000 samples')


%% Define StochasticProcess object by passing the covariance function without assembling the matrix
Vtime =  0:0.1:5;
SP2    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xcovfun,'Mcoord',Vtime,'Lhomogeneous',false);
SP2    = KL_terms(SP2,'NKL_terms',30,'LcovarianceAssemble',true);

% Define StochasticProcess object by passing the covariance matrix
Vtime = 0:0.1:5;
[Mtime1, Mtime2]= meshgrid(Vtime,Vtime);
Vcovariance = evaluate(Xcovfun,[Mtime1(:); Mtime2(:)]);
Mcovariance = reshape(Vcovariance,length(Vtime),length(Vtime));

SP3    = StochasticProcess('Sdistribution','normal','Vmean',5,'Mcovariance',Mcovariance,'Mcoord',Vtime);
SP3    = KL_terms(SP3,'NKL_terms',30);

f3=figure;
mesh(Mtime1,Mtime2,Mcovariance)
xlim([0 5])
ylim([0 5])
xlabel('\Delta t')
ylabel('\Delta t')
zlabel('covariance')
title('Analytical covariance matrix of stationary 1D-stochastic process')


%% close figures and validate solution

close(f1);
close(f2);
close(f3);

% samples of SP1
Vdata = ds1.Xdataseries(1,1).Vdata;
assert(all(abs(Vdata(1:10)-[ -1.2414   -1.2026   -1.0508   -0.8726...
    -0.7642   -0.7896   -0.9509   -1.1840   -1.3871   -1.4710])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution ds1 does not match.');

% eigenvalues of covariance matrix of SP2
assert(all(abs(SP2.VcovarianceEigenvalues(1:10)'-[9.4080, 7.8957, 6.1779, 4.6928, ...
    3.5571, 2.7295, 2.1327, 1.6992, 1.3794, 1.1393])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution SP2 does not match.');

% eigenvalues of covariance matrix of SP3
assert(all(abs(SP3.VcovarianceEigenvalues(1:10)'-[9.4080, 7.8957, 6.1779, 4.6928, ...
    3.5571, 2.7295, 2.1327, 1.6992, 1.3794, 1.1393])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution SP3 does not match.');


%% 2D-STOCHASTIC PROCESS

% Define covariance matrix
Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
    'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'x1','x2'},... % Define the inputs
    'Sscript','sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*sqrt((Tinput(i).x2-Tinput(i).x1)''*(Tinput(i).x2-Tinput(i).x1))); end', ...
    'Coutputnames',{'fcov'}); % Define the outputs

% Define StochasticProcess object by passing the covariance function and assembling the matrix
[Vx ,Vy]  = meshgrid(0:0.5:5,0:0.5:4);
Vx = Vx(:)';
Vy = Vy(:)';
Vxy = [Vx; Vy];
SP4    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xcovfun,...
    'CScoordinateNames',{'x','y'},...
    'CScoordinateUnits',{'',''},...
    'Mcoord',Vxy);
SP4    = KL_terms(SP4,'NKL_terms',30,'LcovarianceAssemble',true);

% Generate samples of the stochastic process and visualize them
ds1 = SP4.sample('Nsamples',1000);
plot(ds1.Xdataseries(1,1).Vdata)

% compare covariance matrix computed from samples and analytical one

[Mindex1, Mindex2]= meshgrid(1:length(Vx),1:length(Vx));
Vcov_true = evaluate(Xcovfun,[Vxy(:,Mindex1(:));Vxy(:,Mindex2(:))]);
Mcov_true = reshape(Vcov_true,length(Vxy),length(Vxy));
f2=figure;
mesh(Mindex1,Mindex2,Mcov_true)
view(0,90)
title('Analytical covariance matrix of homogeneous 2D random field')
xlabel('\Delta x')
ylabel('\Delta y')

Mcov_stat = cov(cat(1,ds1.Xdataseries.Vdata));
f3=figure;
mesh(Mindex1,Mindex2,Mcov_stat)
view(0,90)
title('Covariance matrix of homogeneous, 2D random field estimated from 1000 samples')

%% close figures and validate solution (part of samples of SP1)

close(f1)
close(f2)
close(f3)

Vdata = ds1.Xdataseries(1,1).Vdata;
assert(all(abs(Vdata(1:10)-[-0.0158   -0.1916   -0.5984   -0.8918  ...
    -0.5872    0.2443    0.8785    0.7907    0.2702   -0.0833])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution ds1 does not match.');


%% 3D-STOCHASTIC PROCESS

Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
    'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'x1','x2'},... % Define the inputs
    'Sscript','sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*sqrt((Tinput(i).x2-Tinput(i).x1)''*(Tinput(i).x2-Tinput(i).x1))); end',...
    'Coutputnames',{'fcov'}); % Define the outputs

% Define 3d-coordinates for random field
[Vx, Vy, Vz]  = meshgrid(0:0.5:5,0:0.5:5,0:0.5:5);
Vx = Vx(:)';
Vy = Vy(:)';
Vz = Vz(:)';
Vxyz = [Vx; Vy; Vz];

% Define random field
SP5    = StochasticProcess('Sdistribution','normal','Vmean',0,...
    'Xcovariancefunction',Xcovfun,'Mcoord',Vxyz,...
    'CScoordinateNames',{'x','y','z'},...
    'CScoordinateUnits',{'','',''});
SP5    = KL_terms(SP5,'NKL_terms',30,'LcovarianceAssemble',true);

% Generate 1 sample
ds1 = SP5.sample('Nsamples',1);

% visualization of volumetric evolution using 4 slices
[Vx, Vy, Vz]  = meshgrid(0:0.5:5,0:0.5:5,0:0.5:5);
Vv = ds1.Xdataseries(1,1).Vdata;
Vv = reshape(Vv, size(Vx));
xmin = min(Vx(:));
ymin = min(Vy(:));
zmin = min(Vz(:));

xmax = max(Vx(:));
ymax = max(Vy(:));
zmax = max(Vz(:));

% create 45 degrees slice
f1 = figure;
hslice = surf(linspace(xmin,xmax,20),...
    linspace(ymin,ymax,20),...
    2.5+zeros(20));
rotate(hslice,[-1,0,0],-45)
xd = get(hslice,'XData');
yd = get(hslice,'YData');
zd = get(hslice,'ZData');
delete(hslice)

h = slice(Vx,Vy,Vz,Vv,xd,yd,zd);
set(h,'FaceColor','interp',...
    'EdgeColor','none',...
    'DiffuseStrength',.8)

% create slices in x, y and z directions
hold on
hx = slice(Vx,Vy,Vz,Vv,xmax,[],[]);
set(hx,'FaceColor','interp','EdgeColor','none')

hy = slice(Vx,Vy,Vz,Vv,[],ymax,[]);
set(hy,'FaceColor','interp','EdgeColor','none')

hz = slice(Vx,Vy,Vz,Vv,[],[],zmin);
set(hz,'FaceColor','interp','EdgeColor','none')

% change the view and add light to make the plot prettier
daspect([1,1,1])
axis tight
box on
view(-38.5,16)
lightangle(-45,45)
set(gcf,'Renderer','zbuffer')
xlabel('x')
ylabel('y')
zlabel('z')
title('1 sample of 3D-stochastic process visualized on 4 planes')

%% close figures and validate solution (part of samples of SP5)

close(f1)
Vdata = ds1.Xdataseries(1,1).Vdata;
assert(all(abs(Vdata(1:10)-[-0.2910   -0.3283   -0.2528   -0.1158...
    0.0115    0.0839    0.0996    0.0856    0.0686    0.0565])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution ds1 does not match.');
% free some memory (2d and 3d processes use quite some...)
clear SP4 SP5
%% NON-HOMOGENEOUS STOCHASTIC PROCESS

Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
    'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'t1','t2'},... % Define the inputs
    'Sscript','for i=1:length(Tinput),Toutput(i).fcov = min(Tinput(i).t1,Tinput(i).t2); end',...
    'Coutputnames',{'fcov'}); % Define the outputs

% Create StochasticProcess object by passing the covariance function and assembling the matrix

Vtime =  linspace(0,5,100);
SP6    = StochasticProcess('Sdistribution','normal','Vmean',0,'Xcovariancefunction',Xcovfun,'Mcoord',Vtime);
SP6    = KL_terms(SP6,'NKL_terms',30,'LcovarianceAssemble',true);

% Generate 1000 samples
ds1 = SP6.sample('Nsamples',1000);

% Compare the analytical covariance to the statisctical covariance
[Mtime1, Mtime2]= meshgrid(Vtime,Vtime);
Vcov_true = evaluate(Xcovfun,[Mtime1(:);Mtime2(:)]);
Mcov_true = reshape(Vcov_true,length(Vtime),length(Vtime));

f1=figure;
surf(Mtime1,Mtime2,Mcov_true)
view(0,90)
title('Analytical covariance matrix of non-homogeneous stochastic process')
xlabel('x')
ylabel('y')

Mcov_stat = cov(cat(1,ds1.Xdataseries.Vdata));
f2=figure;
surf(Mtime1,Mtime2,Mcov_stat)
view(0,90)
title('covariance matrix of non-homogeneous stochastic process estimated from 1000 samples')
xlabel('x')
ylabel('y')

%% close figures and validate solution (part of samples of SP6)

close(f1)
close(f2)

Vdata = ds1.Xdataseries(1,1).Vdata;
assert(all(abs(Vdata(1,1:10)-[-0.0000,-0.3692,-0.6252,-0.7183,...
    -0.6851,-0.6188,-0.6066,-0.6786,-0.7978,-0.8952])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution ds1 does not match.');


%% USAGE OF STOCHASTIC PROCESS WITH INPUT OBJECT

Xin    = Input('CXmembers',{SP1,SP2,SP3},'CSmembers',{'SP1','SP2','SP3'});

% Generate samples of all defined stochastic processes
Xin = Xin.sample('Nsamples',2);
Xs  = Xin.Xsamples;

% The object contains three dataseries (collection of dataseries)
ds2 = Xs.Xdataseries;
display(ds2);

Xin = sample(Xin,'Nsamples',15,'Ladd',true); % add samples to already generates samples in Xin
Xs  = Xin.Xsamples;
ds2 = Xs.Xdataseries;
% The collection of dataseies contains 
display(ds2);

% Define parameters, functions, random variables and random variable sets

Xmat1   = Parameter('Sdescription','material 1 E','value',7E+7);
Xmat2   = Parameter('Sdescription','material 2 E','value',2E+7);
Xmat3   = Parameter('Sdescription','material 3 E','value',1E+4);
Mconf   = unidrnd(3,16,16);
Xconfiguration  = Parameter('Sdescription','material configuration','Mvalue',Mconf);

Xin = add(Xin,Xmat1);
Xin = add(Xin,Xmat2);
Xin = add(Xin,Xmat3);
Xin = add(Xin,Xconfiguration);

Xfun1   = Function('Sdescription','function #1','Sexpression','<&Xmat3&>*<&x1&>');
Xfun2   = Function('Sdescription','function #2','Sexpression','<&Xmat3&>+1');
Xin = add( Xin,Xfun2);
x1  = RandomVariable('Sdistribution','normal','mean',2.763,'std',0.4);
x2  = RandomVariable('Sdistribution','normal','mean',1.25,'std',0.4);
x3  = RandomVariable('Sdistribution','normal','mean',4,'std',0.4);
x4  = RandomVariable('Sdistribution','uniform','mean',5,'std',1);

Cmems   = {'x1'; 'x2'};
Xrvs1     = RandomVariableSet('Cmembers',Cmems);
Cmems   = {'x3'; 'x4'};
Xrvs2   = RandomVariableSet('Cmembers',Cmems);
Xin     = add(Xin,Xrvs1);
Xin     = add(Xin,Xrvs2);

% Generate samples of random variables and stochastic processes
Xin = Xin.sample('Nsamples',10);
display(Xin)

%% validate solution (part of samples of SP1)

Vdata = Xin.Xsamples.Xdataseries(1,1).Vdata;
assert(all(abs(Vdata(1:10)-[ -0.0135   -0.4616   -1.1806   -1.8682...
    -2.2480   -2.1991   -1.7961   -1.2487   -0.7832   -0.5369])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...Vdata = Xin.Xsamples.Xdataseries(1,1).Vdata;
    'Reference Solution ds1 does not match.');

% The part with the FE interaction is not always available. It should be
% moved in another tutorial
break 
%% USAGE OF STOCHASTIC PROCESS AS INPUT FOR ABAQUS FE-ANALYSIS
% FE-input file taken from the tutorial database of Abaqus

% define random variables
Emod  = RandomVariable('Sdistribution','normal','mean',200.E9,'std',200E8);
density  = RandomVariable('Sdistribution','normal','mean',7800.,'std',780.);
Cmems   = {'Emod'; 'density'};
Xrvs1     = RandomVariableSet('Cmembers',Cmems);
Xin     = Input;
Xin     = add(Xin,Xrvs1);

% Definition of stochastic process

Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
    'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'t1','t2'},... % Define the inputs
    'Sscript', 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end', ...
    'Coutputnames',{'fcov'}); % Define the outputs

time   = 0:0.001:0.5;
SP1    = StochasticProcess('Sdistribution','normal','Vmean',1.0,'Xcovariancefunction',Xcovfun,'Mcoord',time,'Lhomogeneous',true);
SP1    = KL_terms(SP1,'NKL_terms',30,'Lcovarianceassemble',false);

Xin    = add(Xin,SP1);

% Generation of samples of random variables and stochastic processes

Xin = sample(Xin,'Nsamples',3);

% Create the Injector

Xi=Injector('Sscanfilepath',OpenCossan.getCossanWorkingPath,'Sscanfilename','crane.cossan','Sfile','crane.inp');

%  Create the TableExtractor which reads the time-dependent displacements

Xte1=TableExtractor('Sdescription','Spatial displacement: U1 at Node 104 in NSET TIP', ...
    'Srelativepath','./', ...
    'Sfile','results.txt', ...
    'Nheaderlines', 3, ...
    'Sdelimiter', ' ',...
    'Soutputname','U1_Node104');

% Define connector

Xconn1 = Connector('SpredefinedType','Abaqus',...
    'Smaininputpath',OpenCossan.getCossanWorkingPath,...
    'Smaininputfile','crane.inp',...
    'Caddfiles',{'Readresults.py'},...
    'Sworkingdirectory',OpenCossan.getCossanWorkingPath, ...
    'SpostExecutionCommand','/usr/site/bin/abq671 cae noGUI=Readresults.py', ...
    'Xinjector',Xi, ...
    'Xextractor',Xte1);

% Run the MC simulations and plot the displacements as a function of time

XSimOut = run(Xconn1,Xin);

f1 = figure;
for isample = 1:XSimOut.Nsamples
    plot(XSimOut.Tvalues(isample).U1_Node104.Mcoord,XSimOut.Tvalues(isample).U1_Node104.Mdata);
    hold all;
end
xlabel('time [s]');
ylabel('Displacement [m]')
title('Tip displacment of crane')
legend('sample 1','sample 2','sample 3')

delete([OpenCossan.getCossanWorkingPath '/crane.*']);

%% close figure and validate solution

close(f1)

assert(all(abs(XSimOut.Tvalues(1).U1_Node104.Mdata(1:10)-[0 0.0150 0.0299...
    0.0422 0.0605 0.0741 0.0838 0.0889 0.0832 0.0719])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution of displacement of tip does not match.');


%% USAGE OF STOCHASTIC PROCESS AS INPUT FOR NASTRAN FE-ANALYSIS

% Definition of random variables

Emod  = RandomVariable('Sdistribution','normal','mean',2E11,'std',2E10);
t  = RandomVariable('Sdistribution','normal','mean',0.05,'std',0.005);
nu  = RandomVariable('Sdistribution','normal','mean',0.3,'std',0.003);

Cmems   = {'Emod'; 't'; 'nu'};
Xrvs1     = RandomVariableSet('Cmembers',Cmems);

Xin     = Input;
Xin     = add(Xin,Xrvs1);

% Definition of the stochastic processes

time1   = 0:0.01:1;
time2   = 0:0.01:0.5;
time3   = 0:0.01:0.75;

Xcovfun  = CovarianceFunction('Sdescription','covariance function', ...
    'Lfunction',false,'Liostructure',true,'Liomatrix',false,...
    'Cinputnames',{'t1','t2'},... % Define the inputs
    'Sscript', 'sigma = 1; b = 0.5; for i=1:length(Tinput), Toutput(i).fcov  = sigma^2*exp(-1/b*abs(Tinput(i).t2-Tinput(i).t1)); end', ...
    'Coutputnames',{'fcov'}); % Define the outputs

SP1    = StochasticProcess('Sdistribution','normal','Vmean',80,'Xcovariancefunction',Xcovfun,'Mcoord',time1);
SP1    = KL_terms(SP1,'NKL_terms',30,'Lcovarianceassemble',false);
SP2    = StochasticProcess('Sdistribution','normal','Vmean',50,'Xcovariancefunction',Xcovfun,'Mcoord',time2);
SP2    = KL_terms(SP2,'NKL_terms',30,'Lcovarianceassemble',false);
SP3    = StochasticProcess('Sdistribution','normal','Vmean',150,'Xcovariancefunction',Xcovfun,'Mcoord',time3);
SP3    = KL_terms(SP3,'NKL_terms',30,'Lcovarianceassemble',false);

Xin    = add(Xin,SP1);
Xin    = add(Xin,SP2);
Xin    = add(Xin,SP3);

% Generation of 3 samples of random variables and stochastic processes

Xin = sample(Xin,'Nsamples',3);

% Create the Injector

Xi=Injector('Sscanfilepath',OpenCossan.getCossanWorkingPath,'Sscanfilename','plate_SP.cossan','Sfile','plate_SP.dat');

% Define which responses shall be extracted from PCH-file

Xresp1 = Response('Sname', 'Out_ID170', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =         170'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',inf);

Xresp2 = Response('Sname', 'Out_ID87', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =         187'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',inf);

Xresp3 = Response('Sname', 'Out_ID88', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =         188'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',inf);

Xresp4 = Response('Sname', 'Out_ID89', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =         189'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',inf);

Xresp5 = Response('Sname', 'Out_ID90', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ...
    'Clookoutfor',{'POINT ID =         190'}, ...
    'Ncolnum',1, ...
    'Nrownum',1,...
    'Nrepeat',inf);

% Add Responses to dedicated extractor (=TableExtractor)
Xte=TableExtractor('Sdescription','Extractor for the tutorial', ...
    'Srelativepath','./', ...
    'Sfile','plate_SP.pch', ...
    'Xresponse', [Xresp1 Xresp2 Xresp3 Xresp4 Xresp5]);

% Define Connector
Xc = Connector('SpredefinedType','nastran_x86_64', ...
    'Smaininputpath',OpenCossan.getCossanWorkingPath, ...
    'Smaininputfile','plate_SP.dat', ...
    'Sworkingdirectory',OpenCossan.getCossanWorkingPath);

% Add Injector and Extractor to Connector
Xc = add(Xc,Xi);
Xc = add(Xc,Xte);

% Run the analysis
XSimOut = run(Xc,Xin);
XSimOut.Sdescription = 'Time-dependent response of 5 DOFs';

% visualize results
f1=figure;
for isim = 1:3
    plot(XSimOut.Tvalues(isim).Out_ID170.Mcoord,XSimOut.Tvalues(isim).Out_ID170.Mdata(3,:)); hold all
end
xlabel('time [s]')
ylabel('displacement z [m]')
title('Samples of displacement due to point loads modelled as stochastic processes')
legend('sample 1','sample 2','sample 3')

%% close figures and delete simulation files
close(f1)
delete([OpenCossan.getCossanWorkingPath '/plate_SP.*']);

% validate solution
assert(all(abs(XSimOut.Tvalues(1).Out_ID170.Mdata(11:20)-[-0.0025 0.0000...
    -0.0000 -0.0000  0.0049 -0.0222 -0.0137 0.0000 -0.0000 -0.0000])<1.e-4),...
    'CossanX:Tutorials:TutorialDataseries', ...
    'Reference Solution of displacement of tip does not match.');

