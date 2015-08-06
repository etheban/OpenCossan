%tutorial for the connector of code aster
% TODO: Add description

Xrv1    = RandomVariable('Sdistribution','normal','mean',210e9,'std',210e8);
Xrv2    = RandomVariable('Sdistribution','normal','mean',8e-3,'std',8e-4);


Xrvs    = RandomVariableSet('Cmembers',{'E','leng1','leng2','leng3','leng4'}...
    ,'Xrv',[Xrv1,Xrv2,Xrv2,Xrv2,Xrv2]);%,'Mcorrelation',[1,0;0,1]);

Xin     = Input();
Xin     = add(Xin,Xrvs);
Xin = Xin.sample('Nsamples',3);


%% 1. Create the Injector
Xi=Injector('Stype','scan','Spath','./','Sscanfilename','Cas.cossan','Sfile','Cas.comm');

%% 2. Extractor

% 2.1 Build extractor
%TODO: Add some description comments


Xresp_sif = Response('Sname','OUT1', ...
             'Sfieldformat', '%12e', ...
             'Clookoutfor', { 'K1';}, ...
             'Ncolnum',41, ...
             'Nrownum',1 );

Xe=Extractor( 'Xresponse',Xresp_sif, ...
             'Sdescription','Extractor for the plate', ...
             'Srelativepath','./', ...
             'Sfile','Cas.resu' ...
             );

%% 3. Construct the connector

% 3.1 create the connector
Xc= Connector('Stype','aster','Smaininputfile','Cas',...
    'Smaininputpath',fullfile(COSSANX.ScossanRoot,'examples/Tutorials/Connector/aster'),...
    'Soutputfile','Cas.resu','Sworkingdirectory',pwd);

Xc=add(Xc,Xi);
Xc=add(Xc,Xe);
%% run the connector

Xo=Xc.run(Xin);


