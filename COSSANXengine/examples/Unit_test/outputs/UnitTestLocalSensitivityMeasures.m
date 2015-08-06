% Unit Test for the LocalSensitivityMeasures
function varargout=UnitTestLocalSensitivityMeasures

% Test constructor
Ntest=12;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% empty constructor
itest=1;
try
    Xg1=LocalSensitivityMeasures;
    display(Xg1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end


%% 2 Constructor and display
itest=itest+1;
try
    Xg1=LocalSensitivityMeasures('Sdescription','UnitTest','VreferencePoint',[0 0],'Vmeasures',[0.01 0.6],'Cnames',{'X1' 'X2'},'SfunctionName','MyFunction');
    display(Xg1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 3
itest=itest+1;
try
    Xg2=LocalSensitivityMeasures('Cnames',{'X1' 'X2'},'SfunctionName','MyFunction','Vmeasures',[0.01 0.6],'Nsamples',10,'VreferencePoint',[0 0]);
    display(Xg2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 4
itest=itest+1;
try
    Xg3=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X1' 'X2'},'SfunctionName','MyFunction','Vmeasures',[1 2 3 4],'Nsamples',10,'VreferencePoint',[0 0 0 0]);
    display(Xg3)
    Cmess{itest}='This should fail, duplicated names';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 5
itest=itest+1;
try
    Xg=LocalSensitivityMeasures('SfunctionName','MyFunction','Vmeasures',[1 2 3 4],'Nsamples',10);
    display(Xg)
    Cmess{itest}='This should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 6 No reference point
itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X1' 'X2'},'Vmeasures',[1 2 3 4],'Nsamples',10,'SfunctionName','MyFunction');
    display(Xg)
    Cmess{itest}='This should fail';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 7 pass total variance
itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X3' 'X4'},'Vmeasures',[1 2 3 4],'totalvariance',10,'Nsamples',10,'SfunctionName','MyFunction','VreferencePoint',zeros(4,1));
    display(Xg)
    assert(Xg.Lnormalized,'error flag Lnormalized should be true')
    Cmess{itest}='test total variance';
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 8
itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X3' 'X4'},'Vgradient',[1 2 3],'SfunctionName','MyFunction');
    display(Xg)
    Cmess{itest}='Vgradient fields not allowed';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

%% 9
%% test method plotCompontents
itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X3' 'X4'},'Vmeasures',[1 2 3 4],'Vreferencepoint',[1 2 3 4],'Nsamples',10,'SfunctionName','MyFunction');
    fh=Xg.plotComponents('Scolor','y','Stitle','Local sensitivity');
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X3' 'X4'},'Vmeasures',[1 2 3 4],'Vreferencepoint',[1 2 3 4],'Nsamples',10,'SfunctionName','MyFunction');
    fh=Xg.plotComponents('FigureHandle',fh,'Scolor','m');
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X3' 'X4'},'Vmeasures',[1 2 3 4],'Vreferencepoint',[1 2 3 4],'Nsamples',10,'SfunctionName','MyFunction');
    fh=Xg.plotComponents('Scolor','c','SfigureName','UnitTestPlot');
    if exist('UnitTestPlot.pdf','file')
        Vtest(itest)=true;
        delete('UnitTestPlot.pdf')
    else
         Vtest(itest)=false;
         Cmess{itest}='Figure not saved';
    end
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

itest=itest+1;
try
    Xg=LocalSensitivityMeasures('Cnames',{'X1' 'X2' 'X3' 'X4'},'Vmeasures',[1 2 3 4],'Vreferencepoint',[1 2 3 4],'Nsamples',10,'SfunctionName','MyFunction');
    fh=Xg.plotComponents;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

close all
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of LocalSensitivityMeasures (' datestr(now) ')'])
OpenCossan.cossanDisp('--------------------------------------------------------------------')
%% Show summary of the test
for i=1:length(Vtest)
    if Vtest(i)
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': passed  (' Cmess{i} ')' ]);  
    else
        OpenCossan.cossanDisp(['Test #' sprintf('%3i',i) ': failed  (' Cmess{i} ')' ]);
    end
end

if nargout>0
% Export name of the UnitTest
varargout{1}='LocalSensitivityMeasures';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
