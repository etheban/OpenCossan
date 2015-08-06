% Unit Test for the Gradient

function varargout=UnitTestGradient

% Test constructor
Ntest=11;
Vtest(1:Ntest)=false;
Cmess=cell(Ntest,1);

%% Constructor and display
% 1
itest=1;
try
    Xg1=Gradient('Sdescription','UnitTest','Vgradient',[0.01 0.6],'Cnames',{'X1' 'X2'},'SfunctionName','MyFunction');
    display(Xg1)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 2
itest=itest+1;
try
    Xg2=Gradient('Cnames',{'X1' 'X2'},'SfunctionName','MyFunction','Vgradient',[0.01 0.6],'Nsamples',10);
    display(Xg2)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 3
itest=itest+1;
try
    Xg3=Gradient('Cnames',{'X1' 'X2' 'X1' 'X2'},'SfunctionName','MyFunction','Vgradient',[1 2 3 4],'Nsamples',10);
    display(Xg3)
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 4
itest=itest+1;
try
    Xg=Gradient('SfunctionName','MyFunction','Vgradient',[1 2 3 4],'Nsamples',10);
    display(Xg)
    Cmess{itest}='This should fail: No Cnames';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
% 5
itest=itest+1;
try
    Xg=Gradient('Cnames',{'X1' 'X2' 'X1' 'X2'},'Vgradient',[1 2 3 4 5],'Nsamples',10,'SfunctionName','MyFunction');
    display(Xg)
    Cmess{itest}='This should fail:  Cnames and Vgradient not consistent';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 6
itest=itest+1;
try
    Xg=Gradient('Cnames',{'X1' 'X2' 'X1' 'X2'},'Vgradient',[1 2 3],'Nsamples',10,'SfunctionName','MyFunction');
    display(Xg)
    Cmess{itest}='This should fail:  Cnames and Vgradient not consistent';
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 7 
itest=itest+1;
try
    Xg=Gradient('Cnames',{'X1' 'X2' 'X1' 'X2'},'Vgradient',[1 2 3],'SfunctionName','MyFunction');
    display(Xg)
    Cmess{itest}='This should fail:  Cnames and Vgradient not consistent';    
catch ME
    Vtest(itest)=true;
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 8
% test method plotCompontents
itest=itest+1;
try
    fh=Xg1.plotComponents('Scolor','y');
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 9
itest=itest+1;
try
    fh=Xg3.plotComponents('FigureHandle',fh,'Scolor','m');
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end
%% 10
itest=itest+1;
try
    fh=Xg2.plotComponents('FigureHandle',fh,'Scolor','c','SfigureName','UnitTestPlot');
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
%% 11
itest=itest+1;
try
    fh=Xg2.plotComponents;
    Vtest(itest)=true;
catch ME
    Cmess{itest}=[ME.identifier ' -- ' ME.message];
end

close all
OpenCossan.cossanDisp('--------------------------------------------------------------------')
OpenCossan.cossanDisp([' Unit Test of Gradient (' datestr(now) ')'])
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
varargout{1}='Gradient';
% Export Results of the UnitTest
varargout{2}=Vtest;
varargout{3}=Cmess;
end
