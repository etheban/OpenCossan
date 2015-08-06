%% Tutorial for the TableExtractor
%
% This tutorial shows how a time- or frequency dependent output (i.e.
% table-valued output) is extracted from the result file of a Nastran and
% Abaqus analyis.
% 
% See Also: http://cossan.cfd.liv.ac.uk/wiki/index.php/@TableExtractor
%
%
%  Copyright 1993-2011, COSSAN Working Group
%  University of Innsbruck, Austria
%

% Reset the random number generator in order to always obtain the same results.
% DO NOT CHANGE THE VALUES OF THE SEED
OpenCossan.resetRandomNumberGenerator(51125)

% copy FE-input file with COSSAN-identifiers to working directory
StutorialPath = fileparts(which('TutorialTableExtractor.m'));
copyfile([StutorialPath '/Connector/NASTRAN/beam_transient_analysis.dat'],...
    fullfile(OpenCossan.getCossanWorkingPath),'f');
copyfile([StutorialPath '/Connector/NASTRAN/beam_frf_analysis.dat'],...
    fullfile(OpenCossan.getCossanWorkingPath),'f');


%%  TRANSIENT ANALYSIS USING NASTRAN
% The model is a beam with an I-section modelled with plate elements. The
% beam is fixed at one end and simply sopprted at two other points. As a
% load, two harmonic point loads one the end of the beam are acting. The
% time-dependent structural response is written to a pch-file which is
% extracted by using the TableExtractor.

% Define connector
Xconn1 = Connector('SpredefinedType','nastran_x86_64',...
    'Smaininputpath',OpenCossan.getCossanWorkingPath,...
    'Smaininputfile','beam_transient_analysis.dat',...
    'Sworkingdirectory','/tmp');


% Define response extraction
Xresp1 = Response('Sname', 'displacement', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n','%*s','%f','%f','%f','%*d'], ... % format of repetive pattern
    'Clookoutfor',{'POINT ID =          18'}, ... % search string 
    'Ncolnum',1, ... % column difference relative to Clookoutfor
    'Nrownum',1,... % row difference relative to Clookoutfor
    'Nrepeat',inf); % number of repetitions (inf means until the end of the file)

% Define table extractor
Xte1=TableExtractor('Sdescription','Extractor for the tutorial', ...
             'Srelativepath','./', ... % relative path to the Sworkingdirectory where result file is located
             'Sfile','beam_transient_analysis.pch', ... % name of output file
             'Xresponse', Xresp1); % response to be extracted as defined above

% Add extractor to connector
Xconn1 = Xconn1.add(Xte1);

% Execute deterministic analysis
Xout1 = Xconn1.deterministicAnalysis();         

% Visualize extracted response
Vdisp_ydir = Xout1.Tvalues.displacement.Mdata(2,:);
Vtime =  Xout1.Tvalues.displacement.Mcoord;
f1 = figure;
plot(Vtime,Vdisp_ydir)
grid on
xlabel('time [s]');
ylabel('displacement, y-dir');
h1=gca; h2=get(gca,'XLabel'); h3=get(gca,'YLabel'); 
set([h1 h2 h3],'FontSize',16);

%% Validate solution, close figures and remove simulation files

close(f1)
delete([Xconn1.Sworkingdirectory 'beam_transient_analysis.*'])

assert(all(abs(Vdisp_ydir(1:10) - [0, 3.3317e-06, 1.4490e-05, 3.5931e-05, ...
         6.9865e-05, 1.1660e-04, 1.7304e-04, 2.3162e-04, 2.8011e-04, 3.0219e-04])<1.e-4), ...
        'CossanX:Tutorials:TutorialTableExtractor', ...
        'Reference Solution of displacement (transient analysis) does not match.')

%% 2. FRF ANALYSIS USING NASTRAN

% For the same beam as described above, an FRF analysis will be performed.
% The frequency range for the load is 2-10 Hz.

% Define connector 
Xconn2 = Connector('SpredefinedType','nastran_x86_64',...
    'Smaininputpath',OpenCossan.getCossanWorkingPath,...
    'Smaininputfile','beam_frf_analysis.dat',...
    'Sworkingdirectory','/tmp');

% Define response extraction
Xresp2 = Response('Sname', 'displacement', ...
    'Sfieldformat', ['%f','%*s','%f','%f','%f','%*d','\n', ...
                          '%*s','%f','%f','%f','%*d','\n', ...
                          '%*s','%f','%f','%f','%*d','\n', ...
                          '%*s','%f','%f','%f','%*d'], ... % format of repetive pattern
    'Clookoutfor',{'POINT ID =          18'}, ... 
    'Ncolnum',1, ... % column difference relative to Clookoutfor
    'Nrownum',1,... % row difference relative to Clookoutfor
    'Nrepeat',inf); % number of repetitions (inf means until the end of the file)

% Define table extractor
Xte2=TableExtractor('Sdescription','Extractor for the tutorial', ...
             'Srelativepath','./', ...  % relative path to the Sworkingdirectory where result file is located
             'Sfile','beam_frf_analysis.pch', ... % name of output file
             'Xresponse', Xresp2); % response to be extracted as defined above

% Add extractor to connector         
Xconn2 = Xconn2.add(Xte2);

% Execute deterministic analysis
Xout2 = Xconn2.deterministicAnalysis();

% Visualize extracted response
Vdisp_ydir = Xout2.Tvalues.displacement.Mdata(2,:); % magnitude in y-dir 
Vfreq =  Xout2.Tvalues.displacement.Mcoord;
figure
plot(Vfreq,Vdisp_ydir)
grid on
xlabel('frequency [Hz]');
ylabel('|displacement|, y-dir');
h1=gca; h2=get(gca,'XLabel'); h3=get(gca,'YLabel'); 
set([h1 h2 h3],'FontSize',16);

%% Validate solution, close figures and remove simulation files

close(f1)
delete([Xconn1.Sworkingdirectory 'beam_frf_analysis.*'])

assert(all(abs(Vdisp_ydir(1:10) - [2.0936e-03, 2.4958e-03, 3.1080e-03, 4.1503e-03, ...
         6.2869e-03, 1.2130e-02, 1.4691e-02, 6.7757e-03, 3.9957e-03, 2.7320e-03])<1.e-4), ...
        'CossanX:Tutorials:TutorialTableExtractor', ...
        'Reference Solution of displacement (frf analysis) does not match.')


%% TRANSIENT ANALYSIS USING ABAQUS

% In order to see how the table Extractor can be used in case a
% table-valued output is generated (as it is the case for Abaqus), the user
% is referred to the tutorial of the StochasticProcess 
% -> echodemo TutorialStochasticProcess
