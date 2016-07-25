%% Tutorial Extractor
% This tutorial shows how to create an Extractor object and to use the
% extract method. For this purpose, a dummy textfile called 'outputfile.txt'
% is used.
%
% See Also: http://cossan.cfd.liv.ac.uk/wiki/index.php/@Extractor
%

%  Copyright 1993-2011, COSSAN Working Group
%  University of Innsbruck, Austria

%% Create extractor
SpathName=fullfile(OpenCossan.getCossanRoot,'examples','Tutorials','CossanObjects','Connector','NASTRAN');

% TODO: add mimimal comments
Xresp1 = Response('Sname', 'Out1', ...
             'Sfieldformat', '%d%*', ...
             'Clookoutfor',{}, ...
             'Svarname','', ...
             'Ncolnum',1, ...
             'Nrownum',3, ...
             'Sregexpression', '', ...
             'Nrepeat',2);

Xe=Extractor('Sdescription','Extractor for the tutorial', ...
             'Sworkingdirectory',SpathName, ...
             'Srelativepath','./', ... % this is the directory where the input and output are contained
             'Sfile','outputfile.txt',...
             'Xresponse',Xresp1);
           
%% Add responses 

Xresp2 = Response('Sname','Out2','Sfieldformat','%3f%3f%3f%3f%*','Svarname','Out1','Nrownum',2);
Xresp3 = Response('Sname','Out3','Sfieldformat','%3f','Nrownum',53);
Xe=add(Xe,'Xresponse',Xresp2);      
Xe=add(Xe,'Xresponse',Xresp3);   

display(Xe)

%% use REMOVE method to remove unnecessary responses
Xe=remove(Xe,'Out3'); 
display(Xe)

%% now the extractor contains only 2 responses.
Tout=extract(Xe) %#ok<*NOPTS>

% Validate output
Vreference= [10 11 12 13];
assert(max(abs(Tout.Out2.Vdata-Vreference))<eps,...
    'CossanX:Tutorials:TutorialEvaluator','Reference Solution does not match.')


%% access to the properties to edit response

Xe.Xresponse(2).Sfieldformat = '%3f';
Tout2=extract(Xe)

% Validate output
Vreference= 10;
assert(abs(Tout2.Out2-Vreference)<eps,...
    'CossanX:Tutorials:TutorialEvaluator','Reference Solution does not match.')


