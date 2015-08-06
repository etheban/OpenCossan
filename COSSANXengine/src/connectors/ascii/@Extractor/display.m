function display(Xe)
%DISPLAY  Displays the object extractor
%   
%
%   Example: DISPLAY(Xe) will output the summary of the extractor
% =========================================================================


%% 1.   Output to Screen
% 1.1.   Name and description
OpenCossan.cossanDisp(' ');
OpenCossan.cossanDisp('===================================================================',1);
OpenCossan.cossanDisp([' Extractor Object  -  Name: ' inputname(1)],1);
OpenCossan.cossanDisp([' Description: ' Xe.Sdescription ] ,1);
OpenCossan.cossanDisp('===================================================================',1);
% 1.2.   main paramenters
OpenCossan.cossanDisp(' ',1);
OpenCossan.cossanDisp([' Number of responses: ' num2str(Xe.Nresponse) ],1);
OpenCossan.cossanDisp([' ASCII file: ' Xe.Srelativepath Xe.Sfile ],1);
% 1.3.  Response details
for i=1:length(Xe.Coutputnames)
    OpenCossan.cossanDisp('- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - ',1);
    OpenCossan.cossanDisp([' Response #' num2str(i) ],1) ;
    OpenCossan.cossanDisp([' Output Name: ' Xe.Coutputnames{i} ],1) ;
    if ~isempty(Xe.Xresponse)
        display(Xe.Xresponse(i));
    end
end




