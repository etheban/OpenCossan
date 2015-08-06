function display(Xobj)

OpenCossan.cossanDisp([' Name: ' Xobj.Sname ]) ;

if ~isempty(Xobj.Clookoutfor)
    for ilook=1:length(Xobj.Clookoutfor)
        OpenCossan.cossanDisp([' #' num2str(ilook) ' Position relative to the string: ' Xobj.Clookoutfor{ilook}])
    end
elseif ~isempty(Xobj.Svarname)
    OpenCossan.cossanDisp([' Position relative to the response: ' Xobj.Svarname ])
else
    OpenCossan.cossanDisp(' Absolute Position')
end

OpenCossan.cossanDisp([' Col: ' num2str(Xobj.Ncolnum) ' Row: '   ...
    num2str(Xobj.Nrownum) ' Format: ' ...
    Xobj.Sfieldformat ' Repeat: ' num2str(Xobj.Nrepeat)]) ;

if ~isempty(Xobj.Sregexpression)
    OpenCossan.cossanDisp([' Regular Expression: ' Xobj.Sregexpression])
end

end