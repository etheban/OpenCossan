function [u,p] = getMyCredentials(WebCredentials)
    prompt{1} = 'Username:';
    prompt{2} = 'Password:';
    title = 'Credentials needed for downloading OpenCossan';
    defAns={WebCredentials.Username,'******'};
    answer = inputdlg(prompt, title, [1, 60], defAns, 'on');
    if isempty(answer)
        u=[];
        p=[];
    else
        
    if strcmp(answer{2},defAns{2})
        p = WebCredentials.Password;
    else
        p = answer{2};  
    end
    u = answer{1};  
    end
end