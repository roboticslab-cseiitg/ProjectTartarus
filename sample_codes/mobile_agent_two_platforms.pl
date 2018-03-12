%Description: Create a mobile agent which moves from first platform, reaches second platform and then executes itself.
%Author: Tushar Semwal (semwaltushar@gmail.com)

:- dynamic mobile_agent_handler/3. % This is important as the code will be moving (yes, it will be moving and executing!)

mobile_agent_handler(guid,(_IP,_Port),main):-
        write('Feel my presence!'),
        % Add your awesome code here
        nl,!.
        
        
%=========================================================%
%------Steps to create and execute the static agent-------%
%=========================================================%
% 1. Open two different Tartarus platforms at ports 12121 and 12122. e.g.:
% start_tartarus(localhost,12121).
% start_tartarus(localhost,12122).

% 2. Set a token as for e.g. 5656 on both the Tartarus platform. e.g.:
% set_token(5656). % Use the same token on both the platforms.

% 3. Consult the mobile_agent_two_platforms.pl file. e.g. consult('path_to_file/mobile_agent_two_platforms.pl'). OR if you are in the same directory, just enter consult('mobile_agent_two_platforms.pl').

% 4. Create a mobile agent with a given (or random) name at the Tartarus with Port 12121:
% create_mobile_agent(myagent,mobile_agent_handler).   %--> Remember the DOT (Welcome to Prolog!)

% 5. Add a token number to the agent created, e.g. :
% add_token(myagent,[5656]).
% NOTE: the second argument is a list of tokens, i.e. you can add multiple tokens for multiple platforms with different tokens.

% 6. Move the agent using: 
% move_agent(myagent,(localhost,12122)). 
% NOTE: The agent is created on Port 12121 and moved to port 12122

% 7.You can see the Ack and the agent code executed on platform 12122.

% 8. That's all.
