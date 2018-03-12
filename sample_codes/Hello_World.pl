%Description: Static agent handler code which executes Hello World message and halts.
%Author: Tushar Semwal (semwaltushar@gmail.com)


a_static_agent_handler(guid,(IP,Port),main):-
        writeln('Hello World!'),!.
        
        
        
%Steps to create and execute the static agent

% 1. Start a Tartarus platform on a port. e.g. start_tartarus(localhost,12121).
% 2. Consult the Hello_World.pl file. e.g. consult('path_to_file/Hello_World.pl'). OR if you are in the same directory, just enter consult('Hello_World.pl').
% 3. Create a static agent with a given (or random) name at a port DIFFERENT FROM TARTARUS PORT. e.g. create_static_agent(myagent,(localhost,33333),a_static_agent_handler).
% 4. Execute the agent using: execute_agent(myagent,(localhost,12121),a_static_agent_handler).
% 5. That's all!
