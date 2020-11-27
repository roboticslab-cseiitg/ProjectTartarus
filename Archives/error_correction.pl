:-dynamic error/0.
:-dynamic ecnt/1.

ecnt(0).

check_syntax(assert_file_to_tartarus,FILE):-
            catch((atom(FILE)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error2000,(write('Error# tf1:'),writeln(' The argument is not an atom.'))), chk_err.        

check_syntax(get_new_name_alpha,Agent_name):-
            catch((var(Agent_name)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notVar)),_Error400,(write('Error# gnma1:'),writeln(' The argument is not a variable.'))),chk_err. 
             
check_syntax(add_payload,GUID,AddList):-
            catch((atom(GUID)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error1,(write('Error# ap1:'),writeln(' First argument is not an atom.'))),      
            catch((is_list(AddList)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notList)),_Error2,(write('Error# ap2:'),writeln(' Last argument is not a list.'))),chk_err.

check_syntax(remove_payload,GUID,ToRemove):-
            catch((atom(GUID)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error300,(write('Error# rp1:'),writeln(' First argument is not an atom.'))),      
            catch((is_list(ToRemove)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notList)),_Error301,(write('Error# rp2:'),writeln(' Last argument is not a list.'))),chk_err.

check_syntax(save_agent,GUID,FileName):-
            catch((atom(GUID)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error1000,(write('Error# sav1:'),writeln(' First argument is not an atom.'))),      
            catch((atom(FileName)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notList)),_Error1001,(write('Error# sav2:'),writeln(' Last argument is not an atom.'))),chk_err.

check_syntax(get_tartarus_details,IP,Port,OS):-
            catch((var(IP)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notVar)),_Error3,(write('Error# td1:'),writeln(' First argument is not a variable.'))), 
            catch((var(Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notVar)),_Error4,(write('Error# td2:'),writeln(' Second argument is not a variable.'))),
			catch((var(OS)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notVar)),_Error9876,(write('Error# td3:'),writeln(' Last argument is not a variable.'))),
			chk_err. 
						
check_syntax(start_tartarus,_Ip,Port,Token):-
			 catch((integer(Port)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error5,(write('Error# t1:'),writeln(' Third argument is not an integer.'))),
			 catch((integer(Token)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error6,(write('Error# t2:'),writeln('  Last argument is not an integer.'))),
			 ((integer(Port)->(catch((pre1(Port)),_Error900,(write('Error# t3:'),writeln(' Second argument is not a positive number.')))));chk_err),
			 chk_err.	
			 
check_syntax(move_agent,GUID,_Ip_other_end,Port_other_end):-
             catch((atom(GUID)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Err,(write('Error# mov1:'),writeln(' First argument is not an atom.'))),
             catch((integer(Port_other_end)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Err1,(write('Error# mov2:'),writeln(' Last argument is not an integer.'))),	
             ((integer(Port_other_end)->(catch((pre1(Port_other_end)),_Err2,(write('Error# mov3:'),writeln(' Last argument is not a positive number.')))));chk_err),
			 chk_err.			 

check_syntax(create_mobile_agent,_GUID,Handler,List):-
			 catch((atom(Handler)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Er,(write('Error# ma21:'),writeln(' Second argument is not an atom.'))), 
			 catch((is_list(List)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notList)),_Er1,(write('Error # ma22:'),writeln(' Last argument is not a list.'))),
             ((is_list(List)->(catch((pre(List)),_Er2,(write('Error# ma23:'),writeln(' Last argument is not an integer.')))));chk_err),chk_err.			 
			 
check_syntax(execute_agent3,Agent_name,_Ip,Port,Handler):-
            catch((atom(Agent_name)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error7,(write('Error# ea1:'),writeln(' First argument is not an atom.'))),      		
			catch((integer(Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error8,(write('Error# ea2:'),writeln(' Third argument is not an integer.'))),
            catch((atom(Handler)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notAtom)),_Error9,(write('Error# ea3:'),writeln(' Last argument is not an atom.'))),
			((integer(Port)->(catch((pre1(Port)),_Error50,(write('Error# ea4:'),writeln(' Third argument is not a positive number.')))));chk_err),
			chk_err.			 			 

check_syntax(clone_agent,GUID1,_Send_Ip,Send_Port,GUID2):-
            catch((atom(GUID1)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error10,(write('Error# ca1:'),writeln(' First argument is not an atom.'))),      		
			catch((integer(Send_Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error11,(write('Error# ca2:'),writeln(' Third argument is not an integer.'))),
			catch((var(GUID2)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notVar)),_Error12,(write('Error# ca3:'),writeln(' Last argument is not a variable.'))),
			((integer(Send_Port)->(catch((pre1(Send_Port)),_Error51,(write('Error# ca4:'),writeln(' Third argument is not a positive number.')))));chk_err),
			chk_err.
			
check_syntax(create_mobile_agent,_GUID,_Ip,Port,Handler,List):-
             catch((integer(Port)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error1600,(write('Error# ma1:'),writeln(' Third argument is not an integer.'))),
			 catch((atom(Handler)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Error14,(write('Error# ma2:'),writeln(' Fourth argument is not an atom.'))), 
			 catch((is_list(List)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notList)),_Error15,(write('Error # ma3:'),writeln(' Last argument is not a list.'))),
             ((is_list(List)->(catch((pre(List)),_Error56,(write('Error# ma4:'),writeln(' Last argument is not an integer.')))));true),
			 ((integer(Port)->(catch((pre1(Port)),_Error5300,(write('Error# ma5:'),writeln(' Third argument is not positive.')))));chk_err),chk_err.  
			 
			
check_syntax(create_static_agent,_Agent_name,_Platform_Ip,Port,Handler,List):-
            catch((integer(Port)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notInt)),_Error16,(write('Error# sa1:'),writeln(' Third argument is not an integer.'))),
			((integer(Port)->(catch((pre1(Port)),_Error53,(write('Error# sa2:'),writeln(' Third argument is not positive.')))));true),
			catch((atom(Handler)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notAtom)),_Error17,(write('Error# sa3:'),writeln(' Fourth argument is not an atom.'))),		
			catch((is_list(List)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notList)),_Error18,(write('Error# sa4:'),writeln(' Last argument is not a list.'))),
            ((is_list(List)->(catch((pre(List)),_Error55,(write('Error# sa5:'),writeln(' Last argument is not an integer.')))));chk_err),
			chk_err.  
			
check_syntax(execute_agent4,Agent_name,_Ip,Port,Handler,Start_function):-
            catch((atom(Agent_name)->true;assert(error),retract(ecnt(X)),X1 is X+1,assert(ecnt(X1)),throw(notAtom)),_Error20,(write('Error# ea5:'),writeln(' First argument is not an atom.'))),      		
			catch((integer(Port)->true;assert(error),retract(ecnt(Y)),Y1 is Y+1,assert(ecnt(Y1)),throw(notInt)),_Error21,(write('Error# ea6:'),writeln(' Third argument is not an integer.'))),
            catch((atom(Handler)->true;assert(error),retract(ecnt(Z)),Z1 is Z+1,assert(ecnt(Z1)),throw(notAtom)),_Error22,(write('Error# ea7:'),writeln(' Fourth argument is not an atom.'))),
            catch((atom(Start_function)->true;assert(error),retract(ecnt(A)),A1 is A+1,assert(ecnt(A1)),throw(notAtom)),_Error23,(write('Error# ea8:'),writeln(' Last argument is not an atom.'))),
			((integer(Port)->(catch((pre1(Port)),_Error54,(write('Error# ea9:'),writeln(' Third argument is not a positive number.')))));chk_err),
			chk_err.	
%-----------------------------------------------------------------------------------------------------------------%
	
chk_err:- 
	   error,retractall(error),
	   chk_ecnt.	   

chk_err.	

chk_ecnt:-
           write('errors found: '),ecnt(Num_err),write(Num_err),nl,retract(ecnt(Num_err)), assert(ecnt(0)),abort.

chk_ecnt.	
%-----------------------------------------------------------------------------------------------------------------%

%-------- Program to access the elements of a list one by one and check whether it is an integer.--------------%
pre(List):-
      length(List,L), 
	  testloop(List,L).	
	  
testloop(_Token_list,0).
testloop(Token_list,N) :- N>0, nth1(N, Token_list, Elem),(integer(Elem)->true;assert(error),retract(ecnt(B)),B1 is B+1,assert(ecnt(B1)),throw(notInt)), M is N-1, testloop(Token_list,M).	  
%---------------------------------------------------------------------------------------------------------------%
% ----------------------To check whether the given port number is positive---------------------------------------%  
 pre1(Port):-
     ((Port>0)->true;assert(error),retract(ecnt(C)),C1 is C+1,assert(ecnt(C1)),throw(notPositive)).
%----------------------------------------------------------------------------------------------------------------%
