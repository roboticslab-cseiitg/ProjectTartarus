# Programs to get started with Tartarus

This repo contains few of the programs to demonstrate the features of tartarus and to get started with Tartarus.

Visit [here](https://github.com/roboticslab-cseiitg/ProjectTartarus) and get the platform.pl file to run these Tartarus programs. The User manual for Tartrus can also be found in the same link.

------------------------------------------------------------------
### TO INSTANTIATE A TARTARUS NODE:

1. OPEN SWI PROLOG WINDOW (search "swi-prolog" in the search bar in windows start)
2. Go to file -> consult -> select the "platform.pl" file

OR

1. Double click on platform.pl and open with "swi-prolog"

-> type 
		start_tartarus(localhost,60000,1).
   in the prompt of swi prolog

-> node is instantiated on port 60000

REFER TARTARUS MANUAL FOR MORE DETAILS

NOTE: THE "platform.pl" IN THE FOLDER TARTARUS MATERIAL IS FOR THE WINDOWS OS.
		IN THE FOLDER "Other Platforms", there are platform files for the OS Ubuntu and Raspberry Pi.
*********************************************************************************

### PROGRAM : Program to demonstrate simple agent movement from one node to another
#### RELEVANT FILES : agent_movement.pl ; agent_movement_node2.pl

1. Consult the file agent_movement_node2.pl and run the predicate node2.
2. Consult the file agent_movement.pl and run the predicate node1.

Observe the movement of agent from node 1 to node 2

**********************************************************************************

### PROGRAM : Program to demonstrate simple agent (with payload) movement from one node to another
#### RELEVANT FILES : agent_payload.pl ; agent_payload_node2.pl

1. Consult the file agent_payload_node2.pl and run the predicate node2.
2. Consult the file agent_payload.pl and run the predicate node1.

Observe the movement of agent from node 1 to node 2 with the payload.

**********************************************************************************

### PROGRAM : Program to demonstrate cloning of an agent
#### RELEVANT FILES : cloning_agent.pl ; cloning_node2.pl

1. Consult the file cloning_agent.pl and run the predicate node1.pl (this is node1)
2. Consult the file cloning_node2.pl and run the predicate node2.pl
3. from node1 run the predicate agent_init.

***********************************************************************************

### PROGRAM : Posting a message from one node to another
#### RELEVANT FILES : post_agent_example_node1.pl, post_agent_example_node2.pl

1. Consult the post_agent_example_node1.pl file and run the predicate node1 (this is node 1)
2. Consult the post_agent_example_node2.pl file and run the predicate node2 (this is node 2)
3. Run the predicate post_msg from node 1 
4. Message will be posted from node 1 to node 2

***********************************************************************************

### PROGRAM : To get the synonyms and antonyms of the word entered
#### RELEVANT FILES : syno_anto_node1.pl ; syno_anto_node2.pl ; syno_anto_node3.pl

1. Run the predicates node1, node2 and node3 from the files syno_anto_node1.pl, syno_anto_node2.pl and syno_anto_node3.pl respectively in three separate swi-prolog windows.

2. Run agent_init from the port 7777 i.e., node1 (file syno_anto_node1.pl).

3. Enter the word whose synonyms and antonyms are to be fetched (current words : "intelligence" and "kind").

************************************************************************************
