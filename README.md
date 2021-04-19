# ProjectTartarus <img src="https://github.com/tushar-semwal/ProjectTartarus/blob/master/Images/Tartarus.ico" width="80" height="80" /> 
## Overture
As per Greek mythology, Tartarus refers to an oubliette or dungeon, possibly like Hades, where the evil and wicked were sent to be tormented. The name has also been used to refer to a primordial force or deity [Source: Wiki]. In our world, Tartarus refers to a Multi-Agent platform which can take in your problems and churn out solutions. Tartarus is the new version of Typhon and is built using SWI-Prolog. For any queries kindly reach out us tartarus.iitg@gmail.com. You can drop a mail to us after downloading the file : Tartarus<version>.pl, so that we can keep you posted about the updates and revisions.

## Introduction
Written in SWI-Prolog, Tartarus, facilitates users to create overlay sort of network of nodes comprising either a single PC/laptop/embedded system or several such devices connected as a LAN (wired/wireless) and then program both static and mobile agents. Agents in Tartarus are basically programs written in Prolog. They can be programmed to perform tasks autonomously at select nodes and even migrate to others in the network they inhabit. Such agents can even be programmed to clone (copy and multiply) on-the-fly and then move around the network and execute tasks concurrently, providing a distributed and decentralized processing environment. These agents can also carry programs as payloads. Payloads could be written in Prolog or Python and executed at desired nodes. One could try out using other languages as well. Agents can communicate amongst one another and also with programs at a node. As of now, Tartarus can be run on Windows, Ubuntu and Raspbian operating systems.
Tartarus can run on the Raspberry Pi. It can be used to sense the sensors on-board and also control the actuators (motors, relays, etc.) connected to the board.

***Tartarus - Some salient features***

* Multi-threading: Tartarus allows for the threaded execution of agents so that they all function concurrently. These threads are managed by its core engine thereby ridding the developers of comprehending the complexities of implementing mutual exclusion.

* Heterogeneity: Tartarus supports both Windows, Ubuntu and Raspbian and can thus be installed in most hardware which runs these operating systems. Agents running on top of any of these operating systems can communicate with one another providing cross-platform interchange.

* Hardware-in-the-loop: Tartarus can be ported on embedded boards such as the Raspberry Pi. It also comes with an interface for the LEGO MINDSTORM NXT robots. These special interfaces allow developers to access and control the underlying hardware, thus making it easier for a developer to build applications with hardware-in-the-loop.

* Distributed Control: Multi-Agent Systems (MAS) provide for ideal solutions for applications that demand distributed computations and communications among heterogeneous entities. Tartarus agents can share information between nodes, migrate amongst them and even execute the code at a destination/remote node.

* Mobility & Cloning: Agents can also be mobile and can migrate from one node to another in a network they inhabit. They can also clone as and when required contributing to a degree of parallelism in the network.

* Payloads: A mobile agent within Tartarus can carry a set of programs/data as a payload to be either executed at the destination node or be downloaded for use at some node. Agents can also be programmed to either upload/download/offload these payloads at specific nodes or based on an event. Changing a payload can thus facilitate the altering of the behaviour of an agent even in runtime.

* Security: Security implemented for the agents in Tartarus. Mobile agents can enter a node only if they possess a key (provided to them a priori). This key should match with the one in the platform installed at the node; else entry is rejected. Developers can put in a bit of effort to provide for a more sophisticated security arrangement based on their application.

Check out a video on Tartarus here: https://www.youtube.com/watch?v=VeryfhtT5Tk

## What has Tartarus been used for, so far?
Agents provide for distributed, centralized/decentralized and concurrent executions across a network.
They can execute asynchronously and autonomously. At the Robotics Lab. we have used it for a range of application scenarios which include –
Networked Robot control, IoT based control, Indoor Localization, Synchronization, VANETs, Asynchronous Intelligence Sharing across a network, Decentralized Clonal Controller, On-the-fly-Programming, Green Corridors for urban traffic, IoT for Railways, Algorithm selection, Mutual Exclusions in Real Robots, Emulation of Bio-inspired mechanisms such as Pheromoning, Stigmergy, Clonal Selection, Idiotypic Networks, Genetic Algorithms, etc.

## How can Tartarus help in other areas?
A fairly stable version of Tartarus is now also available for the Raspberry Pi. We will be making the same available shortly. This version designated Agents-on-Pi (AgPi) also provides an interface to sensors and actuators on-board the Pi making it easier to program for autonomous decision making. This facilitates rapid prototyping of an intelligent Internet of Things (IoT).
With data distributed across several nodes, agents (both static and mobile) can be programmed to process them in a decentralized and concurrent manner. Mobile agents can be made to move from one node to another and autonomously control the flow of information between such data islands. Bio-inspired (optimizing) algorithms, which are mostly decentralized and distributed, can be emulated, in almost the way Mother Nature does. By injecting new programs via agents, the behavior of a system can also be changed on-the-fly.
Thinking aloud, it could be interesting to develop a network wherein the nodes programmed to behave like atoms/molecules in a liquid act concurrently. This could aid in the study of the ways in which a more complex protein molecule develops using mobile agents enacting as portions of the protein chain. Molecule formation could thus be investigated within a network.
The platform could aid in the making smarter homes, even Greenhouses and campuses.
Simulations could be given up for emulations in Tartarus based networks to churn better and more realistic data in decentralized and distributed scenarios.
We leave it to you to think louder (!) and get back to the Tartarus team, if required, with the problems you need to tackle. We shall do our best to aid you in making this demon help you find a solution.

**To Get Started**, check the [WiKi](https://github.com/roboticslab-cseiitg/ProjectTartarus/wiki) page.

## One last yet important note
Tartarus culminated from years of work and yet we offer the same to you free. It is thus only logical that you cite the following papers when you publish your research derived using this software:

Tushar Semwal, Nikhil S., Shashi Shekhar Jha, Shivashankar B. Nair, TARTARUS: A Multi Agent Platform for Bridging the gap between Cyber and Physical Systems, *Proceedings of the 2016 Autonomous Agents and Multi-Agent Systems Conference, (AAMAS), International Foundation for Autonomous Agents and Multiagent Systems*, pp. 1493-1495, Singapore.

Tushar Semwal, Shivashankar B. Nair, AgPi: Agents on Raspberry Pi, *Electronics 2016, 5, 72*.

Cheers,
The Tartarus Team
