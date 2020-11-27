node2:-                                           % Merely consults the Tartarus platform file and initiates a Tartarus paltform.
  consult('platform.pl'),
  start_tartarus(localhost,50001,1111),nl,
  writeln('BTW: This is the Node-2 of the network').
