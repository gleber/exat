eXAT
====

This is an authorized fork of original eXAT created by Corrado Santoro and
Francesca Gandemi.

Version 1.5.0 has the following big changes in comparison to original
eXAT:
- internal http server has been replaced with Misultin (which in turn
  will be replaced with Cowboy or Mochiweb, since it got discontinued )
- Eresye has been replaced with Seresye
- agent.erl has been replaced with much simpler implementation
  closely following OTP standards
- object.erl, multisync.erl and all OOP-like stuff has been dropped

eXAT Release 1.5.0
======================


1. Compiling eXAT

   $ make deps
   $ make

   eXAT now will be compiled.

2. Testing eXAT

   You can test eXAT by using the "simple_pingagent" and
   "simple_pingeragent" provided in examples/simple. Do the following
   steps:

   a) Check that TCP ports 7779 and 7780 are not used in your PC;

   b) $ cd examples/simple
      $ make
      
   b) From one shell issue

   $ ./run_first.sh
   
   c) From another shell run:

   $ ./run_second.sh

   Now you'll see, in simple_pingerpinger shell, that he can receive
   messages from simple_pingagent.
