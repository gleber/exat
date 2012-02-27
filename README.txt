eXAT
====

This is an authorized fork of original eXAT created by Corrado Santoro and
Francesca Gandemi.

eXAT Release 1.3 "EYE"
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
