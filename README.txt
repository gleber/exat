
eXAT Release 1.3 "EYE"
======================


1. Unpacking and Compiling eXAT

   "cd" to your local dir where you want to install eXAT and issue the
command:

   $ tar zxf <put the pathname of 'eXAT-1.3.tar.gz' here>

   A directory eXAT-1.2 will be created. Then issue the following commands

   $ cd eXAT-1.3
   $ gmake

   eXAT now will be compiled.


2. Testing eXAT

   You can test eXAT by using the "pingagent" and "pingeragent" provided in
the package (see sub-dir "src"). To this aim, use the following steps:


   a) Check that TCP ports 7779 and 7780 are not used in your PC;
   b) From the "eXAT-1.3" directory, launch the "pingagent" by using the
   following command (this will start the reponder agent):

   $ ./exat.sh platform1 -start pingagent

   The output will be as follows:

(platform@csanto)1>
              __      __    __    _______
              \ \    / /   /  \  |__   __|
        ____   \ \  / /   / /\ \    | |
       / __ \   \ \/ /   | |__| |   | |
      | ____/   / /\ \   |  __  |   | |
      | \____  / /  \ \  | |  | |   | |
       \____/ /_/    \_\ |_|  |_|   |_|
*****                                     *****************
* The erlang eXperimental Agent Tool -- Release 1.3.0-EYE *
***********************************************************
eXAT, an erlang eXperimental Agent Tool
ERESYE, an ERlang Expert SYstem Engine
Copyright (C) 2003-07 Corrado Santoro (csanto@diit.unict.it)
Copyright (C) 2005-07 Francesca Gangemi (francesca@erlang-consulting.com)

['HTTP-MTP'] MTP Started at port 7779
['ONTOLOGY'] Started
['AMS'] Staring AMS.
['ONTOLOGY'] Registering Codec fipa_ontology_sl_codec for ontology FIPA-Agent-Management
['AMS'] Staring Applications: {pingagent}


    c) From another shell, reach eXAT-1.3 dir and issue the following
command (this will start the ping initiator agent):

    $ ./exat.sh plaftform2 -http_port 7780 -start pingeragent -destagent pingagent,http://localhost:7779/acc

    Now you'll see, in both shells, the messages flowing from one agent to
the other one.

--- END OF README --

