395-2012
========

This repo contains the K models as we built them 
in EECS 395 in 2012 at Northwestern.

# Automatic Compilation

To build, type:
   $ make kompile

To run tests (needs PLT racket!), type:
   $ make tests.out

...or simply...
   $ make

To turn on cron job to automate building/testing daemon, type:
   $ make kron-on

To stop cron job:
   $ make kron-off
