twipper
-------

A Twitter clone that exposes a hypermedia API

-------

**GET /**
  The main entry point to the API.

  It will contain a **list** of **tweep**.

  It also contains a form to create a **new tweep**

**tweep** extends 
  A tweet contains the properties:

  **text**
    the content of the tweep
  **user**
    the **user** who owns the tweep
  **replies**
  	the **tweep**s that are in reply to this tweep.

forms
=====

**tweep**
  The **tweep** form contains these fields:

  **text**
    the content of the new **tweep**



