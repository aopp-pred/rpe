=========================
Accessing the Source Code
=========================

If you just want to access the source code, but you don't want to do any development for now, you can follow these instructions.
The :doc:`/developerguide/index` has instructions for those wishing to do development work with the code.


Get a local copy of the code
============================

The source code of the emulator is sotred in a git repository, you therefore need to have git installed to access the code.
Currently the repository is located on the ECMWF Stash server, to clone it use::

    git clone https://software.ecmwf.int/stash/scm/~uka7/rpe.git

This will ask you for your Stash username and password.
You will now have a copy of the code in the new ``rpe`` directory.


Updating the code
=================

from time to time you may want to pull down the latest code. Do this with::

    cd rpe
    git pull

The code in ``rpe`` will now have the latest changes from the initial repository.
