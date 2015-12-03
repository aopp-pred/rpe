===================
Git for Development
===================

This section describes the typical git workflow for development of the project.
It covers both how to use git to achieve the required results, and the general version control strategy for the project.


Setting up a development fork
=============================

Create your own forked copy of the emulator
-------------------------------------------

#. Log in to your `Gitlab`_ account.
#. Go to the `rpe`_ repository page.
#. Click the fork button at the centre of the page, and on the next page click your Gitlab user name.


Clone your fork
---------------

#. Clone your fork to the local computer with::

    git clone git@gitlab.physics.ox.ac.uk:your-user-name/rpe.git

#. Investigate. Change directory to your new repo: ``cd rpe``. Then ``git branch -a`` to show you all branches. You’ll get something like::

        * master
        remotes/origin/master

   This tells you that you are currently on the ``master`` branch, and that you also have a ``remote`` connection to ``origin/master``.
   What remote repository is ``remote/origin``?
   Try ``git remote -v`` to see the URLs for the remote.
   They will point to your Gitlab fork.

   Now you want to connect to the upstream `rpe`_ repository, so you can merge in changes from trunk.


.. _linking-to-upstream:

Linking your repository to the upstream repository
--------------------------------------------------

::

    cd rpe
    git remote add upstream git@gitlab.physics.ox.ac.uk:aopp-pred/rpe.git

``upstream`` here is just the arbitrary name we're using to refer to the main `rpe`_ repository.

The upstream repository is read-only access, which means you can't accidentally (or deliberately) write to the upstream repository, you only use it to merge into your own code.

You can list your remote repositories with ``git remote -v``, which gives you something like this::

    upstream    git@gitlab.physics.ox.ac.uk:aopp-pred/rpe.git (fetch)
    upstream    git@gitlab.physics.ox.ac.uk:aopp-pred/rpe.git (push)
    origin      git@gitlab.physics.ox.ac.uk:your-user-name/rpe.git (fetch)
    origin      git@gitlab.physics.ox.ac.uk:your-user-name/rpe.git (push)


Tell git who you are
--------------------

It is good practice to tell git who you are, for labeling any changes you make to the code. The simplest way to do this is from the command line::

    git config --global user.name "Your Name"
    git config --global user.email you@yourdomain.example.com

This will write the settings into your git configuration file.
Make sure that the email address you use is one that Gitlab knows about.
You can either use the default address (your @physics.ox.ac.uk address) or add other email addresses you want to use via the Gitlab settings page.


Development workflow
====================

Summary
-------

* Don’t use your master branch for anything.
* When you are starting a new set of changes, fetch any changes from upstream trunk, and start a new feature branch from that.
* Make a new branch for each separable set of changes - "one task, one branch".
* Name your branch for the purpose of the changes - e.g. bugfix-for-issue-14 or refactor-database-code.
* If you can possibly avoid it, avoid merging trunk or any other branches into your feature branch while you are working.
* If you do find yourself needing to merge from trunk, consider Rebasing on trunk instead.
* Ask for code review by submitting a pull request!


.. _update-mirror-trunk:

Update your mirror of trunk
---------------------------

First make sure you have done :ref:`linking-to-upstream`.

From time to time, and always before creating a new feature branch, you should fetch the upstream (trunk)::

    git fetch upstream

This will pull down any commits you don’t have, and set the remote branches to point to the right commit.
For example, 'trunk' is the branch referred to by (remote/branchname) upstream/master - and if there have been commits since you last checked, upstream/master will change after you do the fetch.


.. _make-feature-branch:

Make a new feature branch
-------------------------

When you are ready to make some changes to the code, you should start a new branch.
Branches that are for a collection of related edits are often called 'feature branches'.

Making an new branch for each set of related changes will make it easier for someone reviewing your branch to see what you are doing.

Choose an informative name for the branch to remind yourself and the rest of us what the changes in the branch are for.
For example ``add-ability-to-fly``, or ``buxfix-for-issue-42``.

::

    # Update the mirror of trunk
    git fetch upstream
    # Make a new feature branch starting at current trunk
    git checkout -b my-new-feature upstream/master

Generally, you will want to keep your feature branches on your fork of `rpe`_.
To do this, you `git push`_ this new branch up to your Gitlab repository.
Generally (if you followed the instructions in these pages, and by default), git will have a link to your Gitlab repository, called ``origin``.
You push up to your own repo on Gitlab with::

    git push origin my-new-feature

In git >= 1.7 you can ensure that the link is correctly set by using the ``--set-upstream`` option::

   git push --set-upstream origin my-new-feature

From now on git will know that ``my-new-feature`` is related to the ``my-new-feature`` branch in the Gitlab repository.


Making changes
--------------

#. Make some changes on your feature branch.

#. See which files have changed with ``git status``. You'll see a listing like this one::

        # On branch my-new-feature
        # Changed but not updated:
        #   (use "git add <file>..." to update what will be committed)
        #   (use "git checkout -- <file>..." to discard changes in working directory)
        #
        #  modified:   src/rp_emulator.F90
        #
        # Untracked files:
        #   (use "git add <file>..." to include in what will be committed)
        #
        #  tests/unit/new-suite
        no changes added to commit (use "git add" and/or "git commit -a")

#. Check what the actual changes are with ``git diff`` (`git diff`_).

#. Add any new files to version control ``git add new_file_name`` (see `git add`_).

#. To commit all modified files into the local copy of your repository, do ``git commit -a``.
   Note the ``-a`` option to ``commit``, which automatically performs `git add` on all modified files.
   You may prefer to manually add the files you wish to commit and then just use plain ``git commit``.
   The latter method is useful if you want to make a commit out of only some of the modified files (this is quite normal).

#. To push the changes up to your forked repository on Gitlab, do a ``git push`` (see `git push`_).


Ask for your changes to be reviewed or merged
---------------------------------------------

When you are ready to ask for someone to review your code and consider a merge:

#. Go to the URL of your forked repo, say ``https://gitlab.physics.ox.ac.uk/your-user-name/rpe.git``.

#. Click the plus sign button on the row of action buttons under the repository name on the main page, then click *New merge request* in the menu that pops up.

#. Use the drop-down boxes to select the branch you wish to be reviewed under the *Source branch* section, and the branch that this feature should be merged into (usually master) under the *Target branch* section.

#. Click *Compare branches*.

#. Enter a title for the set of changes, and some explanation of what you've done.
   Say if there is anything you'd like particular attention for - like a complicated change or some code you are not happy with or are unsure about.

   If you don't think your request is ready to be merged, just say so in your pull request message.
   This is still a good way of getting some preliminary code review.

   You can also select reviewers for your change.
   Doing so will send them a notification that you have submitted a pull request, so it is a good idea to name at least 1 reviewer in order to get your request reviewed in a timely manner.

#. When you are happy that you have entered all the required information you can click the *Submit new merge request* button.

.. note::

   Before submitting a pull request you should first check that all the tests pass locally yourself.
   This will save the reviewer from having to point out that your shiny new feature breaks something!


Some other things you might want to do
======================================


Delete a branch on Gitlab
-------------------------

::

   git checkout master
   # delete branch locally
   git branch -D my-unwanted-branch
   # delete branch on Gitlab
   git push origin :my-unwanted-branch

Note the colon ``:`` before ``my-unwanted-branch``.


Explore your repository
-----------------------

To see a graphical representation of the repository branches and
commits::

   gitk --all

To see a linear list of commits for this branch::

   git log


.. _rebase-on-trunk:

Rebasing on trunk
-----------------

Let's say you thought of some work you'd like to do.
You :ref:`update-mirror-trunk` and :ref:`make-feature-branch` called ``cool-feature``.
At this stage trunk is at some commit, let's call it E.
Now you make some new commits on your ``cool-feature`` branch, let's call them A, B, C.
Maybe your changes take a while, or you come back to them after a while.
In the meantime, trunk has progressed from commit E to commit (say) G::

          A---B---C cool-feature
         /
    D---E---F---G trunk

At this stage you consider merging trunk into your feature branch, and you remember that this here page sternly advises you not to do that, because the history will get messy.
Most of the time you can just ask for a review, and not worry that trunk has got a little ahead.  But sometimes, the chan
But sometimes, the changes in trunk might affect your changes, and you need to harmonize them.
In this situation you may prefer to do a rebase.

Rebase takes your changes (A, B, C) and replays them as if they had been made to the current state of ``trunk``.
In other words, in this case, it takes the changes represented by A, B, C and replays them on top of G.
After the rebase, your history will look like this::

                  A'--B'--C' cool-feature
                 /
    D---E---F---G trunk

See `rebase without tears`_ for more detail.

To do a rebase on trunk::

    # Update the mirror of trunk
    git fetch upstream
    # go to the feature branch
    git checkout cool-feature
    # make a backup in case you mess up
    git branch tmp cool-feature
    # rebase cool-feature onto trunk
    git rebase --onto upstream/master upstream/master cool-feature

In this situation, where you are already on branch ``cool-feature``, the last command can be written more succinctly as::

    git rebase upstream/master

When all looks good you can delete your backup branch::

   git branch -D tmp

If it doesn't look good you may need to have a look at :ref:`recovering-from-mess-up`.

If you have made changes to files that have also changed in trunk, this may generate merge conflicts that you need to resolve - see the `git rebase`_ man page for some instructions at the end of the "Description" section.
There is some related help on merging in the git user manual - see `resolving a merge`_.


.. _recovering-from-mess-up:

Recovering from mess-ups
------------------------

Sometimes, you mess up merges or rebases.
Luckily, in git it is relatively straightforward to recover from such mistakes.

If you mess up during a rebase::

   git rebase --abort

If you notice you messed up after the rebase::

   # reset branch back to the saved point
   git reset --hard tmp

If you forgot to make a backup branch::

   # look at the reflog of the branch
   git reflog show cool-feature

   8630830 cool-feature@{0}: commit: BUG: io: close file handles immediately
   278dd2a cool-feature@{1}: rebase finished: refs/heads/my-feature-branch onto 11ee694744f2552d
   26aa21a cool-feature@{2}: commit: BUG: lib: make seek_gzip_factory not leak gzip obj
   ...

   # reset the branch to where it was before the botched rebase
   git reset --hard cool-feature@{2}

.. _rewriting-commit-history:


Rewriting commit history
------------------------

.. note::

   Do this only for your own feature branches.

There's an embarassing typo in a commit you made?
Or perhaps the you made several false starts you would like the posterity not to see.

This can be done via *interactive rebasing*.

Suppose that the commit history looks like this::

    git log --oneline
    eadc391 Fix some remaining bugs
    a815645 Modify it so that it works
    2dec1ac Fix a few bugs + disable
    13d7934 First implementation
    6ad92e5 * masked is now an instance of a new object, MaskedConstant
    29001ed Add pre-nep for a copule of structured_array_extensions.
    ...

and ``6ad92e5`` is the last commit in the ``cool-feature`` branch.
Suppose we want to make the following changes:

* Rewrite the commit message for ``13d7934`` to something more sensible.
* Combine the commits ``2dec1ac``, ``a815645``, ``eadc391`` into a single one.

We do as follows::

    # make a backup of the current state
    git branch tmp HEAD
    # interactive rebase
    git rebase -i 6ad92e5

This will open an editor with the following text in it::

    pick 13d7934 First implementation
    pick 2dec1ac Fix a few bugs + disable
    pick a815645 Modify it so that it works
    pick eadc391 Fix some remaining bugs

    # Rebase 6ad92e5..eadc391 onto 6ad92e5
    #
    # Commands:
    #  p, pick = use commit
    #  r, reword = use commit, but edit the commit message
    #  e, edit = use commit, but stop for amending
    #  s, squash = use commit, but meld into previous commit
    #  f, fixup = like "squash", but discard this commit's log message
    #
    # If you remove a line here THAT COMMIT WILL BE LOST.
    # However, if you remove everything, the rebase will be aborted.
    #

To achieve what we want, we will make the following changes to it::

    r 13d7934 First implementation
    pick 2dec1ac Fix a few bugs + disable
    f a815645 Modify it so that it works
    f eadc391 Fix some remaining bugs

This means that (i) we want to edit the commit message for ``13d7934``, and (ii) collapse the last three commits into one.
Now we save and quit the editor.

Git will then immediately bring up an editor for editing the commit message.
After revising it, we get the output::

    [detached HEAD 721fc64] FOO: First implementation
     2 files changed, 199 insertions(+), 66 deletions(-)
    [detached HEAD 0f22701] Fix a few bugs + disable
     1 files changed, 79 insertions(+), 61 deletions(-)
    Successfully rebased and updated refs/heads/my-feature-branch.

and the history looks now like this::

     0f22701 Fix a few bugs + disable
     721fc64 ENH: Sophisticated feature
     6ad92e5 * masked is now an instance of a new object, MaskedConstant

If it went wrong, recovery is again possible as explained :ref:`above <recovering-from-mess-up>`.

.. include:: git_links.inc

.. _`rpe`: https://gitlab.physics.ox.ac.uk/aopp-pred/rpe
.. _`Gitlab`: https://gitlab.physics.ox.ac.uk
