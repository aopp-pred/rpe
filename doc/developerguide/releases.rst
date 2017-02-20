================
Making a release
================

This is a short guide on how to make a new release of the rpe library.
There are several stages to making a feature release which are outlined below.
Bugfix releases are simpler and don't require making a new branch, the first
step is only relevant to feature releases.
Making a release requires developer access to the rpe repository.


Cutting a release branch
========================

The first step is to create a branch for a feature release.
Each major and minor release version gets its own branch, but bugfixes releases
do not (kind of obvious when you think about it but worth saying anyway).
Release branches should come off the master branch when you are ready to start
preparing the release.
Release branches should be named ``v{major}.{minor}.x``, so the release branch
for version 6.0 would be ``v6.0.x`` and for version 6.1 it would be ``v6.1.x``.
The ``x`` in the version number represents the fact that all bugfix releases
will come from the same branch, for example the ``v6.1.x`` branch will be
created for ``v6.1.0`` but will also be used for ``v6.1.1`` if it is required,
along with any further bugfix releases.
The release branch should be pushed to the github organisation (assumed to be
the remote named upstream)::

    git push upstream v{major}.{minor}.x

Once the release branch has been cut it is a good time to update the ``VERSION``
file on the **master** branch.
You should set it to the major and minor numbers of the anticipated next release
after the one you are making (it doesn't matter if plans change later), with the
``dev0`` suffix (e.g., for next release 6.2 use ``v6.2.dev0``).
Doing this ensures that people contributing to the master branch know which
release their work will likely end up in.


Set version and tagging
=======================

The release branch should receive only minor changes (no new features, those
should have been committed to the master branch before the release branch was
cut).
Once you have all the changes required for the release you should set the
version number in the file ``VERSION`` in the repository root.
The version for a feature release will be ``v{major}.{minor}.0``.
Commit the changes to the version file.
Push the changes to the release branch (usually via pull request).

Once you have set the version you should make a tag.
The tag will be used to indentify the commit where the release was made, and
will be used to contruct a release on Github.
Tagging should use the following command::

    git tag -a v{major}.{minor}.{bugfix} -m "Version {major}.{minor}.{bugfix}"

You can now push the tag to the upstream::

    git push --tags upstream

You should now reset the version number on the release branch to the next dev
version.
For example, if you just tagged v6.1.0, you should set the version in the
file ``VERSION`` to ``v6.1.dev1``.
Commit this change and push it to the release branch (usually via PR).


Making a Github release
=======================

Once you have pushed the tag you can go to the Github repository, click the
releases icon in the repository summary, and click new release.
Type the name of the tag you made into the box for release name and Github will
automatically create the release from the existing tag.
Write a summary of the release in the text box (a simple changelog is suggested)
and create the release.
Creating a release will automatically trigger a new build of the documentation
on readthedocs and a new Zenodo entry.


Merge-back changes
==================

After you have finalised the release you should merge the release branch back
into the master branch.
This should be done via a pull request.
The way to do it is create a new branch locally, do a non-fast-forward merge of
the release branch into it, fixing any merge conflicts, then make a PR from
this branch into master::

    git fetch upstream
    git checkout -b mergeback-v{major}.{minor}.x upstream/master
    git merge --no-ff upstream v{major}.{minor}.x
    # Fix merge conflicts, the file VERSION will conflict, there may be others

Merging back should be done after all releases, including bugfix releases
