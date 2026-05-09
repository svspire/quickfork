
Quicklisp is great, but it's not designed to solve the kind of problems that a team of developers of a lisp product that depends on a lot of Quicklisp libraries have. Namely, that your team needs to maintain positive control over your entire codebase, including open-source libraries written by people outside the team.

When you update to the latest version of Quicklisp, you're really updating the entire suite of Quicklisp libraries. This may or may not be what you want. Sometimes you just need to update a single library and leave the rest alone; Quicklisp has no ability to do this kind of selective updating. Sometimes you need the latest version of a particular library but it hasn't been incorporated into Quicklisp yet. Sometimes you need to fix bugs or add features to a particular library and you may or may not want to push those changes to the original author; it's hard to do this with Quicklisp alone because even though Quicklisp gives you the source code, that source is not under version control.

The solution to all these problems is to find (or create) a source-code management repo for the librarie(s) you need, fork them, clone them, and then use Quicklisp to load them. Quickfork attempts to automate some of this process by producing the git commands or the github cli commands
that you need to execute in your shell to make this happen.

Some of the problems that forking (in the github sense of the word*) addresses are:

1. Quicklisp doesn't guarantee you have the latest versions of its libraries. Sometimes you have to fork your own copy of a library anyway, just to get the latest version.

2. Quicklisp doesn't guarantee your builds won't break when Quicklisp updates its snapshots. Making your own forks solves this problem.

3. Quicklisp doesn't provide a way to make changes to libraries where those changes are private to your own team. You have to make your own forks to solve this problem.

4. Quicklisp doesn't provide a way to send pull requests to the original authors. Again, you have to make your own forks to solve this problem.


All the metadata needed to do this is in the Quicklisp github repos; specifically "quicklisp-projects".

*"forking" here refers to making a complete copy of a repository for purposes of being able to maintain it independently of the original author. This is the github sense of the word. The motivation is not so much to create a brand-new version of the project and take it in a new direction, as it is to create a private branch from which changes can be cleanly integrated with the original using pull requests and modern distributed source code management systems.

TO USE QUICKFORK:

I develop and test in CCL. I haven't attempted to use this in SBCL, but it's pretty basic. It will likely work.

You must (one time) clone the repo: https://github.com/quicklisp/quicklisp-projects
This is the master database of where the repositories of individual quicklisp projects are located. Quickfork needs this database to do its job.

If your local clone is located at "~/quicklisp-projects/", then ensure that qf::*projects-directory* is set to the "projects/" subdirectory of that directory. See *projects-directory* in quickfork.lisp for an example.

Now you can create the git clone commands you'll need to clone all the projects needed for a given system:

(qf:make-clone-commands :bordeaux-threads) ; for example

#'make-clone-commands does NOT execute any commands in your shell or clone anything. It merely builds git clone commands which you can then copy/paste into your shell.

Example:
? (qf:make-clone-commands :rcl "--mirror")

git clone --mirror "https://github.com/sionescu/bordeaux-threads.git"
git clone --mirror "https://github.com/melisgl/named-readtables.git"
git clone --mirror "https://gitlab.common-lisp.net/alexandria/alexandria.git"
git clone --mirror "https://github.com/Shinmera/dissect.git"
git clone --mirror "https://github.com/cl-babel/babel.git"
git clone --mirror "https://github.com/trivial-garbage/trivial-garbage.git"
git clone --mirror "https://github.com/Shinmera/array-utils.git"
git clone --mirror "https://github.com/Shinmera/simple-tasks.git"
git clone --mirror "https://github.com/trivial-features/trivial-features.git"

Non-git dependencies:
("cffi" :HTTPS "https://common-lisp.net/project/cffi/releases/cffi_latest.tar.gz")
("uiop" :HTTPS "https://common-lisp.net/project/asdf/archives/uiop.tar.gz")
("rcl" :HTTPS "https://common-lisp.net/project/rcl/rcl.tar.gz")
NIL
? 

If you can use the github cli gh command to make forks:

First--in a terminal--do this:
$ gh auth login # this sets up the authentication you need to use the github CLI

Then back in Lisp do this:
(qf:make-fork-commands :rcl "--clone --org=Myorg")

gh repo fork "https://github.com/sionescu/bordeaux-threads.git" --clone --org=Myorg 
gh repo fork "https://github.com/cffi/cffi.git" --clone --org=Myorg 
gh repo fork "https://github.com/Shinmera/array-utils.git" --clone --org=Myorg 
gh repo fork "https://github.com/cl-babel/babel.git" --clone --org=Myorg 
gh repo fork "https://github.com/melisgl/named-readtables.git" --clone --org=Myorg 
gh repo fork "https://github.com/trivial-features/trivial-features.git" --clone --org=Myorg 
gh repo fork "https://github.com/trivial-garbage/trivial-garbage.git" --clone --org=Myorg 
gh repo fork "https://gitlab.common-lisp.net/alexandria/alexandria.git" --clone --org=Myorg 
gh repo fork "https://github.com/Shinmera/dissect.git" --clone --org=Myorg 
gh repo fork "https://github.com/Shinmera/simple-tasks.git" --clone --org=Myorg 

Non-git dependencies:
("uiop" :HTTPS "https://asdf.common-lisp.dev/archives/uiop.tar.gz")
("rcl" :HTTPS "https://common-lisp.net/project/rcl/rcl.tar.gz")
NIL


After you have cloned the necessary repos, the next step is to teach asdf (which is used by ql:quickload) to load code from your cloned repos rather than from your local quicklisp distributions. There are many ways to do this; I have a mechanism that works for me which I can document if anyone is interested.
