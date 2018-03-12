
Quicklisp is great, but it's not designed to solve the kind of problems that a team of developers of a lisp product that depends on a lot of Quicklisp libraries have. Namely, that your team needs to maintain positive control over your entire codebase, including open-source libraries written by people outside the team.

When you update to the latest version of Quicklisp, you're really updating the entire suite of Quicklisp libraries. This may or may not be what you want. Sometimes you just need to update a single library and leave the rest alone; Quicklisp has no ability to do this kind of selective updating. Sometimes you need the latest version of a particular library but it hasn't been incorporated into Quicklisp yet. Sometimes you need to fix bugs or add features to a particular library and you may or may not want to push those changes to the original author; it's hard to do this with Quicklisp alone because even though Quicklisp gives you the source code, that source is not under version control.

The solution to all these problems is to find (or create) a source-code management repo for the librarie(s) you need, fork them, clone them, and then use Quicklisp to load them. Quickfork attempts to automate some of this process.

Some of the problems that forking (in the github sense of the word*) addresses are:

1. Quicklisp doesn't guarantee you have the latest versions of its libraries. Sometimes you have to fork your own copy of a library anyway, just to get the latest version.

2. Quicklisp doesn't guarantee your builds won't break when Quicklisp updates its snapshots. Making your own forks solves this problem.

3. Quicklisp doesn't provide a way to make changes to libraries where those changes are private to your own team. You have to make your own forks to solve this problem.

4. Quicklisp doesn't provide a way to send pull requests to the original authors. Again, you have to make your own forks to solve this problem.


All the metadata needed to do this is in the Quicklisp github repos.

*"forking" here refers to making a complete copy of a repository for purposes of being able to maintain it independently of the original author. This is the github sense of the word. The motivation is not so much to create a brand-new version of the project and take it in a new direction, as it is to create a private branch from which changes can be cleanly integrated with the original using pull requests and modern distributed source code management systems.
