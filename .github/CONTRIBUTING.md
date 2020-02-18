# Contributing

If you discover issues, have ideas for improvements or new features, please
report them to the [issue tracker][1] of the repository or submit a pull
request. Please, try to follow these guidelines when you do so.

## Issue reporting

* Check that the issue has not already been reported.
* Check that the issue has not already been fixed in the latest code
  (a.k.a. `develop`).
* Be clear, concise and precise in your description of the problem.
* Open an issue with a descriptive title and a summary in grammatically correct,
  complete sentences.
* Include any relevant code to the issue summary.
* If you're reporting performance issues it'd be nice if you added some profiling data (Emacs has a built-in profiler).

## Pull requests

* Read [how to properly contribute to open source projects on Github][2].
* Use a topic branch to easily amend a pull request later, if necessary.
* Write [good commit messages][3].
* Mention related tickets in the commit messages (e.g. `[Fix #N] Add missing autoload cookies`)
* Update the [changelog][5].
* Use the same coding conventions as the rest of the project.
* Verify your Emacs Lisp code with `checkdoc` (<kbd>C-c ? d</kbd>).
* Open a [pull request][4] that relates to *only* one subject with a clear title
  and description in grammatically correct, complete sentences.

[1]: https://github.com/jethrokuan/org-roam/issues
[2]: http://gun.io/blog/how-to-github-fork-branch-and-pull-request
[3]: http://tbaggery.com/2008/04/19/a-note-about-git-commit-messages.html
[4]: https://help.github.com/articles/using-pull-requests
[5]: https://github.com/jethrokuan/org-roam/blob/master/CHANGELOG.md
