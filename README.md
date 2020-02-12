[![Documentation Status](https://readthedocs.org/projects/org-roam/badge/?version=latest)](https://org-roam.readthedocs.io/en/latest/?badge=latest)

Org-roam is a rudimentary [Roam][roamresearch] replica built around
the all-powerful [Org-mode][org]. 

Like Roam, Org-roam offers a powerful and effortless non-hierarchical
note-taking approach. With Org-roam, notes flow naturally, making
note-taking fun and easy. 

The goal of the project is to implement core features of Roam around
Org-mode, and eventually introduce features enabled by the Emacs
ecosystem. 

For more documentation, see [the documentation page](https://org-roam.readthedocs.io/en/latest/).

## Understanding Roam

To understand more about Roam, I recommend the following links:

-   [Building a second brain in
    Roam](https://reddit.com/r/RoamResearch/comments/eho7de/building_a_second_brain_in_roamand_why_you_might)
-   [Roam: Why I Love It and How I Use
    It](https://www.nateliason.com/blog/roam)

## Project Status

As of February 2020, it is in a very early stage of development. 

## A Preview

Here's a screenshot of `org-roam`. The `org-roam` buffer shows
backlinks for the active org buffer in the left window, as well as the
surrounding content in the backlink file. The backlink database is
built asynchronously in the background, and is not noticeable to the
end user. The graph is generated from the link structure, and can be
used to navigate to the respective files.

![img](doc/images/org-roam-graph.gif)

## Knowledge Bases using Org-Roam

- [Jethro Kuan](https://braindump.jethro.dev/)
  ([Source](https://github.com/jethrokuan/braindump/tree/master/org))

## Contributing

To report bugs and suggest new feature use the issue tracker. If you
have some code which you would like to be merged, then open a pull
request. Please also see CONTRIBUTING.md.

[roamresearch]: https://www.roamresearch.com/
[org]: https://orgmode.org/
