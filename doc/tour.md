Org-roam was built to support a workflow that was not possible with
vanilla Org-mode. This flow is modelled after the [Zettelkasten
method][zettelkasten], and many of [Roam Research][roam]'s workflows.
It is crucial to understand that Org-roam does not auto-magically make
note-taking better -- it's changing the note-taking workflow that
does.

To understand more about the methods and madness, the [Note-Taking
Workflow][appendix:ntw] page contains a page of useful references. The
author has also written [a post][jethro-blog-post] about how he uses
Org-roam.

## Activating Org-roam

Org-roam's entry point is the global minor `org-roam-mode`. This sets
up Emacs with several hooks, for keeping the org-roam cache
consistently updated, as well as showing the backlinks buffer. 

The cache is a sqlite database named `org-roam.db`, which resides at
the root of the `org-roam-directory`. Activating `org-roam-mode`
builds the cache, which may take a while the first time, but is
generally instantaneous in subsequent runs. To build the cache
manually again, run `M-x org-roam-build-cache`.

## Finding a Note

`org-roam-find-file` shows the list of titles for notes that reside in
`org-roam-directory`. Selecting a note title will bring you to the
corresponding note. Entering a title of a note that does not yet exist
will create a new note with that title.

![org-roam-find-file](images/org-roam-find-file.gif)

## Inserting Links

`org-roam-insert` insert links to existing (or new) notes. Entering a
non-existent title will also create a new note with that title.

![org-roam-insert](images/org-roam-insert-filetag.gif)

Good usage of Org-roam requires liberally linking files. This allows
the build-up of a dense knowledge graph.

## The Org-roam Buffer

All of Org-roam's operations are designed such that the built cache is
a consistent view of the inter-connectivity between your notes. The
Org-roam buffer shows backlinks: i.e. the files that link to the
currently viewed file, along with some surrounding context. The
Org-roam buffer will always show the backlinks for the current
Org-roam file in view.

![org-roam-buffer](images/org-roam-buffer.gif)

## Exporting the Graph

It's also possible to export the links as a graph, using graphviz. The
generated graph is navigable in Emacs, but requires some additional
setup, which I describe in the [Graph Appendix][appendix:graph-setup]
page.

![org-roam-graph](images/org-roam-graph.gif)

[zettelkasten]: https://zettelkasten.de/
[appendix:ntw]: notetaking_workflow.md
[appendix:graph-setup]: graph_setup.md
[roam]: https://www.roamresearch.com/
[jethro-blog-post]: https://blog.jethro.dev/posts/how_to_take_smart_notes_org/
