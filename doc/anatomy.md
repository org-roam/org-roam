The bulk of Org-roam's functionality is built on top of vanilla
Org-mode features. However, to support additional functionality,
Org-roam adds several Org-roam-specific keywords. This functionality
is not crucial to effective use of Org-roam.

## File Aliases

Suppose you want a note to be referred to by different names (e.g.
"World War 2", "WWII"). You may specify such aliases using the file
`#+ROAM_ALIAS` attribute:

```org
#+TITLE: World War 2
#+ROAM_ALIAS: "WWII" "World War II"
```

